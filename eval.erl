-module(eval).
-export([evalexp/2]).
-export([lookup/2, bind/3]).

-define(PLUS, {sym, '+'}).
-define(IF, {sym, 'if'}).

lookup(Name, [{K, V}|_T]) when Name == K -> V;
lookup(Name, [{K, _V}|T]) when Name =/= K -> lookup(Name, T).

bind(Name, Val, Env) -> [{Name, Val}|Env].

evalexp({int, Val}, _Env) -> {int, Val};

evalexp({bool, Val}, _Env) -> {bool, Val};

evalexp({quote, QuotedExp}, _Env) -> QuotedExp;

evalexp({sym, Name}, Env) -> lookup(Name, Env);

evalexp({letv, [], Exp}, Env) -> evalexp(Exp, Env);
evalexp({letv, [{{sym, K}, V}|T], Exp}, Env) ->
    evalexp({letv, T, Exp}, bind(K, V, Env));

evalexp({list, []}, _Env) -> {list, []};
evalexp({list, [?PLUS|[]]}, _Env) -> {int, 0};
evalexp({list, [?PLUS|[H|T]]}, Env) ->
    {int, HV} = evalexp(H, Env),
    {int, TV} = evalexp({list, [?PLUS|T]}, Env),
    {int, HV+TV};

evalexp({list, [?IF, Cond, E1, E2]}, Env) ->
    case evalexp(Cond, Env) of
        {bool, true} -> evalexp(E1, Env);
        {bool, false} -> evalexp(E2, Env)
    end.
