-module(eval).
-export([evalexp/2]).
-export([lookup/2, bind/3, extend/2]).

-define(PLUS, {sym, '+'}).
-define(IF, {sym, 'if'}).
-define(LAMBDA, {sym, lambda}).

%% lookup(Name, []) -> {error, {unbound_variable, Name}};
lookup(Name, []) -> erlang:error({unbound_variable, Name});
lookup(Name, [{K, V}|_T]) when Name == K -> V;
lookup(Name, [{K, _V}|T]) when Name =/= K -> lookup(Name, T).

bind(Name, Val, Env) -> [{Name, Val}|Env].

extend([], Env) -> Env;
extend([{Name, Val}|T], Env) -> bind(Name, Val, extend(T, Env)).

evalexp({int, Val}, Env) -> {{int, Val}, Env};

evalexp({bool, Val}, Env) -> {{bool, Val}, Env};

evalexp({quote, QuotedExp}, Env) -> {QuotedExp, Env};

evalexp({sym, Name}, Env) -> {lookup(Name, Env), Env};

evalexp({letstar, [], Body}, Env) ->
    evalexp(Body, Env);
evalexp({letstar, [{{sym, Name}, Exp}|T], Body}, Env) ->
    {Val, _} = evalexp(Exp, Env),
    evalexp({letstar, T, Body}, bind(Name, Val, Env));

evalexp({letv, Bindings, Body}, Env) ->
    BoundVars = lists:map(fun ({{sym, Name}, Exp}) ->
                                  {Val, _} = evalexp(Exp, Env),
                                  {Name, Val}
                          end, Bindings), 
    NewEnv = extend(BoundVars, Env),
    evalexp(Body, NewEnv);

evalexp({list, []}, Env) -> {{list, []}, Env};
evalexp({list, [?PLUS|[]]}, Env) -> {{int, 0}, Env};
evalexp({list, [?PLUS|[H|T]]}, Env) ->
    {{int, HV}, _} = evalexp(H, Env),
    {{int, TV}, _} = evalexp({list, [?PLUS|T]}, Env),
    {{int, HV+TV}, Env};

evalexp({list, [?IF, Cond, E1, E2]}, Env) ->
    case evalexp(Cond, Env) of
        {bool, true} -> evalexp(E1, Env);
        {bool, false} -> evalexp(E2, Env);
        _ -> {{error, bad_if}, Env}
    end;

evalexp({list, [?LAMBDA, _VarList, _Body]}, Env) ->
    {{ok, lambda}, Env};

evalexp([], Env) -> {ok, Env};
evalexp([E|[]], Env) -> evalexp(E, Env);
evalexp([FirstExp|RestExps], Env) ->
    {_V, Env} = evalexp(FirstExp, []),
    evalexp(RestExps, Env).
