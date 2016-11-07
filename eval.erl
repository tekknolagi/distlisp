-module(eval).
-export([evalexp/2]).
-export([lookup/2, bind/3, extend/2]).
-export([printexp/1]).

-define(PLUS, {sym, '+'}).
-define(IF, {sym, 'if'}).
-define(LAMBDA, {sym, lambda}).
-define(QUOTE, {sym, quote}).
-define(LET, {sym, 'let'}).
-define(LETSTAR, {sym, 'let*'}).
-define(DEFINE, {sym, 'define'}).

lookup(Name, []) -> erlang:error({unbound_variable, Name});
lookup(Name, [{K, V}|_T]) when Name == K -> V;
lookup(Name, [{K, _V}|T]) when Name =/= K -> lookup(Name, T).


bind(Name, Val, Env) -> [{Name, Val}|Env].


extend([], Env) -> Env;
extend([{Name, Val}|T], Env) -> bind(Name, Val, extend(T, Env)).


printlist([]) -> io:format("", []);
printlist([E]) -> printexp(E);
printlist([H|T]) ->
    printexp(H),
    io:format(" "),
    printlist(T).


printexp({int, Val}) -> io:format("~p", [Val]);
printexp({sym, Val}) -> io:format("~p", [Val]);
printexp({bool, true}) -> io:format("#t", []);
printexp({bool, false}) -> io:format("#f", []);
printexp({quote, QuotedExp}) ->
    io:format("'", []),
    printexp(QuotedExp);
printexp({list, L}) ->
    io:format("(", []),
    printlist(L),
    io:format(")", []);
printexp([]) -> io:format("", []);
printexp([H|T]) ->
    printexp(H),
    io:format("~n", []),
    printexp(T).


tuplezip([], []) -> [];
tuplezip([HA|TA], [HB|TB]) -> [{HA,HB}|tuplezip(TA, TB)];
tuplezip(LA, LB) -> erlang:error({tuplezip_mismatch, LA, LB}).


evalexp({int, Val}, Env) -> {{int, Val}, Env};

evalexp({bool, Val}, Env) -> {{bool, Val}, Env};

evalexp({sym, Name}, Env) -> {lookup(Name, Env), Env};

evalexp({list, [?LETSTAR, {list, []}, Body]}, Env) ->
    evalexp(Body, Env);
evalexp({list, [?LETSTAR,
                {list, [{list, [{sym, Name}, Exp]}|T]},
                Body]}, Env) ->
    {Val, _} = evalexp(Exp, Env),
    evalexp({list, [?LETSTAR, {list, T}, Body]}, bind(Name, Val, Env));

evalexp({list, [?LET, {list, Bindings}, Body]}, Env) ->
    BoundVars = lists:map(fun ({list, [{sym, Name}, Exp]}) ->
                                  {Val, _} = evalexp(Exp, Env),
                                  {Name, Val}
                          end, Bindings), 
    NewEnv = extend(BoundVars, Env),
    evalexp(Body, NewEnv);

evalexp({list, [?QUOTE, QuotedExp]}, Env) -> {QuotedExp, Env};

evalexp({list, [?DEFINE, {sym, Name}, Formals, Body]}, Env) ->
    {Closure, _} = evalexp({list, [?LAMBDA, Formals, Body]}, Env),
    {Closure, bind(Name, Closure, Env)};

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
        _ -> erlang:error({bad_if, Cond})
    end;

evalexp({list, [{closure, Formals, Body, CapturedEnv}|Actuals]}, Env) ->
    ActualValues = lists:map(fun (Actual) ->
                                     {Val, _} = evalexp(Actual, Env),
                                     Val
                             end, Actuals),
    FormalsEnv = tuplezip(Formals, ActualValues),
    CombinedEnv = extend(FormalsEnv, extend(CapturedEnv, Env)),
    evalexp(Body, CombinedEnv);

evalexp({list, [?LAMBDA, {list, Formals}, Body]}, Env) ->
    FormalNames = lists:map(fun ({sym, Name}) -> Name end, Formals),
    {{closure, FormalNames, Body, Env}, Env};

%evalexp({list, [{sym, FnName}|Args]}, Env) ->
%    case FnName of
%  when isbound(FnName, Env) ->
%    ok;

evalexp({list, [LispFn|Args]}, Env) ->
    {FnVal, _} = evalexp(LispFn, Env),
    evalexp({list, [FnVal|Args]}, Env);

evalexp([], Env) -> {ok, Env};
evalexp([E|[]], Env) -> evalexp(E, Env);
evalexp([FirstExp|RestExps], Env) ->
    {_V, Env} = evalexp(FirstExp, []),
    evalexp(RestExps, Env).
