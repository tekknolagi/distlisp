-module(eval).
-export([evalexp/2]).
-export([lookup/2, bind/3, extend/2, extend/3]).
-export([printexp/1]).
-export([type/1]).
-export([tuplezip/2]).

-export([name_free/2]).

-define(IF,      {sym, 'ifx'}).
-define(LAMBDA,  {sym, 'lambda'}).
-define(QUOTE,   {sym, 'quote'}).
-define(LET,     {sym, 'letx'}).
-define(LETSTAR, {sym, 'letx*'}).
-define(DEFINE,  {sym, 'define'}).
-define(VAL,     {sym, 'valx'}).
-define(EVAL,    {sym, 'eval'}).
-define(APPLY,   {sym, 'apply'}).


lookup(Name, []) -> erlang:error({unbound_variable, Name});
lookup(Name, [{Name, V}|_T]) -> V;
lookup(Name, [{_K, _V}|T]) -> lookup(Name, T).


% Simple bind.
bind(Name, Val, Env) -> [{Name, Val}|Env].


remove(_Name, []) -> [];
remove(Name, [{Name, _}|T]) -> remove(Name, T);
remove(Name, [H|T]) -> [H|remove(Name, T)].


% Bind but removes duplicates.
slimbind(Name, Val, Env) -> [{Name, Val}|remove(Name, Env)].


extend([], Env) -> Env;
extend([{Name, Val}|T], Env) -> bind(Name, Val, extend(T, Env)).
extend([{Name, Val}|T], Env, slim) -> slimbind(Name, Val, extend(T, Env)).


printlist([]) -> ok;
printlist([E]) -> printexp(E);
printlist([H|T]) ->
    printexp(H),
    io:format(" "),
    printlist(T).


printexp({int, Val}) -> io:format("~B", [Val]);
printexp({sym, Val}) -> io:format("~s", [Val]);
printexp({bool, true}) -> io:format("#t");
printexp({bool, false}) -> io:format("#f");
printexp({quote, QuotedExp}) ->
%    io:format("'"),
    printexp(QuotedExp);
printexp({list, L}) ->
    io:format("("),
    printlist(L),
    io:format(")");
printexp({prim, _}) -> io:format("<prim>");
printexp({closure, Formals, Body, _CapturedEnv}) ->
    PrintableFormals = {list, lists:map(fun(Name) -> {sym,Name} end, Formals)},
    printexp({list, [{sym, lambda}, PrintableFormals, Body]});
printexp([]) -> io:format("");
printexp([H|T]) ->
    printexp(H),
    io:format("~n"),
    printexp(T).


type({T, _}) -> T.


tuplezip([], []) -> [];
tuplezip([HA|TA], [HB|TB]) -> [{HA,HB}|tuplezip(TA, TB)];
tuplezip(LA, LB) -> erlang:error({tuplezip_mismatch, LA, LB}).


member(_X, []) -> false;
member(X, [X|_T]) -> true;
member(X, [_|T]) -> member(X, T).


name_free({int, _}, _) -> false;

name_free({bool, _}, _) -> false;

name_free({quote, _}, _) -> false;

name_free({sym, V}, N) -> V =:= N;

name_free({list, [?IF, E1, E2, E3]}, N) ->
    name_free(E1, N) or
    name_free(E2, N) or
    name_free(E3, N);

name_free({list, [?LET, {list, Bindings}, Body]}, N) ->
    lists:any(fun ({list, [_, E]}) -> name_free(E, N) end, Bindings) or
    (not member(N, lists:map(fun ({list, [Name, _]}) -> Name end, Bindings)) and
     name_free(Body, N));

name_free({list, [?LETSTAR, {list, []}, Body]}, N) ->
    name_free(Body, N);
name_free({list, [?LETSTAR, {list, [B|BS]}, Body]}, N) ->
    name_free({list, [?LET,
                      {list, [B]},
                      {list, [?LETSTAR, {list, BS}, Body]}]}, N);

name_free({list, [?LAMBDA, {list, Formals}, Body]}, N) ->
    not member(N, lists:map(fun ({sym, Name}) -> Name end, Formals)) and
    name_free(Body, N);

name_free({list, []}, _N) -> false;

name_free({list, [FnName|Args]}, N) ->
    lists:any(fun (E) -> name_free(E, N) end, [FnName|Args]).

% fun improve (l, rho) = (l, List.filter (fn (x, _) => freeIn (LAMBDA l) x) rho)
improve({Formals, Body}, Env) -> {{Formals, Body},
                    lists:filter(fun ({X, _}) ->
                                         name_free({list, [?LAMBDA,
                                                           {list, Formals},
                                                           Body]}, X)
                                 end,
                                 Env)}.

evalexps(Es, Env) ->
    lists:map(fun (E) -> {Val, _} = evalexp(E, Env),
                         Val
              end, Es).

evalexp({int, Val}, Env) -> {{int, Val}, Env};

evalexp({bool, Val}, Env) -> {{bool, Val}, Env};

evalexp({sym, nil}, Env) -> {{list, []}, Env};

evalexp({sym, Name}, Env) -> {lookup(Name, Env), Env};

evalexp({quote, {list, Ls}}, Env) ->
    {{list, lists:map(fun (E) -> {quote, E} end, Ls)}, Env};

evalexp({quote, Exp}, Env) -> {Exp, Env};

evalexp({list, []}, Env) -> {{list, []}, Env};

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

evalexp({list, [?DEFINE, {sym, Name}, Formals, Body]}, Env) ->
    {Closure, _} = evalexp({list, [?LAMBDA, Formals, Body]}, Env),
    {Closure, bind(Name, Closure, Env)};

evalexp({list, [?VAL, {sym, Name}, Exp]}, Env) ->
    {Val, _} = evalexp(Exp, Env),
    {Val, bind(Name, Val, Env)};

evalexp({list, [{closure, ['...'], Body, CapturedEnv}|Actuals]}, Env) ->
    ActualValues = {list, evalexps(Actuals, Env)},
    CombinedEnv = bind('...', ActualValues, extend(CapturedEnv, Env)),
    evalexp(Body, CombinedEnv);

evalexp({list, [{closure, Formals, Body, CapturedEnv}|Actuals]}, Env) ->
    ActualValues = evalexps(Actuals, Env),
    FormalsEnv = tuplezip(Formals, ActualValues),
    CombinedEnv = extend(FormalsEnv, extend(CapturedEnv, Env)),
    evalexp(Body, CombinedEnv);

evalexp({list, [?LAMBDA, {list, Formals}, Body]}, Env) ->
    FormalNames = lists:map(fun ({sym, Name}) -> Name end, Formals),
    {_, ImprovedEnv} = improve({Formals, Body}, Env),
    {{closure, FormalNames, Body, ImprovedEnv}, Env};

evalexp({list, [?IF, Cond, E1, E2]}, Env) ->
    {CondV, _} = evalexp(Cond, Env),
    case CondV of
        {bool, true} -> evalexp(E1, Env);
        {bool, false} -> evalexp(E2, Env);
        _ -> erlang:error({bad_if, Cond})
    end;

evalexp({list, [{prim, PrimFn}|Args]}, Env) -> PrimFn(Args, Env);

evalexp({list, [?EVAL, GivenExp]}, Env) ->
    {ExpVal, _} = evalexp(GivenExp, Env),
    evalexp(ExpVal, Env);

evalexp({list, [?APPLY, FnExp, ArgExps]}, Env) ->
    {{list, ArgVals}, _} = evalexp(ArgExps, Env),
    evalexp({list, [FnExp|ArgVals]}, Env);

evalexp({list, [LispFn|Args]}, Env) ->
    {FnVal, _} = evalexp(LispFn, Env),
    evalexp({list, [FnVal|Args]}, Env);

evalexp([], Env) -> {ok, Env};
evalexp([E], Env) -> evalexp(E, Env);
evalexp([FirstExp|RestExps], Env) ->
    {_V, Env2} = evalexp(FirstExp, Env),
    evalexp(RestExps, Env2).
