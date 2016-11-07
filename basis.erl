-module(basis).
-export([basis/0]).

basis() ->
    {ok, Defs} = file:consult("generated_basis.erl"),
    lists:foldl(fun (Cur, Env) ->
                        {_, NewEnv} = eval:evalexp(Cur, Env),
                        NewEnv
                end,
                [],
                Defs).
