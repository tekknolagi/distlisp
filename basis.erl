-module(basis).
-export([basis/0]).

basis() ->
    {ok, Defs} = file:consult("generated_basis.erl"),
    lists:foldl(fun (Cur, Env) ->
                        {_, NewEnv} = eval:evalexp(Cur, Env),
                        NewEnv
                end,
                [
                 {'+', {prim, fun add_proc/2}},
                 {'-', {prim, fun sub_proc/2}},
                 {'*', {prim, fun mul_proc/2}},
                 {'exp', {prim, fun exp_proc/2}},
                 {'<', {prim, fun lt_proc/2}},
                 {'>', {prim, fun gt_proc/2}},
                 {'not', {prim, fun not_proc/2}},
                 {'and', {prim, fun and_proc/2}},
                 {'or', {prim, fun or_proc/2}},
                 {'null?', {prim, fun nullp_proc/2}},
                 {'cons', {prim, fun cons_proc/2}},
                 {'car', {prim, fun car_proc/2}},
                 {'cdr', {prim, fun cdr_proc/2}},
                 {'list1', {prim, fun list1_proc/2}},
                 {'list2', {prim, fun list2_proc/2}},
                 {'workers', {list, [{sym, node()}]}},
                 {'worker', {prim, fun worker_proc/2}}
                ],
                Defs).


add_proc([A, B], Env) -> {{int, AV}, _} = eval:evalexp(A, Env),
                         {{int, BV}, _} = eval:evalexp(B, Env),
                         {{int, AV+BV}, Env}.

sub_proc([A, B], Env) -> {{int, AV}, _} = eval:evalexp(A, Env),
                         {{int, BV}, _} = eval:evalexp(B, Env),
                         {{int, AV-BV}, Env}.

mul_proc([A, B], Env) -> {{int, AV}, _} = eval:evalexp(A, Env),
                         {{int, BV}, _} = eval:evalexp(B, Env),
                         {{int, AV*BV}, Env}.

exp_proc([A, B], Env) -> {{int, AV}, _} = eval:evalexp(A, Env),
                         {{int, BV}, _} = eval:evalexp(B, Env),
                         {{int, round(math:pow(AV, BV))}, Env}.

lt_proc([A, B], Env)  -> {{int, AV}, _} = eval:evalexp(A, Env),
                         {{int, BV}, _} = eval:evalexp(B, Env),
                         {{bool, AV < BV}, Env}.

gt_proc([A, B], Env)  -> {{int, AV}, _} = eval:evalexp(A, Env),
                         {{int, BV}, _} = eval:evalexp(B, Env),
                         {{bool, AV > BV}, Env}.

not_proc([A], Env) -> {{bool, V}, _} = eval:evalexp(A, Env),
                      {{bool, not V}, Env}.

and_proc([A, B], Env) -> {{bool, AV}, _} = eval:evalexp(A, Env),
                         {{bool, BV}, _} = eval:evalexp(B, Env),
                         {{bool, AV and BV}, Env}.

or_proc([A, B], Env) -> {{bool, AV}, _} = eval:evalexp(A, Env),
                        if
                            AV == true -> {{bool, true}, Env};
                            true       -> eval:evalexp(B, Env)
                        end.

nullp_proc([L], Env) -> {LV, _} = eval:evalexp(L, Env),
                        {{bool, LV == {list, []}}, Env}.

cons_proc([X, XS], Env) -> {XV, _} = eval:evalexp(X, Env),
                           {{list, XSV}, _} = eval:evalexp(XS, Env),
                           {{list, [XV|XSV]}, Env}.

car_proc([L], Env) -> {{list, [H|_T]}, _} = eval:evalexp(L, Env),
                      {H, Env}.

cdr_proc([L], Env) -> {{list, [_H|T]}, _} = eval:evalexp(L, Env),
                      {{list, T}, Env}.

list1_proc([A], Env) -> {AV, _} = eval:evalexp(A, Env),
                        {{list, [AV]}, Env}.


list2_proc([A, B], Env) -> {AV, _} = eval:evalexp(A, Env),
                           {BV, _} = eval:evalexp(B, Env),
                           {{list, [AV, BV]}, Env}.

worker_proc([NodeExp], Env) -> {NodeName, _} = eval:evalexp(NodeExp, Env),
                               {list, Workers} = eval:lookup(workers, Env),
                               NewWorkers = {list, [NodeName|Workers]},
                               {Workers,
                                eval:bind(workers, NewWorkers, Env)}.
