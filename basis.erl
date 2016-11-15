-module(basis).
-export([basis/0]).

basis() ->
    Defs = reader:read_program(file, "basis.lisp"),
    lists:foldl(fun (Cur, Env) ->
                        {_, NewEnv} = eval:evalexp(Cur, Env),
                        NewEnv
                end,
                [
                 {'rat', {prim, fun rat_proc/2}},
                 {'+', {prim, binop(fun erlang:'+'/2, int)}},
                 {'+r', {prim, fun plusrat_proc/2}},
                 {'-', {prim, binop(fun erlang:'-'/2, int)}},
                 {'*', {prim, binop(fun erlang:'*'/2, int)}},
                 {'/', {prim, binop(fun intdiv/2, int)}},
                 {'=', {prim, binop(fun erlang:'=:='/2, bool)}},
                 {'exp', {prim, fun exp_proc/2}},
                 {'<', {prim, binop(fun erlang:'<'/2, bool)}},
                 {'>', {prim, binop(fun erlang:'>'/2, bool)}},
                 {'not', {prim, fun not_proc/2}},
                 {'and', {prim, fun and_proc/2}},
                 {'or', {prim, fun or_proc/2}},
                 {'cons', {prim, fun cons_proc/2}},
                 {'car', {prim, fun car_proc/2}},
                 {'cdr', {prim, fun cdr_proc/2}},
                 {'workers', {list, [{sym, node()}]}},
                 {'worker', {prim, fun worker_proc/2}},
                 {'check-expect', {prim, fun check_expect/2}}
                ],
                Defs).

binop(F, RT) ->
    fun ([A, B], Env) ->
            {{_, AV}, _} = eval:evalexp(A, Env),
            {{_, BV}, _} = eval:evalexp(B, Env),
            {{RT, F(AV, BV)}, Env}
    end.

rat_proc([N, D], Env) -> {{rat, N, D}, Env}.

gcd(A, 0) -> A;
gcd(A, B) -> gcd(B, A rem B).

intdiv(A, B) -> trunc(A/B).

simplify(N, D) ->
    Gcd = gcd(N, D),
    {rat, {int, intdiv(N, Gcd)}, {int, intdiv(D, Gcd)}}.

plusrat_proc([A, B], Env) ->
    {{rat, {int, AN}, {int, AD}}, _} = eval:evalexp(A, Env),
    {{rat, {int, BN}, {int, BD}}, _} = eval:evalexp(B, Env),
    {simplify(AN*BD+AD*BN, AD*BD), Env}.

exp_proc([A, B], Env) -> {{int, AV}, _} = eval:evalexp(A, Env),
                         {{int, BV}, _} = eval:evalexp(B, Env),
                         {{int, round(math:pow(AV, BV))}, Env}.

not_proc([A], Env) -> {{bool, V}, _} = eval:evalexp(A, Env),
                      {{bool, not V}, Env}.

and_proc([A, B], Env) -> {{bool, AV}, _} = eval:evalexp(A, Env),
                         if
                             AV == false -> {{bool, false}, Env};
                             true        -> eval:evalexp(B, Env)
                         end.

or_proc([A, B], Env) -> {{bool, AV}, _} = eval:evalexp(A, Env),
                        if
                            AV == true -> {{bool, true}, Env};
                            true       -> eval:evalexp(B, Env)
                        end.

cons_proc([X, XS], Env) -> {XV, _} = eval:evalexp(X, Env),
                           {{list, XSV}, _} = eval:evalexp(XS, Env),
                           {{list, [XV|XSV]}, Env}.

car_proc([L], Env) -> {{list, [H|_T]}, _} = eval:evalexp(L, Env),
                      {H, Env}.

cdr_proc([L], Env) -> {{list, [_H|T]}, _} = eval:evalexp(L, Env),
                      {{list, T}, Env}.

worker_proc([NodeExp], Env) -> {NodeName, _} = eval:evalexp(NodeExp, Env),
                               {list, Workers} = eval:lookup(workers, Env),
                               NewWorkers = {list, [NodeName|Workers]},
                               {NewWorkers,
                                eval:bind(workers, NewWorkers, Env)}.

check_expect([A, B], Env) ->
    {{bool, AreEq}, _} = eval:evalexp({list, [{sym, '='}, A, B]}, Env),
    if
        AreEq -> %%io:format("check-expect passed~n", []),
                 {{bool, true}, Env};
        true  -> io:format("check-expect failed~n", []),
                 {{bool, false}, Env}
    end.
