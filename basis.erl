-module(basis).
-export([basis/0]).
-export([binop/2, intdiv/2, exp_proc/2, not_proc/2, and_proc/2, or_proc/2,
         cons_proc/2, car_proc/2, cdr_proc/2, worker_proc/2, check_expect/2,
         save_state/2, load_state/2, print_proc/2, compile_proc/2, env_proc/2,
         class_proc/2, remove_prims/1, bif/1]).


basis() ->
    Defs = reader:read_program(file, "basis.lisp"),
    lists:foldl(fun (Cur, Env) ->
                        {_, NewEnv} = eval:evalexp(Cur, Env),
                        NewEnv
                end, [
                      {'+2', basis:bif(basis:binop(fun erlang:'+'/2, int))},
                      {'-2', basis:bif(basis:binop(fun erlang:'-'/2, int))},
                      {'*2', basis:bif(basis:binop(fun erlang:'*'/2, int))},
                      {'/2', basis:bif(basis:binop(fun basis:intdiv/2, int))},
                      {'=', basis:bif(basis:binop(fun erlang:'=:='/2, bool))},
                      {'exp', basis:bif(fun basis:exp_proc/2)},
                      {'<', basis:bif(basis:binop(fun erlang:'<'/2, bool))},
                      {'>', basis:bif(basis:binop(fun erlang:'>'/2, bool))},
                      {'not', basis:bif(fun basis:not_proc/2)},
                      {'and', {prim, fun basis:and_proc/2}},
                      {'or', {prim, fun basis:or_proc/2}},
                      {'cons', basis:bif(fun basis:cons_proc/2)},
                      {'car', basis:bif(fun basis:car_proc/2)},
                      {'cdr', basis:bif(fun basis:cdr_proc/2)},
                      {'worker', basis:bif(fun basis:worker_proc/2)},
                      {'check-expect', {prim, fun basis:check_expect/2}},
                      {'save-state', basis:bif(fun basis:save_state/2)},
                      {'load-state', basis:bif(fun basis:load_state/2)},
                      {'print', basis:bif(fun basis:print_proc/2)},
                      {'c', basis:bif(fun basis:compile_proc/2)},
                      {'env', basis:bif(fun basis:env_proc/2)},
                      {'class', {prim, fun basis:class_proc/2}},
                      {'workers', {list, [{sym, node()}]}},
                      {'ok', {sym, ok}}
                     ],
                Defs).


binop(F, RT) ->
    fun ([{_, AV}, {_, BV}], Env) ->
            {{RT, F(AV, BV)}, Env}
    end.


bif(F) ->
    {prim, fun (Args, Env) ->
            Vals = lists:map(fun (Exp) ->
                                     {V, _} = eval:evalexp(Exp, Env),
                                     V
                             end, Args),
            F(Vals, Env)
    end}.


intdiv(A, B) -> trunc(A/B).


exp_proc([{int, AV}, {int, BV}], Env) -> {{int, round(math:pow(AV, BV))}, Env}.


not_proc([{bool, AV}], Env) -> {{bool, not AV}, Env}.


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


cons_proc([X, {list, XSV}], Env) -> {{list, [X|XSV]}, Env}.


car_proc([{list, [H|_T]}], Env) -> {H, Env}.


cdr_proc([{list, [_H|T]}], Env) -> {{list, T}, Env}.


worker_proc([NodeName], Env) ->
    {list, Workers} = eval:lookup(workers, Env),
    NewWorkers = {list, [NodeName|Workers]},
    {NewWorkers, eval:bind(workers, NewWorkers, Env)}.


check_expect([A, B], Env) ->
    {{bool, AreEq}, _} = eval:evalexp({list, [{sym, '='}, A, B]}, Env),
    if
        AreEq -> %%io:format("check-expect passed~n", []),
            {{bool, true}, Env};
        true  -> io:format("check-expect failed~n", []),
                 {{bool, false}, Env}
    end.


remove_prims({closure, Formals, Body, CapturedEnv}) ->
    {closure, Formals, basis:remove_prims(Body), basis:remove_prims(CapturedEnv)};

remove_prims([]) -> [];
remove_prims([{N, {closure, F, B, C}}|T]) ->
    [{N, basis:remove_prims({closure, F, B, C})}|basis:remove_prims(T)];
remove_prims([{_N, {prim, _}}|T]) -> basis:remove_prims(T);
remove_prims([H|T]) -> [basis:remove_prims(H)|basis:remove_prims(T)];

remove_prims(O) ->
    O.


save_state([{sym, FileName}], Env) ->
    EnvNoPrims = basis:remove_prims(Env),
    ok = file:write_file(atom_to_list(FileName),
                         io_lib:fwrite("~p.\n", [EnvNoPrims])),
    {{bool, true}, Env}.


load_state([{sym, FileName}], Env) ->
    {ok, [NewEnv]} = file:consult(atom_to_list(FileName)),
    {{bool, true}, eval:extend(NewEnv, Env, slim)}.


print_proc([Val], Env) -> eval:printexp(Val),
                          io:format("~n"),
                          {{sym, ok}, Env}.


compile_proc([{sym, Name}], _Env) ->
    code:purge(Name),
    {ok, Name} = compile:file(Name),
    code:load_file(Name),
    throw(code_reload).


env_proc([], Env) ->
    EnvNoPrims = basis:remove_prims(Env),
    EnvWithSyms = lists:map(fun ({Name, V}) ->
                                    {list, [{sym, Name}, V]}
                            end, EnvNoPrims),
    {{list, EnvWithSyms}, Env}.


class_proc([Name, Parent], Env) ->
    {{list, [Name, Parent]}, Env}.
