-module(reader).
-export([repl/0, repl/1, read_program/2]).

read_program(string, Data) ->
    {ok, T, _} = scanner:string(Data),
    {ok, {form, Prog}} = parser:parse(T),
    Prog;

read_program(file, FileName) ->
    {ok, Data} = file:read_file(FileName),
    read_program(string, binary:bin_to_list(Data)).

repl() ->
    repl(basis:basis()).
repl(Env) ->
    try eval:evalexp(read_program(string, io:get_line("> ")), Env) of
        {Val, NewEnv} ->  if Val == {sym, ok} ->
                                 repl(eval:bind(it, Val, NewEnv));
                             Val == {sym, quit} ->
                                 io:format("Thank you for trying DLisp.~n");
                             true ->
                                 eval:printexp(Val),
                                 io:format("~n"),
                                 repl(eval:bind(it, Val, NewEnv))
                          end
    catch
        throw:code_reload ->
            repl(eval:bind(it, {sym, ok}, Env));
        error:{unbound_variable,V} ->
            io:format("ERROR: Unbound variable ~p~n", [V]),
            repl(Env);
        error:{badmatch, G} ->
            io:format("ERROR: Type mismatch: ~p~n", [G]),
            repl(Env);
        error:{tuplezip_mismatch, _, _} ->
            io:format("ERROR: Wrong number of arguments~n"),
            repl(Env);
        error:E ->
            io:format("ERROR: ~p~n", [E]),
            repl(Env)
    end.
