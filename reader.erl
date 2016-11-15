-module(reader).
-export([repl/0, read_program/2]).

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
        {Val, NewEnv} ->  if Val =/= {sym, quit} ->
                                 eval:printexp(Val),
                                 io:format("~n"),
                                 repl(eval:bind(it, Val, NewEnv));
                             true ->
                                 io:format("Thank you for trying DLisp.~n")
                          end
    catch
        error:{unbound_variable,V} -> io:format("ERROR: Unbound variable ~p~n", [V]),
                                      repl(Env);
        error:E -> io:format("ERROR: ~p~n", [E]),
                   repl(Env)
    end.
