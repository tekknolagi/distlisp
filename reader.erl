-module(reader).
-export([read_program/0, read_program/1]).

read_program(FileName) ->
    {ok, Data} = file:read_file(FileName),
    {ok, T, _} = scanner:string(binary:bin_to_list(Data)),
    io:format("scanned: ~p~n", [T]),
    {ok, {form, Prog}} = parser:parse(T),
    Prog.

read_program() ->
    S = io:get_line(">>> "),
    {ok, T, _} = scanner:string(S),
    io:format("scanned: ~p~n", [T]),
    {ok, {form, Prog}} = parser:parse(T),
    Prog.
