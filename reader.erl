-module(reader).
-export([read_program/0, read_program/2]).

read_program(string, Data) ->
    {ok, T, _} = scanner:string(Data),
    {ok, {form, Prog}} = parser:parse(T),
    Prog;

read_program(file, FileName) ->
    {ok, Data} = file:read_file(FileName),
    read_program(string, binary:bin_to_list(Data)).

read_program() ->
    S = io:get_line(">>> "),
    read_program(string, S).
