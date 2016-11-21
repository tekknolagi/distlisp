#!/usr/bin/env escript
%% -*- erlang -*-
main([]) ->
    {ok, Parser} = yecc:file("parser.yrl"),
    {ok, Scanner} = leex:file("scanner.xrl"),
    compile:file(parser),
    compile:file(scanner),
    io:format("Parser now available at ~p and scanner at ~p~n",
              [Parser, Scanner]);
main(_) -> usage().

usage() ->
    io:format("usage: ./mkparser\n"),
    halt(1).
