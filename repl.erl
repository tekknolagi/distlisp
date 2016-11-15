#!/usr/bin/env escript
%% -*- erlang -*-
main([]) ->
    reader:repl();
main(_) -> usage().

usage() ->
    io:format("usage: ./repl\n"),
    halt(1).
