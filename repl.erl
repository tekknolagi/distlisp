#!/usr/bin/env escript
%% -*- erlang -*-
main([]) ->
    reader:repl(1, basis:basis());
main(_) -> usage().

usage() ->
    io:format("usage: ./repl\n"),
    halt(1).
