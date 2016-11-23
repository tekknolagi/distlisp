-module(repl).
-export([main/0]).

main() ->
    reader:repl(1, basis:basis()).
