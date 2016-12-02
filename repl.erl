-module(repl).
-export([main/1]).
%% #!/usr/bin/env escript
%% -*- erlang -*-
main([]) -> usage();
main(Machines) when is_list(Machines) ->
    erlang:set_cookie(node(), dlisp),
    % MachineAtoms = lists:map(fun erlang:list_to_atom/1, Machines),
    IdServer = spawn(fun master:idserver/0),
    Agents = master:connect_worker_nodes(Machines, flat),
    StartingEnv = eval:bind('__idserver', IdServer,
                            eval:bind('__agents', Agents,
                                      basis:basis())),
    reader:repl(1, StartingEnv);
main(_) -> usage().

usage() ->
    io:format("usage: ./repl.erl [machineA [machine B [...]]]\n"),
    halt(1).
