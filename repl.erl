-module(repl).
-export([main/1]).

% http://stackoverflow.com/questions/8817171
shuffle(Ls) ->
    [X||{_,X} <- lists:sort([ {random:uniform(), N} || N <- Ls])].

main([]) -> usage();
main(Machines) when is_list(Machines) ->
    erlang:set_cookie(node(), dlisp),
    IdServer = spawn(fun master:idserver/0),
    Agents = master:connect_worker_nodes(Machines, flat, 2),
    AgentList = queue:to_list(Agents),
    Map = fun basis:parallel_map/2,
    Map(fun(Agent) ->
                % Don't let the agent request work from itself.
                OtherAgents = AgentList -- [Agent],
                % And randomize the list so they don't all kill one node
                % at a time.
                Agent ! {other_agents, shuffle(OtherAgents)}
        end, AgentList),
    StartingEnv = eval:bind('__idserver', IdServer,
                            eval:bind('__agents', Agents,
                                      basis:basis())),
    reader:repl(1, StartingEnv);
main(_) -> usage().

usage() ->
    io:format("usage: ./repl.erl [machineA [machine B [...]]]\n"),
    halt(1).
