-module(repl).
-export([main/1]).

% http://stackoverflow.com/questions/8817171
shuffle(Ls) ->
    [X||{_,X} <- lists:sort([ {rand:uniform(), N} || N <- Ls])].

main([]) ->
    reader:repl(1, basis:basis());
main([InitMode|[DistMode|Machines]]) when is_list(Machines) ->
    erlang:set_cookie(node(), dlisp),
    IdServer = spawn(fun master:idserver/0),
    {FlatAgentList, AgentStore} =
            master:connect_worker_nodes(Machines, InitMode, 2),
    Map = fun basis:parallel_map/2,
    Map(fun(Agent) ->
                % Don't let the agent request work from itself.
                OtherAgents = FlatAgentList -- [Agent],
                % And randomize the list so they don't all kill one node
                % at a time.
                Agent ! {other_agents, shuffle(OtherAgents)}
        end, FlatAgentList),
    StartingEnv = eval:bind('__idserver', IdServer,
                  eval:bind('__agents', AgentStore,
                  eval:bind('__distmode', {sym, DistMode},
                            basis:basis()))),
    reader:repl(1, StartingEnv);
main(_) -> usage().

usage() ->
    io:format("usage: ./repl.erl mode machineA [machine B [...]]\n"),
    halt(1).
