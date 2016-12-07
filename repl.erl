-module(repl).
-export([main/1,main/0]).

% http://stackoverflow.com/questions/8817171
shuffle(Ls) ->
    [X||{_,X} <- lists:sort([ {rand:uniform(), N} || N <- Ls])].

main() ->
    reader:repl(1, basis:basis()).
main([DistMode]) when is_atom(DistMode) ->
    erlang:set_cookie(node(), dlisp),
    register(master, self()),
    Delegator = spawn(master, delegator, [{DistMode}]),
    DelegatorAtom = {sym, list_to_atom(pid_to_list(Delegator))},
    %{FlatAgentList, AgentStore} =
    %        master:connect_worker_nodes(Machines, InitMode, 2),
%%     Map = fun basis:parallel_map/2,
%%     Map(fun(Agent) ->
%%                 % Don't let the agent request work from itself.
%%                 OtherAgents = FlatAgentList -- [Agent],
%%                 % And randomize the list so they don't all kill one node
%%                 % at a time.
%%                 Agent ! {other_agents, shuffle(OtherAgents)}
%%         end, FlatAgentList),
    reader:repl(1, eval:bind('__delegator', DelegatorAtom, basis:basis()));
main(_) -> usage().

usage() ->
    io:format("usage: ./repl.erl mode machineA [machine B [...]]\n"),
    halt(1).
