-module(agentregistry).
-export([spawnloop/1]).

% http://stackoverflow.com/questions/8817171
shuffle(Ls) ->
    [X||{_,X} <- lists:sort([ {random:uniform(), N} || N <- Ls])].

spawnloop(Agents) ->
    spawn(fun () -> loop(Agents) end).

loop(Agents) ->
    receive
        {where_are_the_others, RequesterAgent} ->
            ExceptRequester = Agents -- [RequesterAgent],
            RequesterAgent ! {other_agents, shuffle(ExceptRequester)},
            loop(Agents)
    end.
