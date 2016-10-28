-module(thread_pool).
-export([create/1, next_node/1]).

create(0) -> queue:new();
create(NumNodes) when NumNodes > 0 ->
    queue:in(spawn(fun waitforwork/0), create(NumNodes-1)).

next_node(Nodes) ->
    {{value, FirstNode}, RestNodes} = queue:out(Nodes),
    {FirstNode, queue:in(FirstNode, RestNodes)}.

waitforwork() ->
    receive
        {work, Sender, Fun, Arg} -> Sender ! {result, Fun(Arg)}
    end,
    waitforwork().
