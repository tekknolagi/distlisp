-module(thread_pool).
-export([create/1, next_node/1, add/1, add/2]).

create(0) -> queue:new();
create(NumNodes) when NumNodes > 0 ->
    queue:in(spawn(fun waitforwork/0), create(NumNodes-1)).

next_node(Nodes) ->
    {{value, FirstNode}, RestNodes} = queue:out(Nodes),
    {FirstNode, queue:in(FirstNode, RestNodes)}.

add(Nodes, 0) -> Nodes;
add(Nodes, NumNodes) when NumNodes > 0 ->
    queue:in(spawn(fun waitforwork/0), add(Nodes, NumNodes-1)).
add(Nodes) ->
    add(Nodes, 1).

waitforwork() ->
    receive
        {work, Sender, Fun, Arg} -> Sender ! {result, Fun(Arg)}
    end,
    waitforwork().
