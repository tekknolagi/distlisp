-module(thread_pool).
-export([create/1, create/2, next_node/1, add/1, add/2]).

create(0, _ServerPool, t) -> erlang:error(invalid_num_node);
create(1, _ServerPool, t) -> queue:in(spawn(fun waitforwork/0), queue:new());
create(NumNodes, ServerPool, t) when NumNodes > 0 ->
    {NextServer, NewPool} = next_node(ServerPool),
    Pid = spawn(NextServer, ?MODULE, waitforwork, []),
    queue:in(Pid, create(NumNodes-1, NewPool)).
create(NumNodes, ServerPool) ->
    create(NumNodes, queue:from_list([node()|ServerPool]), t).
create(NumNodes) ->
    create(NumNodes, queue:from_list([node()])).

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
