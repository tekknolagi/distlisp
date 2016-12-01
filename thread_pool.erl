-module(thread_pool).
-export([create/1, create/2, next_node/1, add/1, add/2]).
-export([waitforwork/0, init_machine/1, loop/1, calculate/0, reap/3]).


create(0, _ServerPool, t) -> queue:from_list([]);%erlang:error(invalid_num_node);

create(1, ServerPool, t) -> {NextServer, _NewPool} = next_node(ServerPool),
                            Pid = spawn(NextServer, ?MODULE, waitforwork, []),
                            queue:from_list([Pid]);

create(NumNodes, ServerPool, t) when NumNodes > 1 ->
    {NextServer, NewPool} = next_node(ServerPool),
    Pid = spawn(NextServer, ?MODULE, waitforwork, []),
    queue:in(Pid, create(NumNodes-1, NewPool, t)).

create(NumNodes, ServerPool) ->
    create(NumNodes, queue:from_list([node()|ServerPool]), t).

create(NumNodes) ->
    create(NumNodes, []).


next_node(Nodes) ->
    {{value, FirstNode}, RestNodes} = queue:out(Nodes),
    {FirstNode, queue:in(FirstNode, RestNodes)}.


add(Nodes, 0) -> Nodes;

add(Nodes, NumNodes) when NumNodes > 0 ->
    queue:in(spawn(fun waitforwork/0), add(Nodes, NumNodes-1)).

add(Nodes) ->
    add(Nodes, 1).


calculate () -> 
   {Total, Alloc, _} = memsup:get_memory_data(),
   Cores = erlang:system_info(logical_processors_available),
  erlang:max((Total - Alloc)  div 1048576, 2000).


list_delete([], _) -> [];
list_delete([HA|TA], []) -> [HA|TA];
list_delete([HA|TA], [H|T]) -> list_delete(lists:delete(H, [HA|TA]), T);
list_delete([HA|TA], K) -> lists:delete(K, [HA|TA]);
list_delete(_, _) -> error.

key_delete([], _) -> [];
key_delete([Pair | Ps], [Pid|T]) ->
   key_delete(lists:keydelete(Pid,1, [Pair|Ps]) , T);
key_delete([Pair | Ps], []) -> [Pair | Ps];
key_delete(_, _) -> error.


reap(ThreadPool, Pairs, DeadPids) ->
   {list_delete(ThreadPool, DeadPids), key_delete(Pairs, DeadPids)}.
 

init_machine (Master) ->
  application:start(sasl),
  application:start(os_mon),
  ThreadPool = create(calculate()),
  Master ! {self(), ThreadPool, erlang:system_info(logical_processors_available),
            memsup:get_memory_data()},
  loop(Master).

loop(Master) ->
  receive 
    system_check ->
        Master ! {self(), erlang:system_info(logical_processors_available),
                  memsup:get_memory_data()},
        loop(Master);
   Other -> io:format("Machine received ~p~n", [Other]),
            loop(Master)
  end.




waitforwork() ->
    receive
        {work, Sender, Id, {Exp, Env} } -> Sender ! {result, Id, eval:evalexp(Exp,Env)}
    end,
    waitforwork().
% Various ways to calculate init number of processes

 % Master
 %      Waits for 
 %      Sends Init machine to machines after
 %      Periodically keeps track of machine health
 %              Available memory
 %              num processors
 %              how much computation power is being used
 %              Number of idle processes
 %              Dead processes - how to handle?
 %      Various implementations of parallel map
 %              Round robin (already there)
 %              Weighted round robin
 %              Hashing
 %              Most idle processes
 %      Can tell the machine to spin up more processes
 %      
