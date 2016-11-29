-module(thread_pool).
-export([create/1, create/2, next_node/1, add/1, add/2]).
-export([waitforwork/0, init_machine/1, loop/4, calculate/0, reap/3]).


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
   (Total - Alloc) * Cores div 1024.


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
  ThreadPool = create(100),
  machineinfo(Master, queue:len(ThreadPool)),
  ProcessMgr = spawn(process_manager, loop, [self()]),
  loop(Master, ProcessMgr, ThreadPool, []).

loop(Master, ProcessMgr, ThreadPool, Pairs) ->
  receive
    {delegate, {work, Master, Id, {Exp, Env}}} ->
        {ChosenWorker, NewPool} = thread_pool:next_node(ThreadPool),
        ChosenWorker ! {work, Master, Id, {Exp, Env}},
        loop(Master, ProcessMgr, NewPool, [{ChosenWorker, Id}|Pairs]);

    check_who_died ->
        ProcessMgr ! {which_died, Pairs},
        loop(Master, ProcessMgr, ThreadPool, Pairs);

    {dead_procs, DeadIds, DeadPids} ->
        Master ! {dead, self(), DeadIds},
        {NewPool, NewPairs} = reap(queue:to_list(ThreadPool), Pairs, DeadPids),
        loop(Master, ProcessMgr, queue:from_list(NewPool), NewPairs);

    system_check ->
        io:format("Received System Check ~n"),
        NumWorkers = queue:len(ThreadPool),
        machineinfo(Master, NumWorkers),
        io:format("Sent Machine Info ~n"),
        loop(Master, ProcessMgr, ThreadPool, Pairs)
  end.




waitforwork() ->
    receive
        {work, Sender, Id, {Exp, Env} } -> Sender ! {result, Id, eval:evalexp(Exp,Env)}
    end,
    waitforwork().

machineinfo (Master, NumWorkers) ->
   Master ! {self(), NumWorkers,
             erlang:system_info(logical_processors_available),
             memsup:get_memory_data()}.

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
