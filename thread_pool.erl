-module(thread_pool).
-export([create/1, create/2, next_node/1, add/1, add/2]).
-export([waitforwork/0]).



create(0, _ServerPool, t) -> erlang:error(invalid_num_node);

create(1, _ServerPool, t) -> queue:from_list([spawn(fun waitforwork/0)]);

create(NumNodes, ServerPool, t) when NumNodes > 1 ->
    {NextServer, NewPool} = next_node(ServerPool),
    Pid = spawn(NextServer, ?MODULE, waitforwork, []),
    queue:in(Pid, create(NumNodes-1, NewPool, t)).

create(NumNodes, ServerPool) ->
    create(NumNodes, queue:from_list(ServerPool), t).

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



init_machine (Master) ->
  ThreadPool = create(calculate()),
  workerinfo(Master, queue:len(ThreadPool)),
  ProcessMgr = spawn(process_manager, loop, [self()]),
  loop(Master, ProcessMgr, ThreadPool).

loop(Master, ProcessMgr, ThreadPool) ->
  receive
    {delegate, WorkPack} ->
        {ChosenWorker, NewPool} = thread_pool:next_node(ThreadPool),
        ChosenWorker ! WorkPack,
        loop(Master, ProcessMgr, NewPool);
    check_who_died -> ProcessMgr ! {which_died, queue:to_list(ThreadPool)}
  end.



waitforwork() ->
    receive
        {work, Sender, Id, {Exp, Env} } -> Sender ! {result, eval:evalexp(Exp,Env)}
    end,
    waitforwork().

workerinfo (Master, NumWorkers) ->
   Master ! {self(), NumWorkers,
             erlang:system_info(logical_processors_available),
             memsup:get_memory_data()}.


 
