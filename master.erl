-module(master).
-export([connect_worker_nodes/3, timed_pollhealth/1, parallel_map/4]).
-export([idserver/0, freshid/1, test_rr/1, test_mrr/1, test_timed/1]).


shuffle(Ls) ->
   [X || {_, X} <- lists:sort([ {random:uniform(), N} || N <- Ls])].

%%%% Need a requeue function to re-assign dead IDs
%%%
%%%  Need wways to assign IDs
%%%
%%%  Need ways to test failures
%%%
%%%  Need to implement schemes besides round robin

connect_worker_nodes([], bymachine, _) -> {[], []} ;
connect_worker_nodes([], flat, _) -> {[], queue:new()};
connect_worker_nodes([H|T], flat, Alg) ->
  Comp = net_kernel:connect_node(H),
  if Comp ->
    _InitPid = spawn(H, thread_pool, init_workers, [self(), Alg]),
    receive {workers, Threads} ->
      {_OldList, OldQueue} = connect_worker_nodes(T, flat, Alg),
      RList = queue:to_list(queue:join(Threads, OldQueue)),
      Queue = queue:from_list(shuffle(RList)),
      {RList, Queue}
    end;
  true -> error(net_kernel_screwed_up)
  end;

connect_worker_nodes([H|T], bymachine, Alg) ->
  Comp = net_kernel:connect_node(H),
  if Comp ->
    _InitPid = spawn(H, thread_pool, init_machine, [self(), Alg]),
    Prev = erlang:system_time(),
    receive {Pid, Workers, Cpu, {Total, Alloc, _Worst}} ->
      Recvd = erlang:system_time(),
      WorkerList = queue:to_list(Workers),
      {WList, Ms} = connect_worker_nodes(T, bymachine, Alg),
      {WorkerList ++ WList, [{H, Pid, Workers, Cpu, Total-Alloc, Recvd-Prev}|Ms]}
    end;
  true ->  error(net_kernel_screwed_up)
  end.

  

timed_pollhealth([]) -> [];
timed_pollhealth([{Node, Pid, ThreadPool, SysCpu, SysMem, Time}|T]) ->
   PrevTime = erlang:system_time(),
   Pid ! system_check,
   receive {Pid, SysCpu, {Total, Alloc, _Worst}} ->
     Elapsed = erlang:system_time() - PrevTime,
     [{Node, Pid, ThreadPool, SysCpu, Total - Alloc, 0.875*Time + 0.125*Elapsed}|
      timed_pollhealth(T)]
   after 1000 ->
     Elapsed = erlang:system_time(),
     [{Node, Pid, ThreadPool, SysCpu, SysMem, 0.875*Time + 0.125*Elapsed}|
      timed_pollhealth(T)]
   end;
timed_pollhealth(_) -> error.

send_request([]) -> sent;
send_request([{_Node, Pid, _ThreadPool, _Cpu, _Mem, _Time}|T]) ->
    Pid ! system_check,
    send_request(T).

time_machines([{_Node, _Pid, _ThreadPool, _Cpu, _Mem, _Time}=H|T]) ->
    send_request([H|T]),
    receive {Fastest, _Cpu, {_Total, _Alloc, _Worst}} ->
        lists:keytake(Fastest, 2, [H|T])
    end.


idserver() ->
    idserver(0).
idserver(Current) ->
    receive
        {need_id, From} -> From ! {fresh_id, Current},
                           idserver(Current+1)
    end.


freshid(IdServer) ->
    IdServer ! {need_id, self()},
    receive
        {fresh_id, FreshId} -> FreshId
    end.

parallel_map_timed(_IdServer, [], _Machines) -> [];
parallel_map_timed(IdServer, [{Exp, Env}|T], Machines) ->
  {value, Fastest, _Rest} = time_machines(Machines),
  {Node, _Pid, Workers, _Cpu, _Mem, _Time} = Fastest,
  FreshId = freshid(IdServer),
  Worker = queue:get(Workers),
  Worker ! {delegate, FreshId, Exp, Env},
  queue:in(Worker, Workers),
  [FreshId | parallel_map_timed(IdServer, T, Machines)].
   

parallel_map_memrr(_IdServer, [], _Procs) -> [];
parallel_map_memrr(IdServer, [{Exp, Env}|T], Machines) ->
    [{_Node, _Pid, Workers, _Cpu, _Mem, _Time} | _Ms] = lists:keysort(5, Machines),
    FreshId = freshid(IdServer),
    Worker = queue:get(Workers),
    Worker ! {delegate, FreshId, Exp, Env},
    queue:in(Worker, Workers),
    NewMs = timed_pollhealth(Machines),
   [FreshId | parallel_map_memrr(IdServer, T, NewMs)].


parallel_map_rr(_IdServer, [], _MachineQueue) -> [];
parallel_map_rr(IdServer, [{Exp,Env}|T], ProcQueue) ->
    {Proc, NewProcQueue} = thread_pool:next_node(ProcQueue),
    FreshId = freshid(IdServer),
    Proc ! {delegate, FreshId, Exp, Env},
    [FreshId|parallel_map_rr(IdServer, T, NewProcQueue)].


parallel_map(IdServer, WorkPackets, Processes, roundrobin) ->
   Avengers = parallel_map_rr(IdServer, WorkPackets, Processes),
   IdValPairs = assemble(Avengers),
    lists:map(fun({_Id, Val}) -> Val end, IdValPairs);
parallel_map(IdServer, WorkPackets, Machines, memroundrobin) ->
   Avengers = parallel_map_memrr(IdServer, WorkPackets, Machines),
   IdValPairs =  assemble(Avengers),
   lists:map(fun({_Id, Val}) -> Val end, IdValPairs);
parallel_map(IdServer, WorkPackets, Machines, timed) ->
   Avengers = parallel_map_timed(IdServer, WorkPackets, Machines),
   IdValPairs  = assemble(Avengers),
   lists:map(fun({_Id, Val}) -> Val end, IdValPairs).

   
assemble([]) -> [];
assemble([Id|Rest]) -> 
                    receive
                        {result, Id, Val} ->
                                [{Id, Val}|assemble(Rest)]
                    end.

test_rr(Machines) ->
    io:format("~p is extrmely me~n", [self()]),
    global:register_name(iogl, self()),
    IdServer = spawn(fun idserver/0),
    StartingEnv = basis:basis(),
    io:format("Calling pmap...~n"),
    Pids = master:connect_worker_nodes(Machines, flat, 1),
    WP = [{{list, [{sym, binplus}, {int, 3},{int, 3}]}, StartingEnv},
          {{list, [{sym, binplus}, {int, 5}, {int, 5}]}, StartingEnv},
          {{list, [{sym, binplus}, {int, 7}, {int, 5}]}, StartingEnv}],
   io:format("Currently connected to: ~p~n", [nodes()]),
   master:parallel_map(IdServer, WP, Pids, roundrobin).
  

test_mrr(Machines) ->
  IdServer = spawn(fun idserver/0),
  StartingEnv = basis:basis(),
  Ms = master:connect_worker_nodes(Machines, bymachine, 2),
  WP = [{{list, [{sym, binplus}, {int, 3},{int, 3}]}, StartingEnv},
        {{list, [{sym, binplus}, {int, 5}, {int, 5}]}, StartingEnv},
        {{list, [{sym, binplus}, {int, 7}, {int, 5}]}, StartingEnv}],
  master:parallel_map(IdServer, WP, Ms, memroundrobin).

test_timed(Machines) ->
  IdServer = spawn(fun idserver/0),
  StartingEnv = basis:basis(),
  Ms = master:connect_worker_nodes(Machines, bymachine, 2),
  WP = [{{list, [{sym, binplus}, {int, 3},{int, 3}]}, StartingEnv},
        {{list, [{sym, binplus}, {int, 5}, {int, 5}]}, StartingEnv},
        {{list, [{sym, binplus}, {int, 7}, {int, 5}]}, StartingEnv}],
  master:parallel_map(IdServer, WP, Ms, timed).


