module(master).
-export([machinedata/1,pollhealth/1]).



machinedata([]) -> [];
machinedata([H | T]) ->
  if net_kernel:connect_node(H) ->
    Pid = spawn(H, thread_pool, init_machine, [self()]),
    receive {Pid, NumWorkers, SysCpu, SysMem} ->
      [{Pid, NumWorkers, SysCpu, SysMem} | gatherworkers(T)]
    end;
  true -> error
  end;
machinedata(Node) ->
  if net_kernel:connect_node(Node) ->
    Pid = spawn(Node, thread_pool, init_machine, [self()]),
    receive {Node, NumWorkers, SysCpu, SysMem} ->
      {Node, NumWorkers, SysCpu, SysMem}
    end;
  true -> error
  end;

machinedata(_) -> error.

poolhealth([]) -> [].
poolhealth([{Pid, NumWorkers, SysCpu, SysMem}|T]) ->
    Pid ! check_who_died,
    receive {dead, Pid, Dead}, 
      NewNum = NumWorkers - lists:length(Dead)
    end,
    requeue(Dead),
    Pid ! system_check,
    receive {Pid, NumWorkers,SysCpu, SysMem} ->
       [{Pid, NewNum, SysCpu, SysMem} | poolhealth(T)]
    end;
poolhealth(_) -> error.
      

parallel_map(_Collector, _Fun, [], _WorkerQueue) -> [];

parallel_map(Collector, Fun, [H|T], WorkerQueue) ->
    {Worker, NewWorkerQueue} = thread_pool:next_node(WorkerQueue),
    Apply = fun() ->
                    Worker ! {work, self(), Fun, H},
                    receive
                        {result, Val} -> Collector ! {self(), Val}
                    end
            end,
    [spawn(Apply)|parallel_map(Collector, Fun, T, NewWorkerQueue)].

parallel_map(Fun, Ls, PoolSize) ->
    Avengers = parallel_map(self(), Fun, Ls, thread_pool:create(PoolSize)),
    assemble(Avengers). %% Really, they are pids.

parallel_map(Fun, Ls) ->
    parallel_map(Fun, Ls, 1).

parallel_map_pool(Fun, Ls, ThreadPool) ->
    Avengers = parallel_map(self(), Fun, Ls, ThreadPool),
    assemble(Avengers). %% Really, they are pids.


assemble([]) -> [];
assemble([Pid|Rest]) -> receive
                            {Pid, FuncValue} -> [FuncValue|assemble(Rest)]
                        end.


