-module(master).
-export([machinedata/1,pollhealth/1, parallel_map/2, parallel_map/3, parallel_map_pool/3]).

%%%% Need a requeue function to re-assign dead IDs
%%%
%%%  Need wways to assign IDs
%%%
%%%  Need ways to test failures
%%%
%%%  Need to implement schemes besides round robin

machinedata([]) -> [];
machinedata([H | T]) ->
  Comp = net_kernel:connect_node(H),
  if Comp ->
    Pid = spawn(H, thread_pool, init_machine, [self()]),
    receive {Pid, NumWorkers, SysCpu, SysMem} ->
      [{Pid, NumWorkers, SysCpu, SysMem} | machinedata(T)]
    end;
  true -> error
  end;
machinedata(Node) ->
  Comp = net_kernel:connect_node(Node),
  if Comp ->
    Pid = spawn(Node, thread_pool, init_machine, [self()]),
    receive {Node, NumWorkers, SysCpu, SysMem} ->
      {Pid, NumWorkers, SysCpu, SysMem}
    end;
  true -> error
  end;

machinedata(_) -> error.

pollhealth([]) -> [];
pollhealth([{Pid, NumWorkers, SysCpu, SysMem}|T]) ->
    Pid ! check_who_died,
    io:format("Checking who died~n"),
    receive {dead, Pid, Dead} -> 
      NumDead = length(Dead),
      NewNum = NumWorkers - NumDead
    end,
%    requeue(Dead),
    Pid ! system_check,
    receive {Pid, NumWorkers,SysCpu, SysMem} ->
       [{Pid, NewNum, SysCpu, SysMem} | pollhealth(T)]
    end;
pollhealth(_) -> error.
      

parallel_map(_Collector, _Fun, [], _WorkerQueue) -> [];

parallel_map(Collector, Fun, [{Exp, Env}|T], Pool) ->
    {Worker, NewWorkerQueue} = thread_pool:next_node(Pool),
    Apply = fun() ->
                    Worker ! {delegate, {work, self(), 0, {Exp, Env}}},
                    receive
                        {result, Val} -> Collector ! {self(), Val}
                    end
            end,
    [spawn(Apply)|parallel_map(Collector, Fun, T, NewWorkerQueue)].

parallel_map(Fun, Ls, Nodes) ->
    Avengers = parallel_map(self(), Fun, Ls, machinedata(Nodes)),
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


