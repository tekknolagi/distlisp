-module(master).
-export([connect_worker_nodes/2, pollhealth/1, parallel_map/4]).
-export([idserver/0, freshid/1, test/1]).

%%%% Need a requeue function to re-assign dead IDs
%%%
%%%  Need wways to assign IDs
%%%
%%%  Need ways to test failures
%%%
%%%  Need to implement schemes besides round robin

connect_worker_nodes([], bymachine) -> [];
connect_worker_nodes([], flat) -> queue:from_list([]);
connect_worker_nodes([H | T], Type) ->
  Comp = net_kernel:connect_node(H),
  if Comp ->
    case Type of
      bymachine ->
            %% TODO: give algorithm
        _InitPid = spawn(H, thread_pool, init_machine, [self(), 1]),
        receive {Pid, Workers, SysCpu, {Total, Alloc, Worst}} ->
          [ {H, Pid, Workers, SysCpu, Total - Alloc} |  connect_worker_nodes(T, Type)]
        end;
      flat ->
            %% TODO: give algorithm
        _InitPid = spawn(H, thread_pool, init_workers, [self(), 1]),
        receive {workers, Threads} ->
           queue:join(Threads,connect_worker_nodes(T, Type))
        end
     end;
     true ->
       error
  end.
  


pollhealth([]) -> [];
pollhealth([{Node, Pid, ThreadPool, SysCpu, SysMem}|T]) ->
    Pid ! system_check,
    receive {Pid, SysCpu, {Total, Alloc, Worst}} ->
       [{Node, Pid, ThreadPool, SysCpu, Total - Alloc} | pollhealth(T)]
    after 100 -> 
       [{timeout, Node} | pollhealth(T)]
    end;
pollhealth(_) -> error.

timed_pollhealth([]) -> [];
timed_pollhealth([{Node, Pid, ThreadPool, SysCpu, SysMem}|T]) ->
   PrevTime = erlang:system_time(),
   Pid ! system_check,
   receive {Pid, SysCpu, {Total, Alloc, Worst}} ->
      [{Node, Pid, ThreadPool, SysCpu, SysMem, erlang:system_time() - PrevTime} 
      | timed_pollhealth(T)]
   after 100 ->
      [{Node, Pid, ThreadPool, SysCpu, SysMem, 10000000000}
       | timed_pollhealth(T)]
   end;
timed_pollhealth(_) -> error.

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
  Ms = timed_pollhealth(Machines),
  [{Node, Pid, Workers, Cpu, Mem, Time}|T] = lists:keysort(6, Ms),
  FreshId = freshid(IdServer),
  Worker = queue:get(Workers),
  Worker ! {delegate, FreshId, Exp, Env},
  queue:in(Worker, Workers),
  [{FreshId, Worker, Time} | parallel_map_timed(IdServer, T, Ms)].
   

parallel_map_memrr(_IdServer, [], _Procs) -> [];
parallel_map_memrr(IdServer, [{Exp, Env}|T], Machines) ->
    [{Node, Pid, Workers, Cpu, Mem} | Ms] = lists:keysort(5, Machines),
    FreshId = freshid(IdServer),
    Worker = queue:get(Workers),
    Worker ! {delegate, FreshId, Exp, Env},
    queue:in(Worker, Workers),
    NewMs = pollhealth(Machines),
   [FreshId | parallel_map_memrr(IdServer, T, NewMs)].


parallel_map_rr(_IdServer, [], _MachineQueue) -> [];
parallel_map_rr(IdServer, [{Exp,Env}|T], ProcQueue) ->
    {Proc, NewProcQueue} = thread_pool:next_node(ProcQueue),
    %Master = self(),
    %Apply = fun() ->
                    FreshId = freshid(IdServer),
                    Proc ! {delegate, FreshId, Exp, Env},
                    io:format("Delegated packet ~p ~n", [FreshId]),
    %        end,
    [FreshId|parallel_map_rr(IdServer, T, NewProcQueue)].


parallel_map(IdServer, WorkPackets, Processes, roundrobin) ->
   Avengers = parallel_map_rr(IdServer, WorkPackets, Processes),
   IdValPairs = assemble(Avengers),
    lists:map(fun({Id, Val}) -> Val end, IdValPairs);
parallel_map(IdServer, WorkPackets, Machines, memroundrobin) ->
   Avengers = parallel_map_memrr(IdServer, WorkPackets, Machines),
   IdValPairs =  assemble(Avengers),
   lists:map(fun({Id, Val}) -> Val end, IdValPairs).
%parallel_map(IdServer, WorkPackets, Machines, timed) ->
%   
       
%     end,
%    assemble(Avengers). %% Really, they are pids.


assemble([]) -> [];
assemble([Id|Rest]) -> 
                    receive
                        {result, Id, Val} ->
                                [{Id, Val}|assemble(Rest)]
                    end.

test(Machines) ->
    io:format("~p is extrmely me~n", [self()]),
    global:register_name(iogl, self()),
    IdServer = spawn(fun idserver/0),
    StartingEnv = basis:basis(),
    %{ok,T,_} = scanner:string("[op+ 1 2];"),
    %{ok,{prog, SimpleExp}} = parser:parse(T),
    %WorkPacket = {SimpleExp, StartingEnv},
    io:format("Calling pmap...~n"),
    Pids = master:connect_worker_nodes(Machines, flat),
    WP = [{{list, [{sym, binplus}, {int, 3},{int, 3}]}, StartingEnv},
          {{list, [{sym, binplus}, {int, 5}, {int, 5}]}, StartingEnv}],
    io:format("Currently connected to: ~p~n", [nodes()]),
                    %WorkPkt = {work, Master, freshid(IdServer), Work},
                    %Machine ! {delegate, WorkPkt},
    %lists:nth(1, Pids) ! {delegate, {work, self(), 0, WorkPacket}},
    %receive
    %    V -> io:format("RECEIVED ~p~n", [V])
    %end,
    %Res = parallel_map(IdServer, [WorkPacket,WorkPacket], queue:from_list(Pids)),
   % io:format("Res: ~p~n", [Res]).
   master:parallel_map(IdServer, WP, Pids, roundrobin).
  
