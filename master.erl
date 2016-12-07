-module(master).
% -export([connect_worker_nodes/3, timed_pollhealth/1, parallel_map/4]).
-export([idserver/0, freshid/1, test_rr/1, test_mrr/1, test_timed/1]).
-export([delegator/1]).


shuffle(Ls) ->
   [X || {_, X} <- lists:sort([ {random:uniform(), N} || N <- Ls])].

%%%% Need a requeue function to re-assign dead IDs
%%%
%%%  Need wways to assign IDs
%%%
%%%  Need ways to test failures
%%%
%%%  Need to implement schemes besides round robin

%% aggregate_workers(Type) ->
%%    register(node(), self()),
%%    receive
%%      {worker, Node, Pid, Mem, Cpu} ->
%%        [{Node, Pid, Mem, Cpu}|aggregate_workers()];
%%      {last, Node, Pid, Mem, Cpu} ->
%%        [{Node, Pid, Mem, Cpu}|[]]
%%    end.


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


delegator(Modes) ->
    register(master, self()),
    delegator(Modes, spawn(fun idserver/0), [], [], queue:new()).
delegator(Modes={DistMode}, IdServer, Machines, FlatAgentsList, FlatAgentsQueue) ->
    receive
        {register, Pid, Name, Cpu, {Total, Alloc, _Worst}, AgentsQueue} ->
            AgentsList = queue:to_list(AgentsQueue),
            NewAgentsList = AgentsList ++ FlatAgentsList,
            NewAgentsQueue = queue:join(AgentsQueue, FlatAgentsQueue),
            lists:map(fun (Agent) -> Agent ! {other_agents, AgentsQueue} end,
                      FlatAgentsList),
            lists:map(fun (Agent) -> Agent ! {other_agents, FlatAgentsQueue} end,
                      AgentsList),
            Machine = {Name, Pid, AgentsQueue, Cpu, Total-Alloc, 0},
            delegator(Modes, IdServer, [Machine|Machines],
                      NewAgentsList, NewAgentsQueue);
        {delegate, WorkPackets, CallbackPid} ->
            Receivers = case DistMode of
                            roundrobin -> FlatAgentsList;
                            timed -> Machines;
                            memroundrobin -> Machines
                        end,
            Results = parallel_map(IdServer, WorkPackets, Receivers, DistMode),
            CallbackPid ! {results, CallbackPid, Results},
            delegator(Modes, IdServer, Machines,
                      FlatAgentsList, FlatAgentsQueue);
        Other -> io:format("Received malformed message ~p~n", [Other])
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
    {Proc, NewProcQueue} = next_node(ProcQueue),
    FreshId = freshid(IdServer),
    Proc ! {delegate, FreshId, Exp, Env},
    [FreshId|parallel_map_rr(IdServer, T, NewProcQueue)].


next_node(Nodes) ->
    {{value, FirstNode}, RestNodes} = queue:out(Nodes),
    {FirstNode, queue:in(FirstNode, RestNodes)}.


parallel_map(IdServer, WorkPackets, Processes, roundrobin) ->
    Avengers = parallel_map_rr(IdServer, WorkPackets, queue:from_list(Processes)),
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
