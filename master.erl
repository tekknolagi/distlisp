-module(master).
-export([connect_worker_nodes/2, pollhealth/1, parallel_map/3]).
-export([idserver/0, freshid/1, test/1]).

%%%% Need a requeue function to re-assign dead IDs
%%%
%%%  Need wways to assign IDs
%%%
%%%  Need ways to test failures
%%%
%%%  Need to implement schemes besides round robin

connect_worker_nodes([], _Type) -> [];
connect_worker_nodes([H | T], Type) ->
  Comp = net_kernel:connect_node(H),
  if Comp ->
    case Type of
      bymachine ->
            %% TODO: give algorithm
        _InitPid = spawn(H, thread_pool, init_machine, [self(), 1]),
        receive {Pid, Workers, SysCpu, SysMem} ->
          [ {H, Pid, Workers, SysCpu, SysMem} |  connect_worker_nodes(T, Type)]
        end;
      flat ->
            %% TODO: give algorithm
        _InitPid = spawn(H, thread_pool, init_workers, [self(), 1]),
        receive {workers, Threads} ->
           Threads ++ connect_worker_nodes(T, Type)
        end
     end;
     true ->
       error
  end.
  


pollhealth([]) -> [];
pollhealth([{Node, Pid, ThreadPool, SysCpu, SysMem}|T]) ->
    Pid ! system_check,
    receive {Pid, SysCpu, SysMem} ->
       [{Node, Pid, ThreadPool, SysCpu, SysMem} | pollhealth(T)]
    after 100 -> 
       [{timeout, Node} | pollhealth(T)]
    end;
pollhealth(_) -> error.


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


parallel_map_rr(_IdServer, [], _MachineQueue) -> [];
parallel_map_rr(IdServer, [{Exp,Env}|T], MachineQueue) ->
    {Machine, NewMachineQueue} = thread_pool:next_node(MachineQueue),
    Master = self(),
    Apply = fun() ->
                    io:format("Sending delegate to ~p...~n", [Machine]),
                    FreshId = freshid(IdServer),
                    Machine ! {delegate, FreshId, Exp, Env},
                    io:format("...sent.~n"),
                    receive
                        {result, Id, Val} ->
                            io:format("Sending ~p for collation...~n",
                                      [Id]),
                            Master ! {collate, self(), Id, Val}
                    end
            end,
    [spawn(Apply)|parallel_map_rr(Master, T, NewMachineQueue)].


parallel_map(IdServer, WorkPackets, MachineQueue) ->
    Avengers = parallel_map_rr(IdServer, WorkPackets, MachineQueue),
    assemble(Avengers). %% Really, they are pids.


assemble([]) -> [];
assemble([Pid|Rest]) -> receive
                            {collate, Pid, Id, Val} ->
                                io:format("Collating ~p~n", [Id]),
                                io:format("Rest ~p~n", [Rest]),
                                [{Id, Val}|assemble(Rest)]
                        end.

test(Machines) ->
    global:register_name(iogl, self()),
    IdServer = spawn(fun idserver/0),
    StartingEnv = basis:basis(),
    {ok,T,_} = scanner:string("[op+ 1 2];"),
    {ok,{prog, SimpleExp}} = parser:parse(T),
    WorkPacket = {SimpleExp, StartingEnv},
    io:format("Calling pmap...~n"),
    Pids = lists:map(fun({Pid,_,_,_}) -> Pid end, connect_worker_nodes(Machines, flat )),
    io:format("Currently connected to: ~p~n", [nodes()]),
                    %WorkPkt = {work, Master, freshid(IdServer), Work},
                    %Machine ! {delegate, WorkPkt},
    %lists:nth(1, Pids) ! {delegate, {work, self(), 0, WorkPacket}},
    %receive
    %    V -> io:format("RECEIVED ~p~n", [V])
    %end,
    Res = parallel_map(IdServer, [WorkPacket,WorkPacket], queue:from_list(Pids)),
    io:format("Res: ~p~n", [Res]).
