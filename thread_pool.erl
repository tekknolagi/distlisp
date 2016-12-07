-module(thread_pool).
-export([create/2, add/1, add/2]).
-export([waitforwork/0, loop/1, calculate1/0, calculate2/0, reap/3]).
-export([selfstart/3]).


selfstart(MasterNode, CalcAlg, AgentOpts) ->
    Comp = net_kernel:connect_node(MasterNode),
    MasterPid = {master, MasterNode},
    case Comp of
        true ->
            application:load(sasl),
            application:set_env(sasl, sasl_error_logger, {file, "/dev/null"}),
            application:start(sasl),

            application:load(os_mon),
            application:set_env(os_mon, os_mon_error_logger, {file, "/dev/null"}),
            application:start(os_mon),

            NumProcs = case CalcAlg of
                           1 -> calculate1();
                           2 -> calculate2()
                       end,

            AgentsQueue = create(NumProcs, AgentOpts),
            AgentsList = queue:to_list(AgentsQueue),

            lists:map(fun(Agent) ->
                              Agent ! {master, MasterPid},
                              Agent ! {other_agents, AgentsQueue}
                      end, AgentsList),

            MasterPid ! {register, self(), node(),
                         erlang:system_info(logical_processors_available),
                         memsup:get_memory_data(), AgentsQueue};
        false -> error({selfstart, masterconnect_failed})
    end,
    loop(MasterPid).


newagent(Server, Modes) ->
    spawn(Server, stealingworker, agent_loop, [Modes]).

newagent(Server) ->
    newagent(Server, {fullspeed, stealing}).


create(0, _Modes) -> queue:new();

%create(1, ServerPool, AgentOpts, t) ->
%    {NextServer, _NewPool} = next_node(ServerPool),
%    Agent = newagent(NextServer, AgentOpts),
%    queue:from_list([Agent]);

create(NumNodes, Modes={{slow, SlowProb}, IsStealing}) when NumNodes >= 1 ->
    Rand = rand:uniform(),
    Agent = if
                Rand < SlowProb ->
                    newagent(node(), {slow, IsStealing});
                true ->
                    newagent(node(), {fullspeed, IsStealing})
            end,
    queue:in(Agent, create(NumNodes-1, Modes)).


add(Nodes, 0) -> Nodes;

add(Nodes, NumNodes) when NumNodes > 0 ->
    queue:in(spawn(fun waitforwork/0), add(Nodes, NumNodes-1)).

add(Nodes) ->
    add(Nodes, 1).

percore() -> 50.

calculate1() ->
    {Total, Alloc, _} = memsup:get_memory_data(),
    erlang:min((Total - Alloc)  div 1048576, percore()).

calculate2() ->
    {Total, Alloc, _} = memsup:get_memory_data(),
    Cores = erlang:system_info(logical_processors_available),
    erlang:min((Total - Alloc) div 1048576 * 4, percore()*Cores).


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

%% init_workers (Master, CalcAlg) ->
%%   application:start(sasl),
%%   application:start(os_mon),
%%   ThreadPool = case CalcAlg of
%%     1 -> create(calculate1());
%%     2 -> create(calculate2())
%%   end,
%%   lists:map(fun(Agent) -> Agent ! {master, Master} end,
%%             queue:to_list(ThreadPool)),
%%   Master ! {workers, ThreadPool}.
%% 
%% init_machine (Master, CalcAlg) ->
%%   application:start(sasl),
%%   application:start(os_mon),
%%   ThreadPool = case CalcAlg of
%%     1 -> create(calculate1());
%%     2 -> create(calculate2())
%%   end,
%%   lists:map(fun(Agent) -> Agent ! {master, Master} end,
%%             queue:to_list(ThreadPool)),
%%   Master ! {self(), ThreadPool, erlang:system_info(logical_processors_available),
%%             memsup:get_memory_data()},
%%   loop(Master).

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
