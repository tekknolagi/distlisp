-module(stealingworker).
-export([agent_loop/1]).

%%% Worker section.

waitforwork(Agent, {SpeedMode}) ->
    Agent ! {request_for_work, self()},
    receive
        {work, Master, {Id, Exp, Env}} ->
            case SpeedMode of
                fullspeed ->
                    {Val, _} = eval:evalexp(Exp, Env),
                    Master ! {result, Id, Val};
                slow ->
                    basis:timerstart(),
                    {Val, _} = eval:evalexp(Exp, Env),
                    {_, _, wall, Time} = basis:timerend(),
                    timer:sleep(Time), % Sloooooow worker. Takes 2x as long.
                    Master ! {result, Id, Val}
            end;
        nothing_yet ->
            timer:sleep(100);
        What ->
            io:format("Received invalid response of ~p~n", [What])
    end,
    waitforwork(Agent, {SpeedMode}).


%%% Agent section.

% SpeedMode options: fullspeed, slow
% StealingEnabled options: stealing, solitary
agent_loop({SpeedMode, StealingEnabled}) ->
    receive
        {master, Master} ->
            Self = self(),
            agent_loop(spawn(fun() -> waitforwork(Self, {SpeedMode}) end),
                       {Master, queue:new(), queue:new(), {StealingEnabled}})
    end.


checkforwork(OtherAgent, MyWorker) ->
    OtherAgent ! {steal_yo_work, MyWorker}.

agent_loop(Worker, Info={Master, WorkQueue, OtherAgents, Options}) ->
    receive
        {send_state, Requester} ->
            Requester ! {state, Worker, Master, queue:to_list(WorkQueue)},
            agent_loop(Worker, Info);
        {delegate, Id, Exp, Env} ->
            NewWorkQueue = queue:in({work, Master, {Id, Exp, Env}}, WorkQueue),
            agent_loop(Worker, {Master, NewWorkQueue, OtherAgents, Options});
        {steal_yo_work, WorkerFromAgent} ->
            case queue:out(WorkQueue) of
                {empty, WorkQueue} ->
                    WorkerFromAgent ! nothing_yet,
                    agent_loop(Worker, Info);
                {{value, NextItem}, NewWorkQueue} ->
                    WorkerFromAgent ! NextItem,
                    agent_loop(Worker, {Master, NewWorkQueue, OtherAgents,
                               Options})
            end;
        {request_for_work, Requester} ->
            case queue:out(WorkQueue) of
                {empty, WorkQueue} ->
                    case Options of
                        {stealing} ->
                            Requester ! nothing_yet,
                            agent_loop(Worker, Info);
                        {solitary} ->
                            case queue:out(OtherAgents) of
                                {empty, OtherAgents} ->
                                    Requester ! nothing_yet,
                                    agent_loop(Worker, Info);
                                {{value, FirstAgent}, NewAgentQueue} ->
                                    checkforwork(FirstAgent, Requester),
                                    agent_loop(Worker, {Master, WorkQueue,
                                               queue:in(FirstAgent,
                                                        NewAgentQueue),
                                               Options})
                            end
                    end;
                {{value, NextItem}, NewWorkQueue} ->
                    Requester ! NextItem,
                    agent_loop(Worker, {Master, NewWorkQueue, OtherAgents,
                                        Options})
            end;
        {other_agents, AgentsQueue} ->
            agent_loop(Worker, {Master, WorkQueue, queue:join(AgentsQueue,
                                                              OtherAgents),
                                Options});
        _SomethingElse -> agent_loop(Worker, Info)
    end.
