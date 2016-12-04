-module(stealingworker).
-export([agent_loop/0]).

%%% Worker section.

waitforwork(Agent) ->
    Agent ! {request_for_work, self()},
    receive
        {work, Master, {Id, Exp, Env}} ->
            {Val, _} = eval:evalexp(Exp, Env),
            Master ! {result, Id, Val};
        nothing_yet ->
            timer:sleep(100);
        What ->
            io:format("Received invalid response of ~p~n", [What])
    end,
    waitforwork(Agent).

%%% Agent section.

agent_loop() ->
    receive
        {master, Master} ->
            Self = self(),
            Worker = spawn(fun() -> waitforwork(Self) end),
            WorkQueue = queue:new(),
            agent_loop(Worker, Master, WorkQueue, queue:new())
    end.

checkforwork(OtherAgent, MyWorker) ->
    OtherAgent ! {steal_yo_work, MyWorker}.

agent_loop(Worker, Master, WorkQueue, OtherAgents) ->
    receive
        {send_state, Requester} ->
            Requester ! {state, Worker, Master, queue:to_list(WorkQueue)},
            agent_loop(Worker, Master, WorkQueue, OtherAgents);
        {delegate, Id, Exp, Env} ->
            NewWorkQueue = queue:in({work, Master, {Id, Exp, Env}}, WorkQueue),
            agent_loop(Worker, Master, NewWorkQueue, OtherAgents);
        {steal_yo_work, WorkerFromAgent} ->
            case queue:out(WorkQueue) of
                {empty, WorkQueue} ->
                    WorkerFromAgent ! nothing_yet,
                    agent_loop(Worker, Master, WorkQueue, OtherAgents);
                {{value, NextItem}, NewWorkQueue} ->
                    WorkerFromAgent ! NextItem,
                    agent_loop(Worker, Master, NewWorkQueue, OtherAgents)
            end;
        {request_for_work, Requester} ->
            case queue:out(WorkQueue) of
                {empty, WorkQueue} ->
                    case queue:out(OtherAgents) of
                        {empty, OtherAgents} ->
                            Requester ! nothing_yet,
                            agent_loop(Worker, Master, WorkQueue, OtherAgents);
                        {{value, FirstAgent}, NewAgentQueue} ->
                            checkforwork(FirstAgent, Requester),
                            agent_loop(Worker, Master, WorkQueue,
                                       queue:in(FirstAgent, NewAgentQueue))
                   end;
                {{value, NextItem}, NewWorkQueue} ->
                    Requester ! NextItem,
                    agent_loop(Worker, Master, NewWorkQueue, OtherAgents)
            end;
        {other_agents, AgentList} ->
            agent_loop(Worker, Master, WorkQueue, queue:from_list(AgentList));
        _SomethingElse ->
            agent_loop(Worker, Master, WorkQueue, OtherAgents)
    end.
