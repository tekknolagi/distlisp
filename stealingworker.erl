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
            agent_loop(Worker, Master, WorkQueue)
    end.

agent_loop(Worker, Master, WorkQueue) ->
    receive
        {send_state, Requester} ->
            Requester ! {state, Worker, Master, queue:to_list(WorkQueue)},
            agent_loop(Worker, Master, WorkQueue);
        {delegate, Id, Exp, Env} ->
            NewQueue = queue:in({work, Master, {Id, Exp, Env}}, WorkQueue),
            agent_loop(Worker, Master, NewQueue);
        {request_for_work, Requester} ->
            case queue:out(WorkQueue) of
                {empty, OldQueue} ->
                    Requester ! nothing_yet,
                    agent_loop(Worker, Master, OldQueue);
                {{value, NextItem}, NewQueue} ->
                    Requester ! NextItem,
                    agent_loop(Worker, Master, NewQueue)
            end
    end.
