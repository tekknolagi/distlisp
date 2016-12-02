-module(stealingworker).
-export([agent_loop/1]).

%%% Worker section.

waitforwork(Agent) ->
    Agent ! {request_for_work, self()},
    receive
        {work, Master, {Id, Exp, Env}} ->
            Master ! {result, Id, eval:evalexp(Exp, Env)};
        nothing_yet ->
            timer:sleep(500)
    end,
    waitforwork(Agent).

%%% Agent section.

agent_loop(Master) ->
    Worker = spawn(fun() -> waitforwork(self()) end),
    WorkQueue = queue:new(),
    agent_loop(Worker, Master, WorkQueue).

agent_loop(Worker, Master, WorkQueue) ->
    receive
        {request_for_work, Requester} ->
            case queue:out(WorkQueue) of
                {empty, OldQueue} ->
                    Requester ! nothing_yet,
                    agent_loop(Worker, Master, OldQueue);
                {{value, NextItem}, NewQueue} ->
                    Requester ! {work, Master, NextItem},
                    agent_loop(Worker, Master, NewQueue)
            end
    end.
