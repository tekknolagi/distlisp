-module(stealingworker).
-export([agent_loop/1]).

%%% Worker section.

waitforwork(Agent) ->
    io:format("Requesting work...~n"),
    Agent ! {request_for_work, self()},
    receive
        {work, Master, {Id, Exp, Env}} ->
            Master ! {result, Id, eval:evalexp(Exp, Env)};
        nothing_yet ->
            timer:sleep(500);
        What ->
            io:format("Received invalid response of ~p~n", [What])
    end,
    waitforwork(Agent).

%%% Agent section.

agent_loop(Master) ->
    io:format("New agent being created~n"),
    receive
        {master, Master} ->
            Worker = spawn(fun() -> waitforwork(self()) end),
            WorkQueue = queue:new(),
            agent_loop(Worker, Master, WorkQueue)
    end.

agent_loop(Worker, Master, WorkQueue) ->
    receive
        {send_state, Requester} ->
            Requester ! {state, Worker, Master, queue:to_list(WorkQueue)},
            agent_loop(Worker, Master, WorkQueue);
        {delegate, Id, Exp, Env} ->
            io:format("Received delegate~n"),
            NewQueue = queue:in({work, Master, {Id, Exp, Env}}, WorkQueue),
            agent_loop(Worker, Master, NewQueue);
        {request_for_work, Requester} ->
            io:format("Received request for work~n"),
            case queue:out(WorkQueue) of
                {empty, OldQueue} ->
                    Requester ! nothing_yet,
                    agent_loop(Worker, Master, OldQueue);
                {{value, NextItem}, NewQueue} ->
                    Requester ! {work, Master, NextItem},
                    agent_loop(Worker, Master, NewQueue)
            end
    end.
