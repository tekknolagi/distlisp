-module(process_manager).
-export([loop/1]).


isdead({Pid, Id}) -> process_info(Pid) =:= undefined.

get_id({Pid, Id}) -> Id.

get_pid({Pid, Id}) -> Pid.

loop(MachineManager) ->
    io:format("DEBUG: Waiting to receive query.~n"),
    receive
        {which_died, Procs} ->
            DeadPairs = lists:filter(fun isdead/1, Procs),
            DeadIds = lists:map(fun get_id/1, DeadPairs),
            DeadPids = lists:map(fun get_pid/1, DeadPairs),
            MachineManager ! {dead_procs, DeadIds, DeadPids},
            loop(MachineManager)
    end.
