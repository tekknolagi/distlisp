-module(process_manager).
-export([loop/1]).


isdead({Pid, _}) -> process_info(Pid) =:= undefined.

get_id({_, Id}) -> Id.

get_pid({Pid, _}) -> Pid.


loop(MachineManager) ->
    receive
        {which_died, Pairs} ->
            DeadPairs = lists:filter(fun isdead/1, Pairs),
            DeadIds = lists:map(fun get_id/1, DeadPairs),
            DeadPids = lists:map(fun get_pid/1, DeadPairs),
            MachineManager ! {dead_procs, DeadIds, DeadPids},
            loop(MachineManager)
    end.
