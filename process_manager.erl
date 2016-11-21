-module(process_manager).
-export([loop/1]).


isdead(Pid) -> process_info(Pid) =:= undefined.

loop(MachineManager) ->
    io:format("DEBUG: Waiting to receive query.~n"),
    receive
        {which_died, Procs} ->
            MachineManager ! {dead_procs, lists:filter(fun isdead/1, Procs)},
            loop(MachineManager)
    end.
