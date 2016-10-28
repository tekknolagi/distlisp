-module(concurrency).
-export([parallel_map/2, parallel_map/3]).

parallel_map(_Collector, _Fun, [], _WorkerQueue) -> [];
parallel_map(Collector, Fun, [H|T], WorkerQueue) ->
    {Worker, NewWorkerQueue} = thread_pool:next_node(WorkerQueue),
    Apply = fun() ->
                    Worker ! {work, self(), Fun, H},
                    receive
                        {result, Val} -> Collector ! {self(), Val}
                    end
            end,
    [spawn(Apply)|parallel_map(Collector, Fun, T, NewWorkerQueue)].
parallel_map(Fun, Ls, PoolSize) ->
    ThreadPool = thread_pool:create(PoolSize),
    Avengers = parallel_map(self(), Fun, Ls, ThreadPool),
    assemble(Avengers). %% Really, they are pids.
parallel_map(Fun, Ls) ->
    ThreadPool = thread_pool:create(1),
    Avengers = parallel_map(self(), Fun, Ls, ThreadPool),
    assemble(Avengers). %% Really, they are pids.

assemble([]) -> [];
assemble([Pid|Rest]) -> receive
                            {Pid, FuncValue} -> [FuncValue|assemble(Rest)]
                        end.
