-module(concurrency).
-export([parallel_map/2, parallel_map/3]).
-export([parallel_map_pool/3]).
-export([merge_sort/1, test_sort/0]).

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
    Avengers = parallel_map(self(), Fun, Ls, thread_pool:create(PoolSize)),
    assemble(Avengers). %% Really, they are pids.

parallel_map(Fun, Ls) ->
    parallel_map(Fun, Ls, 1).

parallel_map_pool(Fun, Ls, ThreadPool) ->
    Avengers = parallel_map(self(), Fun, Ls, ThreadPool),
    assemble(Avengers). %% Really, they are pids.


assemble([]) -> [];
assemble([Pid|Rest]) -> receive
                            {Pid, FuncValue} -> [FuncValue|assemble(Rest)]
                        end.

merge(A, []) ->
    A;
merge([], B) ->
    B;
merge([HA|TA], [HB|TB]) when HA =< HB ->
    [HA|merge(TA, [HB|TB])];
merge(A, [HB|TB]) ->
    [HB|merge(A, TB)].

merge_sort([]) ->
    [];
merge_sort([A]) ->
    [A];
merge_sort(Ls) ->
    {HalfA, HalfB} = lists:split(length(Ls) div 2, Ls),
    SortSubList = fun(SubList) ->
                          merge_sort(SubList)
                  end,
    %% merge_sort should be able to have two workers per split
    [SortedA, SortedB] = parallel_map(SortSubList, [HalfA, HalfB], 2),
    merge(SortedA, SortedB).

curtime() ->
    erlang:convert_time_unit(erlang:monotonic_time(), native, micro_seconds).

test_sort() ->
    random:seed(curtime()),
    TestLs = [random:uniform(1000) || _ <- lists:seq(1, 10000)],
    StartTime = curtime(),
    MergeSorted = concurrency:merge_sort(TestLs),
    EndTime = curtime(),
    io:format("merge sort took  ~B~n", [EndTime-StartTime]),
    SystemSorted = lists:sort(TestLs),
    EndSystemTime = curtime(),
    io:format("system sort took ~B~n", [EndSystemTime-EndTime]),
    SystemSorted == MergeSorted.
