-module(ets_ui_ets_tests).

-include_lib("eunit/include/eunit.hrl").

-define(M, ets_ui_ets).
-define(TABLE, ?MODULE).

-define(NOFILTER, fun(_,_) -> true end).
-define(FALSEFILTER, fun(_,_) -> false end).

%%% TESTCASES
empty_flow_test_() ->
    {foreach,
     fun setup_empty/0,
     fun teardown/1,
     [{"First on empty table", fun empty_first/0}]
    }.


simple_set_flow_test_() ->
    {foreach,
     fun setup/0,
     fun teardown/1,
     [{"First on a dummy table", fun first/0},
      {"Iterate on a dummy table", fun next/0}]
    }.

empty_first() ->
    ?assertEqual('$end_of_table', ?M:filter_first(?TABLE, ?NOFILTER)),
    ?assertEqual('$end_of_table', ?M:filter_first(?TABLE, ?FALSEFILTER)),
    ok.

first() ->
    ?assertMatch({_A, [{B,B}]}, ?M:filter_first(?TABLE, ?NOFILTER)),
    Filter = fun(3, [{3,3}]) -> true; (_, _) -> false end,
    ?assertMatch({_, [{3,3}]}, ?M:filter_first(?TABLE, Filter)),
    ?assertEqual('$end_of_table', ?M:filter_first(?TABLE, ?FALSEFILTER)),
    ok.

next() ->
    Filter = fun(3, [{3,3}]) -> true; (_, _) -> false end,

    First = ets:first(?TABLE),
    ?assertMatch({_A, [{B, B}]}, ?M:filter_next(?TABLE, First, ?NOFILTER)),
    ?assertMatch({_, [{3,3}]}, ?M:filter_next(?TABLE, First, Filter)),
    ?assertMatch('$end_of_table', ?M:filter_next(?TABLE, First, ?FALSEFILTER)),

    %% THIS ISN'T VERY NICE should always return a single end_of_table
    Last = ets:last(?TABLE),
    ?assertMatch({'$end_of_table', [{B, B}]}, ?M:filter_next(?TABLE, Last, ?NOFILTER)),
    ?assertMatch({'$end_of_table', [{3, 3}]}, ?M:filter_next(?TABLE, Last, Filter)),
    ?assertMatch('$end_of_table', ?M:filter_next(?TABLE, Last, ?FALSEFILTER)),    
    ok.

%%% HELPERS
setup_empty() ->
    ets:new(?TABLE, [named_table, ordered_set]),
    ok.

setup() ->
    ets:new(?TABLE, [named_table, ordered_set]),
    DummyData = lists:map(fun(I) -> {I, I} end, lists:seq(1,3)),
    ets:insert(?TABLE, DummyData),
    ok.

teardown(_) ->
    ets:delete(?TABLE),
    ok.
      
