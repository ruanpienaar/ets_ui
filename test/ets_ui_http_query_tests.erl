-module(ets_ui_http_query_tests).

-include_lib("eunit/include/eunit.hrl").

-define(M, ets_ui_http_query).
-define(TABLE, ?MODULE).

-define(TRUEFILTER, fun(_,_) -> true end).
-define(FALSEFILTER, fun(_,_) -> false end).

%%% TESTCASES
empty_test_() ->
    {foreach,
     fun setup_empty/0,
     fun teardown/1,
     [{"Paged", fun empty_paged/0}]
    }.

with_table_content_test_() ->
    {foreach,
     fun setup/0,
     fun teardown/1,
     [{"With content but filter never satisfied", fun simple_false_filter/0},
      {"With content and filter always satisfied", fun simple_no_filter/0}
     ]
    }.

empty_paged() ->
    ?assertEqual([], ?M:paged(?TABLE, 1, 20, ?TRUEFILTER)),
    ?assertEqual([], ?M:paged(?TABLE, 1, 20, ?FALSEFILTER)),
    ?assertEqual([], ?M:paged(?TABLE, 0, 20, ?TRUEFILTER)),
    ?assertEqual([], ?M:paged(?TABLE, 0, 20, ?FALSEFILTER)),
    ?assertEqual([], ?M:paged(?TABLE, -1, 20, ?TRUEFILTER)),
    ?assertEqual([], ?M:paged(?TABLE, -1, 20, ?FALSEFILTER)),
    ok.

simple_false_filter() ->
    ?assertEqual([], ?M:paged(?TABLE, 1, 20, ?FALSEFILTER)),
    ?assertEqual([], ?M:paged(?TABLE, -1, 20, ?FALSEFILTER)),
    ?assertEqual([], ?M:paged(?TABLE, 0, 20, ?FALSEFILTER)),
    ok.

simple_no_filter() ->
    ?assertEqual(10, length(?M:paged(?TABLE, 0, 20, ?TRUEFILTER))),
    ?assertEqual(10, length(?M:paged(?TABLE, -1, 20, ?TRUEFILTER))),
    ?assertEqual([], ?M:paged(?TABLE, 1, 20, ?TRUEFILTER)),
    ?assertEqual([], ?M:paged(?TABLE, 2, 20, ?TRUEFILTER)),
    ?assertEqual(1, length(?M:paged(?TABLE, 1, 1, ?TRUEFILTER))),
    ?assertEqual(1, length(?M:paged(?TABLE, 9, 1, ?TRUEFILTER))),
    ?assertEqual([], ?M:paged(?TABLE, 10, 1, ?TRUEFILTER)),
    ok.


%%% HELPERS
setup_empty() ->
    ets:new(?TABLE, [named_table, ordered_set]),
    ok.

setup() ->
    ets:new(?TABLE, [named_table, ordered_set]),
    DummyData = lists:map(fun(I) -> {I, I} end, lists:seq(1,10)),
    ets:insert(?TABLE, DummyData),
    ok.

teardown(_) ->
    ets:delete(?TABLE),
    ok.
