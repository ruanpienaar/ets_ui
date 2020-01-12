-module(ets_ui_match_tests).

-include_lib("eunit/include/eunit.hrl").

match_object_unit_test_() ->
    {foreach,
     fun() ->
        A = ets:new(test_table_1,
            [public, named_table, ordered_set]),
        [A]
     end,
     fun( Tables) ->
        [ ets:delete(Table) || Table <- Tables ]
     end,
     [
        {"start and query",
            % fun startup_and_query/0
            ?_assert(unit_testing:try_test_fun(fun match_object_startup_and_query/0))
        },
        {"start and expire",
            % fun startup_and_expire/0
            ?_assert(unit_testing:try_test_fun(fun match_object_startup_and_expire/0))
        }
     ]
    }.

match_object_startup_and_query() ->
    {ok, Pid1} = ets_ui_match:start_link(match_object, test_table_1, '_', 1),
    ?assertMatch(
        {ok, []}, %% TODO: get exact continuation
        ets_ui_match:get_more(Pid1)
    ),
    ?assertException(
        exit,
        {noproc, {gen_statem,call,[Pid1, get_more, infinity]}},
        ets_ui_match:get_more(Pid1)
    ),
    ?assertEqual(
        undefined,
        erlang:process_info(Pid1)
    ),
    %% [setup] Insert something
    true = ets:insert(test_table_1, {1, 1}),
    true = ets:insert(test_table_1, {2, 2}),
    true = ets:insert(test_table_1, {3, 3}),
    true = ets:insert(test_table_1, {4, 4}),
    {ok, Pid2} = ets_ui_match:start_link(match_object, test_table_1, '_', 2),
    ?assertEqual(
        {ok, [{1, 1}, {2, 2}]},
        ets_ui_match:get_more(Pid2)
    ),
    ?assertEqual(
        {ok, [{3, 3}, {4, 4}]},
        ets_ui_match:get_more(Pid2)
    ),
    ?assertEqual(
        {ok, []}, %% TODO: get exact continuation
        ets_ui_match:get_more(Pid2)
    ),
    ?assertException(
        exit,
        {noproc, {gen_statem,call,[Pid2, get_more, infinity]}},
        ets_ui_match:get_more(Pid2)
    ),
    ?assertEqual(
        undefined,
        erlang:process_info(Pid2)
    ).

match_object_startup_and_expire() ->
    ExpiryTimeMs = 100,
    ok = application:set_env(ets_ui, continuation_expiry_time, ExpiryTimeMs),
    {ok, Pid1} = ets_ui_match:start_link(match_object, test_table_1, '_', 1),
    timer:sleep(ExpiryTimeMs),
    ?assertException(
        exit,
        {noproc, {gen_statem,call,[Pid1, get_more, infinity]}},
        ets_ui_match:get_more(Pid1)
    ),
    ?assertEqual(
        undefined,
        erlang:process_info(Pid1)
    ).
