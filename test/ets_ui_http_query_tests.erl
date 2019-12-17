-module(ets_ui_http_query_tests).

% -define(NODEBUG, true).

-include_lib("eunit/include/eunit.hrl").
-include("ets_ui.hrl").

-record(test_record, { %% This is element 1 of the tuple ( records are special tuples )
    key_1, %% This will be the KEY of the ETS table ! ( keypos == 2 )
    version = 1
}).

%% @ Memo!
%%   We'll assume page size is not being sent in the correct range for now.
%% @

unit_test_() ->
    {foreach,
     fun() ->
        A = ets:new(test_table_1,
            [public, named_table, ordered_set]),
        B = ets:new(test_table_2,
            [public, named_table, ordered_set, {keypos, 2}]),
        [A, B]
        %% TODO: test diff tables
        % ets:new(test_table_1, [protected, named_table, ordered_set]),
        % ets:new(test_table_1, [private, named_table, ordered_set])
     end,
     fun(Tables) ->
        % Using the delete to clear the table :)
        [ ets:delete(Table) || Table <- Tables ]
     end,
     [
        {"ets_ui_http_query:get_next_n_objects/3", fun get_next_n_objects/0},
        {"ets_ui_http_query:handle_request/3 match_object", fun handle_request_match_object/0},
        {"ets_ui_http_query:handle_request/3 lookup", fun handle_request_lookup/0},
        {"ets_ui_http_query:handle_request/3 page_through_table", fun handle_request_page_through_table/0}
     ]
    }.

get_next_n_objects() ->
    %% Run over empty table ( no continuation + no entries )
    ?assertEqual(
        #{
            continuation => '$end_of_table',
            entries => [],
            key_type => atom
        },
        ets_ui_http_query:get_next_n_objects(
            test_table_1, % Table
            undefined, % Continuation
            20 % PageSize
        )
    ),

    %% [setup] Insert something
    true = ets:insert(test_table_1, {1, 1}),

    %% test 1 entry ( no continuation + 1 entry )
    ?assertEqual(
        #{
            continuation => '$end_of_table',
            entries =>
                [
                    {1, 1}
                ],
            key_type => atom
        },
        ets_ui_http_query:get_next_n_objects(
            test_table_1, % Table
            undefined, % Continuation
            20 % PageSize
        )
    ),

    %% NB! We'll assume the backend will reach its end and not wrap around
    %%     back to the starting point.
    %% test 1 entry ( continuation + 1 entry )
    ?assertEqual(
        #{
            continuation => '$end_of_table',
            entries =>
                [],
            key_type => atom
        },
        ets_ui_http_query:get_next_n_objects(
            test_table_1, % Table
            '$end_of_table', % Continuation
            20 % PageSize
        )
    ),

    %% [setup] Insert some more!!!
    [ true = ets:insert(test_table_1, {N, N}) || N <- lists:seq(2, 1000) ],

    %% Lookup over larger entry set ( no continuation + 1000 entries )
    ?assertEqual(
        #{
            continuation => 21,
            entries =>
            [
                {1,1},
                {2,2},
                {3,3},
                {4,4},
                {5,5},
                {6,6},
                {7,7},
                {8,8},
                {9,9},
                {10,10},
                {11,11},
                {12,12},
                {13,13},
                {14,14},
                {15,15},
                {16,16},
                {17,17},
                {18,18},
                {19,19},
                {20,20}
            ],
            key_type => integer
        },
        ets_ui_http_query:get_next_n_objects(
            test_table_1, % Table
            undefined, % Continuation
            20 % PageSize
        )
    ),

    %% Lookup over larger entry set WITH continuation ( continuation + 1000 entries )
    ?assertEqual(
        #{
            continuation => 25,
            entries =>
            [
                {21,21},
                {22,22},
                {23,23},
                {24,24}
            ],
            key_type => integer
        },
        ets_ui_http_query:get_next_n_objects(
            test_table_1, % Table
            21, % Continuation
            4 % PageSize
        )
    ),

    %% Lookup over larger entry set WITH continuation and smaller page size ( continuation + 1000 entries )
    ?assertEqual(
        #{
            continuation => 45,
            entries =>
            [
                {42,42},
                {43,43},
                {44,44}
            ],
            key_type => integer
        },
        ets_ui_http_query:get_next_n_objects(
            test_table_1, % Table
            42, % Continuation
            3 % PageSize
        )
    ),

    %% Lookup over larger entry set WITH continuation and larger page size ( continuation + 1000 entries )
    ?assertEqual(
        #{
            continuation => 62,
            entries =>
            [
                {45,45},
                {46,46},
                {47,47},
                {48,48},
                {49,49},
                {50,50},
                {51,51},
                {52,52},
                {53,53},
                {54,54},
                {55,55},
                {56,56},
                {57,57},
                {58,58},
                {59,59},
                {60,60},
                {61,61}
            ],
            key_type => integer
        },
        ets_ui_http_query:get_next_n_objects(
            test_table_1, % Table
            45, % Continuation
            17 % PageSize
        )
    ),

    %% Lookup over larger entry set WITH continuation and larger page size ( continuation + 1000 entries )
    ?assertEqual(
        #{
            continuation => 129,
            entries =>
            [
                {99,99},
                {100,100},
                {101,101},
                {102,102},
                {103,103},
                {104,104},
                {105,105},
                {106,106},
                {107,107},
                {108,108},
                {109,109},
                {110,110},
                {111,111},
                {112,112},
                {113,113},
                {114,114},
                {115,115},
                {116,116},
                {117,117},
                {118,118},
                {119,119},
                {120,120},
                {121,121},
                {122,122},
                {123,123},
                {124,124},
                {125,125},
                {126,126},
                {127,127},
                {128,128}
            ],
            key_type => integer
        },
        ets_ui_http_query:get_next_n_objects(
            test_table_1, % Table
            99, % Continuation
            30 % PageSize
        )
    ).

handle_request_match_object() ->
    %% Case 1
    %% tuple_wildcard == '_' ( Match everything )
    %% Continuation == undefined
    %% KeyType == undefined
    %% empty table

    ParsedQsMap1 = #{
        tuple_wildcard => <<"'_'">>,
        continuation => undefined,
        pagesize => ?DEFAULT_PAGESIZE
    },
    StateMap1 = #{
        table => test_table_1,
        table_info => ets:info(test_table_1)
    },
    X1 = ets_ui_http_query:handle_request(ParsedQsMap1, StateMap1),
    ?assertMatch(
        {_, _},
        X1
    ),
    {ResponseCode1, Json1} = X1,
    ?assertEqual(
        200,
        ResponseCode1
    ),
    ?assertEqual(
        [
            {<<"continuation">>, <<"$end_of_table">>},
            {<<"key_type">>, <<"atom">>},
            {<<"rows">>, []}
        ],
        jsx:decode(Json1)
    ),

    %% Case 2
    %% tuple_wildcard == '_' ( Match everything )
    %% Continuation == undefined
    %% KeyType == undefined
    %% table has 1 entry

    %% [setup] Insert something
    true = ets:insert(test_table_1, {1, a}),

    ParsedQsMap2 = #{
        tuple_wildcard => <<"'_'">>,
        continuation => undefined,
        pagesize => ?DEFAULT_PAGESIZE
    },
    StateMap2 = #{
        table => test_table_1,
        table_info => ets:info(test_table_1)
    },
    X2 = ets_ui_http_query:handle_request(ParsedQsMap2, StateMap2),
    ?assertMatch(
        {_, _},
        X2
    ),
    {ResponseCode2, Json2} = X2,
    ?assertEqual(
        200,
        ResponseCode2
    ),
    ?assertEqual(
        [{<<"continuation">>,<<"$end_of_table">>},
          {<<"key_type">>,<<"atom">>},
          {<<"rows">>,
           [[{<<"key">>,1},
             {<<"key_type">>,<<"integer">>},
             {<<"values">>,[{<<"1">>,<<"1">>},{<<"2">>,<<"a">>}]}]]}],
        jsx:decode(Json2)
    ),

    %% Case 3
    %% tuple_wildcard == '_' ( Match everything )
    %% Continuation == undefined
    %% KeyType == undefined
    %% table has many entries

    %% [setup] Insert some more!!!
    [ true = ets:insert(test_table_1, {N, N, is_even(N)}) || N <- lists:seq(2, 50) ],

    ParsedQsMap3 = #{
        tuple_wildcard => <<"'_'">>,
        continuation => undefined,
        pagesize => ?DEFAULT_PAGESIZE
    },
    StateMap3 = #{
        table => test_table_1,
        table_info => ets:info(test_table_1)
    },
    X3 = ets_ui_http_query:handle_request(ParsedQsMap3, StateMap3),
    ?assertMatch(
        {_, _},
        X3
    ),
    {ResponseCode3, Json3} = X3,
    ?assertEqual(
        200,
        ResponseCode3
    ),
    ?assertMatch(
        [{<<"continuation">>, _EtsContinuation},
          {<<"key_type">>,<<"tuple">>},
          {<<"rows">>,
           [[{<<"key">>,20},
             {<<"key_type">>,<<"integer">>},
             {<<"values">>,
              [{<<"1">>,<<"20">>},
               {<<"2">>,<<"20">>},
               {<<"3">>,<<"true">>}]}],
            [{<<"key">>,19},
             {<<"key_type">>,<<"integer">>},
             {<<"values">>,
              [{<<"1">>,<<"19">>},
               {<<"2">>,<<"19">>},
               {<<"3">>,<<"false">>}]}],
            [{<<"key">>,18},
             {<<"key_type">>,<<"integer">>},
             {<<"values">>,
              [{<<"1">>,<<"18">>},
               {<<"2">>,<<"18">>},
               {<<"3">>,<<"true">>}]}],
            [{<<"key">>,17},
             {<<"key_type">>,<<"integer">>},
             {<<"values">>,
              [{<<"1">>,<<"17">>},
               {<<"2">>,<<"17">>},
               {<<"3">>,<<"false">>}]}],
            [{<<"key">>,16},
             {<<"key_type">>,<<"integer">>},
             {<<"values">>,
              [{<<"1">>,<<"16">>},
               {<<"2">>,<<"16">>},
               {<<"3">>,<<"true">>}]}],
            [{<<"key">>,15},
             {<<"key_type">>,<<"integer">>},
             {<<"values">>,
              [{<<"1">>,<<"15">>},
               {<<"2">>,<<"15">>},
               {<<"3">>,<<"false">>}]}],
            [{<<"key">>,14},
             {<<"key_type">>,<<"integer">>},
             {<<"values">>,
              [{<<"1">>,<<"14">>},
               {<<"2">>,<<"14">>},
               {<<"3">>,<<"true">>}]}],
            [{<<"key">>,13},
             {<<"key_type">>,<<"integer">>},
             {<<"values">>,
              [{<<"1">>,<<"13">>},
               {<<"2">>,<<"13">>},
               {<<"3">>,<<"false">>}]}],
            [{<<"key">>,12},
             {<<"key_type">>,<<"integer">>},
             {<<"values">>,
              [{<<"1">>,<<"12">>},
               {<<"2">>,<<"12">>},
               {<<"3">>,<<"true">>}]}],
            [{<<"key">>,11},
             {<<"key_type">>,<<"integer">>},
             {<<"values">>,
              [{<<"1">>,<<"11">>},
               {<<"2">>,<<"11">>},
               {<<"3">>,<<"false">>}]}],
            [{<<"key">>,10},
             {<<"key_type">>,<<"integer">>},
             {<<"values">>,
              [{<<"1">>,<<"10">>},
               {<<"2">>,<<"10">>},
               {<<"3">>,<<"true">>}]}],
            [{<<"key">>,9},
             {<<"key_type">>,<<"integer">>},
             {<<"values">>,
              [{<<"1">>,<<"9">>},
               {<<"2">>,<<"9">>},
               {<<"3">>,<<"false">>}]}],
            [{<<"key">>,8},
             {<<"key_type">>,<<"integer">>},
             {<<"values">>,
              [{<<"1">>,<<"8">>},
               {<<"2">>,<<"8">>},
               {<<"3">>,<<"true">>}]}],
            [{<<"key">>,7},
             {<<"key_type">>,<<"integer">>},
             {<<"values">>,
              [{<<"1">>,<<"7">>},
               {<<"2">>,<<"7">>},
               {<<"3">>,<<"false">>}]}],
            [{<<"key">>,6},
             {<<"key_type">>,<<"integer">>},
             {<<"values">>,
              [{<<"1">>,<<"6">>},
               {<<"2">>,<<"6">>},
               {<<"3">>,<<"true">>}]}],
            [{<<"key">>,5},
             {<<"key_type">>,<<"integer">>},
             {<<"values">>,
              [{<<"1">>,<<"5">>},
               {<<"2">>,<<"5">>},
               {<<"3">>,<<"false">>}]}],
            [{<<"key">>,4},
             {<<"key_type">>,<<"integer">>},
             {<<"values">>,
              [{<<"1">>,<<"4">>},
               {<<"2">>,<<"4">>},
               {<<"3">>,<<"true">>}]}],
            [{<<"key">>,3},
             {<<"key_type">>,<<"integer">>},
             {<<"values">>,
              [{<<"1">>,<<"3">>},
               {<<"2">>,<<"3">>},
               {<<"3">>,<<"false">>}]}],
            [{<<"key">>,2},
             {<<"key_type">>,<<"integer">>},
             {<<"values">>,
              [{<<"1">>,<<"2">>},
               {<<"2">>,<<"2">>},
               {<<"3">>,<<"true">>}]}],
            [{<<"key">>,1},
             {<<"key_type">>,<<"integer">>},
             {<<"values">>,[{<<"1">>,<<"1">>},{<<"2">>,<<"a">>}]}]]}],
        jsx:decode(Json3)
    ),

    %% Case 4
    %% tuple_wildcard == '_' ( Match everything )
    %% Continuation == From Prev Test ( from 21 onwards ... )
    %% KeyType == undefined
    %% table has many entries
    {<<"continuation">>, _NextContinuation1} =
        proplists:lookup(<<"continuation">>, jsx:decode(Json3)),



    %% !!! THIS IS BRITTLE FIX



    % {_, XXXCont} =
    %     ets:match_object(
    %         test_table_1,
    %         '_',
    %         20
    %     ),

    % ?debugFmt("XXXCont ~p\n", [XXXCont]),

    % JsonSaneCont = jsx:encode(
    %     ets_ui_common:json_sanitize({ets_continuation, XXXCont})
    % ),
    % ?debugFmt("JsonSaneCont ~p\n", [JsonSaneCont]),

    % TermCont =
    %     ets_ui_common:normalise_erlang_term(
    %         jsx:decode(JsonSaneCont),
    %         ets_continuation
    %     ),
    % ?debugFmt("TermCont ~p\n", [TermCont]),

    % % It seems the SETTING of the Continuation takes some time in the background....
    % try_x_times(200, fun() ->
    %     ets:match_object(
    %         TermCont
    %     )
    % end),

    % ?debugFmt("NextContinuation1 ~p\n", [NextContinuation1]),

    % ParsedQsMap4 = #{
    %     tuple_wildcard => <<"'_'">>,
    %     continuation => NextContinuation1,
    %     pagesize => ?DEFAULT_PAGESIZE
    % },

    % StateMap4 = #{
    %     table => test_table_1,
    %     table_info => ets:info(test_table_1)
    % },
    % X4 = ets_ui_http_query:handle_request(ParsedQsMap4, StateMap4),
    % ?assertMatch(
    %     {_, _},
    %     X4
    % ),
    % {ResponseCode4, Json4} = X4,
    % ?assertEqual(
    %     200,
    %     ResponseCode4
    % ),
    % ?assertEqual(
    %     [{<<"continuation">>,<<"$end_of_table">>},
    %       {<<"key_type">>,<<"atom">>},
    %       {<<"rows">>,
    %        [[{<<"key">>,1},
    %          {<<"key_type">>,<<"integer">>},
    %          {<<"values">>,[{<<"1">>,<<"1">>},{<<"2">>,<<"a">>}]}]]}],
    %     jsx:decode(Json4)
    % ),
    ok.

% try_x_times(X, _) when X =< 0 ->
%     ?debugFmt("~p\n", [ets:info(test_table_1)]),
%     throw({failed});
% try_x_times(X, F) when X > 0 ->
%     try
%         F()
%     catch
%         C:E:_ ->
%             timer:sleep(5),
%             try_x_times(X-1, F)
%     end.

handle_request_lookup() ->
    ok.

handle_request_page_through_table() ->
    %% Case 1
    %% Continuation == undefined
    %% KeyType == undefined
    %% empty table

    ParsedQsMap1 = #{
        key_type => undefined,
        continuation => undefined,
        pagesize => ?DEFAULT_PAGESIZE
    },
    StateMap1 = #{
        table => test_table_1,
        table_info => ets:info(test_table_1)
    },
    X1 = ets_ui_http_query:handle_request(ParsedQsMap1, StateMap1),
    ?assertMatch(
        {_, _},
        X1
    ),
    {ResponseCode1, Json1} = X1,
    ?assertEqual(
        200,
        ResponseCode1
    ),
    ?assertEqual(
        [
            {<<"continuation">>, <<"$end_of_table">>},
            {<<"key_type">>, <<"atom">>},
            {<<"rows">>, []}
        ],
        jsx:decode(Json1)
    ),

    %% Case 2
    %% Continuation == undefined
    %% KeyType == undefined
    %% table has 1 entry

    %% [setup] Insert something
    true = ets:insert(test_table_1, {1, a}),

    ParsedQsMap2 = #{
        key_type => undefined,
        continuation => undefined,
        pagesize => ?DEFAULT_PAGESIZE
    },
    StateMap2 = #{
        table => test_table_1,
        table_info => ets:info(test_table_1)
    },
    X2 = ets_ui_http_query:handle_request(ParsedQsMap2, StateMap2),
    ?assertMatch(
        {_, _},
        X2
    ),
    {ResponseCode2, Json2} = X2,
    ?assertEqual(
        200,
        ResponseCode2
    ),
    ?assertEqual(
        [
            {<<"continuation">>, <<"$end_of_table">>},
            {<<"key_type">>, <<"atom">>},
            {<<"rows">>, [
                [
                    {<<"key">>,1},
                    {<<"key_type">>,<<"integer">>},
                    {<<"values">>,[
                        {<<"1">>,<<"1">>},
                        {<<"2">>,<<"a">>}
                    ]}
                ]
            ]}
        ],
        jsx:decode(Json2)
    ),

    %% Case 3
    %% Continuation == undefined
    %% KeyType == undefined
    %% table has many entries

    %% [setup] Insert some more!!!
    [ true = ets:insert(test_table_1, {N, N}) || N <- lists:seq(2, 50) ],

    ParsedQsMap3 = #{
        key_type => undefined,
        continuation => undefined,
        pagesize => ?DEFAULT_PAGESIZE
    },
    StateMap3 = #{
        table => test_table_1,
        table_info => ets:info(test_table_1)
    },
    X3 = ets_ui_http_query:handle_request(ParsedQsMap3, StateMap3),
    ?assertMatch(
        {_, _},
        X3
    ),
    {ResponseCode3, Json3} = X3,
    ?assertEqual(
        200,
        ResponseCode3
    ),
    ?assertEqual(
        [{<<"continuation">>,21},
          {<<"key_type">>,<<"integer">>},
          {<<"rows">>,
           [[{<<"key">>,20},
             {<<"key_type">>,<<"integer">>},
             {<<"values">>,[{<<"1">>,<<"20">>},{<<"2">>,<<"20">>}]}],
            [{<<"key">>,19},
             {<<"key_type">>,<<"integer">>},
             {<<"values">>,[{<<"1">>,<<"19">>},{<<"2">>,<<"19">>}]}],
            [{<<"key">>,18},
             {<<"key_type">>,<<"integer">>},
             {<<"values">>,[{<<"1">>,<<"18">>},{<<"2">>,<<"18">>}]}],
            [{<<"key">>,17},
             {<<"key_type">>,<<"integer">>},
             {<<"values">>,[{<<"1">>,<<"17">>},{<<"2">>,<<"17">>}]}],
            [{<<"key">>,16},
             {<<"key_type">>,<<"integer">>},
             {<<"values">>,[{<<"1">>,<<"16">>},{<<"2">>,<<"16">>}]}],
            [{<<"key">>,15},
             {<<"key_type">>,<<"integer">>},
             {<<"values">>,[{<<"1">>,<<"15">>},{<<"2">>,<<"15">>}]}],
            [{<<"key">>,14},
             {<<"key_type">>,<<"integer">>},
             {<<"values">>,[{<<"1">>,<<"14">>},{<<"2">>,<<"14">>}]}],
            [{<<"key">>,13},
             {<<"key_type">>,<<"integer">>},
             {<<"values">>,[{<<"1">>,<<"13">>},{<<"2">>,<<"13">>}]}],
            [{<<"key">>,12},
             {<<"key_type">>,<<"integer">>},
             {<<"values">>,[{<<"1">>,<<"12">>},{<<"2">>,<<"12">>}]}],
            [{<<"key">>,11},
             {<<"key_type">>,<<"integer">>},
             {<<"values">>,[{<<"1">>,<<"11">>},{<<"2">>,<<"11">>}]}],
            [{<<"key">>,10},
             {<<"key_type">>,<<"integer">>},
             {<<"values">>,[{<<"1">>,<<"10">>},{<<"2">>,<<"10">>}]}],
            [{<<"key">>,9},
             {<<"key_type">>,<<"integer">>},
             {<<"values">>,[{<<"1">>,<<"9">>},{<<"2">>,<<"9">>}]}],
            [{<<"key">>,8},
             {<<"key_type">>,<<"integer">>},
             {<<"values">>,[{<<"1">>,<<"8">>},{<<"2">>,<<"8">>}]}],
            [{<<"key">>,7},
             {<<"key_type">>,<<"integer">>},
             {<<"values">>,[{<<"1">>,<<"7">>},{<<"2">>,<<"7">>}]}],
            [{<<"key">>,6},
             {<<"key_type">>,<<"integer">>},
             {<<"values">>,[{<<"1">>,<<"6">>},{<<"2">>,<<"6">>}]}],
            [{<<"key">>,5},
             {<<"key_type">>,<<"integer">>},
             {<<"values">>,[{<<"1">>,<<"5">>},{<<"2">>,<<"5">>}]}],
            [{<<"key">>,4},
             {<<"key_type">>,<<"integer">>},
             {<<"values">>,[{<<"1">>,<<"4">>},{<<"2">>,<<"4">>}]}],
            [{<<"key">>,3},
             {<<"key_type">>,<<"integer">>},
             {<<"values">>,[{<<"1">>,<<"3">>},{<<"2">>,<<"3">>}]}],
            [{<<"key">>,2},
             {<<"key_type">>,<<"integer">>},
             {<<"values">>,[{<<"1">>,<<"2">>},{<<"2">>,<<"2">>}]}],
            [{<<"key">>,1},
             {<<"key_type">>,<<"integer">>},
             {<<"values">>,[{<<"1">>,<<"1">>},{<<"2">>,<<"a">>}]}]]}],
        jsx:decode(Json3)
    ),

    %% Case 4
    %% Continuation == 21
    %% KeyType == integer
    %% table has many entries

    ParsedQsMap4 = #{
        key_type => <<"integer">>,
        continuation => <<"21">>,
        pagesize => ?DEFAULT_PAGESIZE
    },
    StateMap4 = #{
        table => test_table_1,
        table_info => ets:info(test_table_1)
    },
    X4 = ets_ui_http_query:handle_request(ParsedQsMap4, StateMap4),
    ?assertMatch(
        {_, _},
        X4
    ),
    {ResponseCode4, Json4} = X4,
    ?assertEqual(
        200,
        ResponseCode4
    ),
    ?assertEqual(
        [{<<"continuation">>,41},
          {<<"key_type">>,<<"integer">>},
          {<<"rows">>,
           [[{<<"key">>,40},
             {<<"key_type">>,<<"integer">>},
             {<<"values">>,[{<<"1">>,<<"40">>},{<<"2">>,<<"40">>}]}],
            [{<<"key">>,39},
             {<<"key_type">>,<<"integer">>},
             {<<"values">>,[{<<"1">>,<<"39">>},{<<"2">>,<<"39">>}]}],
            [{<<"key">>,38},
             {<<"key_type">>,<<"integer">>},
             {<<"values">>,[{<<"1">>,<<"38">>},{<<"2">>,<<"38">>}]}],
            [{<<"key">>,37},
             {<<"key_type">>,<<"integer">>},
             {<<"values">>,[{<<"1">>,<<"37">>},{<<"2">>,<<"37">>}]}],
            [{<<"key">>,36},
             {<<"key_type">>,<<"integer">>},
             {<<"values">>,[{<<"1">>,<<"36">>},{<<"2">>,<<"36">>}]}],
            [{<<"key">>,35},
             {<<"key_type">>,<<"integer">>},
             {<<"values">>,[{<<"1">>,<<"35">>},{<<"2">>,<<"35">>}]}],
            [{<<"key">>,34},
             {<<"key_type">>,<<"integer">>},
             {<<"values">>,[{<<"1">>,<<"34">>},{<<"2">>,<<"34">>}]}],
            [{<<"key">>,33},
             {<<"key_type">>,<<"integer">>},
             {<<"values">>,[{<<"1">>,<<"33">>},{<<"2">>,<<"33">>}]}],
            [{<<"key">>,32},
             {<<"key_type">>,<<"integer">>},
             {<<"values">>,[{<<"1">>,<<"32">>},{<<"2">>,<<"32">>}]}],
            [{<<"key">>,31},
             {<<"key_type">>,<<"integer">>},
             {<<"values">>,[{<<"1">>,<<"31">>},{<<"2">>,<<"31">>}]}],
            [{<<"key">>,30},
             {<<"key_type">>,<<"integer">>},
             {<<"values">>,[{<<"1">>,<<"30">>},{<<"2">>,<<"30">>}]}],
            [{<<"key">>,29},
             {<<"key_type">>,<<"integer">>},
             {<<"values">>,[{<<"1">>,<<"29">>},{<<"2">>,<<"29">>}]}],
            [{<<"key">>,28},
             {<<"key_type">>,<<"integer">>},
             {<<"values">>,[{<<"1">>,<<"28">>},{<<"2">>,<<"28">>}]}],
            [{<<"key">>,27},
             {<<"key_type">>,<<"integer">>},
             {<<"values">>,[{<<"1">>,<<"27">>},{<<"2">>,<<"27">>}]}],
            [{<<"key">>,26},
             {<<"key_type">>,<<"integer">>},
             {<<"values">>,[{<<"1">>,<<"26">>},{<<"2">>,<<"26">>}]}],
            [{<<"key">>,25},
             {<<"key_type">>,<<"integer">>},
             {<<"values">>,[{<<"1">>,<<"25">>},{<<"2">>,<<"25">>}]}],
            [{<<"key">>,24},
             {<<"key_type">>,<<"integer">>},
             {<<"values">>,[{<<"1">>,<<"24">>},{<<"2">>,<<"24">>}]}],
            [{<<"key">>,23},
             {<<"key_type">>,<<"integer">>},
             {<<"values">>,[{<<"1">>,<<"23">>},{<<"2">>,<<"23">>}]}],
            [{<<"key">>,22},
             {<<"key_type">>,<<"integer">>},
             {<<"values">>,[{<<"1">>,<<"22">>},{<<"2">>,<<"22">>}]}],
            [{<<"key">>,21},
             {<<"key_type">>,<<"integer">>},
             {<<"values">>,
              [{<<"1">>,<<"21">>},{<<"2">>,<<"21">>}]}]]}],
        jsx:decode(Json4)
    ),

    %% Case 5 ( reach the end of the table )
    %% Continuation == 41
    %% KeyType == integer
    %% table has many entries
    ParsedQsMap5 = #{
        key_type => <<"integer">>,
        continuation => <<"41">>,
        pagesize => ?DEFAULT_PAGESIZE
    },
    StateMap5 = #{
        table => test_table_1,
        table_info => ets:info(test_table_1)
    },
    X5 = ets_ui_http_query:handle_request(ParsedQsMap5, StateMap5),
    ?assertMatch(
        {_, _},
        X5
    ),
    {ResponseCode5, Json5} = X5,
    ?assertEqual(
        200,
        ResponseCode5
    ),
    ?assertEqual(
        [{<<"continuation">>,<<"$end_of_table">>},
          {<<"key_type">>,<<"atom">>},
          {<<"rows">>,
           [[{<<"key">>,50},
             {<<"key_type">>,<<"integer">>},
             {<<"values">>,[{<<"1">>,<<"50">>},{<<"2">>,<<"50">>}]}],
            [{<<"key">>,49},
             {<<"key_type">>,<<"integer">>},
             {<<"values">>,[{<<"1">>,<<"49">>},{<<"2">>,<<"49">>}]}],
            [{<<"key">>,48},
             {<<"key_type">>,<<"integer">>},
             {<<"values">>,[{<<"1">>,<<"48">>},{<<"2">>,<<"48">>}]}],
            [{<<"key">>,47},
             {<<"key_type">>,<<"integer">>},
             {<<"values">>,[{<<"1">>,<<"47">>},{<<"2">>,<<"47">>}]}],
            [{<<"key">>,46},
             {<<"key_type">>,<<"integer">>},
             {<<"values">>,[{<<"1">>,<<"46">>},{<<"2">>,<<"46">>}]}],
            [{<<"key">>,45},
             {<<"key_type">>,<<"integer">>},
             {<<"values">>,[{<<"1">>,<<"45">>},{<<"2">>,<<"45">>}]}],
            [{<<"key">>,44},
             {<<"key_type">>,<<"integer">>},
             {<<"values">>,[{<<"1">>,<<"44">>},{<<"2">>,<<"44">>}]}],
            [{<<"key">>,43},
             {<<"key_type">>,<<"integer">>},
             {<<"values">>,[{<<"1">>,<<"43">>},{<<"2">>,<<"43">>}]}],
            [{<<"key">>,42},
             {<<"key_type">>,<<"integer">>},
             {<<"values">>,[{<<"1">>,<<"42">>},{<<"2">>,<<"42">>}]}],
            [{<<"key">>,41},
             {<<"key_type">>,<<"integer">>},
             {<<"values">>,
              [{<<"1">>,<<"41">>},{<<"2">>,<<"41">>}]}]]}],
        jsx:decode(Json5)
    ),

    %% Case 6 ( query with continuation as $end_of_table )
    %% Continuation == '$end_of_table'
    %% KeyType == atom
    %% table has many entries
    ParsedQsMap6 = #{
        key_type => <<"atom">>,
        continuation => <<"$end_of_table">>,
        pagesize => ?DEFAULT_PAGESIZE
    },
    StateMap6 = #{
        table => test_table_1,
        table_info => ets:info(test_table_1)
    },
    X6 = ets_ui_http_query:handle_request(ParsedQsMap6, StateMap6),
    ?assertMatch(
        {_, _},
        X6
    ),
    {ResponseCode6, Json6} = X6,
    ?assertEqual(
        200,
        ResponseCode6
    ),
    ?assertEqual(
        [
            {<<"continuation">>, <<"$end_of_table">>},
            {<<"key_type">>, <<"atom">>},
            {<<"rows">>, [
            ]}
        ],
        jsx:decode(Json6)
    ),

    %% Case 7 table with keypos /= 1.
    %% tabe == test_table_2
    %% Continuation == undefined
    %% KeyType == undefined
    %% table has many entries

    %% [setup] Insert some more!!!
    %% Let's insert string numbers as the second value
    [ true = ets:insert(test_table_2, #test_record{ key_1 = N }) || N <- lists:seq(1, 1000) ],

    ParsedQsMap7 = #{
        key_type => undefined,
        continuation => undefined,
        pagesize => ?DEFAULT_PAGESIZE
    },
    StateMap7 = #{
        table => test_table_2,
        table_info => ets:info(test_table_2)
    },
    X7 = ets_ui_http_query:handle_request(ParsedQsMap7, StateMap7),
    ?assertMatch(
        {_, _},
        X7
    ),
    {ResponseCode7, Json7} = X7,
    ?assertEqual(
        200,
        ResponseCode7
    ),
    ?assertEqual(
        [{<<"continuation">>,21},
          {<<"key_type">>,<<"integer">>},
          {<<"rows">>,
           [[{<<"key">>,20},
             {<<"key_type">>,<<"integer">>},
             {<<"values">>,
              [{<<"1">>,<<"test_record">>},
               {<<"2">>,<<"20">>},
               {<<"3">>,<<"1">>}]}],
            [{<<"key">>,19},
             {<<"key_type">>,<<"integer">>},
             {<<"values">>,
              [{<<"1">>,<<"test_record">>},
               {<<"2">>,<<"19">>},
               {<<"3">>,<<"1">>}]}],
            [{<<"key">>,18},
             {<<"key_type">>,<<"integer">>},
             {<<"values">>,
              [{<<"1">>,<<"test_record">>},
               {<<"2">>,<<"18">>},
               {<<"3">>,<<"1">>}]}],
            [{<<"key">>,17},
             {<<"key_type">>,<<"integer">>},
             {<<"values">>,
              [{<<"1">>,<<"test_record">>},
               {<<"2">>,<<"17">>},
               {<<"3">>,<<"1">>}]}],
            [{<<"key">>,16},
             {<<"key_type">>,<<"integer">>},
             {<<"values">>,
              [{<<"1">>,<<"test_record">>},
               {<<"2">>,<<"16">>},
               {<<"3">>,<<"1">>}]}],
            [{<<"key">>,15},
             {<<"key_type">>,<<"integer">>},
             {<<"values">>,
              [{<<"1">>,<<"test_record">>},
               {<<"2">>,<<"15">>},
               {<<"3">>,<<"1">>}]}],
            [{<<"key">>,14},
             {<<"key_type">>,<<"integer">>},
             {<<"values">>,
              [{<<"1">>,<<"test_record">>},
               {<<"2">>,<<"14">>},
               {<<"3">>,<<"1">>}]}],
            [{<<"key">>,13},
             {<<"key_type">>,<<"integer">>},
             {<<"values">>,
              [{<<"1">>,<<"test_record">>},
               {<<"2">>,<<"13">>},
               {<<"3">>,<<"1">>}]}],
            [{<<"key">>,12},
             {<<"key_type">>,<<"integer">>},
             {<<"values">>,
              [{<<"1">>,<<"test_record">>},
               {<<"2">>,<<"12">>},
               {<<"3">>,<<"1">>}]}],
            [{<<"key">>,11},
             {<<"key_type">>,<<"integer">>},
             {<<"values">>,
              [{<<"1">>,<<"test_record">>},
               {<<"2">>,<<"11">>},
               {<<"3">>,<<"1">>}]}],
            [{<<"key">>,10},
             {<<"key_type">>,<<"integer">>},
             {<<"values">>,
              [{<<"1">>,<<"test_record">>},
               {<<"2">>,<<"10">>},
               {<<"3">>,<<"1">>}]}],
            [{<<"key">>,9},
             {<<"key_type">>,<<"integer">>},
             {<<"values">>,
              [{<<"1">>,<<"test_record">>},
               {<<"2">>,<<"9">>},
               {<<"3">>,<<"1">>}]}],
            [{<<"key">>,8},
             {<<"key_type">>,<<"integer">>},
             {<<"values">>,
              [{<<"1">>,<<"test_record">>},
               {<<"2">>,<<"8">>},
               {<<"3">>,<<"1">>}]}],
            [{<<"key">>,7},
             {<<"key_type">>,<<"integer">>},
             {<<"values">>,
              [{<<"1">>,<<"test_record">>},
               {<<"2">>,<<"7">>},
               {<<"3">>,<<"1">>}]}],
            [{<<"key">>,6},
             {<<"key_type">>,<<"integer">>},
             {<<"values">>,
              [{<<"1">>,<<"test_record">>},
               {<<"2">>,<<"6">>},
               {<<"3">>,<<"1">>}]}],
            [{<<"key">>,5},
             {<<"key_type">>,<<"integer">>},
             {<<"values">>,
              [{<<"1">>,<<"test_record">>},
               {<<"2">>,<<"5">>},
               {<<"3">>,<<"1">>}]}],
            [{<<"key">>,4},
             {<<"key_type">>,<<"integer">>},
             {<<"values">>,
              [{<<"1">>,<<"test_record">>},
               {<<"2">>,<<"4">>},
               {<<"3">>,<<"1">>}]}],
            [{<<"key">>,3},
             {<<"key_type">>,<<"integer">>},
             {<<"values">>,
              [{<<"1">>,<<"test_record">>},
               {<<"2">>,<<"3">>},
               {<<"3">>,<<"1">>}]}],
            [{<<"key">>,2},
             {<<"key_type">>,<<"integer">>},
             {<<"values">>,
              [{<<"1">>,<<"test_record">>},
               {<<"2">>,<<"2">>},
               {<<"3">>,<<"1">>}]}],
            [{<<"key">>,1},
             {<<"key_type">>,<<"integer">>},
             {<<"values">>,
              [{<<"1">>,<<"test_record">>},
               {<<"2">>,<<"1">>},
               {<<"3">>,<<"1">>}]}]]}],
        jsx:decode(Json7)
    ).

http_unit_test_() ->
    {setup,
     fun() ->
        ets:new(test_table_1, [public, named_table, ordered_set]),
        {ok, Apps} = application:ensure_all_started(cowboy),
        {ok, Apps2} = application:ensure_all_started(holster),
        ok = application:load(ets_ui),
        ok = application:set_env(ets_ui, http_port, 0),
        {ok, Pid} = ets_ui_http:start_link(),
        {{ok, Pid}, lists:reverse(Apps) ++ lists:reverse(Apps2)}
     end,
     fun({{ok, Pid}, Apps}) ->
        true = erlang:unlink(Pid),
        [ ok = application:stop(App) || App <- Apps ],
        true = erlang:exit(Pid, kill)
     end,
     [
        {foreachx,
         fun(_TestString) ->
            {ok, Port} = application:get_env(ets_ui, assigned_port),
            Port
         end,
         fun(_TestString, _AssignedPort) ->
            ets:delete_all_objects(test_table_1)
         end,
         [
            {"api page entries ( no results )",
                fun api_page_entries_no_results/2},
            {"api page entries ( one result )",
                fun api_page_entries_one_result/2},
            {"api page entries ( lots of results )",
                fun api_page_entries_lots_results/2},
            {"api page entries ( continuation )",
                fun api_page_entries_with_continuation/2}
         ]
        }
     ]
    }.

api_page_entries_no_results(_TestString, AssignedPort) ->

    %% TODO: use the eunit ifdef macro to enable/disable logging

    %% Query empty table
    W = erlang_testing_web:url_req("http://localhost:"++integer_to_list(AssignedPort)++"/api/query?table=test_table_1"),
    % ?debugFmt("W ~p\n", [W]),
    ?assertMatch(
        {ok,{200,
         [{<<"content-length">>,<<"60">>},
          {<<"content-type">>,<<"application/json">>},
          {<<"date">>, _},
          {<<"server">>,<<"Cowboy">>}],
         _ % Body
         }},
        W
    ),
    {ok, {200, _, WBody}} = W,
    ?_assertEqual(
        #{
            <<"continuation">> => <<"$end_of_table">>,
            <<"key_type">> => <<"atom">>,
            <<"rows">> => []
        },
        jsx:decode(WBody, [return_maps])
    ).

api_page_entries_one_result(_TestString, AssignedPort) ->

    %% [setup] Insert something
    true = ets:insert(test_table_1, {1, 1}),
    ?assert( 1 =:= ets:info(test_table_1, size) ),

    %% Query table with 1 entry
    X = erlang_testing_web:url_req("http://localhost:"++integer_to_list(AssignedPort)++"/api/query?table=test_table_1"),
    ?debugFmt("X ~p\n", [X]),
    ?assertMatch(
        {ok,{200,
         [{<<"content-length">>,<<"117">>},
          {<<"content-type">>,<<"application/json">>},
          {<<"date">>,_},
          {<<"server">>,<<"Cowboy">>}],
         _ % Body
        }},
        X
    ),
    {ok, {200, _, XBody}} = X,
    ?_assertEqual(
        #{
            <<"continuation">> => <<"$end_of_table">>,
            <<"key_type">> => <<"atom">>,
            <<"rows">> => [
                #{
                    <<"key">> => 1,
                    <<"key_type">> => <<"integer">>,
                    <<"values">> => #{
                        <<"1">> => <<"1">>,
                        <<"2">> => <<"1">>
                    }
                }
            ]
        },
        jsx:decode(XBody, [return_maps])
    ).

api_page_entries_lots_results(_TestString, AssignedPort) ->

    %% [setup] Insert some more!!!
    [ true = ets:insert(test_table_1, {N, N}) || N <- lists:seq(2, 1000) ],

    Y = erlang_testing_web:url_req("http://localhost:"++integer_to_list(AssignedPort)++"/api/query?table=test_table_1"),
    % ?debugFmt("Y ~p\n", [Y]),
    ?assertMatch(
        {ok,{200,
         [{<<"content-length">>,<<"1245">>},
          {<<"content-type">>,<<"application/json">>},
          {<<"date">>,_},
          {<<"server">>,<<"Cowboy">>}],
         _ % Body
         }},
        Y
    ),
    {ok, {200, _, YBody}} = Y,
    ?debugFmt("YBODY ~p\n", [jsx:decode(YBody, [return_maps])]),
    ?_assertEqual(
        #{<<"continuation">> => 22,<<"key_type">> => <<"integer">>,
        <<"rows">> =>
            [#{<<"key">> => 21,<<"key_type">> => <<"integer">>,
               <<"values">> => #{<<"1">> => <<"21">>,<<"2">> => <<"21">>}},
             #{<<"key">> => 20,<<"key_type">> => <<"integer">>,
               <<"values">> => #{<<"1">> => <<"20">>,<<"2">> => <<"20">>}},
             #{<<"key">> => 19,<<"key_type">> => <<"integer">>,
               <<"values">> => #{<<"1">> => <<"19">>,<<"2">> => <<"19">>}},
             #{<<"key">> => 18,<<"key_type">> => <<"integer">>,
               <<"values">> => #{<<"1">> => <<"18">>,<<"2">> => <<"18">>}},
             #{<<"key">> => 17,<<"key_type">> => <<"integer">>,
               <<"values">> => #{<<"1">> => <<"17">>,<<"2">> => <<"17">>}},
             #{<<"key">> => 16,<<"key_type">> => <<"integer">>,
               <<"values">> => #{<<"1">> => <<"16">>,<<"2">> => <<"16">>}},
             #{<<"key">> => 15,<<"key_type">> => <<"integer">>,
               <<"values">> => #{<<"1">> => <<"15">>,<<"2">> => <<"15">>}},
             #{<<"key">> => 14,<<"key_type">> => <<"integer">>,
               <<"values">> => #{<<"1">> => <<"14">>,<<"2">> => <<"14">>}},
             #{<<"key">> => 13,<<"key_type">> => <<"integer">>,
               <<"values">> => #{<<"1">> => <<"13">>,<<"2">> => <<"13">>}},
             #{<<"key">> => 12,<<"key_type">> => <<"integer">>,
               <<"values">> => #{<<"1">> => <<"12">>,<<"2">> => <<"12">>}},
             #{<<"key">> => 11,<<"key_type">> => <<"integer">>,
               <<"values">> => #{<<"1">> => <<"11">>,<<"2">> => <<"11">>}},
             #{<<"key">> => 10,<<"key_type">> => <<"integer">>,
               <<"values">> => #{<<"1">> => <<"10">>,<<"2">> => <<"10">>}},
             #{<<"key">> => 9,<<"key_type">> => <<"integer">>,
               <<"values">> => #{<<"1">> => <<"9">>,<<"2">> => <<"9">>}},
             #{<<"key">> => 8,<<"key_type">> => <<"integer">>,
               <<"values">> => #{<<"1">> => <<"8">>,<<"2">> => <<"8">>}},
             #{<<"key">> => 7,<<"key_type">> => <<"integer">>,
               <<"values">> => #{<<"1">> => <<"7">>,<<"2">> => <<"7">>}},
             #{<<"key">> => 6,<<"key_type">> => <<"integer">>,
               <<"values">> => #{<<"1">> => <<"6">>,<<"2">> => <<"6">>}},
             #{<<"key">> => 5,<<"key_type">> => <<"integer">>,
               <<"values">> => #{<<"1">> => <<"5">>,<<"2">> => <<"5">>}},
             #{<<"key">> => 4,<<"key_type">> => <<"integer">>,
               <<"values">> => #{<<"1">> => <<"4">>,<<"2">> => <<"4">>}},
             #{<<"key">> => 3,<<"key_type">> => <<"integer">>,
               <<"values">> => #{<<"1">> => <<"3">>,<<"2">> => <<"3">>}},
             #{<<"key">> => 2,<<"key_type">> => <<"integer">>,
               <<"values">> => #{<<"1">> => <<"2">>,<<"2">> => <<"2">>}}]},
        jsx:decode(YBody, [return_maps])
    ).

api_page_entries_with_continuation(_TestString, AssignedPort) ->
    [ true = ets:insert(test_table_1, {N, N}) || N <- lists:seq(1, 1000) ],

    Z = erlang_testing_web:url_req("http://localhost:"++integer_to_list(AssignedPort)++"/api/query?table=test_table_1&pagesize=3"),
    % ?debugFmt("~p\n", [Z]),
    ?assertMatch(
      {ok,{200,
       [{<<"content-length">>,<<"222">>},
        {<<"content-type">>,<<"application/json">>},
        {<<"date">>,_},
        {<<"server">>,<<"Cowboy">>}],
       _ % Body
       }},
      Z
    ),
    {ok, {200, _, ZBody}} = Z,
    ?debugFmt("ZBody ~p\n", [jsx:decode(ZBody, [return_maps])]),
    ?assertEqual(
      #{
          <<"continuation">> => 4,
          <<"key_type">> => <<"integer">>,
          <<"rows">> =>
              [#{<<"key">> => 3,
                 <<"key_type">> => <<"integer">>,
                 <<"values">> =>
                     #{<<"1">> => <<"3">>,
                       <<"2">> => <<"3">>}},
               #{<<"key">> => 2,
                 <<"key_type">> => <<"integer">>,
                 <<"values">> =>
                     #{<<"1">> => <<"2">>,
                       <<"2">> => <<"2">>}},
               #{<<"key">> => 1,
                 <<"key_type">> => <<"integer">>,
                 <<"values">> =>
                     #{<<"1">> => <<"1">>,
                       <<"2">> => <<"1">>}}]
    },
    jsx:decode(ZBody, [return_maps])
    ),

    AB = erlang_testing_web:url_req("http://localhost:"++integer_to_list(AssignedPort)++"/api/query?table=test_table_1&pagesize=3&continuation=4&key_type=integer"),
    % ?debugFmt("AB ~p\n", [AB]),
    ?assertMatch(
      {ok,{200,
       [{<<"content-length">>,<<"222">>},
        {<<"content-type">>,<<"application/json">>},
        {<<"date">>,_},
        {<<"server">>,<<"Cowboy">>}],
       _ % Body
       }},
      AB
    ),
    {ok, {200, _, ABBody}} = AB,
    ?debugFmt("ABBody ~p\n", [jsx:decode(ABBody, [return_maps])]),
    ?_assertEqual(
        #{<<"continuation">> => 7,<<"key_type">> => <<"integer">>,
         <<"rows">> =>
             [#{<<"key">> => 6,<<"key_type">> => <<"integer">>,
                <<"values">> => #{<<"1">> => <<"6">>,<<"2">> => <<"6">>}},
              #{<<"key">> => 5,<<"key_type">> => <<"integer">>,
                <<"values">> => #{<<"1">> => <<"5">>,<<"2">> => <<"5">>}},
              #{<<"key">> => 4,<<"key_type">> => <<"integer">>,
                <<"values">> => #{<<"1">> => <<"4">>,<<"2">> => <<"4">>}}]
        },
        jsx:decode(ABBody, [return_maps])
    ).

% create_reply(AssignedPort) ->
%     ?assertEqual(
%         {ok, #{}, #{}},
%         ets_ui_http_query:create_reply(200, [], #{}, example_cowboy_req(AssignedPort))
%     ).

% example_cowboy_req(AssignedPort) ->
%     #{
%         bindings => #{},
%         body_length => 0,
%         cert => undefined,
%         has_body => false,
%         headers => #{
%             <<"accept">> => <<"*/*">>,
%             <<"accept-encoding">> => <<"gzip, deflate, br">>,
%             <<"accept-language">> => <<"en-US,en;q=0.9,af;q=0.8">>,
%             <<"connection">> => <<"keep-alive">>,
%             <<"dnt">> => <<"1">>,
%             <<"host">> => <<"localhost:54321">>,
%             <<"referer">> => list_to_binary([<<"http://localhost:">>, list_to_binary(integer_to_list(AssignedPort)), <<"/query.html?table=ssl_pem_cache">>]),
%             <<"sec-fetch-mode">> => <<"cors">>,
%             <<"sec-fetch-site">> => <<"same-origin">>,
%             <<"user-agent">> => <<"Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/78.0.3904.108 Safari/537.36">>,
%             <<"x-requested-with">> => <<"XMLHttpRequest">>
%         },
%         host => <<"localhost">>,
%         host_info => undefined,
%         method => <<"GET">>,
%         path => <<"/api/query">>,
%         path_info => undefined,
%         peer => {{127,0,0,1},36316},
%         pid => self(), %% SOme pid
%         port => 54321,
%         qs => <<"table=ssl_pem_cache">>,
%         ref => ets_ui_http,
%         scheme => <<"http">>,
%         sock => {{127,0,0,1},54321},
%         streamid => 3,
%         version => 'HTTP/1.1'
%     }.

is_even(X) ->
    X rem 2 == 0.