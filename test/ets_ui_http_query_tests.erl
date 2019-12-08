-module(ets_ui_http_query_tests).

-include_lib("eunit/include/eunit.hrl").

%% @ Memo!
%%   We'll assume page size is not being sent in the correct range for now.
%% @

unit_test_() ->
    {foreach,
     fun() ->
        ets:new(test_table_1, [public, named_table, ordered_set])
        % ets:new(test_table_1, [protected, named_table, ordered_set]),
        % ets:new(test_table_1, [private, named_table, ordered_set])
     end,
     fun(Table) ->
        ets:delete(Table)
     end,
     [
        {"ets_ui_http_query:get_next_n_objects/3", fun get_next_n_objects/0}
        % {"ets_ui_http_query:create_reply/5", fun create_reply/0}
     ]
    }.

get_next_n_objects() ->
    %% Run over empty table ( no continuation + no entries )
    ?assertEqual(
        #{
            continuation => '$end_of_table',
            entries => []
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
                ]
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
                []
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
            ]
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
            ]
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
            ]
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
            ]
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
            ]
        },
        ets_ui_http_query:get_next_n_objects(
            test_table_1, % Table
            99, % Continuation
            30 % PageSize
        )
    ).


http_unit_test_() ->
    {setup,
     fun() ->
        ets:new(test_table_1, [public, named_table, ordered_set]),
        {ok, Apps} = application:ensure_all_started(cowboy),
        {ok, Apps2} = application:ensure_all_started(holster),
        {ok, Pid} = ets_ui_http:start_link(),
        {{ok, Pid}, lists:reverse(Apps) ++ lists:reverse(Apps2)}
     end,
     fun({{ok, Pid}, Apps}) ->
        true = erlang:unlink(Pid),
        [ ok = application:stop(App) || App <- Apps ],
        true = erlang:exit(Pid, kill)
     end,
     [
        {"api query", fun api_query/0}
     ]
    }.

api_query() ->

    %% TODO: use the eunit ifdef macro to enable/disable logging

    %% Query empty table
    W = erlang_testing_web:url_req("http://localhost:54321/api/query?table=test_table_1"),
    % ?debugFmt("~p\n", [W]),
    ?assertMatch(
        {ok,{200,
         [{<<"content-length">>,<<"42">>},
          {<<"content-type">>,<<"application/json">>},
          {<<"date">>, _},
          {<<"server">>,<<"Cowboy">>}],
         _ % Body
         }},
        W
    ),
    {ok, {200, _, WBody}} = W,
    ?assertEqual(
        #{
            <<"continuation">> => <<"$end_of_table">>,
            <<"rows">> => []
        },
        jsx:decode(WBody, [return_maps])
    ),

    %% [setup] Insert something
    true = ets:insert(test_table_1, {1, 1}),

    %% Query table with 1 entry
    X = erlang_testing_web:url_req("http://localhost:54321/api/query?table=test_table_1"),
    % ?debugFmt("~p\n", [X]),
    ?assertMatch(
        {ok,{200,
         [{<<"content-length">>,<<"91">>},
          {<<"content-type">>,<<"application/json">>},
          {<<"date">>,_},
          {<<"server">>,<<"Cowboy">>}],
         _ % Body
        }},
        X
    ),
    {ok, {200, _, XBody}} = X,
    ?assertEqual(
        #{
            <<"continuation">> => <<"$end_of_table">>,
            <<"rows">> => [
                #{
                    <<"key">> => 1,
                    <<"key_type">> => <<"integer">>,
                    <<"values">> => #{
                        <<"2">> => <<"1">>
                    }
                }
            ]
        },
        jsx:decode(XBody, [return_maps])
    ),

    %% [setup] Insert some more!!!
    [ true = ets:insert(test_table_1, {N, N}) || N <- lists:seq(2, 1000) ],

    %% Query with continuation
    Y = erlang_testing_web:url_req("http://localhost:54321/api/query?table=test_table_1"),
    % ?debugFmt("~p\n", [Y]),
    ?assertMatch(
        {ok,{200,
         [{<<"content-length">>,<<"1050">>},
          {<<"content-type">>,<<"application/json">>},
          {<<"date">>,_},
          {<<"server">>,<<"Cowboy">>}],
         _ % Body
         }},
        Y
    ),
    {ok, {200, _, YBody}} = Y,
    ?debugFmt("~p\n", [jsx:decode(YBody, [return_maps])]),
    ?assertEqual(
        #{
            <<"continuation">> => 21,
            <<"rows">> =>
              [#{<<"key">> => 20,<<"key_type">> => <<"integer">>,
                 <<"values">> => #{<<"2">> => <<"20">>}},
               #{<<"key">> => 19,<<"key_type">> => <<"integer">>,
                 <<"values">> => #{<<"2">> => <<"19">>}},
               #{<<"key">> => 18,<<"key_type">> => <<"integer">>,
                 <<"values">> => #{<<"2">> => <<"18">>}},
               #{<<"key">> => 17,<<"key_type">> => <<"integer">>,
                 <<"values">> => #{<<"2">> => <<"17">>}},
               #{<<"key">> => 16,<<"key_type">> => <<"integer">>,
                 <<"values">> => #{<<"2">> => <<"16">>}},
               #{<<"key">> => 15,<<"key_type">> => <<"integer">>,
                 <<"values">> => #{<<"2">> => <<"15">>}},
               #{<<"key">> => 14,<<"key_type">> => <<"integer">>,
                 <<"values">> => #{<<"2">> => <<"14">>}},
               #{<<"key">> => 13,<<"key_type">> => <<"integer">>,
                 <<"values">> => #{<<"2">> => <<"13">>}},
               #{<<"key">> => 12,<<"key_type">> => <<"integer">>,
                 <<"values">> => #{<<"2">> => <<"12">>}},
               #{<<"key">> => 11,<<"key_type">> => <<"integer">>,
                 <<"values">> => #{<<"2">> => <<"11">>}},
               #{<<"key">> => 10,<<"key_type">> => <<"integer">>,
                 <<"values">> => #{<<"2">> => <<"10">>}},
               #{<<"key">> => 9,<<"key_type">> => <<"integer">>,
                 <<"values">> => #{<<"2">> => <<"9">>}},
               #{<<"key">> => 8,<<"key_type">> => <<"integer">>,
                 <<"values">> => #{<<"2">> => <<"8">>}},
               #{<<"key">> => 7,<<"key_type">> => <<"integer">>,
                 <<"values">> => #{<<"2">> => <<"7">>}},
               #{<<"key">> => 6,<<"key_type">> => <<"integer">>,
                 <<"values">> => #{<<"2">> => <<"6">>}},
               #{<<"key">> => 5,<<"key_type">> => <<"integer">>,
                 <<"values">> => #{<<"2">> => <<"5">>}},
               #{<<"key">> => 4,<<"key_type">> => <<"integer">>,
                 <<"values">> => #{<<"2">> => <<"4">>}},
               #{<<"key">> => 3,<<"key_type">> => <<"integer">>,
                 <<"values">> => #{<<"2">> => <<"3">>}},
               #{<<"key">> => 2,<<"key_type">> => <<"integer">>,
                 <<"values">> => #{<<"2">> => <<"2">>}},
               #{<<"key">> => 1,<<"key_type">> => <<"integer">>,
                 <<"values">> => #{<<"2">> => <<"1">>}}]
        },
        jsx:decode(YBody, [return_maps])
    ),
    ok.

% create_reply() ->
%     ?assertEqual(
%         {ok, #{}, #{}},
%         ets_ui_http_query:create_reply(200, [], #{}, example_cowboy_req())
%     ).

% example_cowboy_req() ->
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
%             <<"referer">> => <<"http://localhost:54321/query.html?table=ssl_pem_cache">>,
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
