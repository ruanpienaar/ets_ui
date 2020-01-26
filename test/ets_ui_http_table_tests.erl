-module(ets_ui_http_table_tests).

-include_lib("eunit/include/eunit.hrl").

http_unit_test_() ->
    {setup,
     fun() ->
        ets:new(http_test_table_1, [public, named_table, ordered_set]),
        {ok, Apps} = application:ensure_all_started(cowboy),
        {ok, Apps2} = application:ensure_all_started(holster),
        ok = application:load(ets_ui),
        ok = application:set_env(ets_ui, http_port, 0),
        {ok, Pid} = ets_ui_http:start_link(),

        % dbg:tracer(),
        % dbg:p(all, call),
        % dbg:tpl(holster_sm, cx),

        {{ok, Pid}, lists:reverse(Apps) ++ lists:reverse(Apps2)}
     end,
     fun({{ok, Pid}, Apps}) ->

        % ok = dbg:stop_clear(),

        true = erlang:unlink(Pid),
        [ ok = application:stop(App) || App <- Apps ],
        true = erlang:exit(Pid, kill),
        true = ets:delete(http_test_table_1)
     end,
     [
        {foreachx,
         fun(_TestString) ->
            {ok, Port} = application:get_env(ets_ui, assigned_port),
            Port
         end,
         fun(_TestString, _AssignedPort) ->
            ets:delete_all_objects(http_test_table_1)
         end,
         [

            {"api table info",
                fun api_table_info/2}
         ]
        }
     ]
    }.

api_table_info(_TestString, AssignedPort) ->
    TableInfoResp =
        erlang_testing_web:url_req(
            get,
            build_table_info_url(AssignedPort, http_test_table_1)
        ),
    ?assertMatch(
        {ok,
         {200,
         [{<<"content-length">>,_},
          {<<"content-type">>,<<"application/json">>},
          {<<"date">>, _},
          {<<"server">>,<<"Cowboy">>}],
         _
        }},
        TableInfoResp
    ),
    {ok, {200, _, Body}} = TableInfoResp,
    ?_assertMatch(
        [{<<"id">>,_},
         {<<"read_concurrency">>,false},
         {<<"write_concurrency">>,false},
         {<<"compressed">>,false},
         {<<"memory">>,_},
         {<<"owner">>,_},
         {<<"heir">>,<<"none">>},
         {<<"name">>,<<"http_test_table_1">>},
         {<<"size">>,0},
         {<<"node">>,<<"nonode@nohost">>},
         {<<"named_table">>,true},
         {<<"type">>,<<"ordered_set">>},
         {<<"keypos">>,1},
         {<<"protection">>,<<"public">>}],
        jsx:decode(Body)
    ).

%%
%% Internal

build_table_info_url(AssignedPort, Table) ->
    "http://localhost:"++
    integer_to_list(AssignedPort)++
    "/api/table/info/"++
    atom_to_list(Table).
