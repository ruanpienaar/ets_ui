-module(ets_ui_common_tests).

-include_lib("eunit/include/eunit.hrl").

% unit_test_() ->
%     {setup,
%      fun() ->
%         ok
%      end,
%      fun(_) ->
%         ok
%      end,
%      [
%         {"normalise_erlang_term", fun}
%      ]
%     }.

%% TODO: add more cases ....
normalise_erlang_term_test() ->
    ?assertEqual(
        {test_table_1,20,[],20,erlang:list_to_ref("#Ref<0.443993101.1951793154.146180>"),[],0,0},
        ets_ui_common:normalise_erlang_term(
            <<"{test_table_1,20,[],20,\"#Ref<0.443993101.1951793154.146180>\",[],0,0}">>,
            ets_continuation
        )
    ).

%% TODO: add more cases ....
%% NB! doing the erlang:list_to_ref( bit so we can store the ref simply in the test
%% #Ref<.... is not valid syntax...
json_sanitize_test() ->
    ?assertEqual(
        <<"{test_table_1,20,[],20,\"#Ref<0.443993101.1951793154.146180>\",[],0,0}">>,
        ets_ui_common:json_sanitize(
            {ets_continuation, {test_table_1,20,[],20,erlang:list_to_ref("#Ref<0.443993101.1951793154.146180>"),[],0,0}}
        )
    ).
