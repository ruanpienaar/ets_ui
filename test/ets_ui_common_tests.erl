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
    ok.

%% TODO: add more cases ....
%% NB! doing the erlang:list_to_ref( bit so we can store the ref simply in the test
%% #Ref<.... is not valid syntax...
json_sanitize_test() ->
    ok.
