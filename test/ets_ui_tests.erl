-module(ets_ui_tests).
-include_lib("eunit/include/eunit.hrl").

web_requests_unit_test_() ->
    {setup,
     % Setup Fixture
     fun() ->
         xxx
     end,
     % Cleanup Fixture
     fun(xxx) ->
         ok
     end,
     % List of tests
     [
       % Example test
       {"Web request help/0", fun help/0}
     ]
    }.

help() ->
    ?assert(true).
