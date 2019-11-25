-module(ets_ui_tests).
-include_lib("eunit/include/eunit.hrl").

unit_test_() ->
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
       {":func1/0", fun func1/0}
     ]
    }.

func1() ->
    ?assert(true).
