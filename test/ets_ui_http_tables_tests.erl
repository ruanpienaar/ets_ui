-module(ets_ui_http_tables_tests).

-include_lib("eunit/include/eunit.hrl").

unit_test_() ->
    {setup,
     fun() ->
        ok
     end,
     fun(_) ->
        ok
     end,
     [
        % {"", fun /0}
        {"ets_ui_http_tables:tables/1", fun tables/0}
     ]
    }.

% low use test, think of other test criteria..
tables() ->
    ?assert(
        is_list(ets_ui_http_tables:tables(ets))
    ).
