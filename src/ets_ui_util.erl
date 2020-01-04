-module(ets_ui_util).

-export([
    dummy_table/0
]).

-include("ets_ui.hrl").

dummy_table() ->
    table1 = ets:new(table1, [named_table, public, ordered_set]),
    table2 = ets:new(table2, [named_table, public, set]),
    TableRef1 = ets:new(table3, [public, bag]),
    table4 = ets:new(table4, [named_table, protected, ordered_set]),
    table5 = ets:new(table5, [named_table, protected, set]),
    TableRef2 = ets:new(table6, [protected, bag]),
    table7 = ets:new(table7, [named_table, private, ordered_set]),
    table8 = ets:new(table8, [named_table, private, set]),
    TableRef3 = ets:new(table9, [private, bag]),
    Tables = [
        table1, table2, TableRef1,
        table4, table5, TableRef2,
        table7, table8, TableRef3
    ],
    NumberList = lists:seq(1, 100),
    lists:foreach(fun(Table) ->
        [
            true = ets:insert(Table, new_obj(X))
            ||
            X <- NumberList
        ]
    end, Tables),
    composite_key = ets:new(composite_key, [named_table, public, set]),
    lists:foreach(fun(X) ->
        ets:insert(composite_key, { {X, is_even(X)}, X})
    end, NumberList),
    record_table = ets:new(record_table, [named_table, public, set, {keypos, 2}]),
    lists:foreach(fun(X) ->
        case X =< 500 of
            true ->
                ets:insert(record_table, #dummy{
                    key = X,
                    map = #{
                        vsn => ?DUMMY_VERSION_1,
                        is_even => is_even(X),
                        datetime => calendar:universal_time()
                    }
                });
            false ->
                ets:insert(record_table, #dummy{
                    key = X,
                    map = #{
                        vsn => ?DUMMY_VERSION_2,
                        is_even => is_even(X),
                        datetime => calendar:universal_time(),
                        created_by_user_id => 123,
                        approved => true
                    }
                })
        end
    end, NumberList),
    plasticbag = ets:new(plasticbag, [named_table, public, bag]),
    lists:foreach(fun(X) ->
        ets:insert(plasticbag, {1000, X})
    end, NumberList).

new_obj(X) ->
    {links, Links} =
        proplists:lookup(links, erlang:process_info(whereis(user_drv))),
    Port = hd(
        lists:filter(
            fun(L) when is_port(L) ->
                true;
               (_) ->
                false
            end,
            Links
        )
    ),
    {
        X,
        atom,
        <<"binary">>,
        <<1:8, 2:256>>,
        true,
        0.123456,
        fun() -> ok end,
        X,
        "list",
        #{ key => value },
        self(),
        Port,
        erlang:make_ref()
    }.

is_even(X) ->
    X rem 2 == 0.
