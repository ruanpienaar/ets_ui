%%% @doc
%%%
%%% @end
-module(ets_ui_ets).

%% API
-export([filter_first/2,
         filter_next/3]).

%%% API
%% @doc
%% @end
filter_first(Table, FFun) ->
    First = ets:first(Table),
    filter_next(Table, First, FFun).

filter_next(_Table, '$end_of_table', _FFun) ->
    '$end_of_table';
filter_next(Table, Key, FFun) ->
    Objects = ets:lookup(Table, Key),
    case FFun(Key, Objects) of
        true ->
            {ets:next(Table, Key), Objects};
        _ ->
            filter_next(Table, ets:next(Table, Key), FFun)
    end.

%%% Internal functions
