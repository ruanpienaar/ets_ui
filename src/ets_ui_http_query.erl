-module(ets_ui_http_query).

-define(DEFAULT_PAGESIZE, <<"20">>).

-export([
    init/2
        ]).

-define(TABLE, <<"table">>).
-define(PAGE, <<"page">>).
-define(PAGESIZE, <<"pagesize">>).

-ifdef(TEST).
-compile(export_all).
-endif.

init(Req0, Opts) ->
    Params = cowboy_req:parse_qs(Req0),
    Table = get_table(Params),
    Filter = get_filter(Params),
    {Page, PageSize} = get_paging(Params),
    Rows = paged(Table, Page, PageSize, Filter),
    RowsDisplay =
        lists:map(
          fun(Row) ->
                  #{<<"key">> =>% Row}
                        list_to_binary(lists:flatten(io_lib:format("~p", [Row])))}
          end, Rows),

    Req = cowboy_req:reply(
        200,
        #{
            <<"content-type">> => <<"application/json">>
         },
            jsx:encode(RowsDisplay),
        %%<<"{\"key\":\"value\"}">>,
        Req0
    ),
    {ok, Req, Opts}.

get_table(Params) ->
    TBin = proplists:get_value(?TABLE, Params),
    erlang:binary_to_existing_atom(TBin, utf8).

get_filter(_) ->
    fun(_,_) ->
            true
    end.

get_paging(Params) ->
    Page = to_int(proplists:get_value(?PAGE, Params, <<"0">>)),
    PageSize = to_int(proplists:get_value(?PAGESIZE, Params, ?DEFAULT_PAGESIZE)),
    {Page, PageSize}.

paged(Table, Page, PageSize, Filter) when PageSize > 0 ->
    SkipAmount = Page * PageSize,
    case paged_first(Table, SkipAmount, Filter) of
        '$end_of_table' ->
            [];
        {First, Objects} ->
            paged(Table, PageSize, Filter, First, [Objects])
    end.

paged(_Table, _PageSize, _Filter, '$end_of_table', Acc) ->
    Acc;
paged(_Table, PageSize, _Filter, _Key, Acc) when length(Acc) >= PageSize ->
    Acc;
paged(Table, PageSize, Filter, Key, Acc) ->
    case ets_ui_ets:filter_next(Table, Key, Filter) of
        '$end_of_table' ->
            Acc;
        {Next, Objects} ->
            paged(Table, PageSize, Filter, Next, [Objects|Acc])
    end.

paged_first(Table, SkipAmount, Filter) ->
    FirstIterator = ets_ui_ets:filter_first(Table, Filter),
    paged_first(Table, FirstIterator, SkipAmount, Filter).

paged_first(_Table, '$end_of_table', _SkipAmount, _Filter) ->
    '$end_of_table';
paged_first(_Table, Iterator, SkipAmount, _Filter) when SkipAmount =< 0 ->
    Iterator;
paged_first(Table, {Key, _Objects}, SkipAmount, Filter) ->
    paged_first(Table, ets_ui_ets:filter_next(Table, Key, Filter), SkipAmount - 1, Filter).


%% TODO: some advanced validation for integer params
to_int(Bin) ->
    try binary_to_integer(Bin) of
        Int when is_integer(Int) ->
            Int
    catch E:R ->
            {error, {E, R}}
    end.
