-module(ets_ui_http_query).

-behaviour(cowboy_handler).

-define(DEFAULT_PAGESIZE, 20).

-include("ets_ui.hrl").

-export([
    init/2
]).

-define(TABLE, <<"table">>).
-define(PAGE, <<"page">>).
-define(PAGESIZE, <<"pagesize">>).

-ifdef(TEST).

-endif.

init(Req, Opts) ->

    %% 1- use rest
    %% 2- transform data here ( like table to binary )

    % {cowboy_rest, Req, StateMap}.
    handle_request(
        Req, cowboy_req:match_qs([
            table,
            {key, [], undefined},
            {key_type, [], undefined},
            {value, [], undefined},
            {page, int, 0},
            {pagesize, int, ?DEFAULT_PAGESIZE}], Req),
        Opts
    ).

handle_request(Req, #{table := TableBinStr, key := Key, key_type := KeyType} = _Params, Opts) when Key /= undefined ->
    NKey = normalise_erlang_term(Key, KeyType),
    Json = jsx:encode(
        make_json(
            ets:lookup(normalise_table_name(TableBinStr), NKey),
            []
        )
    ),
    create_reply(200, ?DEFAULT_RESP_HEAD, Json, Req, Opts);
handle_request(Req, #{table := TableBinStr, page := Page, pagesize := PageSize} = _Params, Opts) ->
    Filter = fun(_k, _Val) -> true end, %% TODO: placeholder filter for later.
    Rows = paged(normalise_table_name(TableBinStr), Page, PageSize, Filter),
    RowsDisplay =
        make_json(Rows, []),
    create_reply(200, ?DEFAULT_RESP_HEAD, jsx:encode(RowsDisplay), Req, Opts).

make_json([], R) ->
    R;
make_json([Entry|T], R) ->
    RowMap = #{
        key => json_sanitize_key(element(1, Entry)),
        entry => term_to_bin_string(Entry)
    },
    make_json(T, [RowMap|R]).

paged(Table, Page, PageSize, Filter) when PageSize > 0 ->
    SkipAmount = Page * PageSize,
    case paged_first(Table, SkipAmount, Filter) of
        '$end_of_table' ->
            [];
        {First, Objects} ->
            paged(Table, PageSize, Filter, First, Objects)
    end.

paged(_Table, _PageSize, _Filter, '$end_of_table', Acc) ->
    lists:flatten(Acc);
paged(_Table, PageSize, _Filter, _Key, Acc) when length(Acc) >= PageSize ->
    lists:flatten(Acc);
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

create_reply(StatusCode, ResponseHeaders, ResponseValue, Req, Opts) ->
    {ok, cowboy_req:reply(
        StatusCode,
        ResponseHeaders,
        ResponseValue,
        Req
    ), Opts}.

json_sanitize_key(Key) when is_tuple(Key) ->
    term_to_bin_string(Key);
json_sanitize_key(Key) when is_list(Key) ->
    Key;
json_sanitize_key(Key) when is_atom(Key) ->
    Key.

term_to_bin_string(Term) ->
    erlang:iolist_to_binary([list_to_binary(io_lib:format("~p", [Term]))]).

normalise_table_name(Table) when is_binary(Table) ->
    list_to_atom(binary_to_list(Table)).

normalise_erlang_term(Key, <<"atom">>) ->
    list_to_atom(binary_to_list(Key)).


% ensure_atom()
