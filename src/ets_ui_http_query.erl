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
            {table, nonempty},
            {key, [], undefined},
            {key_type, [], undefined},
            {value, [], undefined},
            {page, int, 0},
            {pagesize, int, ?DEFAULT_PAGESIZE},
            {tuple_wildcard, [], undefined}
            ], Req),
        Opts
    ).

%% ets:match_object, match tuple sent in as `tuple_wildcard`
handle_request(Req, #{
        table := TableBinStr,
        tuple_wildcard := TupleWildCard,
        page := _Page,
        pagesize := PageSize} = _Params, Opts) when TupleWildCard /= undefined ->
    Json = jsx:encode(
        make_json(
            ets:match_object(
                normalise_table_name(TableBinStr),
                normalise_erlang_term(TupleWildCard, <<"tuple">>)),
            []
        )
    ),
    create_reply(200, ?DEFAULT_RESP_HEAD, Json, Req, Opts);
%% ets:lookup on key, key sent in, with key type
handle_request(Req, #{
        table := TableBinStr,
        key := Key,
        key_type := KeyType} = _Params, Opts) when Key /= undefined ->
    NKey = normalise_erlang_term(Key, KeyType),
    Json = jsx:encode(
        make_json(
            ets:lookup(normalise_table_name(TableBinStr), NKey),
            []
        )
    ),
    create_reply(200, ?DEFAULT_RESP_HEAD, Json, Req, Opts);
%% listing table entries per page and pagesize
handle_request(Req, #{
        table := TableBinStr,
        page := Page,
        pagesize := PageSize} = _Params, Opts) ->
    Filter = fun(_k, _Val) -> true end, %% TODO: placeholder filter for later.
    Rows = paged(normalise_table_name(TableBinStr), Page, PageSize, Filter),
    RowsDisplay =
        make_json(Rows, []),
    create_reply(200, ?DEFAULT_RESP_HEAD, jsx:encode(RowsDisplay), Req, Opts).

make_json([], R) ->
    R;
make_json([Entry|T], R) ->
    Key = element(1, Entry),
    FormattedValues = format_values(Entry, size(Entry)),
    RowMap = #{
        key => json_sanitize_key(Key),
        key_type => key_type(Key),
        values => FormattedValues
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
    Key;
json_sanitize_key(Key) when is_integer(Key) ->
    Key;
json_sanitize_key(Key) ->
    logger:info("Handing any format key ~p", [Key]),
    Key.

term_to_bin_string(Term) ->
    unicode:characters_to_binary(io_lib:format("~100000p", [Term])).

normalise_table_name(Table) ->
    try
        TableName =
            case is_table_ref(Table) of
                true ->
                    erlang:list_to_ref(binary_to_list(Table));
                false ->
                    list_to_existing_atom(binary_to_list(Table))
            end,
        _ = ets:info(TableName, type),
        TableName
    catch
        error:badarg:_ ->
            {error, {table, Table, undefined}}
    end.

is_table_ref(Table) ->
    case re:run(Table, "^#Ref<.*>$", [global]) of
        nomatch ->
            false;
        {match, _} ->
            true
    end.

normalise_erlang_term(Key, <<"atom">>) ->
    list_to_atom(binary_to_list(Key));
normalise_erlang_term(Key, <<"binary_string">>) ->
    Key;
normalise_erlang_term(Key, <<"integer">>) ->
    list_to_integer(binary_to_list(Key));
normalise_erlang_term(Key, <<"tuple">>) ->
    case erl_scan:string(binary_to_list(Key) ++ ".") of
        {ok, Tokens, _EndLocation} ->
            case erl_parse:parse_exprs(Tokens) of
                {ok, ExprList} ->
                    %% NB!
                    %% We're only expecting one ERL expression as a key!!!
                    erl_parse:normalise( hd(ExprList) );
                {error, ErrorInfo} ->
                    {erl_parse_error, {error, ErrorInfo}}
            end;
        {error, ErrorInfo, ErrorLocation} ->
            {error, ErrorInfo, ErrorLocation}
    end.

format_values(Entry, Size) ->
    format_values(Entry, Size, []).

format_values(_Entry, 1, R) ->
    % lists:reverse(R);
    R;
format_values(Entry, Size, R) when Size > 1 ->
    Val = element(Size, Entry),
    FormVal = term_to_bin_string(Val),
    format_values(Entry, Size - 1, [ {Size, FormVal} | R ]).

key_type(Key) when is_atom(Key) ->
    atom;
key_type(Key) when is_binary(Key) ->
    binary;
key_type(Key) when is_bitstring(Key) ->
    bitstring;
key_type(Key) when is_boolean(Key) ->
    boolean;
key_type(Key) when is_float(Key) ->
    float;
key_type(Key) when is_function(Key) ->
    function;
key_type(Key) when is_integer(Key) ->
    integer;
key_type(Key) when is_list(Key) ->
    list;
key_type(Key) when is_map(Key) ->
    map;
key_type(Key) when is_number(Key) ->
    number;
key_type(Key) when is_pid(Key) ->
    pid;
key_type(Key) when is_port(Key) ->
    port;
key_type(Key) when is_reference(Key) ->
    reference;
key_type(Key) when is_tuple(Key) ->
    tuple.
