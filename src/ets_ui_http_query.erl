-module(ets_ui_http_query).

-behaviour(cowboy_handler).

-include("ets_ui.hrl").

-export([
    init/2
]).

-ifdef(TEST).
-export([
    get_next_n_objects/3,
    handle_request/2
]).
-endif.

init(Req, StateMap) ->
    % {cowboy_rest, Req, StateMap}.
    %% 1- use rest
    %% 2- transform data here ( like table to binary )
    % {cowboy_rest, Req, StateMap}.
    % TableChecker = fun(_, TableBinStr) ->
    %     NormTable = normalise_table_name(TableBinStr),
    %     case ets:info(NormTable, id) of
    %         undefined ->
    %             {error, {table, undefined}};
    %         _ ->
    %             {ok, NormTable}
    %     end
    % end,
    QueryMap = parse_qs(Req),
    % erlang:display(QueryMap),
    #{ table := TableBinStr } = QueryMap,
    Table = normalise_table_name(TableBinStr),
    % try
        case ets:info(Table) of
            undefined ->
                ets_ui_common:create_reply(400, <<"">>, Req, StateMap);
            TableInfo ->
                {ResponseCode, Json} =
                handle_request(
                    QueryMap,
                    StateMap#{
                        table => Table,
                        table_info => TableInfo
                    }
                ),
                ets_ui_common:create_reply(ResponseCode, Json, Req, StateMap)
        end.
    % catch
    %     C:E:_ ->
    %         ets_ui_common:create_reply(
    %             400, ets_ui_common:json_sanitize({C, E}), Req, StateMap)
    % end.

parse_qs(Req) ->
    QueryMap = cowboy_req:match_qs(
        [
            {table, nonempty },% [TableChecker]},
            {key, [], undefined},
            {key_type, [], undefined},
            {value, [], undefined},
            {continuation, [], undefined},
            {pagesize, int, ?DEFAULT_PAGESIZE},
            {tuple_wildcard, [], undefined}
        ],
        Req
    ),
    QueryMap.

%% TODO: spec out continuation() in X_common.erl
-spec continuation_function({tuple()} | {atom(), term(), non_neg_integer()})
        -> {list(), tuple()} | '$end_of_table'.
continuation_function({Continuation}) ->
    XXX = ets_ui_common:normalise_erlang_term(
        Continuation,
        ets_continuation
    ),
    case erlang:is_reference(element(5, XXX)) of
        false ->
            true = erlang:is_reference(element(4, XXX));
        true ->
            ok
    end,
    % erlang:display("USE IT "),
    % erlang:display(XXX),
    ets:match_object(
        XXX
    );
continuation_function({Table, TupleWildCard, PageSize}) ->
    RRR = ets:match_object(
        Table,
        ets_ui_common:normalise_erlang_term(
            TupleWildCard,
            erl_string
        ),
        PageSize
    ),
    timer:sleep(50),
    RRR.

%% Match object
handle_request(
        #{
            tuple_wildcard := TupleWildCard,
            continuation := Continuation, %% This will be ETS continuation
            pagesize := PageSize
        },
        #{
            table := Table,
            table_info := TableInfo
        } = _StateMap)
        when TupleWildCard /= undefined ->

    MatchArgs =
        case Continuation of
            undefined ->
                {Table, TupleWildCard, PageSize};
            _ ->
                {Continuation}
        end,
    {Objects, NextContinuation} =
        case continuation_function(MatchArgs) of
            '$end_of_table' ->
                {[], '$end_of_table'};
            {_, XXX} = NewMatchObjResponse ->
                % erlang:display("RETURN IT "),
                % erlang:display(XXX),
                NewMatchObjResponse
        end,

    {keypos, KeyPos} = proplists:lookup(keypos, TableInfo),
    Rows =
        (json_rows(
            KeyPos,
            Objects
        ))#{
            continuation => ets_ui_common:json_sanitize({ets_continuation, NextContinuation}),
            key_type => data_type(NextContinuation)
        },
    Json = jsx:encode(Rows),
    {200, Json};
%% Lookup
handle_request(
        #{
            key := Key,
            key_type := KeyType
        },
        #{
            table := Table,
            table_info := TableInfo
        } = _StateMap) when Key /= undefined ->
    NKey = ets_ui_common:normalise_erlang_term(Key, KeyType),
    {keypos, KeyPos} = proplists:lookup(keypos, TableInfo),
    Json = jsx:encode(
        json_rows(
            KeyPos,
            ets:lookup(Table, NKey)
        )
    ),
    {200, Json};
%% Page through table
%% TODO: Fix page sending in continuation... ( and the erlang term type for continuation )
handle_request(
        #{
            key_type := KeyType,
            continuation := Continuation, %% This will be the next key ( NOT! to be confused with ets continuation )
            pagesize := PageSize
        },
        #{
            table := Table,
            table_info := TableInfo
        } = _StateMap) ->

    case
        (Continuation == undefined andalso KeyType == undefined)
        orelse
        (Continuation /= undefined andalso KeyType /= undefined)

    of
        false ->
            {400, <<"">>};
        true ->
            #{
                continuation := NextContinuation,
                key_type := NextKeyType,
                entries := Entries
             } = case Continuation of
                undefined ->
                    get_next_n_objects(Table, undefined, PageSize);
                _ ->
                    get_next_n_objects(Table, ets_ui_common:normalise_erlang_term(Continuation, KeyType), PageSize)
            end,
            {keypos, KeyPos} = proplists:lookup(keypos, TableInfo),
            RowsDisplay = json_rows(KeyPos, Entries),
            Json = jsx:encode(RowsDisplay#{
                continuation => NextContinuation,
                key_type => NextKeyType
            }),
            {200, Json}
    end.

json_rows(KeyPos, Entries) ->
    json_rows(KeyPos, Entries, []).

json_rows(_KeyPos, [], R) ->
    #{ rows => R };
json_rows(KeyPos, [Entry|T], R) ->
    Key = element(KeyPos, Entry), %% TODO: use table key_pos to know where the key is !!!
    FormattedValues = format_values(Entry, size(Entry)),
    RowMap = #{
        key => ets_ui_common:json_sanitize(Key),
        key_type => data_type(Key),
        values => FormattedValues
    },
    json_rows(KeyPos, T, [RowMap|R]).

%% @doc Here we deal with the key first to get to the staring point.
%% @end
get_next_n_objects(Table, undefined, PageSize) when PageSize > 0 ->
    get_next_n_objects(Table, ets:first(Table), PageSize);
get_next_n_objects(_Table, '$end_of_table' = Key, PageSize) when PageSize > 0 ->
    #{
        continuation => Key,
        key_type => data_type(Key),
        entries => []
    };
get_next_n_objects(Table, Continuation, PageSize) when PageSize > 0 ->
    get_next_n_objects(Table, PageSize, Continuation, ets:lookup(Table, Continuation), []).

%% @doc Here we deal with the key and entry, and try and fill up the page count
%%      with entries
%% @end
% get_next_n_objects(Table, 0, Key, _, R) ->

get_next_n_objects(Table, PageCount, Key, [Entry], R) ->
    case ets:next(Table, Key) of
        '$end_of_table' ->
            #{
                continuation => '$end_of_table',
                key_type => atom,
                entries => lists:reverse([Entry|R])
            };
        NextKey ->
            case PageCount-1 of
                0 ->
                    #{
                        continuation => ets:next(Table, Key),% Key, % Should that not be ets:next ?
                        key_type => data_type(Key),
                        entries => lists:reverse([Entry|R])
                    };
                NewPageCount ->
                    get_next_n_objects(Table, NewPageCount, NextKey, ets:lookup(Table, NextKey), [Entry|R])
            end
    end.

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

format_values(Entry, Size) ->
    format_values(Entry, Size, []).

format_values(_Entry, 0, R) ->
    % lists:reverse(R);
    R;
format_values(Entry, Size, R) when Size > 0 ->
    Val = element(Size, Entry),
    FormVal = ets_ui_common:term_to_bin_string(Val),
    format_values(Entry, Size - 1, [ {Size, FormVal} | R ]).

data_type(Key) when is_atom(Key) ->
    atom;
data_type(Key) when is_binary(Key) ->
    binary;
data_type(Key) when is_bitstring(Key) ->
    bitstring;
data_type(Key) when is_boolean(Key) ->
    boolean;
data_type(Key) when is_float(Key) ->
    float;
data_type(Key) when is_function(Key) ->
    function;
data_type(Key) when is_integer(Key) ->
    integer;
data_type(Key) when is_list(Key) ->
    list;
data_type(Key) when is_map(Key) ->
    map;
data_type(Key) when is_number(Key) ->
    number;
data_type(Key) when is_pid(Key) ->
    pid;
data_type(Key) when is_port(Key) ->
    port;
data_type(Key) when is_reference(Key) ->
    reference;
data_type(Key) when is_tuple(Key) ->
    tuple.
