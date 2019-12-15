-module(ets_ui_http_query).

-behaviour(cowboy_handler).

-define(DEFAULT_PAGESIZE, 20).
-define(DEFAULT_PAGE, 0).

-include("ets_ui.hrl").

-export([
    init/2
]).

-ifdef(TEST).
-export([
    get_next_n_objects/3,
    create_reply/4,
    create_reply/5
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
    QueryMap = cowboy_req:match_qs([
        {table, nonempty },% [TableChecker]},
        {key, [], undefined},
        {key_type, [], undefined},
        {value, [], undefined},
        % {page, int, ?DEFAULT_PAGE},
        {continuation, [], undefined},
        {pagesize, int, ?DEFAULT_PAGESIZE},
        {tuple_wildcard, [], undefined}
        ], Req
    ),
    erlang:display(QueryMap),
    #{ table := TableBinStr } = QueryMap,
    Table = normalise_table_name(TableBinStr),
    case ets:info(Table) of
        undefined ->
            create_reply(400, <<"">>, Req, StateMap);
        _ ->
        % case #{ table := {error, {table, undefined}} } = QueryMap of
        %     false ->
                handle_request(
                    Req,
                    QueryMap,
                    StateMap#{ table => Table }
                )
        %     true ->
        %         create_reply(400, <<"">>, Req, StateMap)
        % end.
    end.

%% ets:match_object, match tuple sent in as `tuple_wildcard`
%% TODO: do match object with list of entries...!!!!
%%
%% match_object(Tab, Pattern, Limit) ->
%%                 {[Object], Continuation} | '$end_of_table'
%%
%% match_object(Continuation) ->
%%                {[Object], Continuation} | '$end_of_table'
handle_request(
        Req,
        #{
            tuple_wildcard := TupleWildCard,
            continuation := Continuation, %% This will be ETS continuation
            pagesize := PageSize
        },
        #{ table := Table } = StateMap)
        when TupleWildCard /= undefined ->
    io:format("~p", [normalise_erlang_term(TupleWildCard, <<"tuple">>)]),
    {Objects, NextContinuation} = case Continuation of 
        undefined ->      
            ets:match_object(
                Table,
                normalise_erlang_term(TupleWildCard, <<"tuple">>),
                PageSize
           );
       _ ->
           ets:match_object(Continuation)
    end,
       
    Rows =
        (json_rows(
            Objects
        ))#{ 
            continuation => term_to_bin_string(NextContinuation),
            key_type => <<"tuple">>
        },
    %io:format("~p\n", [Rows]),
    Json = jsx:encode(
        Rows
    ),
    create_reply(200, Json, Req, StateMap);
%% ets:lookup on key, key sent in, with key type
handle_request(
        Req,
        #{
            key := Key,
            key_type := KeyType
        },
        #{ table := Table } = StateMap) when Key /= undefined ->
    NKey = normalise_erlang_term(Key, KeyType),
    Json = jsx:encode(
        json_rows(
            ets:lookup(Table, NKey)
        )
    ),
    create_reply(200, Json, Req, StateMap);
%% listing table entries per page and pagesize

%% TODO: Fix page sending in continuation... ( and the erlang term type for continuation )
handle_request(
        Req,
        #{
            key_type := KeyType,
            continuation := Continuation, %% This will be the next key ( NOT! to be confused with ets continuation )
            pagesize := PageSize
        },
        #{ table := Table } = StateMap) ->
    #{
        continuation := NextContinuation,
        key_type := NextKeyType,
        entries := Rows
     } = case Continuation of
        undefined ->
            get_next_n_objects(Table, undefined, PageSize);
        _ ->
            get_next_n_objects(Table, normalise_erlang_term(Continuation, KeyType), PageSize)
    end,
    RowsDisplay =
        json_rows(Rows),
    create_reply(200, jsx:encode(#{
        continuation => json_sanitize_key(NextContinuation),
        key_type => NextKeyType,
        rows => RowsDisplay
    }), Req, StateMap).

json_rows(Entries) ->
    json_rows(Entries, [], '$end_of_table').

json_rows([], R, LastKey) ->
    #{
        continuation => LastKey,
        rows => R
    };
json_rows([Entry|T], R, _LastKey) ->
    Key = element(1, Entry), %% TODO: use table key_pos to know where the key is !!!
    FormattedValues = format_values(Entry, size(Entry)),
    RowMap = #{
        key => json_sanitize_key(Key),
        key_type => key_type(Key),
        values => FormattedValues
    },
    json_rows(T, [RowMap|R], Key).

%% @doc Here we deal with the key first to get to the staring point.
%% @end
get_next_n_objects(Table, undefined, PageSize) when PageSize > 0 ->
    get_next_n_objects(Table, ets:first(Table), PageSize);
get_next_n_objects(_Table, '$end_of_table' = Key, PageSize) when PageSize > 0 ->
    #{
        continuation => Key,
        key_type => key_type(Key),
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
                        key_type => key_type(Key),
                        entries => lists:reverse([Entry|R])
                    };
                NewPageCount ->
                    get_next_n_objects(Table, NewPageCount, NextKey, ets:lookup(Table, NextKey), [Entry|R])
            end
    end.

% get_next_n_objects(_Table, 0, Key, Acc) ->
%     #{ continuation => Key, entries => lists:reverse(Acc) };
% get_next_n_objects(_Table, _PageSize, '$end_of_table', Acc) ->
%     #{ continuation => '$end_of_table', entries => lists:reverse(Acc) };
% get_next_n_objects(Table, PageSize, NextKey, Acc) ->
%     get_next_n_objects(Table, PageSize-1, ets:next(Table, NextKey), [ets:lookup(Table, NextKey)|Acc]).

create_reply(StatusCode, ResponseValue, Req, StateMap) ->
    create_reply(StatusCode, ?DEFAULT_RESP_HEAD, ResponseValue, Req, StateMap).

create_reply(StatusCode, ResponseHeaders, ResponseValue, Req, StateMap) ->
    {ok, cowboy_req:reply(
        StatusCode,
        ResponseHeaders,
        ResponseValue,
        Req
    ), StateMap}.

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