-module(ets_ui_http_query).

-include_lib("kernel/include/logger.hrl").

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

%% TODO:
%% rename continuation to ContinuationPid

init(Req, StateMap) ->
    case parse_query_string_into_map(Req) of
        #{ table := Table } = QueryStringMap when is_atom(Table) ->
            ?LOG_DEBUG(QueryStringMap#{ step => decoded_query_string }),
            {ResponseCode, Json} =
            handle_request(
                QueryStringMap,
                StateMap#{
                    table => Table,
                    table_info => ets:info(Table)
                }
            ),
            ets_ui_common:create_reply(
                ResponseCode, Json, Req, StateMap);
        #{ table := {error, undefined_table} } ->
            ErrorJson = ets_ui_common:json_sanitize({error, table_does_not_exist}),
            ets_ui_common:create_reply(
                400, ErrorJson, Req, StateMap);
        #{ table := {error, unsuitable_table} } ->
            ets_ui_common:create_reply(
                400, <<"Unsuitable table. Either protection == private and/or Type == bag | Type == duplicate_bag">>, Req, StateMap)
    end.

parse_query_string_into_map(Req) ->
    cowboy_req:match_qs(
        [{table, [nonempty, fun(_, TableBinStr) -> table_info(TableBinStr) end]},
         {key, [], undefined},
         {value, [], undefined},
         {continuation, [], undefined},
         % {continuation_type, [], <<"key">>}, %% <<"key">> | <<"pid">>
         {pagesize, int, ets_ui_common:default_pagesize()},
         {tuple_wildcard, [fun(_, TupleWildCardBinStr) -> check_tuplwwildcard(TupleWildCardBinStr) end], undefined},
         {query_type, [], <<"page">>} % <<"page">> | <<"lookup">> | <<"match_object">>
        ],
        Req
    ).

table_info(Table) ->
    try
        TableName =
            case is_table_ref(Table) of
                true ->

                    % Maybe get all table names,
                    % filter out refs and compare the ref to the string
                    % if match done
                    % if not match keep looping

                    %% TODO: list_to_ref .... NOT MEANT TO BE USED ...

                    %% THen once found, give a id, or some value
                    %% that can be used to find that ref table easier

                    erlang:list_to_ref(binary_to_list(Table));
                false ->
                    list_to_existing_atom(binary_to_list(Table))
            end,
        TableInfo = ets:info(TableName),
        case TableInfo /= undefined andalso suitable_table(TableInfo) of
            true ->
                {ok, TableName};
            false ->
                {error, unsuitable_table}
        end
    catch
        error:badarg:_ ->
            {error, undefined_table}
    end.

is_table_ref(Table) ->
    case re:run(Table, "^#Ref<.*>$", [global]) of
        nomatch ->
            false;
        {match, _} ->
            true
    end.

check_tuplwwildcard(TupleWildCardBinStr) ->
    case TupleWildCardBinStr /= undefined of
        true ->
            case
                ets_ui_common:normalise_erlang_term(
                    TupleWildCardBinStr,
                    erl_string
                )
            of
                MatchSpec when is_atom(MatchSpec) orelse
                               is_tuple(MatchSpec) ->
                    {ok, MatchSpec};
                _ ->
                    {error, bad_match_spec}
            end;
        false ->
            {ok, undefined}
    end.

%% @doc
%% Bag tables break the lookup by key, since a table might have thousands/milions of records with the same key
%% and therefore doing a lookup might cause harm to the running node.
%% Protection type private is only readible by the owner, so it's not useful for this tool. Yet.
%% Maybe we could make a callback behaviour in the private owners, so that this tool can call into that.
%% @end
suitable_table(TableInfo) ->
    case [proplists:lookup(type, TableInfo),
          proplists:lookup(protection, TableInfo)]
    of
        [{type, bag},
         _] ->
            false;
        [_,
         {protection, private}] ->
            false;
        _ ->
            true
    end.

%% Match object / Match
handle_request(
        #{
            tuple_wildcard := MatchSpec,
            continuation := Continuation, %% This is the PID used to lookup entries (ets_ui_match.erl)
            pagesize := PageSize,
            query_type := QueryType
        },
        #{
            table := Table,
            table_info := TableInfo
        } = _StateMap)
            when QueryType == <<"match_object">> orelse
                 QueryType == <<"match">> ->
    {Objects, NextContinuation} =
        case Continuation == undefined andalso MatchSpec =/= undefined of
            true ->
                {ok, ContinuationPid} = ets_ui_match:start_link(
                    list_to_existing_atom(binary_to_list(QueryType)), Table, MatchSpec, PageSize),
                {ok, MoreObjects} = ets_ui_match:get_more(ContinuationPid),
                {MoreObjects, ContinuationPid};
            false ->
                ParsedContinuationPid = ets_ui_common:normalise_erlang_term(Continuation, <<"pid">>),
                {ok, MoreObjects} = ets_ui_match:get_more(ParsedContinuationPid),
                {MoreObjects, Continuation}
        end,
    ResponseMap1 =
        case Objects of
            [] ->
                #{}; % No more objects ( pid has terminates, no more continuation )
            _ ->
                #{continuation => ets_ui_common:json_sanitize(NextContinuation)}
        end,
    {keypos, KeyPos} = proplists:lookup(keypos, TableInfo),
    Json = jsx:encode(maps:merge(json_rows(KeyPos, Objects), ResponseMap1)),
    {200, Json};
%% Lookup
handle_request(
        #{
            key := Key,
            query_type := <<"lookup">>
        },
        #{
            table := Table,
            table_info := TableInfo
        } = _StateMap) when Key /= undefined ->
    try
        case ets_ui_common:normalise_erlang_term(Key, erl_string) of
            {error, ParseReason} ->
                ?LOG_DEBUG(#{
                    step => failed_parsing_key,
                    normalised_key => ParseReason
                }),
                {400, jsx:encode(#{<<"responseText">> => ets_ui_common:term_to_bin_string({error, ParseReason})})};
            NKey ->
                ?LOG_DEBUG(#{
                    step => parsed_key,
                    normalised_key => NKey
                }),
                {keypos, KeyPos} = proplists:lookup(keypos, TableInfo),
                Json = jsx:encode(json_rows(KeyPos, ets:lookup(Table, NKey))),
                {200, Json}
        end
    catch
        error:{badarg, {var, _, _VariableSyntax}} ->
            {400, jsx:encode(#{<<"responseText">> => <<"Bad Erlang syntax.">>})};
        C:E:S ->
            ?LOG_DEBUG(#{
                step => handle_lookup_request,
                c => C,
                e => E,
                stack => S
            }),
            {400, jsx:encode(#{<<"responseText">> => <<"Bad Erlang syntax.">>})}
    end;
%% Page through results
handle_request(
        #{
            continuation := Continuation, %% This will be the next key ( NOT! to be confused with ets continuation )
            pagesize := PageSize,
            query_type := <<"page">>
        },
        #{
            table := Table,
            table_info := TableInfo
        } = _StateMap) ->
        ResponseMap1 =
            case Continuation of
                undefined ->
                    get_next_n_objects(Table, undefined, PageSize);
                _ ->
                    ErlTermContinuation =
                        ets_ui_common:normalise_erlang_term(Continuation, erl_string),
                    get_next_n_objects(Table, ErlTermContinuation, PageSize)
            end,
        {keypos, KeyPos} = proplists:lookup(keypos, TableInfo),
        RowsDisplay = json_rows(KeyPos, maps:get(entries, ResponseMap1)),
        ResponseMap2 =
            case maps:is_key(continuation, ResponseMap1) of
                true ->
                    NextContinuation = maps:get(continuation, ResponseMap1),
                    #{continuation => ets_ui_common:term_to_bin_string(NextContinuation)};
                false ->
                    #{}
            end,
        Json = jsx:encode(maps:merge(RowsDisplay, ResponseMap2)),
        {200, Json}.

json_rows(KeyPos, Entries) ->
    json_rows(KeyPos, Entries, []).

json_rows(_KeyPos, [], R) ->
    #{ rows => lists:reverse(R) };
json_rows(KeyPos, [Entry|T], R) when is_tuple(Entry) ->
    Key = element(KeyPos, Entry),
    FormattedValues = format_values(Entry, size(Entry)),
    % ?LOG_DEBUG(#{
    %     step => formatted_values,
    %     values => FormattedValues
    % }),
    RowMap = #{
        key => ets_ui_common:json_sanitize(Key),
        values => FormattedValues
    },
    json_rows(KeyPos, T, [RowMap|R]);
json_rows(KeyPos, [Entry|T], R) when is_list(Entry) ->
    FormattedValues = format_values(Entry, length(Entry)),
    RowMap = #{
        values => FormattedValues
    },
    json_rows(KeyPos, T, [RowMap|R]).

format_values(Entry, Size) ->
    format_values(Entry, Size, []).

format_values([], _, R) ->
    R;
format_values(_Entry, 0, R) ->
    R;
format_values(Entry, Size, R) when is_tuple(Entry) andalso Size > 0 ->
    Val = element(Size, Entry),
    FormVal = ets_ui_common:term_to_bin_string(Val),
    format_values(Entry, Size - 1, [[ data_type(Val), FormVal ] | R ]);
format_values(Entry, _Size, _R) when is_list(Entry) ->
    lists:map(fun(Val) ->
        FormVal = ets_ui_common:term_to_bin_string(Val),
        [ data_type(Val), FormVal ]
    end, Entry).


%% @doc Here we deal with the key first to get to the staring point.
%% @end
get_next_n_objects(Table, undefined, PageSize) when PageSize > 0 ->
    get_next_n_objects(Table, ets:first(Table), PageSize);
get_next_n_objects(_Table, '$end_of_table' = Key, PageSize) when PageSize > 0 ->
    #{
        continuation => Key,
        entries => []
    };
get_next_n_objects(Table, Continuation, PageSize) when PageSize > 0 ->
    get_next_n_objects(Table, PageSize, Continuation, ets:lookup(Table, Continuation), []).

%% @doc Here we deal with the key and entry, and try and fill up the page count
%%      with entries
%% @end
% get_next_n_objects(Table, 0, Key, _, R) ->
%% TODO: improve. we reverse twice, once here, and another in json_rows ...
get_next_n_objects(Table, PageCount, Key, [Entry], R) ->
    case ets:next(Table, Key) of
        '$end_of_table' ->
            #{entries => lists:reverse([Entry|R])};
        NextKey ->
            case PageCount-1 of
                0 ->
                    #{continuation => ets:next(Table, Key),
                      entries => lists:reverse([Entry|R])};
                NewPageCount ->
                    get_next_n_objects(Table, NewPageCount, NextKey, ets:lookup(Table, NextKey), [Entry|R])
            end
    end.

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
