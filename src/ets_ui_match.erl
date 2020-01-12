-module(ets_ui_match).

%% @doc This function holds on to the continuation from
%%      ets:match_object/3 OR ets:match/3
%%      And allows clients to page through a table with a specified page limit
%% @end

-include_lib("kernel/include/logger.hrl").

-export([
    start_link/4,
    get_more/1
]).

-behaviour(gen_statem).

%% TODO: maybe stop when reached end_of_table

%% gen_statem callbacks
-export([callback_mode/0, init/1, terminate/3, code_change/4]).
-export([alive/3]).

% -spec start_link() ->
%             {ok, Pid :: pid()} |
%             ignore |
%             {error, Error :: term()}.
start_link(MatchFunc, Table, MatchSpec, Limit)
        when ( MatchFunc == match_object orelse
               MatchFunc == match orelse
               MatchFunc == fun_to_ms ) andalso
             is_atom(Table) andalso
             (is_atom(MatchSpec) orelse
              is_tuple(MatchSpec) orelse
              is_function(MatchSpec)) andalso
             is_integer(Limit) ->
    gen_statem:start_link(?MODULE, {MatchFunc, Table, MatchSpec, Limit}, []).

get_more(Pid) when is_pid(Pid) ->
    gen_statem:call(Pid, ?FUNCTION_NAME).

% -spec callback_mode() -> gen_statem:callback_mode_result().
callback_mode() ->
    state_functions.

%% TODO: create state_timeout that will delete/kill the gen_statem,
%%       and the associated state if no queries has been performed.

% -spec init(Args :: term()) ->
%           gen_statem:init_result(term()).
init({MatchFunc = fun_to_ms, Table, Fun, Limit}) ->
    {ok, alive, #{
        match_function => MatchFunc,
        table => Table,
        limit => Limit,
        compiled_match_spec => ets:fun2ms(Fun)
    }, [{state_timeout, expiry_time(), expired}]};
init({MatchFunc, Table, MatchSpec, Limit}) ->
    {ok, alive, #{
        match_function => MatchFunc,
        table => Table,
        match_spec => MatchSpec,
        limit => Limit
    }, [{state_timeout, expiry_time(), expired}]}.

% -spec alive('enter',
%          OldState :: atom(),
%          Data :: term()) ->
%             gen_statem:state_enter_result('alive');
%         (gen_statem:event_type(),
%          Msg :: term(),
%          Data :: term()) ->
%             gen_statem:event_handler_result(atom()).
alive(state_timeout, expired, Data) ->
    ?LOG_INFO(
        "Stopping expired continuation\nData ~p\n",
        [Data]),
    {stop, normal};
alive({call, From}, get_more, #{
        continuation := Continuation,
        match_function := fun_to_ms = MatchFunc,
        table := _Table,
        limit := _Limit } = Data) when Continuation =/= undefined ->
    try_do_ets_match_or_stop(
        MatchFunc, From, Data, {Continuation});
alive({call, From}, get_more, #{
        continuation := Continuation,
        match_function := MatchFunc,
        table := _Table,
        match_spec := _MatchSpec,
        limit := _Limit } = Data) when Continuation =/= undefined ->
    try_do_ets_match_or_stop(
        MatchFunc, From, Data, {Continuation});
alive({call, From}, get_more, #{
        match_function := fun_to_ms = MatchFunc,
        table := Table,
        limit := Limit,
        compiled_match_spec := CompiledMatchSpec } = Data) ->
    try_do_ets_match_or_stop(
        MatchFunc, From, Data, {Table, CompiledMatchSpec, Limit});
alive({call, From}, get_more, #{
        match_function := MatchFunc,
        table := Table,
        match_spec := MatchSpec,
        limit := Limit } = Data) ->
    try_do_ets_match_or_stop(
        MatchFunc, From, Data, {Table, MatchSpec, Limit});
alive(info, Msg, Data) ->
    ?LOG_ERROR(
        "Info message received ~p\nData ~p\n",
        [Msg, Data]),
    {next_state, alive, Data}.

terminate(_Reason, _StateName, _Data) ->
    void.

code_change(_OldVsn, StateName, Data, _Extra) ->
    {ok, StateName, Data}.

try_do_ets_match_or_stop(MatchFunc, From, Data, EtsMatchObjectDetails) ->
    try
        do_ets_match_object_and_keep_state(
            MatchFunc, From, Data, EtsMatchObjectDetails)
    catch
        error:badarg:_S ->
            ?LOG_ERROR("Table doesn't exist\n", []),
            {stop, normal}
    end.
%% @doc
%% Reply's to client with {ok, list(ets_entry()) | '$end_of_table'}.
%% Response is {stop, normal} | {keep_state, Data, Actions}.
%% @end
do_ets_match_object_and_keep_state(
        MatchFunc, From, Data, EtsMatchObjectDetails) ->
    case match_function(MatchFunc, EtsMatchObjectDetails) of
        '$end_of_table' ->
            ok = gen_statem:reply({reply, From, {ok, []}}),
            {stop, normal};
        {Objects, Continuation} ->
            {keep_state,
             Data#{ continuation => Continuation },
             [{reply, From, {ok, Objects}},
              {state_timeout, expiry_time(), expired}
             ]
            }
    end.

match_function(match_object, {Continuation}) ->
    ets:match_object(Continuation);
match_function(match_object, {Table, MatchSpec, Limit}) ->
    ets:match_object(Table, MatchSpec, Limit);
match_function(match, {Continuation}) ->
    ets:match(Continuation);
match_function(match, {Table, MatchSpec, Limit}) ->
    ets:match(Table, MatchSpec, Limit);
match_function(fun_to_ms, {Continuation}) ->
    ets:select(Continuation);
match_function(fun_to_ms, {Table, CompiledMatchSpec, Limit}) ->
    ets:select(Table, CompiledMatchSpec, Limit).

expiry_time() ->
    application:get_env(
        ets_ui,
        continuation_expiry_time,
        5 * ( 60 * 1000 )
    ).
