-module(ets_ui_meta_node).

-export([start_link/1]).

-behaviour(gen_statem).

%% gen_statem callbacks
-export([callback_mode/0, init/1, terminate/3, code_change/4]).
-export([ping/3]).

-define(SERVER, ?MODULE).

% -spec start_link({Host, Port}) ->
%             {ok, Pid :: pid()} |
%             ignore |
%             {error, Error :: term()}.
start_link({Host, Port}) ->
    process_flag(trap_exit, true),
    gen_statem:start_link(?MODULE, {Host, Port}, []).

-spec callback_mode() -> gen_statem:callback_mode_result().
callback_mode() ->
    [state_enter, state_functions].

-spec init(Args :: term()) ->
          gen_statem:init_result(term()).
init({Host, Port}) ->
    {ok, ping, #{host => Host, port => Port}}.

-spec ping('enter',
         OldState :: atom(),
         Data :: term()) ->
            gen_statem:state_enter_result('ping');
        (gen_statem:event_type(),
         Msg :: term(),
         Data :: term()) ->
            gen_statem:event_handler_result(atom()).
ping(enter, PrevState, #{
        host := Host,
        port := Port} = Data) when PrevState =:= ping ->
    Response = try
        case holster:simple_proc_req(
            get,
            "http://"++Host++":"++integer_to_list(Port)++"/ping",
            #{domain_lookup_timeout=>500, connect_timeout => 500, retry_timeout => 500, retry => 0}
        ) of
            {response,{200, _, <<"\"pong\"">>}} ->
                true;
            _ ->
                false
        end
    catch
        'EXIT':shutdown:_ ->
            false
    end,
    {keep_state, Data#{ ping => Response }};
ping({call, From}, _Msg, Data) ->
    {next_state, ping, Data, [{reply, From, ok}]};
ping(cast, _Msg, Data) ->
    {next_state, ping, Data};
ping(info, _Msg, Data) ->
    {next_state, ping, Data}.

terminate(_Reason, _StateName, _Data) ->
    void.

code_change(_OldVsn, StateName, Data, _Extra) ->
    {ok, StateName, Data}.
