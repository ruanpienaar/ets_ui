-module(ets_ui_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-include("ets_ui.hrl").

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    create_atoms(),
    set_otp_version(),
    {ok,
        {
            #{
                strategy  => one_for_one, % optional
                intensity => 1,           % optional
                period    => 10           % optional
            },
            case application:get_env(ets_ui, self_start, false) of
              false ->
                [child()];
              true ->
                []
            end
        }
    }.
    
child() ->
    #{
        id       => webserver_child,               % mandatory
        start    => {ets_ui_http, start_link, []}, % mandatory
        restart  => permanent,                     % optional
        shutdown => brutal_kill,                   % optional
        type     => worker,                        % optional
        modules  => [ets_ui_http]                  % optional
    }.

set_otp_version() ->
    % http://erlang.org/doc/system_principles/versions.html
    case
        file:read_file(
            filename:join([
                code:root_dir(), "releases", erlang:system_info(otp_release), "OTP_VERSION"
            ])
        )
    of
        {ok, VsnString} ->
            OtpVersion = hd(binary:split(VsnString, <<"\n">>)),
            ok = application:set_env(ets_ui, otp_version, OtpVersion);
        _ ->
            ok
    end.

create_atoms() ->
    [fun_to_ms].
