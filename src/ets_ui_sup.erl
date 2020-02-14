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
    supervisor:start_link({local, ?MODULE}, ?MODULE, ets_ui_common:mode()).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init(meta) ->
    {ok,
        {
            #{
                strategy  => one_for_one, % optional
                intensity => 1,           % optional
                period    => 10           % optional
            },
            [
                ets_ui_common:supervisor(ets_ui_meta_node_sup, ets_ui_meta_node_sup, start_link, []),
                ets_ui_common:worker(webserver_child, ets_ui_http, start_link, [])
            ]
        }
    };
init(client) ->
    create_atoms(),
    set_otp_version(),
    {ok,
        {
            #{
                strategy  => one_for_one, % optional
                intensity => 1,           % optional
                period    => 10           % optional
            },
            [
                ets_ui_common:worker(webserver_child, ets_ui_http, start_link, [])
            ]
        }
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
