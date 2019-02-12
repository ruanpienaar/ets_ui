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
    {ok, 
        {
            #{
                strategy  => one_for_one, % optional
                intensity => 1,           % optional
                period    => 10           % optional
            },
            [
                #{
                    id       => webserver_child,               % mandatory
                    start    => {ets_ui_http, start_link, []}, % mandatory
                    restart  => permanent,                     % optional
                    shutdown => brutal_kill,                   % optional
                    type     => worker,                        % optional
                    modules  => [ets_ui_http]                  % optional
                }   
            ]
        }
    }.