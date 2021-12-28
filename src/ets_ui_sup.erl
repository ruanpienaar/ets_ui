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

    tbl = ets:new(tbl, [ordered_set, public, named_table]),
    tbl2 = ets:new(tbl2, [set, public, named_table]),
    tbl3 = ets:new(tbl3, [bag, public, named_table]),
    tbl4 = ets:new(tbl4, [duplicate_bag, public, named_table]),
    DoInsert = fun(X) ->
        true = ets:insert(tbl, {X, true}),
        true = ets:insert(tbl2, {X, true}),
        true = ets:insert(tbl3, make_random_tuple(X)),
        true = ets:insert(tbl4, make_random_tuple(X))
    end,
    ok = lists:foreach(
        fun(X) ->
            true = DoInsert(X),
            true = DoInsert(X),
            true = DoInsert(X),
            true = DoInsert(X),
            true = DoInsert(X),
            true = DoInsert(X),
            true = DoInsert(X),
            true = DoInsert(X)
        end,
        lists:seq(0, 1000)
    ),
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

make_random_tuple(X) ->
    erlang:setelement(1, list_to_tuple(lists:seq(1, round(rand:uniform() * 10) + 2)), X).

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
