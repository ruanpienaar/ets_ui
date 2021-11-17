-module(ets_ui_http).

-export([
    start_link/0
]).



% -spec start_link() -> {ok, pid()}.

start_link() ->
    {ok, Pid} = cowboy_startup(),
    true = erlang:link(Pid),
    {ok, Pid}.

cowboy_startup() ->
    Routes = [
        {'_', routes()}
    ],
    Dispatch = cowboy_router:compile(Routes),
    Port = application:get_env(ets_ui, http_port, 0), % 54321),
    {ok, RanchPid} = cowboy:start_clear(
        ?MODULE,
        [{port, Port}],
        #{
            env => #{
                dispatch => Dispatch
            }
        }
    ),
    UsedPort =
    case ranch_server:get_addr(ets_ui_http) of
        {_Ip, Port} ->
            Port;
        {_Ip, AssignedPort} ->
            ok = application:set_env(ets_ui, assigned_port, AssignedPort),
            %% TODO: change this to be logging
            AssignedPort
    end,
    logger:notice(#{
        action => "Cowboy webserver started",
        port => UsedPort
    }),
    {ok, RanchPid}.

% -spec routes() -> list({term(), atom(), term()}).
routes() ->
    [
        {"/help",
            ets_ui_http_help, #{}},
        {"/api/tables",
            ets_ui_http_tables, #{}},
        {"/api/table/info/:table",
            ets_ui_http_table, #{}},
        {"/api/query",
            ets_ui_http_query, #{}},
        {"/api/time",
            ets_ui_http_time, #{}},
        {"/",
            cowboy_static, {priv_file, ets_ui, "www/index.html"}},
        {"/[...]",
            cowboy_static, {priv_dir, ets_ui, "www/"}}
    ].
