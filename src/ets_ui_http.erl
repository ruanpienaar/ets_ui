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
    ModeRoutes = case ets_ui_common:mode() of
        client ->
            client_routes();
        meta ->
            meta_routes()
    end,
    Routes = [
        {'_', ModeRoutes}
    ],
    {ok, RanchPid} = cowboy:start_clear(
        ?MODULE,
        [{port, application:get_env(ets_ui, http_port, 0)}],
        #{
            env => #{
                dispatch => cowboy_router:compile(Routes)
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
    logger:warning("!\n\nCowboy started on port ~p\n\n!", [UsedPort]),
    {ok, RanchPid}.

% -spec client_routes() -> list({term(), atom(), term()}).
client_routes() ->
    [
        {"/ping",
            ets_ui_http_ping, #{}},
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

meta_routes() ->
    [
        {"/api/nodes",
            ets_ui_meta_http_nodes, #{}},
        {"/",
            cowboy_static, {priv_file, ets_ui, "meta_www/index.html"}},
        {"/[...]",
            cowboy_static, {priv_dir, ets_ui, "meta_www/"}}
    ].
