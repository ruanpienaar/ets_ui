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
    Port = application:get_env(?MODULE, http_port, 54321),
    {ok, _} = cowboy:start_clear(
        ?MODULE, 
        [{port, Port}],
        #{
            env => #{
                dispatch => Dispatch
            }
        }
    ).

% -spec routes() -> list({term(), atom(), term()}).
routes() ->
    [
        {"/api/tables",
            ets_ui_http_handler, []},
        {"/api/table/:table_name",
            ets_ui_http_query, []},
        {"/api/table/all/:table_name",
            ets_ui_http_all, []},
        {"/",
            cowboy_static, {priv_file, ets_ui, "www/index.html"}},
        {"/[...]", 
            cowboy_static, {priv_dir, ets_ui, "www/"}}
    ].