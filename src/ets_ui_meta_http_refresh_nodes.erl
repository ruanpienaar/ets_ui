-module(ets_ui_meta_http_refresh_nodes).

-behaviour(cowboy_handler).

-export([
    init/2
]).

-include("ets_ui.hrl").

init(Req, Opts) ->
    ok = ets_ui_meta_node_sup:refresh_nodes(),
    {ok, cowboy_req:reply(
        200,
        ?DEFAULT_RESP_HEAD,
        <<"{}">>,
        Req
    ), Opts}.
