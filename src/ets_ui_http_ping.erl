-module(ets_ui_http_ping).

-behaviour(cowboy_handler).

-export([
    init/2
]).

-include("ets_ui.hrl").

init(Req, Opts) ->
    {ok, cowboy_req:reply(
        200,
        ?DEFAULT_RESP_HEAD,
        jsx:encode(pong),
        Req
    ), Opts}.
