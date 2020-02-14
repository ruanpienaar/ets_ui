-module(ets_ui_meta_http_nodes).

-behaviour(cowboy_handler).

-export([
    init/2
]).

-include("ets_ui.hrl").

init(Req, Opts) ->
    {ok, cowboy_req:reply(
        200,
        ?DEFAULT_RESP_HEAD,
        jsx:encode(nodes_to_json(ets_ui_meta_node_sup:nodes())),
        Req
    ), Opts}.

nodes_to_json(Nodes) when is_list(Nodes) ->
    [{nodes, [ nodes_to_json(Node) || Node <- Nodes ]}];
nodes_to_json({Host, Port, Ping}) ->
    [
        {host, ets_ui_common:json_sanitize(Host)},
        {port, ets_ui_common:json_sanitize(Port)},
        {ping, ets_ui_common:json_sanitize(Ping)}
    ].
