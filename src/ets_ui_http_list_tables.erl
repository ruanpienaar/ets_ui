-module(ets_ui_http_list_tables).

-behaviour(cowboy_handler).

-export([
    init/2
]).

init(Req, StateMap) ->
    case cowboy_req:method(Req) of
        <<"GET">> ->
            ets_ui_common:create_reply(
                200,
                jsx:encode(lists:map(fun(Tbl) -> ets_ui_common:json_sanitize(Tbl) end, ets:all())),
                Req,
                StateMap
            )
    end.