-module(ets_ui_http_query).

-export([
    init/2
]).

init(Req0, Opts) ->
    io:format("~p\n", [Req0]),
    Req = cowboy_req:reply(
        200,
        #{
            <<"content-type">> => <<"application/json">>
        },
        <<"{\"key\":\"value\"}">>,
        Req0
    ),
    {ok, Req, Opts}.