-module(ets_ui_http_help).

-behaviour(cowboy_handler).

-export([
    init/2
]).

-include("ets_ui.hrl").

init(Req, Opts) ->
    HelpTerm = [
        #{
            <<"request">> => <<"help">>,
            <<"description">> => <<"ETS ui rest endpoint help">>,
            <<"url">> => <<"/help">>
        },
        #{
            <<"request">> => <<"List ETS tables">>,
            <<"description">> => <<"Get a list of ets tables on this node">>,
            <<"url">> => <<"/api/tables">>
        },
        #{
            <<"request">> => <<"query">>,
            <<"description">> => <<"">>,
            <<"url">> => <<"/api/query">>,
            <<"query_string_requirements">> =>
            [
                {table, mandatory},
                {key, optional},
                {key_type, optional},
                {value, optional},
                {page, optional},
                {pagesize, optional}
            ]
        }
    ],
    {ok, cowboy_req:reply(
        200,
        ?DEFAULT_RESP_HEAD,
        jsx:encode(HelpTerm),
        Req
    ), Opts}.
