-module(ets_ui_http_time).

-behaviour(cowboy_handler).

-export([
    init/2
]).

% Request time

% init(Req, StateMap) ->
%     ets_ui_common:create_reply(
%         200,
%         % #{<<"content-type">> => <<"application/octet-stream">>},
%         jsx:encode(
%             binary_to_list(erlang:term_to_binary(calendar:universal_time()))
%         ),
%         Req,
%         StateMap
%     ).

init(Req, StateMap) ->
    case cowboy_req:method(Req) of
        <<"GET">> ->
            ets_ui_common:create_reply(
                200,
                % #{<<"content-type">> => <<"application/octet-stream">>},
                jsx:encode(
                    binary_to_list(erlang:term_to_binary(calendar:universal_time()))
                ),
                Req,
                StateMap
            )
    end.
