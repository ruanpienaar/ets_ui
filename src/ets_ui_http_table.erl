-module(ets_ui_http_table).

-behaviour(cowboy_handler).

-export([
    init/2,
    known_methods/2,
    allowed_methods/2,
    malformed_request/2,
    content_types_provided/2
]).

%% Cowboy exports
-define(COWBOY_JSON_RESPONSE, create_json_response).
-export([
    ?COWBOY_JSON_RESPONSE/2
]).

init(Req, State) ->
    % erlang:display(Req),
    % erlang:display(cowboy_req:parse_header(<<"if-match">>, Req)),
    Table = cowboy_req:binding(table, Req),
    {cowboy_rest, Req, State#{
        table => ets_ui_common:normalise_erlang_term(Table, known_atom)
    }}.

known_methods(Req, State) ->
    {[<<"GET">>], Req, State}.

allowed_methods(Req, State) ->
    {[<<"GET">>], Req, State}.

malformed_request(Req, #{ table := Table} = State) ->
    case ets:info(Table) of
        undefined ->
            {true, Req, State};
        Info when is_list(Info) ->
            {false, Req, State#{ table_info => Info }}
    end.

content_types_provided(Req, State) ->
    {
        [
            {
                {<<"application">>, <<"json">>, []},
                ?COWBOY_JSON_RESPONSE
            }
        ],
        Req,
        State
    }.

?COWBOY_JSON_RESPONSE(Req, #{ table_info := TableInfo } = State) ->
    SaneJson = lists:map(fun({K, V}) ->
        {
            ets_ui_common:json_sanitize(K),
            ets_ui_common:json_sanitize(V)
        }
    end, TableInfo),
    % erlang:display(SaneJson),
    {
        jsx:encode(SaneJson),
        Req,
        State
    }.
