-module(ets_ui_common).

-include_lib("kernel/include/logger.hrl").

-include("ets_ui.hrl").

-export([
    normalise_erlang_term/2,
    json_sanitize/1,
    term_to_bin_string/1,
    create_reply/4,
    create_reply/5,
    default_pagesize/0
]).

%% I've used the binary strings, as type indicators as they're sent
%% in that format directly from the web handler
%%

%% TODO: don't use bin_string data types ( was sent from webpage )
%% change args to be atoms...

normalise_erlang_term(V, known_atom) when is_binary(V) ->
    list_to_existing_atom(binary_to_list(V));
normalise_erlang_term(V, erl_string) ->
    case erl_scan:string(binary_to_list(V) ++ ".") of
        {ok, Tokens, _EndLocation} ->
            case erl_parse:parse_exprs(Tokens) of
                {ok, ExprList} ->
                    %% NB!
                    %% Only handling first/one statement ( hd/1 ) !!!
                    erl_parse:normalise( hd(ExprList) );
                {error, ErrorInfo} ->
                    {erl_parse_error, {error, ErrorInfo}}
            end;
        {error, ErrorInfo, ErrorLocation} ->
            {error, ErrorInfo, ErrorLocation}
    end;
normalise_erlang_term(V, <<"atom">>) ->
    list_to_atom(binary_to_list(V));
normalise_erlang_term(V, <<"binary_string">>) ->
    V;
normalise_erlang_term(V, <<"integer">>) ->
    list_to_integer(binary_to_list(V));
normalise_erlang_term(V, <<"pid">>) when is_binary(V) ->
    list_to_pid(binary_to_list(V)).

json_sanitize(V) when is_atom(V) ->
    V;
json_sanitize(V) when is_binary(V) ->
    V;
json_sanitize(V) when is_bitstring(V) ->
    V;
json_sanitize(V) when is_boolean(V) ->
    V;
json_sanitize(V) when is_float(V) ->
    V;
json_sanitize(V) when is_function(V) ->
    term_to_bin_string(V);
json_sanitize(V) when is_integer(V) ->
    V;
json_sanitize(V) when is_list(V) ->
    list_to_binary(V);
json_sanitize(V) when is_map(V) ->
    V;
json_sanitize(V) when is_pid(V) ->
    list_to_binary(pid_to_list(V));
json_sanitize(V) when is_port(V) ->
    list_to_binary(port_to_list(V));
json_sanitize(V) when is_reference(V) ->
    list_to_binary(ref_to_list(V));
json_sanitize(V) when is_tuple(V) ->
    term_to_bin_string(V).

term_to_bin_string(Term) ->
    unicode:characters_to_binary(io_lib:format("~100000p", [Term])).

create_reply(StatusCode, ResponseValue, Req, StateMap) ->
    create_reply(StatusCode, ?DEFAULT_RESP_HEAD, ResponseValue, Req, StateMap).

create_reply(StatusCode, ResponseHeaders, ResponseValue, Req, StateMap) ->
    {ok, cowboy_req:reply(
        StatusCode,
        ResponseHeaders,
        ResponseValue,
        Req
    ), StateMap}.

default_pagesize() ->
    case application:get_env(ets_ui, default_page_size, {default, 20}) of
        {default, X} ->
            X;
        X when is_integer(X) andalso X =< 10000 ->
            X;
        X when is_integer(X) andalso X > 10000 ->
            10000; %% Limit to 10K
        _X -> % when fails...
            20
    end.
