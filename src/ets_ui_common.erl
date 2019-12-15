-module(ets_ui_common).

-include("ets_ui.hrl").

-export([
    normalise_erlang_term/2,
    json_sanitize/1,
    term_to_bin_string/1,
    create_reply/4,
    create_reply/5
]).

%% I've used the binary strings, as type indicators as they're sent
%% in that format directly from the web handler
%%
normalise_erlang_term(V, known_atom) when is_binary(V) ->
    list_to_existing_atom(binary_to_list(V));
normalise_erlang_term(V, ets_continuation) ->
    case normalise_erlang_term(V, erl_string) of
        % {tab(),integer(),integer(),comp_match_spec(),list(),integer()}
        {A, B, C, Ref, D, E} when is_list(Ref) ->
            {A, B, C, erlang:list_to_ref(Ref), D, E};
        % {tab(),_,_,integer(),comp_match_spec(),list(),integer(),integer()}
        {A, B, C, D, Ref, E, F, G} when is_list(Ref) ->
            {A, B, C, D, erlang:list_to_ref(Ref), E, F, G}
    end;
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
    list_to_integer(binary_to_list(V)).

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
    V;
json_sanitize(V) when is_map(V) ->
    V;
json_sanitize(V) when is_pid(V) ->
    list_to_binary(pid_to_list(V));
json_sanitize(V) when is_port(V) ->
    list_to_binary(port_to_list(V));
json_sanitize(V) when is_reference(V) ->
    list_to_binary(ref_to_list(V));
%% Ripped out from stdlib/ets ( 21.0 )
% -type continuation() :: '$end_of_table'
%                       | {tab(),integer(),integer(),comp_match_spec(),list(),integer()}
%                       | {tab(),_,_,integer(),comp_match_spec(),list(),integer(),integer()}.
%
%% Yes i chose cryptic vars, did not want to keep reading the ETS c code ...
json_sanitize({ets_continuation, '$end_of_table'=X}) ->
    json_sanitize(X);
json_sanitize({ets_continuation, {A, B, C, Ref, D, E}}) when is_reference(Ref) ->
    json_sanitize({A, B, C, erlang:ref_to_list(Ref), D, E});
json_sanitize({ets_continuation, {A, B, C, D, Ref, E, F, G}}) when is_reference(Ref) ->
    json_sanitize({A, B, C, D, erlang:ref_to_list(Ref), E, F, G});
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
