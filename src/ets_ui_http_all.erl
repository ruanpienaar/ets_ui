-module(ets_ui_http_all).

-export([
    init/2
]).

init(Req0, Opts) ->
    % io:format("~p\n", [Req0]),
    TableName = list_to_atom(binary_to_list(cowboy_req:binding(table_name, Req0))),
    Req = cowboy_req:reply(
        200, 
        #{
            <<"content-type">> => <<"html/text">>
        },
        % jsx:encode(
        %     [{
        %         <<"all_entries">>, 
        %        lists:map(fun(Entry) ->
        %             #{
        %                 key => element(1, Entry),
        %                 %% TODO: only take REST.
        %                 value => list_to_binary(io_lib:format("~1000p", [Entry]))
        %             }
        %         end, ets:tab2list(TableName))
        %     }]
        % ),
        lists:foldl(fun(Entry, Acc) ->            
            erlang:iolist_to_binary([Acc, list_to_binary(io_lib:format("~p\n", [Entry]))])
        end, <<>>, ets:tab2list(TableName)),
        Req0
    ),
    {ok, Req, Opts}.