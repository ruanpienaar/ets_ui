-module(ets_ui_meta_node_sup).

-behaviour(supervisor).

-export([
    start_link/0,
    nodes/0
]).

-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, {}).

init({}) ->
    MetaNodes = application:get_env(ets_ui, meta_nodes, []),
    WorkerMetaNodes = lists:map(fun({MetaNodeHost, MetaNodePort}) ->
        ets_ui_common:worker({ets_ui_meta_node, MetaNodeHost, MetaNodePort}, ets_ui_meta_node, start_link, [{MetaNodeHost, MetaNodePort}])
    end, MetaNodes),
    {ok,
        {
            % Restart strategy
            #{
                strategy  => one_for_one, % optional
                intensity => 1,           % optional
                period    => 10           % optional
            },
            % Children
            WorkerMetaNodes
        }
    }.

nodes() ->
    [
        {Host, Port, get_node_ping(Pid)}
        || {{ets_ui_meta_node, Host, Port}, Pid, worker, [ets_ui_meta_node]}
        <- supervisor:which_children(?MODULE)
    ].

get_node_ping(Pid) ->
    {_StateName, Data} = sys:get_state(Pid),
    maps:get(ping, Data).
