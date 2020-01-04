-module(ets_ui_http_tables).

-behaviour(cowboy_handler).

-export([
    init/2
]).

-ifdef(TEST).
-export([
    tables/1
]).
-endif.

init(Req0, Opts) ->
    Req = cowboy_req:reply(
        200,
        #{
            <<"content-type">> => <<"application/json">>
        },
        jsx:encode([{tables, tables(ets)}]),
        Req0
    ),
    {ok, Req, Opts}.

%% -------------------------
%% Internal

tables(ets) ->
    lists:foldl(fun all_tables/2, [], ets:all()).

all_tables(Table, Acc) ->
    Info = ets:info(Table),
    {name, Name} = proplists:lookup(name, Info),
    {owner, Owner} = proplists:lookup(owner, Info),
    RegName = get_table_reg_name(Owner),
    case exclude(Info, Name, RegName) of
        false ->
            [ sanitize_map(maps:merge(#{
                table => Table,
                name => Name,
                reg_name => RegName},
              maps:from_list(ets:info(Table)))
              ) | Acc ];
        true ->
            Acc
    end.
%% @doc References break the json decoder
%% @end
sanitize_map(Map) ->
    maps:map(
        fun(_K, V) when is_reference(V) ->
            list_to_binary(erlang:ref_to_list(V));
           (_K, V) when is_pid(V) ->
            list_to_binary(erlang:pid_to_list(V));
           (_K, V) ->
            V
        end,
        Map
    ).

exclude(Info, Name, RegName) ->
    {protection, _Protec} = proplists:lookup(protection, Info),
    case
        lists:member(Name, sys_tables())
        %% TODO: Check that the front-end/api calls don't query private tables
        % orelse
        % Protec =:= private
    of
        true ->
            true;
        false ->
            lists:member(RegName, sys_processes())
    end.

get_table_reg_name(Owner) ->
    case catch process_info(Owner, registered_name) of
        [] ->
            undefined;
        {registered_name, ProcName} ->
            ProcName
    end.

sys_tables() ->
    [ac_tab,asn1,cdv_dump_index_table,cdv_menu_table,
     cdv_decode_heap_table,cell_id,cell_pos,clist,
     cover_internal_data_table,cover_collected_remote_data_table,
     cover_binary_code_table,code,code_names,cookies,
     corba_policy,corba_policy_associations,dets,dets_owners,
     dets_registry,disk_log_names,disk_log_pids,eprof,
     erl_atom_cache,erl_epmd_nodes,etop_accum_tab,etop_tr,
     ets_coverage_data,file_io_servers,gs_mapping,gs_names,
     gstk_db,gstk_grid_cellid,gstk_grid_cellpos,gstk_grid_id,
     httpd,id,ign_req_index,ign_requests,index,inet_cache,
     inet_db,inet_hosts,'InitialReferences',int_db,
     interpreter_includedirs_macros,ir_WstringDef,lmcounter,
     locks,mnesia_gvar,mnesia_stats,pg2_table,queue,schema,
     shell_records,snmp_agent_table,snmp_local_db2,snmp_mib_data,
     snmp_note_store,snmp_symbolic_ets,tkFun,tkLink,tkPriv,ttb,
     ttb_history_table,udp_fds,udp_pids,mnesia_monitor].

sys_processes() ->
    [auth, code_server, global_name_server, inet_db,
     mnesia_recover, net_kernel, timer_server, wxe_master].
