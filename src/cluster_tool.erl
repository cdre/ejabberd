%%
%% Module that exposes API for clustering Ejabberd nodes.
%%

-module(cluster_tool).

%% Main Commands
-export([test_node/1,
         attach_node/1,
         attach_master/1]).

%% Debug Commands
-export([attach_mnesia/1]).

%% @doc Test the connectivity between this nodes.
test_node(NodeName) ->
    NodeAtom = if is_list(NodeName) ->
                       list_to_atom(NodeName);
                  true ->
                       NodeName
               end,
    case net_adm:ping(NodeAtom) of 'pong' ->
            io:format("server is reachable.~n"),
            ok;
        _ ->
            io:format("server could NOT be reached.~n"),
            error
    end.

%% @doc Attach to a cluster as a node.
attach_node(NodeName) ->
    NodeAtom = if is_list(NodeName) ->
                       list_to_atom(NodeName);
                  true ->
                       NodeName
               end,
    case test_node(NodeAtom) of
        ok ->
            stop(),
            Ret = attach_mnesia(NodeAtom),
            start(),
            Ret;
        error ->
            {error, io_lib:format("Remote node ~p cannot be pinged on EPMD.~n", [NodeName])}
    end.

%% @doc Attach to a cluster as a replicated master node.
attach_master(NodeName) ->
    NodeAtom = if is_list(NodeName) ->
                       list_to_atom(NodeName);
                  true ->
                       NodeName
               end,
    case test_node(NodeAtom) of
        ok ->
            stop(),
            Ret = attach_mnesia(NodeAtom),
            io:format("Adding table copies on this node...~n", []),
            sync_node(),
            io:format("Tables copied.~n", []),
            start(),
            Ret;
        error ->
            {error, io_lib:format("Remote node ~p cannot be pinged on EPMD.~n", [NodeName])}
    end.

%% @doc Cluster mneisa db without starting or stopping ejabberd.
%%      Exported mostly for debugging purposes.
attach_mnesia(NodeAtom) ->
    try
        io:format("Stopping mnesia...", []),
        mnesia:stop(),
        io:format("ok.~n", []),
        io:format("Erasing local schema...", []),
        ok = mnesia:delete_schema([node()]),
        io:format("ok.~n", []),
        io:format("Starting mnesia...", []),
        mnesia:start(),
        io:format("ok.~n", []),
        io:format("Adding node to cluster...", []),
        {ok, _} = mnesia:change_config(extra_db_nodes, [NodeAtom]),
        {atomic, ok} = mnesia:change_table_copy_type(schema, node(), disc_copies),
        io:format("ok.~n", []),
        io:format("Node attached.~n", []),
        ok
    catch
        Type:Reason ->
            {error, io_lib:format("Clustering failed with exception type ~p: ~p~n", [Type, Reason])}
    end.

%% @doc Adds replicated local copies of all tables.
sync_node() ->
    [{Tb, mnesia:add_table_copy(Tb, node(), Type)}
     || {Tb, [{_NodeName, Type}]} <- [{T, mnesia:table_info(T, where_to_commit)}
                                     || T <- mnesia:system_info(tables)]].

stop() ->
    io:format("Stopping ejabberd...", []),
    application:stop(ejabberd),
    io:format("stopped.~n", []).

start() ->
    io:format("Starting ejabberd...", []),
    application:start(ejabberd),
    io:format("started.~n", []).
