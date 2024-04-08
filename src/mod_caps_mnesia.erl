-module(mod_caps_mnesia).
-behaviour(mod_caps_backend).

-export([init/2,
         read/2,
         write/3,
         delete_node/2]).

-record(caps_features,
        {
          node_pair = {<<>>, <<>>} :: mod_caps:node_pair(),
          features = [] :: mod_caps:maybe_pending_features()
        }).

-spec init(mongooseim:host_type(), gen_mod:module_opts()) -> ok.
init(_HostName, _Opts) ->
    case catch mnesia:table_info(caps_features, storage_type) of
        {'EXIT', _} ->
            ok;
        disc_only_copies ->
            ok;
        _ ->
            mnesia:delete_table(caps_features)
    end,
    mongoose_mnesia:create_table(caps_features,
        [{disc_only_copies, [node()]},
         {local_content, true},
         {attributes, record_info(fields, caps_features)}]),
    ok.

-spec read(mongooseim:host_type(), mod_caps:node_pair()) ->
    {ok, mod_caps:maybe_pending_features()} | error.
read(_HostType, Node) ->
    case mnesia:dirty_read({caps_features, Node}) of
        [#caps_features{features = Features}] -> {ok, Features};
        _ -> error
    end.

-spec write(mongooseim:host_type(), mod_caps:node_pair(),
            mod_caps:maybe_pending_features()) -> ok.
write(_HostType, Node, Features) ->
    mnesia:dirty_write(#caps_features{node_pair = Node,
                                      features = Features}),
    ok.

-spec delete_node(mongooseim:host_type(), mod_caps:node_pair()) -> ok.
delete_node(_HostType, Node) ->
    mnesia:dirty_delete(caps_features, Node),
    ok.
