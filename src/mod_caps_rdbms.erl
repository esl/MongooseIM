-module(mod_caps_rdbms).
-behaviour(mod_caps_backend).

-export([init/2,
         read/2,
         write/3,
         delete_node/2]).

-spec init(mongooseim:host_type(), gen_mod:module_opts()) -> ok.
init(HostType, _Opts) ->
    rdbms_queries:prepare_upsert(HostType, caps_upsert, caps,
                                 [<<"node">>, <<"sub_node">>, <<"features">>],
                                 [<<"features">>],
                                 [<<"node">>, <<"sub_node">>]),
    mongoose_rdbms:prepare(caps_delete, caps,
                           [node, sub_node],
                           <<"DELETE FROM caps WHERE node=? AND sub_node=?">>),
    mongoose_rdbms:prepare(caps_select, caps,
                           [node, sub_node],
                           <<"SELECT features FROM caps WHERE node=? AND sub_node=?">>),
    ok.

-spec read(mongooseim:host_type(), mod_caps:node_pair()) ->
    {ok, mod_caps:maybe_pending_features()} | error.
read(HostType, {Node, SubNode}) ->
    case mongoose_rdbms:execute(HostType, caps_select, [Node, SubNode]) of
        [{selected, [{Encoded}]}] -> {ok, jiffy:decode(Encoded)};
        _ -> error
    end.

-spec write(mongooseim:host_type(), mod_caps:node_pair(),
            mod_caps:maybe_pending_features()) -> ok.
write(HostType, {Node, SubNode}, Features) ->
    Encoded = jiffy:encode(Features),
    InsertParams = [Node, SubNode, Encoded],
    UpdateParams = [Encoded],
    UniqueKeyValues = [Node, SubNode],
    rdbms_queries:execute_upsert(HostType, caps_upsert,
                                 InsertParams, UpdateParams, UniqueKeyValues),
    ok.

-spec delete_node(mongooseim:host_type(), mod_caps:node_pair()) -> ok.
delete_node(HostType, {Node, SubNode}) ->
    {updated, _} = mongoose_rdbms:execute(HostType, caps_delete, [Node, SubNode]),
    ok.
