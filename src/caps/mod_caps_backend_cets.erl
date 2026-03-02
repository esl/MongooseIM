-module(mod_caps_backend_cets).

-behaviour(mod_caps_backend).

-export([init/1,
         stop/1,
         get_hash_features/2,
         get_jid_features/2,
         get_bare_jid_features/2,
         set_hash_features/3,
         set_jid_features/3,
         delete_hash_features/2,
         delete_jid_features/2]).

-include_lib("jid/include/jid.hrl").

%% Behaviour callbacks

-spec init(mongooseim:host_type()) -> ok.
init(HostType) ->
    store_table_names(HostType),
    create_table(hash_table(HostType)),
    create_table(jid_table(HostType)).

-spec stop(mongooseim:host_type()) -> ok.
stop(HostType) ->
    delete_table(hash_table(HostType)),
    delete_table(jid_table(HostType)),
    delete_table_names(HostType),
    ok.

-spec get_hash_features(mongooseim:host_type(), mod_caps:hash()) ->
          {ok, [mod_caps:feature()]} | {error, not_found}.
get_hash_features(HostType, Hash) ->
    case ets:lookup(hash_table(HostType), Hash) of
        [] -> {error, not_found};
        [{_, Features}] -> {ok, Features}
    end.

-spec get_jid_features(mongooseim:host_type(), jid:jid()) -> [mod_caps:feature()].
get_jid_features(HostType, Jid) ->
    case ets:lookup(jid_table(HostType), jid:to_lower(Jid)) of
        [] -> [];
        [{_, Features}] -> Features
    end.

-spec get_bare_jid_features(mongooseim:host_type(), jid:jid()) ->
    [{jid:lresource(), [mod_caps:feature()]}].
get_bare_jid_features(HostType, #jid{luser = LUser, lserver = LServer}) ->
    ets:select(jid_table(HostType), [{{{LUser, LServer, '$1'}, '$2'}, [], [{{'$1', '$2'}}]}]).

-spec set_hash_features(mongooseim:host_type(), mod_caps:hash(), [mod_caps:feature()]) -> ok.
set_hash_features(HostType, Hash, Features) ->
    cets:insert(hash_table(HostType), {Hash, Features}).

-spec set_jid_features(mongooseim:host_type(), jid:jid(), [mod_caps:feature()]) -> ok.
set_jid_features(HostType, Jid, Features) ->
    cets:insert(jid_table(HostType), {jid:to_lower(Jid), Features}).

-spec delete_hash_features(mongooseim:host_type(), mod_caps:hash()) -> ok.
delete_hash_features(HostType, Hash) ->
    cets:delete(hash_table(HostType), Hash).

-spec delete_jid_features(mongooseim:host_type(), jid:jid()) -> ok.
delete_jid_features(HostType, Jid) ->
    cets:delete(jid_table(HostType), jid:to_lower(Jid)).

%% Helpers

-spec jid_table(mongooseim:host_type()) -> cets:table_name().
jid_table(HostType) ->
    persistent_term:get({?MODULE, HostType, jid_tab}).

-spec hash_table(mongooseim:host_type()) -> cets:table_name().
hash_table(HostType) ->
    persistent_term:get({?MODULE, HostType, hash_tab}).

-spec store_table_names(mongooseim:host_type()) -> ok.
store_table_names(HostType) ->
    persistent_term:put({?MODULE, HostType, jid_tab},
                        binary_to_atom(<<"cets_caps_jid_", HostType/binary>>)),
    persistent_term:put({?MODULE, HostType, hash_tab},
                        binary_to_atom(<<"cets_caps_hash_", HostType/binary>>)).

delete_table_names(HostType) ->
    persistent_term:erase({?MODULE, HostType, jid_tab}),
    persistent_term:erase({?MODULE, HostType, hash_tab}).

-spec create_table(cets:table_name()) -> ok.
create_table(Tab) ->
    cets:start(Tab, #{}),
    cets_discovery:add_table(mongoose_cets_discovery, Tab).

-spec delete_table(cets:table_name()) -> ok.
delete_table(Tab) ->
    cets_discovery:delete_table(mongoose_cets_discovery, Tab),
    cets:stop(Tab).
