-module(mod_keystore_cets).
-behaviour(mod_keystore_backend).

-export([init/2,
         stop/1,
         init_ram_key/1,
         get_key/1]).

%% CETS callbacks
-export([handle_conflict/2]).

-ignore_xref([get_key/1, init/2, init_ram_key/1]).

-include("mod_keystore.hrl").
-include("mongoose_logger.hrl").

table_name(HostType) ->
    binary_to_atom(<<"cets_keystore_", HostType/binary>>).

-spec init(mongooseim:host_type(), gen_mod:module_opts()) -> ok.
init(HostType, _Opts) ->
    %% There is no logic to remove keys.
    %% Separate table per HostType (so we could remove the table once the module is unloaded).
    Tab = table_name(HostType),
    cets:start(Tab, #{handle_conflict => fun ?MODULE:handle_conflict/2}),
    cets_discovery:add_table(mongoose_cets_discovery, Tab),
    ok.

-spec stop(mongooseim:host_type()) -> ok.
stop(HostType) ->
    Tab = table_name(HostType),
    cets_discovery:delete_table(mongoose_cets_discovery, Tab),
    cets:stop(Tab),
    ok.

%% We need to choose one key consistently.
handle_conflict(Rec1, Rec2) ->
    max(Rec1, Rec2).

-spec init_ram_key(ProposedKey) -> Result when
      ProposedKey :: mod_keystore:key(),
      Result :: {ok, ActualKey} | {error, init_ram_key_failed},
      ActualKey :: mod_keystore:key().
init_ram_key(#key{id = Id = {_, HostType}, key = PropKey}) ->
    Tab = table_name(HostType),
    {_, [{Id, Key}]} = cets:insert_new_or_lookup(Tab, {Id, PropKey}),
    {ok, #key{id = Id, key = Key}}.

-spec get_key(Id :: mod_keystore:key_id()) -> mod_keystore:key_list().
get_key(Id = {_, HostType}) ->
    Tab = table_name(HostType),
    ets:lookup(Tab, Id).
