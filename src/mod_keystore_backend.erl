%% Just a proxy interface module between the main mod_keystore module and
%% the backend modules (i.e. mod_keystore_mnesia...).
-module(mod_keystore_backend).

-export([init/2, stop/1, init_ram_key/2, get_key/1]).

-define(MAIN_MODULE, mod_keystore).

-callback init(mongooseim:host_type(), gen_mod:module_opts()) -> ok.
-callback stop(mongooseim:host_type()) -> ok.

%% Cluster members race to decide whose key gets stored in the distributed database.
%% That's why ProposedKey (the key this cluster member tries to propagate to other nodes)
%% might not be the same as ActualKey (key of the member who will have won the race).
-callback init_ram_key(ProposedKey) -> Result when
      ProposedKey :: mod_keystore:key(),
      Result :: {ok, ActualKey} | {error, any()},
      ActualKey :: mod_keystore:key().

-callback get_key(ID :: mod_keystore:key_id()) -> mod_keystore:key_list().

-spec init(mongooseim:host_type(), gen_mod:module_opts()) -> ok.
init(HostType, Opts) ->
    mongoose_backend:init(HostType, ?MAIN_MODULE, [], Opts),
    Args = [HostType, Opts],
    mongoose_backend:call(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec stop(mongooseim:host_type()) -> ok.
stop(HostType) ->
    Args = [HostType],
    mongoose_backend:call(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

%% Cluster members race to decide whose key gets stored in the distributed database.
%% That's why ProposedKey (the key this cluster member tries to propagate to other nodes)
%% might not be the same as ActualKey (key of the member who will have won the race).
-spec init_ram_key(HostType, ProposedKey) -> Result when
      HostType :: mongooseim:host_type(),
      ProposedKey :: mod_keystore:key(),
      Result :: {ok, ActualKey} | {error, any()},
      ActualKey :: mod_keystore:key().
init_ram_key(HostType, ProposedKey) ->
    Args = [ProposedKey],
    mongoose_backend:call(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec get_key(ID :: mod_keystore:key_id()) -> mod_keystore:key_list().
get_key({_, HostType} = ID) ->
    Args = [ID],
    mongoose_backend:call(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, Args).
