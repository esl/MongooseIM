-module(mod_keystore).

-behaviour(gen_mod).
-behaviour(mongoose_module_metrics).

%% gen_mod callbacks
-export([start/2]).
-export([stop/1]).
-export([supported_features/0]).
-export([config_spec/0]).

%% Hook handlers
-export([get_key/2]).

-export([process_key/1]).

%% Public types
-export_type([key/0,
              key_id/0,
              key_list/0,
              key_name/0,
              raw_key/0]).

-ignore_xref([get_key/2]).

-include("mod_keystore.hrl").
-include("mongoose.hrl").
-include("mongoose_config_spec.hrl").

-define(DEFAULT_RAM_KEY_SIZE, 2048).

%% A key name is used in the config file to name a key (a class of keys).
%% The name doesn't differentiate between virtual hosts
%% (i.e. there are multiple keys with the same name,
%% one per each XMPP domain).
-type key_name() :: atom().
%% A key ID is used to uniquely identify a key for storage backends.
%% It's used to maintain separate instances of a key with the same name
%% for different virtual hosts.
-type key_id() :: {key_name(), mongooseim:host_type()}.
-type raw_key() :: binary().
-type key_list() :: [{key_id(), raw_key()}].
-type key_type() :: ram | {file, file:name_all()}.

-type key() :: #key{id :: key_id(), key :: raw_key()}.

%%
%% gen_mod callbacks
%%

-spec start(mongooseim:host_type(), gen_mod:module_opts()) -> ok.
start(HostType, Opts) ->
    create_keystore_ets(),
    mod_keystore_backend:init(HostType, Opts),
    init_keys(HostType, Opts),
    ejabberd_hooks:add(hooks(HostType)),
    ok.

-spec stop(mongooseim:host_type()) -> ok.
stop(HostType) ->
    ejabberd_hooks:delete(hooks(HostType)),
    clear_keystore_ets(HostType),
    ok.

hooks(HostType) ->
    [
     {get_key, HostType, ?MODULE, get_key, 50}
    ].

-spec supported_features() -> [atom()].
supported_features() ->
    [dynamic_domains].

-spec config_spec() -> mongoose_config_spec:config_section().
config_spec() ->
    #section{
        items = #{<<"ram_key_size">> => #option{type = integer,
                                                validate = non_negative},
                  <<"keys">> => #list{items = keys_spec(),
                                      format_items = map}
        },
        defaults = #{<<"ram_key_size">> => ?DEFAULT_RAM_KEY_SIZE,
                     <<"keys">> => #{}},
        format_items = map
    }.

keys_spec() ->
    #section{
        items = #{<<"name">> => #option{type = atom,
                                        validate = non_empty},
                  <<"type">> => #option{type = atom,
                                        validate = {enum, [file, ram]}},
                  <<"path">> => #option{type = string,
                                        validate = filename}
        },
        required = [<<"name">>, <<"type">>],
        format_items = map,
        process = fun ?MODULE:process_key/1
    }.

process_key(#{name := Name, type := file, path := Path}) ->
    {Name, {file, Path}};
process_key(#{name := Name, type := ram}) ->
    {Name, ram}.

%%
%% Hook handlers
%%

-spec get_key(HandlerAcc, KeyID) -> Result when
      HandlerAcc :: key_list(),
      KeyID :: key_id(),
      Result :: key_list().
get_key(HandlerAcc, KeyID) ->
    try
        %% This is OK, because the key is
        %% EITHER stored in ETS
        %% OR stored in BACKEND,
        %% with types of both stores returning
        %% AT MOST ONE value per key.
        (ets_get_key(KeyID) ++
         mod_keystore_backend:get_key(KeyID) ++
         HandlerAcc)
    catch
        E:R:S ->
            ?LOG_ERROR(#{what => get_key_failed,
                         error => E, reason => R, stacktrace => S}),
            HandlerAcc
    end.

%%
%% Internal functions
%%

create_keystore_ets() ->
    case does_table_exist(keystore) of
        true -> ok;
        false ->
            BaseOpts = [named_table, public,
                        {read_concurrency, true}],
            Opts = maybe_add_heir(whereis(ejabberd_sup), self(), BaseOpts),
            ets:new(keystore, Opts),
            ok
    end.

%% In tests or when module is started in run-time, we need to set heir to the
%% ETS table, otherwise it will be destroy when the creator's process finishes.
%% When started normally during node start up, self() =:= EjdSupPid and there
%% is no need for setting heir
maybe_add_heir(EjdSupPid, EjdSupPid, BaseOpts) when is_pid(EjdSupPid) ->
     BaseOpts;
maybe_add_heir(EjdSupPid, _Self, BaseOpts) when is_pid(EjdSupPid) ->
      [{heir, EjdSupPid, testing} | BaseOpts];
maybe_add_heir(_, _, BaseOpts) ->
      BaseOpts.

clear_keystore_ets(HostType) ->
    Pattern = {{'_', HostType}, '$1'},
    ets:match_delete(keystore, Pattern).

does_table_exist(NameOrTID) ->
    ets:info(NameOrTID, name) /= undefined.

init_keys(HostType, Opts = #{keys := Keys}) ->
    maps:map(fun(KeyName, KeyType) -> init_key({KeyName, KeyType}, HostType, Opts) end, Keys).

-spec init_key({key_name(), key_type()}, mongooseim:host_type(), gen_mod:module_opts()) -> ok.
init_key({KeyName, {file, Path}}, HostType, _Opts) ->
    {ok, Data} = file:read_file(Path),
    true = ets_store_key({KeyName, HostType}, Data),
    ok;
init_key({KeyName, ram}, HostType, #{ram_key_size := KeySize}) ->
    ProposedKey = crypto:strong_rand_bytes(KeySize),
    KeyRecord = #key{id = {KeyName, HostType},
                     key = ProposedKey},
    {ok, _ActualKey} = mod_keystore_backend:init_ram_key(HostType, KeyRecord),
    ok.

%% It's easier to trace these than ets:{insert, lookup} - much less noise.
ets_get_key(KeyID) ->
    ets:lookup(keystore, KeyID).

ets_store_key(KeyID, RawKey) ->
    ets:insert(keystore, {KeyID, RawKey}).
