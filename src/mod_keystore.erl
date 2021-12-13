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

%% Tests only!
-export([validate_opts/1]).

-export([config_metrics/1]).
-export([process_keys/1]).

%% Public types
-export_type([key/0,
              key_id/0,
              key_list/0,
              key_name/0,
              raw_key/0]).

-ignore_xref([
    behaviour_info/1, get_key/2, validate_opts/1
]).

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

-spec start(mongooseim:host_type(), list()) -> ok.
start(HostType, Opts) ->
    validate_opts(Opts),
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
                  <<"keys">> => #list{items = keys_spec()}
        }
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
        process = fun ?MODULE:process_keys/1
    }.

process_keys(KVs) ->
    {[[{name, Name}], [{type, Type}]], PathOpts} = proplists:split(KVs, [name, type]),
    process_key_opts(Name, Type, PathOpts).

process_key_opts(Name, ram, []) -> {Name, ram};
process_key_opts(Name, file, [{path, Path}]) -> {Name, {file, Path}}.

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

init_keys(HostType, Opts) ->
    [ init_key(K, HostType, Opts) || K <- proplists:get_value(keys, Opts, []) ].

-spec init_key({key_name(), key_type()}, mongooseim:host_type(), list()) -> ok.
init_key({KeyName, {file, Path}}, HostType, _Opts) ->
    {ok, Data} = file:read_file(Path),
    true = ets_store_key({KeyName, HostType}, Data),
    ok;
init_key({KeyName, ram}, HostType, Opts) ->
    ProposedKey = crypto:strong_rand_bytes(get_key_size(Opts)),
    KeyRecord = #key{id = {KeyName, HostType},
                     key = ProposedKey},
    {ok, _ActualKey} = mod_keystore_backend:init_ram_key(HostType, KeyRecord),
    ok.

%% It's easier to trace these than ets:{insert, lookup} - much less noise.
ets_get_key(KeyID) ->
    ets:lookup(keystore, KeyID).

ets_store_key(KeyID, RawKey) ->
    ets:insert(keystore, {KeyID, RawKey}).

get_key_size(Opts) ->
    case lists:keyfind(ram_key_size, 1, Opts) of
        false -> ?DEFAULT_RAM_KEY_SIZE;
        {ram_key_size, KeySize} -> KeySize
    end.

validate_opts(Opts) ->
    validate_key_ids(proplists:get_value(keys, Opts, [])).

validate_key_ids(KeySpecs) ->
    KeyIDs = [ KeyID || {KeyID, _} <- KeySpecs ],
    SortedAndUniqueKeyIDs = lists:usort(KeyIDs),
    case KeyIDs -- SortedAndUniqueKeyIDs of
        [] -> ok;
        [_|_] -> error(non_unique_key_ids, KeySpecs)
    end.

config_metrics(Host) ->
    OptsToReport = [{backend, mnesia}], %list of tuples {option, defualt_value}
    mongoose_module_metrics:opts_for_module(Host, ?MODULE, OptsToReport).
