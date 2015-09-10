-module(mod_keystore).

-behaviour(gen_mod).

%% gen_mod callbacks
-export([start/2,
         stop/1]).

%% Hook handlers
-export([get_key/2]).

%% Public types
-export_type([key_id/0]).

-include("ejabberd.hrl").
-include("mod_keystore.hrl").

-define(BACKEND, mod_keystore_backend).
-define(DEFAULT_RAM_KEY_SIZE, 2048).
-define(iol2b(L), iolist_to_binary(L)).

-type key_id() :: atom().
-type raw_key() :: binary().
-type key_list() :: [{key_id(), raw_key()}].
-type key_type() :: ram | {file, file:name_all()}.

-type key() :: #key{id :: key_id(),
                    key :: raw_key()}.

-callback init(Domain, Opts) -> ok when
      Domain :: ejabberd:server(),
      Opts :: [any()].

%% Cluster members race to decide whose key gets stored in the distributed database.
%% That's why ProposedKey (the key this cluster member tries to propagate to other nodes)
%% might not be the same as ActualKey (key of the member who will have won the race).
-callback init_ram_key(ProposedKey) -> Result when
      ProposedKey :: mod_keystore:key(),
      Result :: {ok, ActualKey} | {error, any()},
      ActualKey :: mod_keystore:key().

-callback get_key(ID :: key_id()) -> key_list().

%%
%% gen_mod callbacks
%%

-spec start(ejabberd:server(), list()) -> ok.
start(Domain, Opts) ->
    create_keystore_ets(),
    gen_mod:start_backend_module(?MODULE, Opts),
    ?BACKEND:init(Domain, Opts),
    init_keys(Opts),
    [ ejabberd_hooks:add(Hook, Domain, ?MODULE, Handler, Priority)
      || {Hook, Handler, Priority} <- hook_handlers() ],
    ok.

-spec stop(ejabberd:server()) -> ok.
stop(Domain) ->
    [ ejabberd_hooks:delete(Hook, Domain, ?MODULE, Handler, Priority)
      || {Hook, Handler, Priority} <- hook_handlers() ],
    delete_keystore_ets(),
    ok.

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
         ?BACKEND:get_key(KeyID) ++
         HandlerAcc)
    catch
        E:R ->
            ?ERROR_MSG("handler error: {~p, ~p}", [E, R]),
            HandlerAcc
    end.

%%
%% Internal functions
%%

hook_handlers() ->
    [
     {get_key, get_key, 50}
    ].

create_keystore_ets() ->
    case does_table_exist(keystore) of
        true -> ok;
        false ->
            keystore = ets:new(keystore, [named_table, public,
                                          {read_concurrency, true}]),
            ok
    end.

delete_keystore_ets() ->
    ets:delete(keystore).

does_table_exist(NameOrTID) ->
    ets:info(NameOrTID, name) /= undefined.

init_keys(Opts) ->
    [ init_key(K, Opts) || K <- proplists:get_value(keys, Opts, []) ].

-spec init_key({key_id(), key_type()}, list()) -> ok.
init_key({KeyID, {file, Path}}, _Opts) ->
    {ok, Data} = file:read_file(Path),
    Trimmed = ?iol2b(re:replace(Data, <<"[\n\r]+">>, <<>>,
                                [global, {newline, any}])),
    true = ets_store_key(KeyID, Trimmed),
    ok;
init_key({KeyID, ram}, Opts) ->
    ProposedKey = crypto:strong_rand_bytes(get_key_size(Opts)),
    {ok, _ActualKey} = ?BACKEND:init_ram_key(#key{id = KeyID, key = ProposedKey}),
    ok.

%% It's easier to trace these than ets:{insert,lookup} - much less noise.
ets_get_key(KeyID) ->
    ets:lookup(keystore, KeyID).

ets_store_key(KeyID, RawKey) ->
    ets:insert(keystore, {KeyID, RawKey}).

get_key_size(Opts) ->
    case lists:keyfind(ram_key_size, 1, Opts) of
        false -> ?DEFAULT_RAM_KEY_SIZE;
        {ram_key_size, KeySize} -> KeySize
    end.
