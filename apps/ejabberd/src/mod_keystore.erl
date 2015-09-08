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

-define(iol2b(L), iolist_to_binary(L)).

-type key_id() :: atom().
-type key() :: binary().
-type key_list() :: [{key_id(), key()}].
-type key_type() :: ram | {file, file:name_all()}.

%%
%% gen_mod callbacks
%%

-spec start(ejabberd:server(), list()) -> ok.
start(Domain, Opts) ->
    create_keystore_ets(),
    load_keys(Opts),
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
        ets:lookup(keystore, KeyID) ++ HandlerAcc
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

load_keys(Opts) ->
    [ load_key(K) || K <- proplists:get_value(keys, Opts, []) ].

-spec load_key({key_id(), key_type()}) -> ok.
load_key({KeyID, {file, Path}}) ->
    {ok, Data} = file:read_file(Path),
    Trimmed = ?iol2b(re:replace(Data, <<"[\n\r]+">>, <<>>,
                                [global, {newline, any}])),
    true = ets:insert(keystore, {KeyID, Trimmed}),
    ok;
load_key({KeyID, ram}) ->
    error(not_implemented, [{KeyID, ram}]).
