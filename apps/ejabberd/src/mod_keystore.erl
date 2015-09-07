-module(mod_keystore).

-behaviour(gen_mod).

%% gen_mod callbacks
-export([start/2,
         stop/1]).

%% Hook handlers
-export([get_key/2]).

-include("ejabberd.hrl").

-define(iol2b(L), iolist_to_binary(L)).

%%
%% gen_mod callbacks
%%

start(Domain, Opts) ->
    create_keystore_ets(),
    load_keys(Opts),
    [ ejabberd_hooks:add(Hook, Domain, ?MODULE, Handler, Priority)
      || {Hook, Handler, Priority} <- hook_handlers() ],
    ok.

stop(Domain) ->
    [ ejabberd_hooks:delete(Hook, Domain, ?MODULE, Handler, Priority)
      || {Hook, Handler, Priority} <- hook_handlers() ],
    delete_keystore_ets(),
    ok.

%%
%% Hook handlers
%%

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

load_key({KeyID, {file, Path}}) ->
    {ok, Data} = file:read_file(Path),
    Trimmed = ?iol2b(re:replace(Data, <<"[\n\r]+">>, <<>>,
                                [global, {newline, any}])),
    ets:insert(keystore, {KeyID, Trimmed});
load_key({KeyID, ram}) ->
    error(not_implemented, [{KeyID, ram}]).
