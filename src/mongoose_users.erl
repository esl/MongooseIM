%%% @doc Caches info about non-anonymous users using an ets table.
-module(mongoose_users).

%% API
-export([start/1]).
-export([stop/1]).
-export([does_user_exist/2]).

%% Hooks.
-export([remove_user/3]).
-export([remove_domain/3]).

-include("mongoose.hrl").
-include("jlib.hrl").

tbl_name() ->
    mongoose_users.

-spec tbl_name(mongooseim:host_type()) -> atom().
tbl_name(HostType) ->
    gen_mod:get_module_proc(HostType, tbl_name()).

%%====================================================================
%% API
%%====================================================================

-spec start(mongooseim:host_type()) -> ok.
start(HostType) ->
    start_ets(HostType),
    ejabberd_hooks:add(hooks(HostType)),
    ok.

-spec stop(mongooseim:host_type()) -> ok.
stop(HostType) ->
    ejabberd_hooks:delete(hooks(HostType)),
    stop_ets(HostType),
    ok.

hooks(HostType) ->
    [
     {remove_user, HostType, ?MODULE, remove_user, 50},
     {remove_domain, HostType, ?MODULE, remove_domain, 50}
    ].

-spec start_ets(mongooseim:host_type()) -> any().
start_ets(HostType) ->
    Tab = tbl_name(HostType),
    BaseOpts = [named_table, public, set,
                {read_concurrency, true},
                {write_concurrency, false}],
    Opts = maybe_add_heir(whereis(ejabberd_sup), self(), BaseOpts),
    ets:new(Tab, Opts).

-spec stop_ets(mongooseim:host_type()) -> any().
stop_ets(HostType) ->
    Tab = tbl_name(HostType),
    ets:delete(Tab).

-spec does_user_exist(HostType :: mongooseim:host_type(), JID :: jid:jid()) -> boolean().
does_user_exist(HostType, #jid{luser = LUser, lserver = LServer} = JID) ->
    case does_cached_user_exist(HostType, LServer, LUser) of
        true -> true;
        false ->
            case does_stored_user_exist(HostType, JID) of
                true ->
                    put_user_into_cache(HostType, LServer, LUser),
                    true;
                false -> false
        end
    end.

%%====================================================================
%% Hooks
%%====================================================================

-spec remove_user(Acc :: mongoose_acc:t(),
                  LUser :: jid:luser(),
                  LServer :: jid:lserver() | string()) -> mongoose_acc:t().
remove_user(Acc, LUser, LServer) ->
    HostType = mongoose_acc:host_type(Acc),
    delete_user(HostType, LServer, LUser),
    Acc.

-spec remove_domain(mongoose_hooks:simple_acc(),
                    mongooseim:host_type(), jid:lserver()) ->
    mongoose_hooks:simple_acc().
remove_domain(Acc, HostType, Domain) ->
    Tab = tbl_name(HostType),
    ets:match_delete(Tab, {Domain, '_'}),
    Acc.

%%====================================================================
%% Helpers
%%====================================================================

-spec does_stored_user_exist(mongooseim:host_type(), jid:jid()) -> boolean().
does_stored_user_exist(HostType, JID) ->
    true =:= ejabberd_auth:does_stored_user_exist(HostType, JID).

-spec does_cached_user_exist(mongooseim:host_type(), jid:lserver(), jid:luser()) -> boolean().
does_cached_user_exist(HostType, LServer, LUser) ->
    Key = key(LUser, LServer),
    Tab = tbl_name(HostType),
    ets:info(Tab) =/= undefined andalso ets:member(Tab, Key).

-spec put_user_into_cache(mongooseim:host_type(), jid:lserver(), jid:luser()) -> ok.
put_user_into_cache(HostType, LServer, LUser) ->
    Key = key(LUser, LServer),
    Tab = tbl_name(HostType),
    ets:insert(Tab, Key),
    ok.

-spec delete_user(mongooseim:host_type(), jid:lserver(), jid:luser()) -> ok.
delete_user(HostType, LServer, LUser) ->
    Key = key(LUser, LServer),
    Tab = tbl_name(HostType),
    ets:delete(Tab, Key),
    ok.

-spec key(jid:luser(), jid:lserver()) -> {jid:lserver(), jid:luser()}.
key(LUser, LServer) ->
    {LServer, LUser}.

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
