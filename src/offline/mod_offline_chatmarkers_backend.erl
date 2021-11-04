%% Just a proxy interface module between the main mod_offline_chatmarkers module and
%% the backend modules (i.e. mod_offline_chatmarkers_rdbms...).
-module(mod_offline_chatmarkers_backend).

-export([init/2,
         get/2,
         maybe_store/5,
         remove_user/2]).

-define(MAIN_MODULE, mod_offline_chatmarkers).

-callback init(mongooseim:host_type(), gen_mod:module_opts()) -> ok.
-callback get(mongooseim:host_type(), jid:jid()) -> {ok, [{Thread :: undefined | binary(),
                                                           Room :: undefined | jid:jid(),
                                                           Timestamp :: integer()}]}.

%%% Jid, Thread, and Room parameters serve as a composite database key. If
%%% key is not available in the database, then it must be added with the
%%% corresponding timestamp. Otherwise this function does nothing, the stored
%%% timestamp for the composite key MUST remain unchanged!
-callback maybe_store(mongooseim:host_type(), Jid :: jid:jid(), Thread :: undefined | binary(),
                      Room :: undefined | jid:jid(), Timestamp :: integer()) -> ok.
-callback remove_user(mongooseim:host_type(), Jid :: jid:jid()) -> ok.

-spec init(mongooseim:host_type(), gen_mod:module_opts()) -> ok.
init(HostType, Opts) ->
    mongoose_backend:init(HostType, ?MAIN_MODULE, [get, maybe_store], Opts),
    Args = [HostType, Opts],
    mongoose_backend:call(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec get(mongooseim:host_type(), jid:jid()) -> {ok, [{Thread :: undefined | binary(),
                                                           Room :: undefined | jid:jid(),
                                                           Timestamp :: integer()}]}.
get(HostType, UserJID) ->
    Args = [HostType, UserJID],
    mongoose_backend:call_tracked(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec maybe_store(mongooseim:host_type(), Jid :: jid:jid(), Thread :: undefined | binary(),
                      Room :: undefined | jid:jid(), Timestamp :: integer()) -> ok.
maybe_store(HostType, UserJID, Thread, Room, Timestamp) ->
    Args = [HostType, UserJID, Thread, Room, Timestamp],
    mongoose_backend:call_tracked(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec remove_user(mongooseim:host_type(), Jid :: jid:jid()) -> ok.
remove_user(HostType, UserJID) ->
    Args = [HostType, UserJID],
    mongoose_backend:call(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, Args).
