-module(mod_smart_markers_backend).

-define(MAIN_MODULE, mod_smart_markers).

-export([init/2]).
-export([update_chat_marker/2]).
-export([get_chat_markers/4]).
-export([remove_domain/2]).
-export([remove_user/2]).

%%--------------------------------------------------------------------
%% DB backend behaviour definition
%%--------------------------------------------------------------------
-callback init(mongooseim:host_type(), gen_mod:module_opts()) -> ok.

%%% 'domain', from', 'to', 'thread' and 'type' keys of the ChatMarker map serve
%%% as a composite database key. If key is not available in the database,
%%% then chat marker must be added. Otherwise this function must update
%%% chat marker record for that composite key.
-callback update_chat_marker(mongooseim:host_type(), mod_smart_markers:chat_marker()) -> ok.

%%% This function must return the latest chat markers sent to the
%%% user/room (with or w/o thread) later than provided timestamp.
-callback get_chat_markers(HostType :: mongooseim:host_type(),
                           To :: jid:jid(),
                           Thread :: mod_smart_markers:maybe_thread(),
                           Timestamp :: integer()) ->
    [mod_smart_markers:chat_marker()].

-callback remove_domain(mongooseim:host_type(), jid:lserver()) -> term().

-callback remove_user(mongooseim:host_type(), jid:jid()) -> term().

-spec init(mongooseim:host_type(), gen_mod:module_opts()) -> ok.
init(HostType, Opts) ->
    FOpts = add_default_backend(Opts),
    TrackedFuns = [get_chat_markers, update_chat_marker],
    mongoose_backend:init(HostType, ?MAIN_MODULE, TrackedFuns, FOpts),
    Args = [HostType, Opts],
    mongoose_backend:call(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec update_chat_marker(mongooseim:host_type(),
                         mod_smart_markers:chat_marker()) -> ok.
update_chat_marker(HostType, ChatMarker) ->
    Args = [HostType, ChatMarker],
    mongoose_backend:call_tracked(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec get_chat_markers(HostType :: mongooseim:host_type(),
                       To :: jid:jid(),
                       Thread :: mod_smart_markers:maybe_thread(),
                       Timestamp :: integer()) -> [mod_smart_markers:chat_marker()].
get_chat_markers(HostType, To, Thread, TS) ->
    Args = [HostType, To, Thread, TS],
    mongoose_backend:call_tracked(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

%% @doc remove all entries for a given domain
-spec remove_domain(mongooseim:host_type(), jid:lserver()) -> term().
remove_domain(HostType, Domain) ->
    Args = [HostType, Domain],
    mongoose_backend:call(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

%% @doc remove all stored interactions with a given user
-spec remove_user(mongooseim:host_type(), jid:jid()) -> term().
remove_user(HostType, User) ->
    Args = [HostType, User],
    mongoose_backend:call(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

add_default_backend(Opts) ->
    case lists:keyfind(backend, 2, Opts) of
        false -> [{backend, rdbms} | Opts];
        _ -> Opts
    end.
