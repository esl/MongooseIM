-module(mod_smart_markers_backend).

-define(MAIN_MODULE, mod_smart_markers).

-export([init/2]).
-export([update_chat_marker/2]).
-export([get_conv_chat_marker/6]).
-export([get_chat_markers/4]).
-export([remove_domain/2]).
-export([remove_user/2]).
-export([remove_to/2]).
-export([remove_to_for_user/3]).

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
-callback get_conv_chat_marker(HostType :: mongooseim:host_type(),
                               From :: jid:jid(),
                               To :: jid:jid(),
                               Thread :: mod_smart_markers:maybe_thread(),
                               Timestamp :: integer(),
                               Private :: boolean()) ->
    [mod_smart_markers:chat_marker()].

-callback get_chat_markers(HostType :: mongooseim:host_type(),
                           To :: jid:jid(),
                           Thread :: mod_smart_markers:maybe_thread(),
                           Timestamp :: integer()) ->
    [mod_smart_markers:chat_marker()].

-callback remove_domain(mongooseim:host_type(), jid:lserver()) -> term().

-callback remove_user(mongooseim:host_type(), jid:jid()) -> term().

-callback remove_to(mongooseim:host_type(), jid:jid()) -> term().

-callback remove_to_for_user(mongooseim:host_type(), From :: jid:jid(), To :: jid:jid()) -> term().

-spec init(mongooseim:host_type(), gen_mod:module_opts()) -> ok.
init(HostType, Opts) ->
    TrackedFuns = [get_chat_markers, get_conv_chat_marker, update_chat_marker],
    mongoose_backend:init(HostType, ?MAIN_MODULE, TrackedFuns, Opts),
    Args = [HostType, Opts],
    mongoose_backend:call(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec update_chat_marker(mongooseim:host_type(),
                         mod_smart_markers:chat_marker()) -> ok.
update_chat_marker(HostType, ChatMarker) ->
    Args = [HostType, ChatMarker],
    mongoose_backend:call_tracked(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec get_conv_chat_marker(HostType :: mongooseim:host_type(),
                           From :: jid:jid(),
                           To :: jid:jid(),
                           Thread :: mod_smart_markers:maybe_thread(),
                           Timestamp :: integer(),
                           Private :: boolean()) -> [mod_smart_markers:chat_marker()].
get_conv_chat_marker(HostType, From, To, Thread, TS, Private) ->
    Args = [HostType, From, To, Thread, TS, Private],
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

%% @doc remove all markers a user has ever been sent
%% Useful for example for `forget_room', when we need to drop all knowledge
%% other users had of this room
-spec remove_to(mongooseim:host_type(), jid:jid()) -> term().
remove_to(HostType, To) ->
    Args = [HostType, To],
    mongoose_backend:call(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

%% @doc remove markers a user sent to a given other
%% Useful for when a user leaves a room, but the room still exists
-spec remove_to_for_user(mongooseim:host_type(), From :: jid:jid(), To :: jid:jid()) -> term().
remove_to_for_user(HostType, From, To) ->
    Args = [HostType, From, To],
    mongoose_backend:call(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, Args).
