%%%-------------------------------------------------------------------
%%% @author Piotr Nosek <piotr.nosek@erlang-solutions.com>
%%% @doc MAM hooks for general metrics
%%%
%%% @end
%%% Created : 13 Feb 2017 by Piotr Nosek
%%%-------------------------------------------------------------------
-module(mongoose_metrics_mam_hooks).

-include("mongoose.hrl").
-include("jlib.hrl").

-export([get_hooks/1]).

%%-------------------
%% Internal exports
%%-------------------
-export([mam_get_prefs/4,
         mam_set_prefs/7,
         mam_remove_archive/4,
         mam_lookup_messages/3,
         mam_archive_message/10,
         mam_muc_get_prefs/4,
         mam_muc_set_prefs/7,
         mam_muc_remove_archive/4,
         mam_muc_lookup_messages/3,
         mam_muc_archive_message/10,
         mam_muc_flush_messages/3]).

-type metrics_notify_return() :: mongoose_metrics_hooks:metrics_notify_return().

%%-------------------
%% Implementation
%%-------------------

%% @doc Here will be declared which hooks should be registered
-spec get_hooks(_) -> [mongoose_metrics_hooks:t(), ...].
get_hooks(Host) ->
    [
     [mam_set_prefs, Host, ?MODULE, mam_set_prefs, 50],
     [mam_get_prefs, Host, ?MODULE, mam_get_prefs, 50],
     [mam_archive_message, Host, ?MODULE, mam_archive_message, 50],
     [mam_remove_archive, Host, ?MODULE, mam_remove_archive, 50],
     [mam_lookup_messages, Host, ?MODULE, mam_lookup_messages, 100],
     [mam_muc_set_prefs, Host, ?MODULE, mam_muc_set_prefs, 50],
     [mam_muc_get_prefs, Host, ?MODULE, mam_muc_get_prefs, 50],
     [mam_muc_archive_message, Host, ?MODULE, mam_muc_archive_message, 50],
     [mam_muc_remove_archive, Host, ?MODULE, mam_muc_remove_archive, 50],
     [mam_muc_lookup_messages, Host, ?MODULE, mam_muc_lookup_messages, 100]
    ].

-spec mam_get_prefs(Result :: any(),
                    Host :: jid:server(),
                    _ArcID :: mod_mam:archive_id(),
                    _ArcJID :: jid:jid()) -> any().
mam_get_prefs(Result, Host, _ArcID, _ArcJID) ->
    mongoose_metrics:update(Host, modMamPrefsGets, 1),
    Result.

-spec mam_set_prefs(Result :: any(), Host :: jid:server(),
    _ArcID :: mod_mam:archive_id(), _ArcJID :: jid:jid(),
    _DefaultMode :: any(), _AlwaysJIDs :: [jid:literal_jid()],
    _NeverJIDs :: [jid:literal_jid()]) -> any().
mam_set_prefs(Result, Host, _ArcID, _ArcJID, _DefaultMode, _AlwaysJIDs, _NeverJIDs) ->
    mongoose_metrics:update(Host, modMamPrefsSets, 1),
    Result.

-spec mam_remove_archive(Acc :: map(),
                         Host :: jid:server(),
                         _ArcID :: mod_mam:archive_id(),
                         _ArcJID :: jid:jid()) -> metrics_notify_return().
mam_remove_archive(Acc, Host, _ArcID, _ArcJID) ->
    mongoose_metrics:update(Host, modMamArchiveRemoved, 1),
    Acc.

mam_lookup_messages(Result = {ok, {_TotalCount, _Offset, MessageRows}},
                    Host, #{is_simple := IsSimple}) ->
    mongoose_metrics:update(Host, modMamForwarded, length(MessageRows)),
    mongoose_metrics:update(Host, modMamLookups, 1),
    case IsSimple of
        true ->
            mongoose_metrics:update(Host, [modMamLookups, simple], 1);
        _ ->
            ok
    end,
    Result;
mam_lookup_messages(Result = {error, _}, _Host, _Params) ->
    Result.

-spec mam_archive_message(Result :: any(), Host :: jid:server(),
    _MessId :: mod_mam:message_id(), _ArcID :: mod_mam:archive_id(),
    _LocJID :: jid:jid(), _RemJID :: jid:jid(),
    _SrcJID :: jid:jid(), _OriginID :: binary(),
    _Dir :: atom(), _Packet :: exml:element()) -> any().
mam_archive_message(Result, Host,
    _MessID, _ArcID, _LocJID, _RemJID, _SrcJID, _OriginID, _Dir, _Packet) ->
    mongoose_metrics:update(Host, modMamArchived, 1),
    Result.

%% ----------------------------------------------------------------------------
%% mod_mam_muc

mam_muc_get_prefs(Result, Host, _ArcID, _ArcJID) ->
    mongoose_metrics:update(Host, modMucMamPrefsGets, 1),
    Result.

mam_muc_set_prefs(Result, Host, _ArcID, _ArcJID, _DefaultMode, _AlwaysJIDs, _NeverJIDs) ->
    mongoose_metrics:update(Host, modMucMamPrefsSets, 1),
    Result.

mam_muc_remove_archive(Acc, Host, _ArcID, _ArcJID) ->
    mongoose_metrics:update(Host, modMucMamArchiveRemoved, 1),
    Acc.

mam_muc_lookup_messages(Result = {ok, {_TotalCount, _Offset, MessageRows}},
    Host, _Params) ->
    mongoose_metrics:update(Host, modMucMamForwarded, length(MessageRows)),
    mongoose_metrics:update(Host, modMucMamLookups, 1),
    Result;
mam_muc_lookup_messages(Result = {error, _},
    _Host, _Params) ->
    Result.


mam_muc_archive_message(Result, Host,
    _MessID, _ArcID, _LocJID, _RemJID, _SrcJID, _OriginID, _Dir, _Packet) ->
    mongoose_metrics:update(Host, modMucMamArchived, 1),
    Result.

%% #rh
mam_muc_flush_messages(Acc, Host, MessageCount) ->
    mongoose_metrics:update(Host, modMucMamFlushed, MessageCount),
    Acc.

%%% vim: set sts=4 ts=4 sw=4 et filetype=erlang foldmarker=%%%',%%%. foldmethod=marker:
