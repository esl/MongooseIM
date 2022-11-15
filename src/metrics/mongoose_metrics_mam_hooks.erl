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

-export([get_mam_hooks/1,
         get_mam_muc_hooks/1]).

%%-------------------
%% Internal exports
%%-------------------
-export([mam_get_prefs/3,
         mam_set_prefs/3,
         mam_remove_archive/3,
         mam_lookup_messages/3,
         mam_archive_message/3,
         mam_flush_messages/3,
         mam_muc_get_prefs/3,
         mam_muc_set_prefs/3,
         mam_muc_remove_archive/3,
         mam_muc_lookup_messages/3,
         mam_muc_archive_message/3,
         mam_muc_flush_messages/3]).

%%-------------------
%% Implementation
%%-------------------

%% @doc Here will be declared which hooks should be registered when mod_mam_pm is enabled.
-spec get_mam_hooks(mongooseim:host_type()) -> gen_hook:hook_list().
get_mam_hooks(HostType) ->
    [
        {mam_set_prefs, HostType, fun ?MODULE:mam_set_prefs/3, #{}, 50},
        {mam_get_prefs, HostType, fun ?MODULE:mam_get_prefs/3, #{}, 50},
        {mam_archive_message, HostType, fun ?MODULE:mam_archive_message/3, #{}, 50},
        {mam_remove_archive, HostType, fun ?MODULE:mam_remove_archive/3, #{}, 50},
        {mam_lookup_messages, HostType, fun ?MODULE:mam_lookup_messages/3, #{}, 100},
        {mam_flush_messages, HostType, fun ?MODULE:mam_flush_messages/3, #{}, 50}
    ].

%% @doc Here will be declared which hooks should be registered when mod_mam_muc is enabled.
-spec get_mam_muc_hooks(mongooseim:host_type()) -> gen_hook:hook_list().
get_mam_muc_hooks(HostType) ->
    [
        {mam_muc_set_prefs, HostType, fun ?MODULE:mam_muc_set_prefs/3, #{}, 50},
        {mam_muc_get_prefs, HostType, fun ?MODULE:mam_muc_get_prefs/3, #{}, 50},
        {mam_muc_archive_message, HostType, fun ?MODULE:mam_muc_archive_message/3, #{}, 50},
        {mam_muc_remove_archive, HostType, fun ?MODULE:mam_muc_remove_archive/3, #{}, 50},
        {mam_muc_lookup_messages, HostType, fun ?MODULE:mam_muc_lookup_messages/3, #{}, 100},
        {mam_muc_flush_messages, HostType, fun ?MODULE:mam_muc_flush_messages/3, #{}, 50}
    ].

-spec mam_get_prefs(Acc, Params, Extra) -> {ok, Acc} when
    Acc :: mod_mam:preference() | {error, Reason :: term()},
    Params :: map(),
    Extra :: gen_hook:extra().
mam_get_prefs(Result, _Params, #{host_type := HostType}) ->
    mongoose_metrics:update(HostType, modMamPrefsGets, 1),
    {ok, Result}.

-spec mam_set_prefs(Acc, Params, Extra) -> {ok, Acc} when
    Acc :: term(),
    Params :: map(),
    Extra :: gen_hook:extra().
mam_set_prefs(Result, _Params, #{host_type := HostType}) ->
    mongoose_metrics:update(HostType, modMamPrefsSets, 1),
    {ok, Result}.

-spec mam_remove_archive(Acc, Params, Extra) -> {ok, Acc} when
    Acc :: term(),
    Params :: #{archive_id := mod_mam:archive_id() | undefined, owner := jid:jid()},
    Extra :: gen_hook:extra().
mam_remove_archive(Acc, _Params, #{host_type := HostType}) ->
    mongoose_metrics:update(HostType, modMamArchiveRemoved, 1),
    {ok, Acc}.

-spec mam_lookup_messages(Acc, Params, Extra) -> {ok, Acc} when
    Acc :: {ok, mod_mam:lookup_result()} | {error, term()},
    Params :: mam_iq:lookup_params(),
    Extra :: gen_hook:extra().
mam_lookup_messages({ok, {_TotalCount, _Offset, MessageRows}} = Result,
                    #{is_simple := IsSimple},
                    #{host_type := HostType}) ->
    mongoose_metrics:update(HostType, modMamForwarded, length(MessageRows)),
    mongoose_metrics:update(HostType, modMamLookups, 1),
    case IsSimple of
        true ->
            mongoose_metrics:update(HostType, [modMamLookups, simple], 1);
        _ ->
            ok
    end,
    {ok, Result};
mam_lookup_messages(Result = {error, _}, _Params, _Extra) ->
    {ok, Result}.

-spec mam_archive_message(Acc, Params, Extra) -> {ok, Acc} when
    Acc :: ok | {error, timeout},
    Params :: mod_mam:archive_message_params(),
    Extra :: gen_hook:extra().
mam_archive_message(Result, _Params, #{host_type := HostType}) ->
    mongoose_metrics:update(HostType, modMamArchived, 1),
    {ok, Result}.

-spec mam_flush_messages(Acc, Params, Extra) -> {ok, Acc} when
    Acc :: ok,
    Params :: #{count := integer()},
    Extra :: gen_hook:extra().
mam_flush_messages(Acc, #{count := MessageCount}, #{host_type := HostType}) ->
    mongoose_metrics:update(HostType, modMamFlushed, MessageCount),
    {ok, Acc}.

%% ----------------------------------------------------------------------------
%% mod_mam_muc

-spec mam_muc_get_prefs(Acc, Params, Extra) -> {ok, Acc} when
    Acc :: mod_mam:preference() | {error, Reason :: term()},
    Params :: map(),
    Extra :: gen_hook:extra().
mam_muc_get_prefs(Result, _Params, #{host_type := HostType}) ->
    mongoose_metrics:update(HostType, modMucMamPrefsGets, 1),
    {ok, Result}.

-spec mam_muc_set_prefs(Acc, Params, Extra) -> {ok, Acc} when
    Acc :: term(),
    Params :: map(),
    Extra :: gen_hook:extra().
mam_muc_set_prefs(Result, _Params, #{host_type := HostType}) ->
    mongoose_metrics:update(HostType, modMucMamPrefsSets, 1),
    {ok, Result}.

-spec mam_muc_remove_archive(Acc, Params, Extra) -> {ok, Acc} when
    Acc :: term(),
    Params :: #{archive_id := mod_mam:archive_id() | undefined, room := jid:jid()},
    Extra :: gen_hook:extra().
mam_muc_remove_archive(Acc, _Params, #{host_type := HostType}) ->
    mongoose_metrics:update(HostType, modMucMamArchiveRemoved, 1),
    {ok, Acc}.

-spec mam_muc_lookup_messages(Acc, Params, Extra) -> {ok, Acc} when
    Acc :: {ok, mod_mam:lookup_result()} | {error, term()},
    Params :: mam_iq:lookup_params(),
    Extra :: gen_hook:extra().
mam_muc_lookup_messages({ok, {_TotalCount, _Offset, MessageRows}} = Result,
                        _Params,
                        #{host_type := HostType}) ->
    mongoose_metrics:update(HostType, modMucMamForwarded, length(MessageRows)),
    mongoose_metrics:update(HostType, modMucMamLookups, 1),
    {ok, Result};
mam_muc_lookup_messages(Result = {error, _}, _Params, _Extra) ->
    {ok, Result}.

-spec mam_muc_archive_message(Acc, Params, Extra) -> {ok, Acc} when
    Acc :: ok | {error, timeout},
    Params :: mod_mam:archive_message_params(),
    Extra :: gen_hook:extra().
mam_muc_archive_message(Result, _Params, #{host_type := HostType}) ->
    mongoose_metrics:update(HostType, modMucMamArchived, 1),
    {ok, Result}.

-spec mam_muc_flush_messages(Acc, Params, Extra) -> {ok, Acc} when
    Acc :: ok,
    Params :: map(),
    Extra :: gen_hook:extra().
mam_muc_flush_messages(Acc, #{count := MessageCount}, #{host_type := HostType}) ->
    mongoose_metrics:update(HostType, modMucMamFlushed, MessageCount),
    {ok, Acc}.
