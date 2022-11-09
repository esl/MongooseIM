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
get_mam_hooks(Host) ->
    [
        {mam_set_prefs, Host, fun ?MODULE:mam_set_prefs/3, #{}, 50},
        {mam_get_prefs, Host, fun ?MODULE:mam_get_prefs/3, #{}, 50},
        {mam_archive_message, Host, fun ?MODULE:mam_archive_message/3, #{}, 50},
        {mam_remove_archive, Host, fun ?MODULE:mam_remove_archive/3, #{}, 50},
        {mam_lookup_messages, Host, fun ?MODULE:mam_lookup_messages/3, #{}, 100},
        {mam_flush_messages, Host, fun ?MODULE:mam_flush_messages/3, #{}, 50}
    ].

%% @doc Here will be declared which hooks should be registered when mod_mam_muc is enabled.
-spec get_mam_muc_hooks(mongooseim:host_type()) -> gen_hook:hook_list().
get_mam_muc_hooks(Host) ->
    [
        {mam_muc_set_prefs, Host, fun ?MODULE:mam_muc_set_prefs/3, #{}, 50},
        {mam_muc_get_prefs, Host, fun ?MODULE:mam_muc_get_prefs/3, #{}, 50},
        {mam_muc_archive_message, Host, fun ?MODULE:mam_muc_archive_message/3, #{}, 50},
        {mam_muc_remove_archive, Host, fun ?MODULE:mam_muc_remove_archive/3, #{}, 50},
        {mam_muc_lookup_messages, Host, fun ?MODULE:mam_muc_lookup_messages/3, #{}, 100},
        {mam_muc_flush_messages, Host, fun ?MODULE:mam_muc_flush_messages/3, #{}, 50}
    ].

-spec mam_get_prefs(Acc, Params, Extra) -> {ok, Acc} when
    Acc :: mod_mam:preference() | {error, Reason :: term()},
    Params :: map(),
    Extra :: map().
mam_get_prefs(Result, _Params, #{host_type := Host}) ->
    mongoose_metrics:update(Host, modMamPrefsGets, 1),
    {ok, Result}.

-spec mam_set_prefs(Acc, Params, Extra) -> {ok, Acc} when
    Acc :: term(),
    Params :: map(),
    Extra :: map().
mam_set_prefs(Result, _Params, #{host_type := Host}) ->
    mongoose_metrics:update(Host, modMamPrefsSets, 1),
    {ok, Result}.

-spec mam_remove_archive(Acc, Params, Extra) -> {ok, Acc} when
    Acc :: term(),
    Params :: map(),
    Extra :: map().
mam_remove_archive(Acc, _Params, #{host_type := Host}) ->
    mongoose_metrics:update(Host, modMamArchiveRemoved, 1),
    {ok, Acc}.

-spec mam_lookup_messages(Acc, Params, Extra) -> {ok, Acc} when
    Acc :: {ok, mod_mam:lookup_result()} | {error, term()},
    Params :: map(),
    Extra :: map().
mam_lookup_messages({ok, {_TotalCount, _Offset, MessageRows}} = Result,
                    #{is_simple := IsSimple},
                    #{host_type := Host}) ->
    mongoose_metrics:update(Host, modMamForwarded, length(MessageRows)),
    mongoose_metrics:update(Host, modMamLookups, 1),
    case IsSimple of
        true ->
            mongoose_metrics:update(Host, [modMamLookups, simple], 1);
        _ ->
            ok
    end,
    {ok, Result};
mam_lookup_messages(Result = {error, _}, _Params, _Extra) ->
    {ok, Result}.

-spec mam_archive_message(Acc, Params, Extra) -> {ok, Acc} when
    Acc :: ok | {error, timeout},
    Params :: map(),
    Extra :: map().
mam_archive_message(Result, _Params, #{host_type := Host}) ->
    mongoose_metrics:update(Host, modMamArchived, 1),
    {ok, Result}.

-spec mam_flush_messages(Acc, Params, Extra) -> {ok, Acc} when
    Acc :: ok,
    Params :: map(),
    Extra :: map().
mam_flush_messages(Acc, #{count := MessageCount}, #{host_type := Host}) ->
    mongoose_metrics:update(Host, modMamFlushed, MessageCount),
    {ok, Acc}.

%% ----------------------------------------------------------------------------
%% mod_mam_muc

-spec mam_muc_get_prefs(Acc, Params, Extra) -> {ok, Acc} when
    Acc :: mod_mam:preference() | {error, Reason :: term()},
    Params :: map(),
    Extra :: map().
mam_muc_get_prefs(Result, _Params, #{host_type := Host}) ->
    mongoose_metrics:update(Host, modMucMamPrefsGets, 1),
    {ok, Result}.

-spec mam_muc_set_prefs(Acc, Params, Extra) -> {ok, Acc} when
    Acc :: term(),
    Params :: map(),
    Extra :: map().
mam_muc_set_prefs(Result, _Params, #{host_type := Host}) ->
    mongoose_metrics:update(Host, modMucMamPrefsSets, 1),
    {ok, Result}.

-spec mam_muc_remove_archive(Acc, Params, Extra) -> {ok, Acc} when
    Acc :: term(),
    Params :: map(),
    Extra :: map().
mam_muc_remove_archive(Acc, _Params, #{host_type := Host}) ->
    mongoose_metrics:update(Host, modMucMamArchiveRemoved, 1),
    {ok, Acc}.

-spec mam_muc_lookup_messages(Acc, Params, Extra) -> {ok, Acc} when
    Acc :: {ok, mod_mam:lookup_result()} | {error, term()},
    Params :: map(),
    Extra :: map().
mam_muc_lookup_messages({ok, {_TotalCount, _Offset, MessageRows}} = Result,
                        _Params,
                        #{host_type := Host}) ->
    mongoose_metrics:update(Host, modMucMamForwarded, length(MessageRows)),
    mongoose_metrics:update(Host, modMucMamLookups, 1),
    {ok, Result};
mam_muc_lookup_messages(Result = {error, _}, _Params, _Extra) ->
    {ok, Result}.

-spec mam_muc_archive_message(Acc, Params, Extra) -> {ok, Acc} when
    Acc :: ok | {error, timeout},
    Params :: map(),
    Extra :: map().
mam_muc_archive_message(Result, _Params, #{host_type := Host}) ->
    mongoose_metrics:update(Host, modMucMamArchived, 1),
    {ok, Result}.

-spec mam_muc_flush_messages(Acc, Params, Extra) -> {ok, Acc} when
    Acc :: ok,
    Params :: map(),
    Extra :: map().
mam_muc_flush_messages(Acc, #{count := MessageCount}, #{host_type := Host}) ->
    mongoose_metrics:update(Host, modMucMamFlushed, MessageCount),
    {ok, Acc}.
