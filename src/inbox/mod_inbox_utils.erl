%%%-------------------------------------------------------------------
%%% @copyright (C) 2018, Erlang-Solutions
%%% @doc
%%%
%%% @end
%%% Created : 30. Jan 2018 13:22
%%%-------------------------------------------------------------------
-module(mod_inbox_utils).

-include("mod_inbox.hrl").
-include("jlib.hrl").

-type inbox_fun() :: fun((mongooseim:host_type(),
                          jid:jid(),
                          jid:jid(),
                          exml:element(),
                          mongoose_acc:t()) -> mod_inbox:count_res()).

%%%%%%%%%%%%%%%%%%%
%% DB Operations shared by mod_inbox_one2one and mod_inbox_muclight
-export([maybe_reset_unread_count/4,
         reset_unread_count_to_zero/3,
         maybe_write_to_inbox/6,
         write_to_sender_inbox/5,
         write_to_receiver_inbox/5,
         clear_inbox/3,
         get_reset_markers/1,
         if_chat_marker_get_id/2,
         has_chat_marker/1,
         fill_from_attr/2,
         wrapper_id/0,
         get_option_write_aff_changes/1,
         get_option_remove_on_kicked/1,
         extract_attr_jid/1,
         maybe_binary_to_positive_integer/1,
         maybe_muted_until/2,
         binary_to_bool/1,
         bool_to_binary/1,
         build_inbox_entry_key/2
        ]).

-ignore_xref([
    fill_from_attr/2, get_reset_markers/1, if_chat_marker_get_id/2
]).

-spec maybe_reset_unread_count(HostType :: mongooseim:host_type(),
                               User :: jid:jid(),
                               Remote :: jid:jid(),
                               Packet :: exml:element()) -> ok.
maybe_reset_unread_count(HostType, User, Remote, Packet) ->
    ResetMarkers = get_reset_markers(HostType),
    case if_chat_marker_get_id(Packet, ResetMarkers) of
        undefined ->
            ok;
        Id ->
            reset_unread_count(HostType, User, Remote, Id)
    end.

-spec reset_unread_count_to_zero(mongooseim:host_type(), jid:jid(), jid:jid()) -> ok.
reset_unread_count_to_zero(HostType, From, Remote) ->
    InboxEntryKey = mod_inbox_utils:build_inbox_entry_key(From, Remote),
    ok = mod_inbox_backend:reset_unread(HostType, InboxEntryKey, undefined).

-spec reset_unread_count(HostType ::mongooseim:host_type(),
                         From :: jid:jid(),
                         Remote :: jid:jid(),
                         MsgId :: id()) -> ok.
reset_unread_count(HostType, From, Remote, MsgId) ->
    InboxEntryKey = mod_inbox_utils:build_inbox_entry_key(From, Remote),
    ok = mod_inbox_backend:reset_unread(HostType, InboxEntryKey, MsgId).

-spec write_to_sender_inbox(HostType :: mongooseim:host_type(),
                            Sender :: jid:jid(),
                            Receiver :: jid:jid(),
                            Packet :: exml:element(),
                            Acc :: mongoose_acc:t()) -> ok.
write_to_sender_inbox(HostType, Sender, Receiver, Packet, Acc) ->
    MsgId = get_msg_id(Packet),
    Content = exml:to_binary(Packet),
    Timestamp = mongoose_acc:timestamp(Acc),
    %% no unread for a user because he writes new messages which assumes he read all previous messages.
    Count = 0,
    InboxEntryKey = mod_inbox_utils:build_inbox_entry_key(Sender, Receiver),
    mod_inbox_backend:set_inbox(HostType, InboxEntryKey, Content, Count, MsgId, Timestamp).

-spec write_to_receiver_inbox(HostType :: mongooseim:host_type(),
                              Sender :: jid:jid(),
                              Receiver :: jid:jid(),
                              Packet :: exml:element(),
                              Acc :: mongoose_acc:t()) -> ok | {ok, integer()}.
write_to_receiver_inbox(HostType, Sender, Receiver, Packet, Acc) ->
    MsgId = get_msg_id(Packet),
    Content = exml:to_binary(Packet),
    Timestamp = mongoose_acc:timestamp(Acc),
    InboxEntryKey = mod_inbox_utils:build_inbox_entry_key(Receiver, Sender),
    mod_inbox_backend:set_inbox_incr_unread(HostType, InboxEntryKey,
                                            Content, MsgId, Timestamp).

-spec clear_inbox(HostType :: mongooseim:host_type(),
                  User :: jid:user(),
                  Server :: jid:server()) -> mod_inbox:write_res().
clear_inbox(HostType, User, Server) when is_binary(User) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Server),
    ok = mod_inbox_backend:clear_inbox(HostType, LUser, LServer).

%%%%%%%%%%%%%%%%%%%
%% Helpers

-spec get_reset_markers(HostType :: mongooseim:host_type()) -> list(marker()).
get_reset_markers(HostType) ->
    gen_mod:get_module_opt(HostType, mod_inbox, reset_markers, [<<"displayed">>]).

-spec if_chat_marker_get_id(Packet :: exml:element(),
                            Markers :: list(marker())) -> undefined | id().
if_chat_marker_get_id(Packet, Markers) when is_list(Markers) ->
    Ids = [if_chat_marker_get_id(Packet, M) || M <- Markers],
    Filtered = [El || El <- Ids, El /= undefined],
    case Filtered of
        [] ->
            undefined;
        [H | _] ->
            H
    end;
if_chat_marker_get_id(Packet, Marker) ->
    case exml_query:paths(Packet, [{element, Marker}, {attr, <<"id">>}]) of
        [Id] ->
            Id;
        _ ->
            undefined
    end.


-spec has_chat_marker(Packet :: exml:element()) -> boolean().
has_chat_marker(Packet) ->
    mongoose_chat_markers:has_chat_markers(Packet).

-spec maybe_write_to_inbox(HostType, User, Remote, Packet, Acc, WriteF) ->
    mod_inbox:count_res() when
      HostType ::mongooseim:host_type(),
      User :: jid:jid(),
      Remote :: jid:jid(),
      Packet :: exml:element(),
      Acc :: mongoose_acc:t(),
      %% WriteF is write_to_receiver_inbox/5 or write_to_sender_inbox/5
      WriteF :: inbox_fun().
maybe_write_to_inbox(HostType, User, Remote, Packet, Acc, WriteF) ->
    case has_chat_marker(Packet) of
        true ->
            ok;
        false ->
            Packet2 = fill_from_attr(Packet, User),
            WriteF(HostType, User, Remote, Packet2, Acc)
    end.

-spec get_msg_id(Msg :: exml:element()) -> binary().
get_msg_id(#xmlel{name = <<"message">>} = Msg) ->
    exml_query:attr(Msg, <<"id">>, <<>>).

-spec fill_from_attr(Msg :: exml:element(), From :: jid:jid()) -> exml:element().
fill_from_attr(Msg = #xmlel{attrs = Attrs}, From) ->
    case exml_query:attr(Msg, <<"from">>, undefined) of
        undefined ->
            FromBin = jid:to_binary(From),
            Msg#xmlel{attrs = [{<<"from">>, FromBin} | Attrs]};
        _ ->
            Msg
    end.

-spec wrapper_id() -> id().
wrapper_id() ->
    uuid:uuid_to_string(uuid:get_v4(), binary_standard).

-spec get_option_write_aff_changes(HostType :: mongooseim:host_type()) -> boolean().
get_option_write_aff_changes(HostType) ->
    gen_mod:get_module_opt(HostType, mod_inbox, aff_changes, true).

-spec get_option_remove_on_kicked(HostType :: mongooseim:host_type()) -> boolean().
get_option_remove_on_kicked(HostType) ->
    gen_mod:get_module_opt(HostType, mod_inbox, remove_on_kicked, true).

extract_attr_jid(ResetStanza) ->
    case exml_query:attr(ResetStanza, <<"jid">>) of
        undefined ->
            {error, <<"jid-required">>};
        Value ->
            case jid:from_binary(Value) of
                error ->
                    {error, <<"invalid-jid">>};
                JID -> JID
            end
    end.

-spec maybe_binary_to_positive_integer(binary()) -> non_neg_integer() | {error, atom()}.
maybe_binary_to_positive_integer(Bin) ->
    try erlang:binary_to_integer(Bin) of
        N when N >= 0 -> N;
        _ -> {error, non_positive_integer}
    catch error:badarg -> {error, 'NaN'}
    end.

-spec maybe_muted_until(integer(), integer()) -> binary().
maybe_muted_until(0, _) -> <<"0">>;
maybe_muted_until(MutedUntil, CurrentTS) ->
    case CurrentTS =< MutedUntil of
        true -> list_to_binary(calendar:system_time_to_rfc3339(MutedUntil, [{offset, "Z"}, {unit, microsecond}]));
        false -> <<"0">>
    end.

-spec binary_to_bool(binary()) -> true | false | error.
binary_to_bool(<<"true">>) -> true;
binary_to_bool(<<"false">>) -> false;
binary_to_bool(_) -> error.

-spec bool_to_binary(boolean()) -> binary() | error.
bool_to_binary(true) -> <<"true">>;
bool_to_binary(false) -> <<"false">>;
bool_to_binary(_) -> error.

build_inbox_entry_key(FromJid, ToJid) ->
    {LUser, LServer} = jid:to_lus(FromJid),
    ToBareJid = jid:to_binary(jid:to_lus(ToJid)),
    {LUser, LServer, ToBareJid}.
