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
-export([maybe_reset_unread_count/5,
         reset_unread_count_to_zero/3,
         maybe_write_to_inbox/6,
         write_to_sender_inbox/5,
         write_to_receiver_inbox/5,
         clear_inbox/3,
         get_reset_markers/1,
         if_chat_marker_get_id/2,
         has_chat_marker/1,
         get_option_write_aff_changes/1,
         get_option_remove_on_kicked/1,
         extract_attr_jid/1,
         maybe_binary_to_positive_integer/1,
         encode_rsm_id/2,
         decode_rsm_id/1,
         binary_to_bool/1,
         bool_to_binary/1,
         build_inbox_entry_key/2,
         build_inbox_result_elements/2,
         build_entry_result_elements/2,
         all_valid_boxes_for_query/1,
         calculate_ts_from/2
        ]).

-ignore_xref([get_reset_markers/1, if_chat_marker_get_id/2]).

-spec maybe_reset_unread_count(HostType :: mongooseim:host_type(),
                               User :: jid:jid(),
                               Remote :: jid:jid(),
                               Packet :: exml:element(),
                               Acc :: mongoose_acc:t()) -> ok.
maybe_reset_unread_count(HostType, User, Remote, Packet, Acc) ->
    ResetMarkers = get_reset_markers(HostType),
    case if_chat_marker_get_id(Packet, ResetMarkers) of
        undefined ->
            ok;
        Id ->
            TS = mongoose_acc:timestamp(Acc),
            reset_unread_count(HostType, User, Remote, Id, TS)
    end.

-spec reset_unread_count_to_zero(mongoose_acc:t(), jid:jid(), jid:jid()) -> ok.
reset_unread_count_to_zero(Acc, From, Remote) ->
    TS = mongoose_acc:timestamp(Acc),
    HostType = mongoose_acc:host_type(Acc),
    InboxEntryKey = build_inbox_entry_key(From, Remote),
    ok = mod_inbox_backend:reset_unread(HostType, InboxEntryKey, undefined, TS).

-spec reset_unread_count(HostType ::mongooseim:host_type(),
                         From :: jid:jid(),
                         Remote :: jid:jid(),
                         MsgId :: id(),
                         TS :: integer()) -> ok.
reset_unread_count(HostType, From, Remote, MsgId, TS) ->
    InboxEntryKey = build_inbox_entry_key(From, Remote),
    ok = mod_inbox_backend:reset_unread(HostType, InboxEntryKey, MsgId, TS).

-spec write_to_sender_inbox(HostType :: mongooseim:host_type(),
                            Sender :: jid:jid(),
                            Receiver :: jid:jid(),
                            Packet :: exml:element(),
                            Acc :: mongoose_acc:t()) -> ok.
write_to_sender_inbox(HostType, Sender, Receiver, Packet, Acc) ->
    MsgId = get_msg_id(Packet),
    Timestamp = mongoose_acc:timestamp(Acc),
    %% no unread for a user because he writes new messages which assumes he read all previous messages.
    Count = 0,
    InboxEntryKey = build_inbox_entry_key(Sender, Receiver),
    mod_inbox_backend:set_inbox(HostType, InboxEntryKey, Packet, Count, MsgId, Timestamp).

-spec write_to_receiver_inbox(HostType :: mongooseim:host_type(),
                              Sender :: jid:jid(),
                              Receiver :: jid:jid(),
                              Packet :: exml:element(),
                              Acc :: mongoose_acc:t()) -> ok | {ok, integer()}.
write_to_receiver_inbox(HostType, Sender, Receiver, Packet, Acc) ->
    MsgId = get_msg_id(Packet),
    Timestamp = mongoose_acc:timestamp(Acc),
    InboxEntryKey = build_inbox_entry_key(Receiver, Sender),
    mod_inbox_backend:set_inbox_incr_unread(HostType, InboxEntryKey,
                                            Packet, MsgId, Timestamp).

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
    gen_mod:get_module_opt(HostType, mod_inbox, reset_markers).

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

-spec get_option_write_aff_changes(HostType :: mongooseim:host_type()) -> boolean().
get_option_write_aff_changes(HostType) ->
    gen_mod:get_module_opt(HostType, mod_inbox, aff_changes).

-spec get_option_remove_on_kicked(HostType :: mongooseim:host_type()) -> boolean().
get_option_remove_on_kicked(HostType) ->
    gen_mod:get_module_opt(HostType, mod_inbox, remove_on_kicked).

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

-spec encode_rsm_id(integer(), binary()) -> binary().
encode_rsm_id(Int, BinJid) ->
    BinInt = integer_to_binary(Int),
    EncodedJid = base64:encode(BinJid),
    <<BinInt/binary, "/", EncodedJid/binary>>.

-spec decode_rsm_id(binary()) -> {integer(), binary()} | error.
decode_rsm_id(Bin) ->
    case binary:split(Bin, <<"/">>) of
        [BinInt, BinJid] ->
            Int = maybe_binary_to_positive_integer(BinInt),
            case Int of
                Int when is_integer(Int) ->
                    Jid = base64:decode(BinJid),
                    {Int, Jid};
                _ -> error
            end;
        _ -> error
    end.

-spec binary_to_bool(binary()) -> true | false | error.
binary_to_bool(<<"true">>) -> true;
binary_to_bool(<<"false">>) -> false;
binary_to_bool(_) -> error.

-spec bool_to_binary(integer() | boolean()) -> binary() | error.
bool_to_binary(1) -> <<"true">>;
bool_to_binary(0) -> <<"false">>;
bool_to_binary(true) -> <<"true">>;
bool_to_binary(false) -> <<"false">>;
bool_to_binary(_) -> error.

build_inbox_entry_key(FromJid, ToJid) ->
    {LUser, LServer} = jid:to_lus(FromJid),
    ToBareJid = jid:nameprep(jid:to_bare_binary(ToJid)),
    {LUser, LServer, ToBareJid}.

-spec build_inbox_result_elements(inbox_res(), integer()) -> [exml:element()].
build_inbox_result_elements(#{msg := Content, timestamp := Timestamp, unread_count := UnreadCount,
                              box := Box, muted_until := MutedUntil,
                              extra := Extra}, AccTS) ->
    [ #xmlel{name = <<"forwarded">>, attrs = [{<<"xmlns">>, ?NS_FORWARD}],
             children = [build_delay_el(Timestamp), Content]},
      kv_to_el(<<"read">>, mod_inbox_utils:bool_to_binary(0 =:= UnreadCount)),
      kv_to_el(<<"box">>, Box),
      kv_to_el(<<"archive">>, is_archive(Box)),
      kv_to_el(<<"mute">>, maybe_muted_until(MutedUntil, AccTS))
      | Extra ].

-spec build_entry_result_elements(entry_properties(), integer()) -> [exml:element()].
build_entry_result_elements(#{box := Box, muted_until := MutedUntil,
                              unread_count := UnreadCount, extra := Extra}, AccTS) ->
    [ kv_to_el(<<"read">>, mod_inbox_utils:bool_to_binary(0 =:= UnreadCount)),
      kv_to_el(<<"box">>, Box), kv_to_el(<<"archive">>, is_archive(Box)),
      kv_to_el(<<"mute">>, maybe_muted_until(MutedUntil, AccTS))
      | Extra ].

-spec kv_to_el(binary(), binary()) -> exml:element().
kv_to_el(Key, Value) ->
    #xmlel{name = Key, children = [#xmlcdata{content = Value}]}.

-spec is_archive(binary()) -> binary().
is_archive(<<"archive">>) -> <<"true">>;
is_archive(_) -> <<"false">>.

-spec maybe_muted_until(integer(), integer()) -> binary().
maybe_muted_until(0, _) -> <<"0">>;
maybe_muted_until(MutedUntil, CurrentTS) ->
    case CurrentTS =< MutedUntil of
        true -> list_to_binary(calendar:system_time_to_rfc3339(MutedUntil, [{offset, "Z"}, {unit, microsecond}]));
        false -> <<"0">>
    end.

-spec build_delay_el(Timestamp :: integer()) -> exml:element().
build_delay_el(Timestamp) ->
    TS = calendar:system_time_to_rfc3339(Timestamp, [{offset, "Z"}, {unit, microsecond}]),
    jlib:timestamp_to_xml(TS, undefined, undefined).

all_valid_boxes_for_query(HostType) ->
    [<<"all">> | gen_mod:get_module_opt(HostType, mod_inbox, boxes)].

-spec calculate_ts_from(integer(), non_neg_integer()) -> integer().
calculate_ts_from(Now, Days) ->
    DaysInMicroSeconds = 86400000000 * Days, % 8.64e+10 microseconds in a day
    Now - DaysInMicroSeconds.
