%%%-------------------------------------------------------------------
%%% @author ludwikbukowski
%%% @copyright (C) 2018, Erlang-Solutions
%%% @doc
%%%
%%% @end
%%% Created : 30. Jan 2018 13:22
%%%-------------------------------------------------------------------
-module(mod_inbox_utils).
-include("mongoose_ns.hrl").
-include("mod_inbox.hrl").
-include("jlib.hrl").

-author("ludwikbukowski").


%%%%%%%%%%%%%%%%%%%
%% DB Operations shared by mod_inbox_one2one and mod_inbox_muclight
-export([maybe_reset_unread_count/4,
         reset_unread_count_to_zero/2,
         maybe_write_to_inbox/6,
         write_to_sender_inbox/5,
         write_to_receiver_inbox/5,
         clear_inbox/1,
         clear_inbox/2,
         get_reset_markers/1,
         if_chat_marker_get_id/2,
         has_chat_marker/1,
         fill_from_attr/2,
         wrapper_id/0,
         get_option_write_aff_changes/1,
         get_option_remove_on_kicked/1,
         reset_marker_to_bin/1,
         extract_attr_jid/1,
         maybe_binary_to_positive_integer/1,
         maybe_muted_until/2,
         binary_to_bool/1,
         bool_to_binary/1
        ]).

-spec maybe_reset_unread_count(Server :: host(),
                               User :: jid:jid(),
                               Remote :: jid:jid(),
                               Packet :: exml:element()) -> ok.
maybe_reset_unread_count(Server, User, Remote, Packet) ->
    ResetMarkers = mod_inbox_utils:get_reset_markers(Server),
    case mod_inbox_utils:if_chat_marker_get_id(Packet, ResetMarkers) of
        undefined ->
            ok;
        Id ->
            reset_unread_count(User, Remote, Id)
    end.

-spec reset_unread_count_to_zero(jid:jid(), jid:jid()) -> ok.
reset_unread_count_to_zero(#jid{luser = FromUsername, lserver = Server}, Remote) ->
    ToBareJid = jid:to_binary(jid:to_bare(Remote)),
    ok = mod_inbox_backend:reset_unread(FromUsername, Server, ToBareJid, undefined).

-spec reset_unread_count(User :: jid:jid(),
                         Remote :: jid:jid(),
                         MsgId :: id()) -> ok.
reset_unread_count(User, Remote, MsgId) ->
    FromUsername = User#jid.luser,
    Server = User#jid.lserver,
    ToBareJid = jid:to_binary(jid:to_bare(Remote)),
    ok = mod_inbox_backend:reset_unread(FromUsername, Server, ToBareJid, MsgId).

-spec write_to_sender_inbox(Server :: host(),
                            Sender :: jid:jid(),
                            Receiver :: jid:jid(),
                            Packet :: exml:element(),
                            Timestamp :: integer()) -> ok.
write_to_sender_inbox(Server, Sender, Receiver, Packet, Timestamp) ->
    MsgId = get_msg_id(Packet),
    Content = exml:to_binary(Packet),
    Username = Sender#jid.luser,
    RemoteBareJid = jid:to_binary(jid:to_bare(Receiver)),
    %% no unread for a user because he writes new messages which assumes he read all previous messages.
    Count = 0,
    ok = mod_inbox_backend:set_inbox(Username, Server, RemoteBareJid,
                                     Content, Count, MsgId, Timestamp).

-spec write_to_receiver_inbox(Server :: host(),
                              Sender :: jid:jid(),
                              Receiver :: jid:jid(),
                              Packet :: exml:element(),
                              Timestamp :: integer()) -> ok | {ok, integer()}.
write_to_receiver_inbox(Server, Sender, Receiver, Packet, Timestamp) ->
    MsgId = get_msg_id(Packet),
    Content = exml:to_binary(Packet),
    Username = Receiver#jid.luser,
    RemoteBareJid = jid:to_binary(jid:to_bare(Sender)),
    mod_inbox_backend:set_inbox_incr_unread(Username, Server, RemoteBareJid,
                                            Content, MsgId, Timestamp).

-spec clear_inbox(User :: jid:luser(), Server :: host()) -> inbox_write_res().
clear_inbox(User, Server) when is_binary(User) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Server),
    ok = mod_inbox_backend:clear_inbox(LUser, LServer).

-spec clear_inbox(Server :: host()) -> inbox_write_res().
clear_inbox(Server) ->
    ok = mod_inbox_backend:clear_inbox(Server).


%%%%%%%%%%%%%%%%%%%
%% Helpers

-spec get_reset_markers(Host :: host()) -> list(marker()).
get_reset_markers(Host) ->
    gen_mod:get_module_opt(Host, mod_inbox, reset_markers, [<<"displayed">>]).

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
    has_chat_marker(Packet, all_chat_markers()).

-spec has_chat_marker(Packet :: exml:element(), list(marker())) -> boolean().
has_chat_marker(Packet, Markers) ->
    case exml_query:subelement_with_ns(Packet, ?NS_CHAT_MARKERS, no_marker) of
        no_marker ->
            false;
        #xmlel{name = Marker} ->
            lists:member(Marker, Markers)
    end.

-spec maybe_write_to_inbox(Host, User, Remote, Packet, TS, WriteF) -> ok | {ok, integer()} when
      Host :: host(),
      User :: jid:jid(),
      Remote :: jid:jid(),
      Packet :: exml:element(),
      TS :: integer(),
      %% WriteF is write_to_receiver_inbox/5 or write_to_sender_inbox/5
      WriteF :: fun().
maybe_write_to_inbox(Host, User, Remote, Packet, TS, WriteF) ->
    case mod_inbox_utils:has_chat_marker(Packet) of
        true ->
            ok;
        false ->
            Packet2 = mod_inbox_utils:fill_from_attr(Packet, User),
            WriteF(Host, User, Remote, Packet2, TS)
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

-spec get_option_write_aff_changes(Host :: host()) -> boolean().
get_option_write_aff_changes(Host) ->
    gen_mod:get_module_opt(Host, mod_inbox, aff_changes, true).

-spec get_option_remove_on_kicked(Host :: host()) -> boolean().
get_option_remove_on_kicked(Host) ->
    gen_mod:get_module_opt(Host, mod_inbox, remove_on_kicked, true).

-spec reset_marker_to_bin(atom()) -> marker().
reset_marker_to_bin(displayed) -> <<"displayed">>;
reset_marker_to_bin(acknowledged) -> <<"acknowledged">>;
reset_marker_to_bin(received) -> <<"received">>;
reset_marker_to_bin(Unknown) -> throw({unknown_marker, Unknown}).

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

all_chat_markers() ->
    [<<"received">>, <<"displayed">>, <<"acknowledged">>].

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
