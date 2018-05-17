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
-compile(export_all).


%%%%%%%%%%%%%%%%%%%
%% DB Operations shared by mod_inbox_one2one and mod_inbox_muclight

-spec reset_unread_count(From :: jid:jid(),
    To :: jid:jid(),
    MsgId :: id()) -> ok.
reset_unread_count(User, Remote, MsgId) ->
    FromUsername = User#jid.luser,
    Server = User#jid.lserver,
    ToBareJid = jid:to_binary(jid:to_bare(Remote)),
    ok = mod_inbox_backend:reset_unread(FromUsername, Server, ToBareJid, MsgId).

write_to_sender_inbox(Server, Sender, Receiver, Packet) ->
    MsgId = get_msg_id(Packet),
    Content = exml:to_binary(Packet),
    Username = Sender#jid.luser,
    RemoteBareJid = jid:to_binary(jid:to_bare(Receiver)),
    %% no unread for a user because he writes new messages which assumes he read all previous messages.
    Count = integer_to_binary(0),
    ok = mod_inbox_backend:set_inbox(Username, Server, RemoteBareJid, Content, Count, MsgId).


write_to_receiver_inbox(Server, Sender, Receiver, Packet) ->
    MsgId = get_msg_id(Packet),
    Content = exml:to_binary(Packet),
    Username = Receiver#jid.luser,
    RemoteBareJid = jid:to_binary(jid:to_bare(Sender)),
    ok = mod_inbox_backend:set_inbox_incr_unread(Username, Server, RemoteBareJid, Content, MsgId).


clear_inbox(User, Server) when is_binary(User) ->
    JidForm = jid:from_binary(User),
    ok = mod_inbox_backend:clear_inbox(JidForm#jid.luser, Server).


%%%%%%%%%%%%%%%%%%%
%% Helpers

get_reset_markers(Host) ->
    Markers = gen_mod:get_module_opt(Host, mod_inbox, markers, [displayed]),
    MarkersBin = [atom_to_binary(M, unicode) || M <- Markers],
    Possible = [<<"acknowledged">>, <<"displayed">>, <<"received">>],
    Vals = lists:all(fun(Marker) -> lists:member(Marker, Possible) end, MarkersBin),
    if
        Vals -> MarkersBin;
        true -> erlang:throw(unknown_markers, MarkersBin)
    end.

if_chat_marker_get_id(Packet, Markers) when is_list(Markers) ->
    Ids = [if_chat_marker_get_id(Packet, M) || M <- Markers],
    Filtered = [El || El <- Ids, El /= undefined],
    case Filtered of
        [] ->
            undefined;
        _ ->
            lists:nth(1, Filtered)
    end;
if_chat_marker_get_id(Packet, Marker) ->
    case exml_query:paths(Packet, [{element, Marker}, {attr, <<"id">>}]) of
        [Id] ->
            Id;
        _ ->
            undefined
    end.

has_chat_marker(_Packet, []) -> false;
has_chat_marker(Packet, [Marker | R]) ->
    case exml_query:subelement_with_ns(Packet, ?NS_CHAT_MARKERS) of
        #xmlel{name = Marker} -> true;
        _ -> has_chat_marker(Packet, R)
    end.

get_msg_id(#xmlel{name = <<"message">>} = Msg) ->
    exml_query:attr(Msg, <<"id">>, <<>>).


fill_from_attr(Msg = #xmlel{attrs = Attrs}, FromBin) ->
    case exml_query:attr(Msg, <<"from">>, undefined) of
        undefined ->
            Msg#xmlel{attrs = [{<<"from">>, FromBin} | Attrs]};
        _ ->
            Msg
    end.

wrapper_id() ->
    uuid:uuid_to_string(uuid:get_v4(), binary_standard).

get_option_write_aff_changes(Host) ->
    gen_mod:get_module_opt(Host, mod_inbox, aff_changes, true).

get_option_remove_on_kicked(Host) ->
    gen_mod:get_module_opt(Host, mod_inbox, remove_on_kicked, true).
