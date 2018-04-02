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
%% DB Operations shared by mod_inbox and mod_inbox_muclight
-spec reset_unread_count(From  :: jid:jid(),
                         To    :: jid:jid(),
                         MsgId :: id())   -> ok.
reset_unread_count(From, To, MsgId) ->
  FromJid = jid:to_binary(jid:to_bare(From)),
  Server = From#jid.lserver,
  ToBareJid = jid:to_binary(jid:to_bare(To)),
  mod_inbox_backend:reset_unread(FromJid, Server, ToBareJid, MsgId).

-spec write_to_inbox(LocJID :: jid:jid(),
                     RemJID :: jid:jid(),
                     Packet :: exml:packet()) -> ok.
write_to_inbox(LocJID, RemJID, Packet) ->
  Server = LocJID#jid.lserver,
  MsgId = mod_inbox_utils:get_msg_id(Packet),
  BareLocJID = jid:to_bare(LocJID),
  write_to_sender_inbox(Server, LocJID, RemJID, BareLocJID, MsgId, Packet),
  write_to_receiver_inbox(Server, LocJID, RemJID, BareLocJID, MsgId, Packet).

-spec write_to_sender_inbox(Server :: host(),
                            From   :: jid:jid(),
                            To   :: jid:jid(),
                            Sender   :: jid:jid(),
                            MsgId   :: id(),
                            Packet   :: exml:packet()) -> ok.
write_to_sender_inbox(Server, From, To, Sender, MsgId, Packet) ->
  Content = exml:to_binary(Packet),
  FromJid = jid:to_binary(jid:to_bare(From)),
  ToBareJid = jid:to_binary(jid:to_bare(To)),
  SenderBin = jid:to_binary(Sender),
  %% no unread for a user because he writes new messages which assumes he read all previous messages.
  Count = integer_to_binary(0),
  mod_inbox_backend:set_inbox(FromJid, Server, ToBareJid, SenderBin, Content, Count, MsgId).


-spec write_to_receiver_inbox(Server :: host(),
                              From   :: jid:jid(),
                              To   :: jid:jid(),
                              Sender   :: jid:jid(),
                              MsgId   :: id(),
                              Packet   :: exml:packet()) -> ok.
write_to_receiver_inbox(Server, From, To, Sender, MsgId, Packet) ->
  Content = exml:to_binary(Packet),
  FromJid = jid:to_binary(jid:to_bare(To)),
  ToBareJid = jid:to_binary(jid:to_bare(From)),
  SenderBin = jid:to_binary(Sender),
  mod_inbox_backend:set_inbox_incr_unread(FromJid, Server, ToBareJid, SenderBin, Content, MsgId).

-spec clear_inbox(binary(), host()) -> ok.
clear_inbox(Username, Server) ->
  mod_inbox_backend:clear_inbox(Username, Server).


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


has_chat_marker(_Packet, []) -> false;
has_chat_marker(Packet, [Marker | R]) ->
  case exml_query:subelement_with_ns(Packet, ?NS_CHAT_MARKERS) of
    #xmlel{name = Marker}    -> true;
    _                        -> has_chat_marker(Packet, R)
  end.

get_markered_msg_id(#xmlel{name = <<"message">>} = Msg) ->
  case exml_query:paths(Msg, [{element, <<"displayed">>}, {attr, <<"id">>}]) of
    [Id] ->
      Id;
    _ ->
      no_id
  end.

get_msg_id(#xmlel{name = <<"message">>} = Msg) ->
  exml_query:attr(Msg, <<"id">>, <<"noid">>).


add_from(Msg = #xmlel{attrs = Attrs}, FromBin) ->
  case exml_query:attr(Msg, <<"from">>, undefined) of
    undefined ->
      Msg#xmlel{attrs = Attrs ++ [{<<"from">>, FromBin}]};
    _ ->
      Msg
  end.

wrapper_id() ->
  uuid:uuid_to_string(uuid:get_v4(), binary_standard).

check_write_aff_changes(Host) ->
  gen_mod:get_module_opt(Host, mod_inbox, aff_changes, true).

check_remove_on_kicked(Host) ->
  gen_mod:get_module_opt(Host, mod_inbox, remove_on_kicked, true).
