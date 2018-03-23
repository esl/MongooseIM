%%%-------------------------------------------------------------------
%%% @author ludwikbukowski
%%% @copyright (C) 2018, Erlang-Solutions
%%% @doc
%%%
%%% @end
%%% Created : 30. Jan 2018 13:22
%%%-------------------------------------------------------------------
-module(mod_inbox).
-author("ludwikbukowski").
-include("mod_inbox.hrl").
-include("jlib.hrl").
-include("mongoose_ns.hrl").

-export([start/2, stop/1]).
-export([process_iq/4, process_message_muclight/9, process_message_one_to_one/9]).
-export([write_to_inbox/3, clear_inbox/2]).


start(Host, Opts) ->
  {ok, _} = gen_mod:start_backend_module(?MODULE, Opts,
    [get_inbox, set_inbox, set_inbox_incr_unread, reset_unread, remove_inbox, clear_inbox]),
  mod_disco:register_feature(Host, ?NS_ESL_INBOX),
  IQDisc = gen_mod:get_opt(iqdisc, Opts, one_queue),
  Mode = gen_mod:get_opt(groupchat, Opts, [muclight]),
  register_handler(Host, Mode),
  gen_iq_handler:add_iq_handler(ejabberd_sm, Host, ?NS_ESL_INBOX, ?MODULE, process_iq, IQDisc).

stop(Host) ->
  mod_disco:unregister_feature(Host, ?NS_ESL_INBOX),
  Mode = gen_mod:get_module_opt(Host, mod_inbox, mode, [muclight]),
  unregister_handler(Host, Mode),
  gen_iq_handler:remove_iq_handler(ejabberd_sm, Host, ?NS_ESL_INBOX).

%%%%%%%%%%%%%%%%%%%
%% Process IQ

process_iq(_From, _To, Acc, #iq{type = set, sub_el = SubEl} = IQ) ->
  {Acc, IQ#iq{type = error, sub_el = [SubEl,mongoose_xmpp_errors:not_allowed()]}};
process_iq(From, To, Acc, #iq{type = get, sub_el = QueryEl} = IQ) ->
  Username = jid:to_binary(jid:to_bare(From)),
  Host = To#jid.lserver,
  List = mod_inbox_backend:get_inbox(Username, Host),
  QueryId = exml_query:attr(QueryEl, <<"queryid">>, <<>>),
  forward_messages(List, QueryId, To),
  BinCount = integer_to_list(length(List)),
  Res = IQ#iq{type = result, sub_el = [build_result_iq(BinCount)]},
  {Acc, Res}.

forward_messages(List, QueryId, To) when is_list(List) ->
  Msgs = [build_inbox_message(El, QueryId) || El <- List],
  [send_message(To, Msg) || Msg <- Msgs].

send_message(To, Mess) ->
  BareTo = jid:to_bare(To),
  ejabberd_sm:route(BareTo, To, Mess).

%%%%%%%%%%%%%%%%%%%
%% Handlers

process_message_one_to_one(Result, Host, _MamID, _UserID, LocJID, RemJID, _SrcJID, outgoing, Packet) ->
  maybe_handle_chat_marker(Host, LocJID, RemJID, Packet),
  Result;
process_message_one_to_one(Result, _Host, _MamID, _UserID, _LocJID, _RemJID, _SrcJID, incomming, _Packet) ->
  Result.

process_message_muclight(Result, Host, _MamID, _UserID, LocJID, RemJID, _SrcJID, outgoing, Packet) ->
  maybe_handle_chat_marker(Host, LocJID, RemJID, Packet),
  Result;
process_message_muclight(Result, Host, _MamID, _UserID, LocJID, RemJID, _SrcJID, incoming, Packet) ->
  case exml_query:attr(Packet, <<"type">>, undefined) of
    <<"groupchat">> ->
      mod_inbox_muclight:maybe_handle_chat_marker(Host, LocJID, RemJID, Packet);
    _ ->
      ok
  end,
  Result;
process_message_muclight(Result, _Host, _MamID, _UserID, _LocJID, _RemJID, _SrcJID, _, _Packet) ->
  Result.

maybe_handle_chat_marker(Host, User, Remote, Packet) ->
  Markers = mod_inbox_utils:get_reset_markers(Host),
  case mod_inbox_utils:has_chat_marker(Packet, Markers) of
    true ->
      maybe_reset_unread_count(User, Remote, Packet);
    false ->
      FromBin = jid:to_binary(User),
      Packet2 = mod_inbox_utils:add_from(Packet, FromBin),
      write_to_inbox(User, Remote, Packet2)
  end.


maybe_reset_unread_count(User, Remote, Packet) ->
  Id = mod_inbox_utils:get_markered_msg_id(Packet),
  mod_inbox_operations:reset_unread_count(User, Remote, Id).


write_to_inbox(LocJID, RemJID, Packet) ->
  Server = LocJID#jid.lserver,
  MsgId = mod_inbox_utils:get_msg_id(Packet),
  BareLocJID = jid:to_bare(LocJID),
  mod_inbox_operations:write_to_sender_inbox(Server, LocJID, RemJID, BareLocJID, MsgId, Packet),
  mod_inbox_operations:write_to_receiver_inbox(Server, LocJID, RemJID, BareLocJID, MsgId, Packet).

clear_inbox(Username, Server) ->
  mod_inbox_operations:clear_inbox(Username, Server).

%%%%%%%%%%%%%%%%%%%
%% Builders

build_inbox_message({_Username, _Sender, Content, Count}, QueryId) ->
  #xmlel{name = <<"message">>, attrs = [{<<"id">>, mod_inbox_utils:wrapper_id()}],
    children=[build_result_el(Content, QueryId, Count)]}.

build_result_el(Msg, QueryId, BinUnread) ->
  Forwarded = build_forward_el(Msg),
  QueryAttr = [{<<"queryid">>, QueryId} || QueryId =/= undefined, QueryId =/= <<>>],
  #xmlel{name = <<"result">>, attrs = [{<<"xmlns">>, ?NS_ESL_INBOX}, {<<"unread">>, BinUnread}] ++
  QueryAttr, children = [Forwarded]}.

build_result_iq(CountBin) ->
  #xmlel{name = <<"count">>, attrs = [{<<"xmlns">>, ?NS_ESL_INBOX}],
    children = [#xmlcdata{content = CountBin}]}.


build_forward_el(Content) ->
  {ok, Parsed} = exml:parse(Content),
  #xmlel{name = <<"forwarded">>,
    attrs = [{<<"xmlns">>, <<"urn:xmpp:forward:0">>}],
    children = [Parsed]}.

%%%%%%%%%%%%%%%%%%%
%% Helpers
register_for_muclight(Mode) ->
  lists:member(muclight, Mode).

register_for_muc(Mode) ->
  lists:member(muc, Mode).

register_handler(Host, Mode) ->
  {M, H} = handler(Mode),
  ejabberd_hooks:add(mam_archive_message, Host, M, H, 90).

unregister_handler(Host, Mode) ->
  {M, H} = handler(Mode),
  ejabberd_hooks:delete(mam_archive_message, Host, M, H, 90).

handler(Mode) ->
  Muclight = register_for_muclight(Mode),
  Muc = register_for_muc(Mode),
  %% TODO implement inbox for MUC
  case {Muclight, Muc} of
    {true, false} ->
      {?MODULE, process_message_muclight};
    {false, false} ->
      {?MODULE, process_message_one_to_one};
    _ ->
      erlang:throw({not_implemented})
  end.
