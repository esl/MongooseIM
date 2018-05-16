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
-include("mongoose.hrl").

-export([start/2, stop/1, deps/2]).
-export([process_iq/4,  user_send_packet/4, filter_packet/1]).
-export([clear_inbox/2]).
-define(NS_FORWARD, <<"urn:xmpp:forward:0">>).

-callback init(Host, Opts) -> ok when
  Host :: binary(),
  Opts :: list().

-callback get_inbox(LUser, LServer) -> any() when
  LUser :: binary(),
  LServer :: binary().

-callback set_inbox(User, Server, ToBareJid, ToResource, Content, Count, MsgId) -> any() when
  User :: binary(),
  Server :: binary(),
  ToBareJid :: binary(),
  ToResource :: binary(),
  Content :: binary(),
  Count :: binary(),
  MsgId :: binary().

-callback remove_inbox(User, Server, ToBareJid) -> any() when
  User :: binary(),
  Server :: binary(),
  ToBareJid :: binary().

-callback set_inbox_incr_unread(User, Server, ToBareJid, ToResource, Content, MsgId) -> any() when
  User :: binary(),
  Server :: binary(),
  ToBareJid :: binary(),
  ToResource :: binary(),
  Content :: binary(),
  MsgId :: binary().

-callback reset_unread(User, Server, BareJid, MsgId) -> any() when
  User :: binary(),
  Server :: binary(),
  BareJid :: binary(),
  MsgId :: binary().

-callback clear_inbox(User, Server) -> any() when
  User :: binary(),
  Server :: binary().

deps(_Host, Opts) ->
  groupchat_deps(Opts).

-spec start(Host :: jid:server(), Opts :: list()) -> ok.
start(Host, Opts) ->
    {ok, _} = gen_mod:start_backend_module(?MODULE, Opts, callback_funs()),
    mod_disco:register_feature(Host, ?NS_ESL_INBOX),
    IQDisc = gen_mod:get_opt(iqdisc, Opts, no_queue),
    ejabberd_hooks:add(user_send_packet, Host, ?MODULE, user_send_packet, 90),
    ejabberd_hooks:add(filter_local_packet, Host, ?MODULE, filter_packet, 90),
    gen_iq_handler:add_iq_handler(ejabberd_sm, Host, ?NS_ESL_INBOX, ?MODULE, process_iq, IQDisc).


-spec stop(Host :: jid:server()) -> ok.
stop(Host) ->
  mod_disco:unregister_feature(Host, ?NS_ESL_INBOX),
  gen_iq_handler:remove_iq_handler(ejabberd_sm, Host, ?NS_ESL_INBOX).

%%%%%%%%%%%%%%%%%%%
%% Process IQ
-spec process_iq(From :: jid:jid(),
                 To :: jid:jid(),
                 Acc :: mongoose_acc:t(),
                 IQ :: jlib:iq()) -> {stop, mongoose_acc:t()} | {mongoose_acc:t(), jlib:iq()}.
process_iq(_From, _To, Acc, #iq{type = set, sub_el = SubEl} = IQ) ->
  {Acc, IQ#iq{type = error, sub_el = [SubEl, mongoose_xmpp_errors:not_allowed()]}};
process_iq(From, To, Acc, #iq{type = get, sub_el = QueryEl} = IQ) ->
  Username = jid:to_binary(jid:to_bare(From)),
  Host = To#jid.lserver,
  List = mod_inbox_backend:get_inbox(Username, Host),
  QueryId = exml_query:attr(QueryEl, <<"queryid">>, <<>>),
  forward_messages(List, QueryId, To),
  BinCount = integer_to_list(length(List)),
  Res = IQ#iq{type = result, sub_el = [build_result_iq(BinCount)]},
  {Acc, Res}.

-spec forward_messages(list(inbox_res()), id(), jid:jid()) -> list(mongoose_acc:t()).
forward_messages(List, QueryId, To) when is_list(List) ->
  Msgs = [build_inbox_message(El, QueryId) || El <- List],
  [send_message(To, Msg) || Msg <- Msgs].

-spec send_message(jid:jid(), exml:element()) -> mongoose_acc:t().
send_message(To, Mess) ->
  BareTo = jid:to_bare(To),
  ejabberd_sm:route(BareTo, To, Mess).

%%%%%%%%%%%%%%%%%%%
%% Handlers
user_send_packet(Acc, From, To, #xmlel{name = <<"message">>} = Msg) ->
  Host = From#jid.server,
  maybe_process_message(Host, From, To, Msg, outgoing),
  Acc;
user_send_packet(Acc, From, To, Packet) ->
  Acc.

filter_packet(drop) ->
  drop;
filter_packet({From, To, Acc, Msg = #xmlel{name = <<"message">>}}) ->
  Host = To#jid.server,
  maybe_process_message(Host, From, To, Msg, incomming),
  {From, To, Acc, Msg};
filter_packet({From, To, Acc, Packet}) ->
  {From, To, Acc, Packet}.


maybe_process_message(Host, From, To, Msg, Dir) ->
  AcceptableMessage = should_be_stored_in_inbox(Msg),
  if AcceptableMessage ->
      Type = get_message_type(Msg),
      GroupchatsEnabled = gen_mod:get_module_opt(Host, ?MODULE, groupchat, [muclight]),
      MuclightEnabled = lists:member(muclight, GroupchatsEnabled),
      MuclightEnabled == true andalso
        process_message(Host, From, To, Msg, Dir, Type);
    true ->
      ok
  end.

process_message(Host, From, To, Message, outgoing, one2one) ->
  mod_inbox_one2one:handle_outgoing_message(Host, From, To, Message);
process_message(Host, From, To, Message, incomming, one2one) ->
  mod_inbox_one2one:handle_incomming_message(Host, From, To, Message);
process_message(_Host, _From, _To, _Message, outgoing, groupchat) ->
  %% For muclight we process only incoming messages
  ok;
process_message(Host, From, To, Message, incomming, groupchat) ->
  mod_inbox_muclight:handle_incoming_message(Host, From, To, Message);
process_message(_, _, _, Message, _, _) ->
  ?WARNING_MSG("unknown messasge not written in inbox='~p'", [Message]),
  ok.

get_message_type(Msg) ->
  case exml_query:attr(Msg, <<"type">>, undefined) of
    <<"groupchat">> ->
      groupchat;
    _ ->
      one2one
  end.

%%%%%%%%%%%%%%%%%%%
%% Stanza builders

-spec build_inbox_message(inbox_res(), id()) -> exml:element().
build_inbox_message({_Username, Content, Count}, QueryId) ->
  #xmlel{name = <<"message">>, attrs = [{<<"id">>, mod_inbox_utils:wrapper_id()}],
    children=[build_result_el(Content, QueryId, Count)]}.

-spec build_result_el(content(), id(), count()) -> exml:element().
build_result_el(Msg, QueryId, BinUnread) ->
  Forwarded = build_forward_el(Msg),
  QueryAttr = [{<<"queryid">>, QueryId} || QueryId =/= undefined, QueryId =/= <<>>],
  #xmlel{name = <<"result">>, attrs = [{<<"xmlns">>, ?NS_ESL_INBOX}, {<<"unread">>, BinUnread}] ++
  QueryAttr, children = [Forwarded]}.

-spec build_result_iq(count()) -> exml:element().
build_result_iq(CountBin) ->
  #xmlel{name = <<"count">>, attrs = [{<<"xmlns">>, ?NS_ESL_INBOX}],
    children = [#xmlcdata{content = CountBin}]}.

-spec build_forward_el(content()) -> exml:element().
build_forward_el(Content) ->
  {ok, Parsed} = exml:parse(Content),
  #xmlel{name = <<"forwarded">>,
    attrs = [{<<"xmlns">>, <<"urn:xmpp:forward:0">>}],
    children = [Parsed]}.

%%%%%%%%%%%%%%%%%%%
%% Helpers
%%

groupchat_deps(Opts) ->
  case lists:keyfind(groupchat, 1, Opts) of
    [] ->
      [];
    {groupchat, List} ->
      muclight_dep(List) ++ muc_dep(List);
    false ->
      []
  end.

muclight_dep(List) ->
  case lists:member(muclight, List) of
    true -> [{mod_muc_light, hard}];
    false -> []
  end.

muc_dep(List) ->
  case lists:member(muc, List) of
    true -> [{mod_muc, hard}];
    false -> []
  end.

callback_funs() ->
  [get_inbox, set_inbox, set_inbox_incr_unread,
    reset_unread, remove_inbox, clear_inbox].

clear_inbox(Username, Server) ->
  mod_inbox_utils:clear_inbox(Username, Server).

%%%%%%%%%%%%%%%%%%%
%% Message Predicates

should_be_stored_in_inbox(Msg) ->
  not is_forwarded_message(Msg) andalso
    not is_error_message(Msg) andalso
    not is_offline_message(Msg).

is_forwarded_message(Msg) ->
  case exml_query:subelement_with_ns(Msg, ?NS_FORWARD, undefined) of
    undefined ->
      false;
    _ ->
      true
  end.

is_error_message(Msg) ->
  case exml_query:attr(Msg, <<"type">>, undefined) of
    <<"error">> ->
      true;
    _ ->
      false
  end.

is_offline_message(Msg) ->
  case exml_query:subelement_with_ns(Msg, ?NS_DELAY, undefined) of
    undefined ->
      false;
    _ ->
      true
  end.