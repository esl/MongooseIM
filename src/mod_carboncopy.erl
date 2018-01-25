%%%----------------------------------------------------------------------
%%% File    : mod_carboncopy.erl
%%% Author  : Eric Cestari <ecestari@process-one.net>
%%% Purpose : Message Carbons XEP-0280 0.8
%%% Created : 5 May 2008 by Mickael Remond <mremond@process-one.net>
%%% Usage   : Add the following line in modules section of ejabberd.cfg:
%%%              {mod_carboncopy, []}
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2014   ProcessOne
%%%
%%% This program is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation; either version 2 of the
%%% License, or (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License along
%%% with this program; if not, write to the Free Software Foundation, Inc.,
%%% 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
%%%
%%%----------------------------------------------------------------------
-module (mod_carboncopy).
-author ('ecestari@process-one.net').
-xep([{xep, 280}, {version, "0.10"}]).
-behavior(gen_mod).

%% API
-export([start/2,
         stop/1,
         is_carbon_copy/1,
         classify_packet/1]).

%% Hooks
-export([user_send_packet/4,
         user_receive_packet/5,
         iq_handler2/4,
         iq_handler1/4,
         remove_connection/5
        ]).

%% For testing and debugging
-export([enable/4, disable/3]).

-define(NS_CC_2, <<"urn:xmpp:carbons:2">>).
-define(NS_CC_1, <<"urn:xmpp:carbons:1">>).
-define(NS_FORWARD, <<"urn:xmpp:forward:0">>).
-define(CC_KEY, 'cc').
-define(CC_DISABLED, undefined).

-include("mongoose.hrl").
-include("jlib.hrl").
-include_lib("session.hrl").


-type classification() :: 'ignore' | 'forward'.

is_carbon_copy(Packet) ->
    case xml:get_subtag(Packet, <<"sent">>) of
        #xmlel{name = <<"sent">>, attrs = AAttrs}  ->
            case xml:get_attr_s(<<"xmlns">>, AAttrs) of
                ?NS_CC_2 -> true;
                ?NS_CC_1 -> true;
                _ -> false
            end;
        _ -> false
    end.

start(Host, Opts) ->
    %% execute disable/enable actions in the c2s process itself
    IQDisc = gen_mod:get_opt(iqdisc, Opts, no_queue),
    mod_disco:register_feature(Host, ?NS_CC_1),
    mod_disco:register_feature(Host, ?NS_CC_2),
    ejabberd_hooks:add(unset_presence_hook, Host, ?MODULE, remove_connection, 10),
    ejabberd_hooks:add(user_send_packet, Host, ?MODULE, user_send_packet, 89),
    ejabberd_hooks:add(user_receive_packet, Host, ?MODULE, user_receive_packet, 89),
    gen_iq_handler:add_iq_handler(ejabberd_sm, Host, ?NS_CC_2, ?MODULE, iq_handler2, IQDisc),
    gen_iq_handler:add_iq_handler(ejabberd_sm, Host, ?NS_CC_1, ?MODULE, iq_handler1, IQDisc).

stop(Host) ->
    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host, ?NS_CC_1),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host, ?NS_CC_2),
    mod_disco:unregister_feature(Host, ?NS_CC_2),
    mod_disco:unregister_feature(Host, ?NS_CC_1),
    ejabberd_hooks:delete(user_send_packet, Host, ?MODULE, user_send_packet, 89),
    ejabberd_hooks:delete(user_receive_packet, Host, ?MODULE, user_receive_packet, 89),
    ejabberd_hooks:delete(unset_presence_hook, Host, ?MODULE, remove_connection, 10).

iq_handler2(From, To, Acc, IQ) ->
    iq_handler(From, To, Acc, IQ, ?NS_CC_2).
iq_handler1(From, To, Acc, IQ) ->
    iq_handler(From, To, Acc, IQ, ?NS_CC_1).

iq_handler(From, _To,  Acc, #iq{type = set, sub_el = #xmlel{name = Operation,
                                                       children = []}} = IQ, CC) ->
    ?DEBUG("carbons IQ received: ~p", [IQ]),
    {U, S, R} = jid:to_lower(From),
    Result = case Operation of
                 <<"enable">> ->
                     ?INFO_MSG("carbons enabled for user ~s@~s/~s", [U, S, R]),
                     enable(S, U, R, CC);
                 <<"disable">> ->
                     ?INFO_MSG("carbons disabled for user ~s@~s/~s", [U, S, R]),
                     disable(S, U, R)
             end,
    case Result of
        ok ->
            ?DEBUG("carbons IQ result: ok", []),
            {Acc, IQ#iq{type=result, sub_el=[]}};
        {error, _Error} ->
            ?WARNING_MSG("Error enabling / disabling carbons: ~p", [Result]),
            {Acc, IQ#iq{type=error, sub_el = [mongoose_xmpp_errors:bad_request()]}}
    end;

iq_handler(_From, _To, Acc, IQ, _CC) ->
    {Acc, IQ#iq{type=error, sub_el = [mongoose_xmpp_errors:not_allowed()]}}.

user_send_packet(Acc, From, To, Packet) ->
    check_and_forward(From, To, Packet, sent),
    Acc.

user_receive_packet(Acc, JID, _From, To, Packet) ->
    check_and_forward(JID, To, Packet, received),
    Acc.

% Check if the traffic is local.
% Modified from original version:
% - registered to the user_send_packet hook, to be called only once even for multicast
% - do not support "private" message mode, and do not modify the original packet in any way
% - we also replicate "read" notifications
check_and_forward(JID, To, #xmlel{name = <<"message">>} = Packet, Direction) ->
    case classify_packet(Packet) of
        ignore -> stop;
        forward  -> send_copies(JID, To, Packet, Direction)
    end;

check_and_forward(_JID, _To, _Packet, _) -> ok.

-spec classify_packet(_) -> classification().
classify_packet(Packet) ->
    is_chat(Packet).

-spec is_chat(_) -> classification().
is_chat(#xmlel{name = <<"message">>, attrs = Attrs} = Packet) ->
    case xml:get_attr_s(<<"type">>, Attrs) of
        <<"chat">> -> is_private(Packet);
        _ -> ignore
    end.

-spec is_private(_) -> classification().
is_private(Packet) ->
    case xml:get_subtag(Packet, <<"private">>) of
        false -> is_no_copy(Packet);
        _ -> ignore
    end.

-spec is_no_copy(_) -> classification().
is_no_copy(Packet) ->
    case xml:get_subtag(Packet, <<"no-copy">>) of
        false -> is_received(Packet);
        _ -> ignore
    end.

-spec is_received(_) -> classification().
is_received(Packet) ->
    case xml:get_subtag(Packet, <<"received">>) of
        false -> is_sent(Packet);
        _ -> ignore
    end.

-spec is_sent(_) -> classification().
is_sent(Packet) ->
    case xml:get_subtag(Packet, <<"sent">>) of
        false -> forward;
        SubTag -> is_forwarded(SubTag)
    end.

-spec is_forwarded(_) -> classification().
is_forwarded(SubTag) ->
    case xml:get_subtag(SubTag, <<"forwarded">>) of
        false -> forward;
        _ -> ignore
    end.

remove_connection(Acc, User, Server, Resource, _Status) ->
    disable(Server, User, Resource),
    Acc.


%%
%% Internal
%%
is_bare_to(Direction, To, _PrioRes) ->
    case {Direction, To} of
        {received, #jid{lresource = <<>>}} -> true;
        _ -> false
    end.

max_prio(PrioRes) ->
    case catch lists:max(PrioRes) of
        {Prio, _Res} -> Prio;
        _ -> 0
    end.

is_max_prio(Res, PrioRes) ->
    lists:member({max_prio(PrioRes), Res}, PrioRes).

jids_minus_max_priority_resource(U, S, _R, CCResList, PrioRes) ->
    [ {jid:make({U, S, CCRes}), CCVersion}
      || {CCVersion, CCRes} <- CCResList, not is_max_prio(CCRes, PrioRes) ].

jids_minus_specific_resource(U, S, R, CCResList, _PrioRes) ->
    [ {jid:make({U, S, CCRes}), CCVersion}
      || {CCVersion, CCRes} <- CCResList, CCRes =/= R ].

%% If the original user is the only resource in the list of targets
%% that means that he/she must have already received the message via
%% normal routing:
drop_singleton_jid(JID, [{JID, _CCVER}]) -> [];
drop_singleton_jid(_JID, Targets)        -> Targets.

%% Direction = received | sent <received xmlns='urn:xmpp:carbons:1'/>
send_copies(JID, To, Packet, Direction) ->
    {U, S, R} = jid:to_lower(JID),
    {PrioRes, CCResList} = get_cc_enabled_resources(U, S),
    Targets = case is_bare_to(Direction, To, PrioRes) of
                  true -> jids_minus_max_priority_resource
                            (U, S, R, CCResList, PrioRes);
                  _    -> jids_minus_specific_resource(U, S, R, CCResList, PrioRes)
              end,
    ?DEBUG("targets ~p from resources ~p and ccenabled ~p",
           [Targets, PrioRes, CCResList]),
    lists:map(fun({Dest, Version}) ->
                      {_, _, Resource} = jid:to_lower(Dest),
                      ?DEBUG("forwarding to ~ts", [Resource]),
                      Sender = jid:make({U, S, <<>>}),
                      New = build_forward_packet
                              (JID, Packet, Sender, Dest, Direction, Version),
                      ejabberd_router:route(Sender, Dest, New)
              end, drop_singleton_jid(JID, Targets)),
    ok.

build_forward_packet(JID, Packet, Sender, Dest, Direction, Version) ->
    #xmlel{name = <<"message">>,
           attrs = [{<<"xmlns">>, <<"jabber:client">>},
                    {<<"type">>, <<"chat">>},
                    {<<"from">>, jid:to_binary(Sender)},
                    {<<"to">>, jid:to_binary(Dest)}],
           children = carbon_copy_children(Version, JID, Packet, Direction)}.

carbon_copy_children(?NS_CC_1, JID, Packet, Direction) ->
    [ #xmlel{name = list_to_binary(atom_to_list(Direction)),
             attrs = [{<<"xmlns">>, ?NS_CC_1}]},
      #xmlel{name = <<"forwarded">>,
             attrs = [{<<"xmlns">>, ?NS_FORWARD}],
             children = [complete_packet(JID, Packet, Direction)]} ];
carbon_copy_children(?NS_CC_2, JID, Packet, Direction) ->
    [ #xmlel{name = list_to_binary(atom_to_list(Direction)),
             attrs = [{<<"xmlns">>, ?NS_CC_2}],
             children = [ #xmlel{name = <<"forwarded">>,
                                 attrs = [{<<"xmlns">>, ?NS_FORWARD}],
                                 children = [complete_packet(JID, Packet, Direction)]} ]} ].

enable(Host, U, R, CC) ->
    ?DEBUG("enabling for ~p/~p", [U, R]),
    KV = {?CC_KEY, cc_ver_to_int(CC)},
    {ok, KV} = ejabberd_sm:store_info(U, Host, R, KV),
    ok.

disable(Host, U, R) ->
    ?DEBUG("disabling for ~ts@~ts/~ts", [U, Host, R]),
    KV = {?CC_KEY, ?CC_DISABLED},
    case ejabberd_sm:store_info(U, Host, R, KV) of
        {error, offline} -> ok;
        {ok, KV} -> ok;
        Err -> {error, Err}
    end.

complete_packet(From, #xmlel{name = <<"message">>, attrs = OrigAttrs} = Packet, sent) ->
    %% if this is a packet sent by user on this host, then Packet doesn't
    %% include the 'from' attribute. We must add it.
    Attrs = lists:keystore(<<"xmlns">>, 1, OrigAttrs, {<<"xmlns">>, <<"jabber:client">>}),
    case proplists:get_value(<<"from">>, Attrs) of
        undefined ->
            Packet#xmlel{attrs = [{<<"from">>, jid:to_binary(From)}|Attrs]};
        _ ->
            Packet#xmlel{attrs = Attrs}
    end;

complete_packet(_From, #xmlel{name = <<"message">>, attrs=OrigAttrs} = Packet, received) ->
    Attrs = lists:keystore(<<"xmlns">>, 1, OrigAttrs, {<<"xmlns">>, <<"jabber:client">>}),
    Packet#xmlel{attrs = Attrs}.

get_cc_enabled_resources(User, Server)->
    AllSessions = ejabberd_sm:get_raw_sessions(User, Server),
    CCs = cat_maybes([maybe_cc_resource(S) || S <- AllSessions]),
    Prios = cat_maybes([maybe_prio_resource(S) || S <- AllSessions]),
    {Prios, CCs}.

maybe_cc_resource(#session{usr = {_, _, R}, info = I}) ->
    case lists:keyfind(?CC_KEY, 1, I) of
        {?CC_KEY, V} when is_integer(V) andalso V =/= ?CC_DISABLED ->
            {{cc_ver_from_int(V), R}};
        _ ->
            {}
    end.

maybe_prio_resource(#session{usr = {_, _, R}, priority = P})
  when is_integer(P) -> {{P, R}};
maybe_prio_resource(_) -> {}.

cc_ver_to_int(?NS_CC_1) -> 1;
cc_ver_to_int(?NS_CC_2) -> 2.

cc_ver_from_int(1) -> ?NS_CC_1;
cc_ver_from_int(2) -> ?NS_CC_2.

-spec cat_maybes([({A}|{})]) -> [A].
cat_maybes(L) -> [ Elem || {Elem} <- L ].
