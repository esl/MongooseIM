%%%----------------------------------------------------------------------
%%% File    : mod_carboncopy.erl
%%% Author  : Eric Cestari <ecestari@process-one.net>
%%% Purpose : Message Carbons XEP-0280 0.8
%%% Created : 5 May 2008 by Mickael Remond <mremond@process-one.net>
%%% Usage   : Add the following line in modules section of ejabberd.yml:
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

-behavior(gen_mod).

%% API:
-export([start/2,
         stop/1]).

%% Hooks:
-export([user_send_packet/3,
	 user_receive_packet/4,
         iq_handler2/3,
         iq_handler1/3,
         remove_connection/4,
         is_carbon_copy/1]).

-define(NS_CC_2, <<"urn:xmpp:carbons:2">>).
-define(NS_CC_1, <<"urn:xmpp:carbons:1">>).
-define(NS_FORWARD, <<"urn:xmpp:forward:0">>).


-include("ejabberd.hrl").
%%-include("logger.hrl").
-include("jlib.hrl").
-define(PROCNAME, ?MODULE).
-define(TABLE, carboncopy).

-type classification() :: 'ignore' | 'forward'.

-type matchspec_atom() :: '_' | '$1' | '$2' | '$3'.
-record(carboncopy,{us :: {binary(), binary()} | matchspec_atom(), 
		    resource :: binary() | matchspec_atom(),
		    version :: binary() | matchspec_atom()}).

is_carbon_copy(Packet) ->
	case xml:get_subtag(Packet, <<"sent">>) of
		#xmlel{name= <<"sent">>, attrs = AAttrs}  ->
	    	case xml:get_attr_s(<<"xmlns">>, AAttrs) of
				?NS_CC_2 -> true;
				?NS_CC_1 -> true;
				_ -> false
			end;
		_ -> false
	end.

start(Host, Opts) ->
    IQDisc = gen_mod:get_opt(iqdisc, Opts, one_queue),
    mod_disco:register_feature(Host, ?NS_CC_1),
    mod_disco:register_feature(Host, ?NS_CC_2),
    Fields = record_info(fields, ?TABLE),
    try mnesia:table_info(?TABLE, attributes) of
	Fields -> ok;
	_ -> mnesia:delete_table(?TABLE)  %% recreate..
    catch _:_Error -> ok  %%probably table don't exist
    end,
    mnesia:create_table(?TABLE,
	[{ram_copies, [node()]}, 
	 {attributes, record_info(fields, ?TABLE)}, 
	 {type, bag}]),
    mnesia:add_table_copy(?TABLE, node(), ram_copies),
    ejabberd_hooks:add(unset_presence_hook,Host, ?MODULE, remove_connection, 10),
    %% why priority 89: to define clearly that we must run BEFORE mod_logdb hook (90)
    ejabberd_hooks:add(user_send_packet,Host, ?MODULE, user_send_packet, 89),
    ejabberd_hooks:add(user_receive_packet,Host, ?MODULE, user_receive_packet, 89),
    gen_iq_handler:add_iq_handler(ejabberd_sm, Host, ?NS_CC_2, ?MODULE, iq_handler2, IQDisc),
    gen_iq_handler:add_iq_handler(ejabberd_sm, Host, ?NS_CC_1, ?MODULE, iq_handler1, IQDisc).

stop(Host) ->
    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host, ?NS_CC_1),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host, ?NS_CC_2),
    mod_disco:unregister_feature(Host, ?NS_CC_2),
    mod_disco:unregister_feature(Host, ?NS_CC_1),
    %% why priority 89: to define clearly that we must run BEFORE mod_logdb hook (90)
    ejabberd_hooks:delete(user_send_packet,Host, ?MODULE, user_send_packet, 89),
    ejabberd_hooks:delete(user_receive_packet,Host, ?MODULE, user_receive_packet, 89),
    ejabberd_hooks:delete(unset_presence_hook,Host, ?MODULE, remove_connection, 10).

iq_handler2(From, To, IQ) ->
	iq_handler(From, To, IQ, ?NS_CC_2).
iq_handler1(From, To, IQ) ->
	iq_handler(From, To, IQ, ?NS_CC_1).

iq_handler(From, _To,  #iq{type=set, sub_el = #xmlel{name = Operation, children = []}} = IQ, CC)->
    ?DEBUG("carbons IQ received: ~p", [IQ]),
    {U, S, R} = jlib:jid_tolower(From),
    Result = case Operation of
        <<"enable">>->
	    ?INFO_MSG("carbons enabled for user ~s@~s/~s", [U,S,R]),
            enable(S,U,R,CC);
        <<"disable">>->
	    ?INFO_MSG("carbons disabled for user ~s@~s/~s", [U,S,R]),
            disable(S, U, R)
    end,
    case Result of 
        ok ->
	    ?DEBUG("carbons IQ result: ok", []),
            IQ#iq{type=result, sub_el=[]};
	{error,_Error} ->
	    ?WARNING_MSG("Error enabling / disabling carbons: ~p", [Result]),
            IQ#iq{type=error,sub_el = [?ERR_BAD_REQUEST]}
    end;

iq_handler(_From, _To, IQ, _CC)->
    IQ#iq{type=error, sub_el = [?ERR_NOT_ALLOWED]}.

user_send_packet(From, To, Packet) ->
    check_and_forward(From, To, Packet, sent).

user_receive_packet(JID, _From, To, Packet) ->
    check_and_forward(JID, To, Packet, received).
    
% verifier si le trafic est local
% Modified from original version: 
%    - registered to the user_send_packet hook, to be called only once even for multicast
%    - do not support "private" message mode, and do not modify the original packet in any way
%    - we also replicate "read" notifications
check_and_forward(JID, To, #xmlel{name = <<"message">>} = Packet, Direction)->
	case classify_packet(Packet) of
		ignore -> stop;
		forward  -> send_copies(JID, To, Packet, Direction);
		_ -> stop
	end;
 
check_and_forward(_JID, _To, _Packet, _)-> ok.

-spec classify_packet(_) -> classification().
classify_packet(Packet)->
	is_chat(Packet).

-spec is_chat(_) -> classification().
is_chat(#xmlel{name = <<"message">>, attrs = Attrs} = Packet) ->
	case xml:get_attr_s(<<"type">>, Attrs) of
		<<"chat">> ->
			is_private(Packet);
		_ ->
			ignore
	end.

-spec is_private(_) -> classification().
is_private(Packet) ->
	case xml:get_subtag(Packet, <<"private">>) of
		false ->
			is_no_copy(Packet);
		_ ->
			ignore
	end.

-spec is_no_copy(_) -> classification().
is_no_copy(Packet) ->
	case xml:get_subtag(Packet, <<"no-copy">>) of
		false ->
			is_received(Packet);
		_ ->
			ignore
	end.

-spec is_received(_) -> classification().
is_received(Packet) ->
	case xml:get_subtag(Packet, <<"received">>) of
		false ->
			is_sent(Packet);
		_ ->
			ignore
	end.

-spec is_sent(_) -> classification().
is_sent(Packet) ->
	SubTag = xml:get_subtag(Packet,<<"sent">>),
	if SubTag == false ->
		forward;
	true ->
		is_forwarded(SubTag)
	end.

-spec is_forwarded(_) -> classification().
is_forwarded(SubTag) ->
	case xml:get_subtag(SubTag, <<"forwarded">>) of
		false ->
			forward;
		_ ->
			ignore
	end.

remove_connection(User, Server, Resource, _Status)->
    disable(Server, User, Resource),
    ok.
    

%%% Internal
%% Direction = received | sent <received xmlns='urn:xmpp:carbons:1'/>
send_copies(JID, To, Packet, Direction)->
    {U, S, R} = jlib:jid_tolower(JID),
    PrioRes = ejabberd_sm:get_user_present_resources(U, S),

    IsBareTo = case {Direction, To} of
	{received, #jid{lresource = <<>>}} -> true;
	{received, #jid{lresource = LRes}} ->
	    %% unavailable resources are handled like bare JIDs
	    case lists:keyfind(LRes, 2, PrioRes) of
		false -> true;
		_ -> false
	    end;
	_ -> false
    end,
    %% list of JIDs that should receive a carbon copy of this message (excluding the
    %% receiver(s) of the original message
    TargetJIDs = if IsBareTo ->
	    MaxPrio = case catch lists:max(PrioRes) of
		{Prio, _Res} -> Prio;
		_ -> 0
	    end,
	    OrigTo = fun(Res) -> lists:member({MaxPrio, Res}, PrioRes) end,
	    [ {jlib:make_jid({U, S, CCRes}), CC_Version}
	     || {CCRes, CC_Version} <- list(U, S), not OrigTo(CCRes) ];
	true ->
	    [ {jlib:make_jid({U, S, CCRes}), CC_Version}
	     || {CCRes, CC_Version} <- list(U, S), CCRes /= R ]
	    %TargetJIDs = lists:delete(JID, [ jlib:make_jid({U, S, CCRes}) || CCRes <- list(U, S) ]),
    end,

    lists:map(fun({Dest,Version}) ->
		    {_, _, Resource} = jlib:jid_tolower(Dest),
		    ?DEBUG("Sending:  ~p =/= ~p", [R, Resource]),
		    Sender = jlib:make_jid({U, S, <<>>}),
		    %{xmlelement, N, A, C} = Packet,
		    New = build_forward_packet(JID, Packet, Sender, Dest, Direction, Version),
		    ejabberd_router:route(Sender, Dest, New)
	      end, TargetJIDs),
    ok.

build_forward_packet(JID, Packet, Sender, Dest, Direction, ?NS_CC_2) ->
    #xmlel{name = <<"message">>, 
	   attrs = [{<<"xmlns">>, <<"jabber:client">>},
		    {<<"type">>, <<"chat">>},
		    {<<"from">>, jlib:jid_to_binary(Sender)},
		    {<<"to">>, jlib:jid_to_binary(Dest)}],
	   children = [	
		#xmlel{name = list_to_binary(atom_to_list(Direction)), 
		       attrs = [{<<"xmlns">>, ?NS_CC_2}],
		       children = [
			#xmlel{name = <<"forwarded">>, 
			       attrs = [{<<"xmlns">>, ?NS_FORWARD}],
			       children = [
				complete_packet(JID, Packet, Direction)]}
		]}
	   ]};
build_forward_packet(JID, Packet, Sender, Dest, Direction, ?NS_CC_1) ->
    #xmlel{name = <<"message">>, 
	   attrs = [{<<"xmlns">>, <<"jabber:client">>},
		    {<<"type">>, <<"chat">>},
		    {<<"from">>, jlib:jid_to_binary(Sender)},
		    {<<"to">>, jlib:jid_to_binary(Dest)}],
	   children = [	
		#xmlel{name = list_to_binary(atom_to_list(Direction)), 
			attrs = [{<<"xmlns">>, ?NS_CC_1}]},
		#xmlel{name = <<"forwarded">>, 
		       attrs = [{<<"xmlns">>, ?NS_FORWARD}],
		       children = [complete_packet(JID, Packet, Direction)]}
		]}.


enable(Host, U, R, CC)->
    ?DEBUG("enabling for ~p", [U]),
     try mnesia:dirty_write(#carboncopy{us = {U, Host}, resource=R, version = CC}) of
	ok -> ok
     catch _:Error -> {error, Error}
     end.	

disable(Host, U, R)->
    ?DEBUG("disabling for ~p", [U]),
    ToDelete = mnesia:dirty_match_object(?TABLE, #carboncopy{us = {U, Host}, resource = R, version = '_'}),
    try lists:foreach(fun mnesia:dirty_delete_object/1, ToDelete) of
	ok -> ok
    catch _:Error -> {error, Error}
    end.

complete_packet(From, #xmlel{name = <<"message">>, attrs = OrigAttrs} = Packet, sent) ->
    %% if this is a packet sent by user on this host, then Packet doesn't
    %% include the 'from' attribute. We must add it.
    Attrs = lists:keystore(<<"xmlns">>, 1, OrigAttrs, {<<"xmlns">>, <<"jabber:client">>}),
    case proplists:get_value(<<"from">>, Attrs) of
	undefined ->
		Packet#xmlel{attrs = [{<<"from">>, jlib:jid_to_binary(From)}|Attrs]};
	_ ->
		Packet#xmlel{attrs = Attrs}
    end;
complete_packet(_From, #xmlel{name = <<"message">>, attrs=OrigAttrs} = Packet, received) ->
    Attrs = lists:keystore(<<"xmlns">>, 1, OrigAttrs, {<<"xmlns">>, <<"jabber:client">>}),
    Packet#xmlel{attrs = Attrs}.

%% list {resource, cc_version} with carbons enabled for given user and host
list(User, Server)->
	mnesia:dirty_select(?TABLE, [{#carboncopy{us = {User, Server}, resource = '$2', version = '$3'}, [], [{{'$2','$3'}}]}]).
