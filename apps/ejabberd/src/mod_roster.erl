%%%----------------------------------------------------------------------
%%% File    : mod_roster.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Roster management
%%% Created : 11 Dec 2002 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2011   ProcessOne
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
%%% You should have received a copy of the GNU General Public License
%%% along with this program; if not, write to the Free Software
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
%%% 02111-1307 USA
%%%
%%%----------------------------------------------------------------------

%%% @doc Roster management (Mnesia storage).
%%%
%%% Includes support for XEP-0237: Roster Versioning.
%%% The roster versioning follows an all-or-nothing strategy:
%%%  - If the version supplied by the client is the latest, return an empty response.
%%%  - If not, return the entire new roster (with updated version string).
%%% Roster version is a hash digest of the entire roster.
%%% No additional data is stored in DB.

-module(mod_roster).
-author('alexey@process-one.net').

-behaviour(gen_mod).

-export([start/2, stop/1,
	 process_iq/3,
	 process_local_iq/3,
	 get_user_roster/2,
	 get_subscription_lists/3,
	 in_subscription/6,
	 out_subscription/4,
	 set_items/3,
	 remove_user/2,
	 get_jid_info/4,
	 item_to_xml/1,
	 webadmin_page/3,
	 webadmin_user/4,
	 get_versioning_feature/2,
	 roster_versioning_enabled/1,
	 roster_version/2]).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("mod_roster.hrl").
-include("ejabberd_http.hrl").
-include("ejabberd_web_admin.hrl").


start(Host, Opts) ->
    IQDisc = gen_mod:get_opt(iqdisc, Opts, one_queue),
    mnesia:create_table(roster,[{disc_copies, [node()]},
				{attributes, record_info(fields, roster)}]),
    mnesia:create_table(roster_version, [{disc_copies, [node()]},
    				{attributes, record_info(fields, roster_version)}]),

    update_table(),
    mnesia:add_table_index(roster, us),
    mnesia:add_table_index(roster_version, us),
    ejabberd_hooks:add(roster_get, Host,
		       ?MODULE, get_user_roster, 50),
    ejabberd_hooks:add(roster_in_subscription, Host,
		       ?MODULE, in_subscription, 50),
    ejabberd_hooks:add(roster_out_subscription, Host,
		       ?MODULE, out_subscription, 50),
    ejabberd_hooks:add(roster_get_subscription_lists, Host,
		       ?MODULE, get_subscription_lists, 50),
    ejabberd_hooks:add(roster_get_jid_info, Host,
		       ?MODULE, get_jid_info, 50),
    ejabberd_hooks:add(remove_user, Host,
		       ?MODULE, remove_user, 50),
    ejabberd_hooks:add(anonymous_purge_hook, Host,
		       ?MODULE, remove_user, 50),
    ejabberd_hooks:add(roster_get_versioning_feature, Host,
		       ?MODULE, get_versioning_feature, 50),
    ejabberd_hooks:add(webadmin_page_host, Host,
		       ?MODULE, webadmin_page, 50),
    ejabberd_hooks:add(webadmin_user, Host,
		       ?MODULE, webadmin_user, 50),
    gen_iq_handler:add_iq_handler(ejabberd_sm, Host, ?NS_ROSTER,
				  ?MODULE, process_iq, IQDisc).

stop(Host) ->
    ejabberd_hooks:delete(roster_get, Host,
			  ?MODULE, get_user_roster, 50),
    ejabberd_hooks:delete(roster_in_subscription, Host,
			  ?MODULE, in_subscription, 50),
    ejabberd_hooks:delete(roster_out_subscription, Host,
			  ?MODULE, out_subscription, 50),
    ejabberd_hooks:delete(roster_get_subscription_lists, Host,
			  ?MODULE, get_subscription_lists, 50),
    ejabberd_hooks:delete(roster_get_jid_info, Host,
			  ?MODULE, get_jid_info, 50),
    ejabberd_hooks:delete(remove_user, Host,
			  ?MODULE, remove_user, 50),
    ejabberd_hooks:delete(anonymous_purge_hook, Host,
			  ?MODULE, remove_user, 50),
    ejabberd_hooks:delete(roster_get_versioning_feature, Host,
		          ?MODULE, get_versioning_feature, 50),
    ejabberd_hooks:delete(webadmin_page_host, Host,
			  ?MODULE, webadmin_page, 50),
    ejabberd_hooks:delete(webadmin_user, Host,
			  ?MODULE, webadmin_user, 50),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host, ?NS_ROSTER).


process_iq(From, To, IQ) ->
    #iq{sub_el = SubEl} = IQ,
    #jid{lserver = LServer} = From,
    case lists:member(LServer, ?MYHOSTS) of
	true ->
	    process_local_iq(From, To, IQ);
	_ ->
	    IQ#iq{type = error, sub_el = [SubEl, ?ERR_ITEM_NOT_FOUND]}
    end.

process_local_iq(From, To, #iq{type = Type} = IQ) ->
    case Type of
	set ->
	    process_iq_set(From, To, IQ);
	get ->
	    process_iq_get(From, To, IQ)
    end.

roster_hash(Items) ->
	sha:sha(term_to_binary(
		lists:sort(
			[R#roster{groups = lists:sort(Grs)} ||
				R = #roster{groups = Grs} <- Items]))).

roster_versioning_enabled(Host) ->
    gen_mod:get_module_opt(Host, ?MODULE, versioning, false).

roster_version_on_db(Host) ->
    gen_mod:get_module_opt(Host, ?MODULE, store_current_id, false).

%% Returns a list that may contain an xmlel with the XEP-237 feature if it's enabled.
get_versioning_feature(Acc, Host) ->
    case roster_versioning_enabled(Host) of
	true ->
	    Feature = #xmlel{name = <<"ver">>,
		             attrs = [{<<"xmlns">>, ?NS_ROSTER_VER}]},
	    [Feature | Acc];
	false -> []
    end.

roster_version(LServer ,LUser) ->
	US = {LUser, LServer},
	case roster_version_on_db(LServer) of
		true ->
			case mnesia:dirty_read(roster_version, US) of
				[#roster_version{version = V}] -> V;
				[] -> not_found
			end;
		false ->
			roster_hash(ejabberd_hooks:run_fold(roster_get, LServer, [], [US]))
	end.

%% Load roster from DB only if neccesary.
%% It is neccesary if
%%     - roster versioning is disabled in server OR
%%     - roster versioning is not used by the client OR
%%     - roster versioning is used by server and client, BUT the server isn't storing versions on db OR
%%     - the roster version from client don't match current version.
process_iq_get(From, To, #iq{sub_el = SubEl} = IQ) ->
    LUser = From#jid.luser,
    LServer = From#jid.lserver,
    US = {LUser, LServer},
    try
	    {ItemsToSend, VersionToSend} =
		case {xml:get_tag_attr(<<"ver">>, SubEl),
		      roster_versioning_enabled(LServer),
		      roster_version_on_db(LServer)} of
		{{value, RequestedVersion}, true, true} ->
			%% Retrieve version from DB. Only load entire roster
			%% when neccesary.
			case mnesia:dirty_read(roster_version, US) of
				[#roster_version{version = RequestedVersion}] ->
					{false, false};
				[#roster_version{version = NewVersion}] ->
					{lists:map(fun item_to_xml/1,
						ejabberd_hooks:run_fold(roster_get, To#jid.lserver, [], [US])), NewVersion};
				[] ->
					RosterVersion = sha:sha(term_to_binary(now())),
					mnesia:dirty_write(#roster_version{us = US, version = RosterVersion}),
					{lists:map(fun item_to_xml/1,
						ejabberd_hooks:run_fold(roster_get, To#jid.lserver, [], [US])), RosterVersion}
			end;

		{{value, RequestedVersion}, true, false} ->
			RosterItems = ejabberd_hooks:run_fold(roster_get, To#jid.lserver, [] , [US]),
			case roster_hash(RosterItems) of
				RequestedVersion ->
					{false, false};
				New ->
					{lists:map(fun item_to_xml/1, RosterItems), New}
			end;

		_ ->
			{lists:map(fun item_to_xml/1,
					ejabberd_hooks:run_fold(roster_get, To#jid.lserver, [], [US])), false}
		end,
		IQ#iq{type = result, sub_el = case {ItemsToSend, VersionToSend} of
					 	 {false, false} ->  [];
					      {Items, false} -> [#xmlel{name = <<"query">>,
						                        attrs = [{<<"xmlns">>, ?NS_ROSTER}],
						                        children = Items}];
                                              {Items, Version} -> [#xmlel{name = <<"query">>,
                                                                          attrs = [{<<"xmlns">>, ?NS_ROSTER}, {<<"ver">>, Version}],
                                                                          children = Items}]
                                          end}
    catch
    	_:_ ->
		IQ#iq{type =error, sub_el = [SubEl, ?ERR_INTERNAL_SERVER_ERROR]}
    end.


get_user_roster(Acc, US) ->
    case catch mnesia:dirty_index_read(roster, US, #roster.us) of
	Items when is_list(Items) ->
	    lists:filter(fun(#roster{subscription = none, ask = in}) ->
				 false;
			    (_) ->
				 true
			 end, Items) ++ Acc;
	_ ->
	    Acc
    end.


item_to_xml(Item) ->
    Attrs1 = [{<<"jid">>, jlib:jid_to_binary(Item#roster.jid)}],
    Attrs2 = case Item#roster.name of
		 <<>> ->
		     Attrs1;
		 Name ->
		     [{<<"name">>, Name} | Attrs1]
	     end,
    Attrs3 = case Item#roster.subscription of
		 none ->
		     [{<<"subscription">>, <<"none">>} | Attrs2];
		 from ->
		     [{<<"subscription">>, <<"from">>} | Attrs2];
		 to ->
		     [{<<"subscription">>, <<"to">>} | Attrs2];
		 both ->
		     [{<<"subscription">>, <<"both">>} | Attrs2];
		 remove ->
		     [{<<"subscription">>, <<"remove">>} | Attrs2]
	     end,
    Attrs4 = case ask_to_pending(Item#roster.ask) of
		 out ->
		     [{<<"ask">>, <<"subscribe">>} | Attrs3];
		 both ->
		     [{<<"ask">>, <<"subscribe">>} | Attrs3];
		 _ ->
		     Attrs3
	     end,
    SubEls1 = lists:map(fun(G) ->
				#xmlel{name = <<"group">>,
                                       children = [#xmlcdata{content = G}]}
			end, Item#roster.groups),
    SubEls = SubEls1 ++ Item#roster.xs,
    #xmlel{name = <<"item">>, attrs = Attrs4, children = SubEls}.


process_iq_set(From, To, #iq{sub_el = SubEl} = IQ) ->
    #xmlel{children = Els} = SubEl,
    #jid{lserver = LServer} = From,
    ejabberd_hooks:run(roster_set, LServer, [From, To, SubEl]),
    lists:foreach(fun(El) -> process_item_set(From, To, El) end, Els),
    IQ#iq{type = result, sub_el = []}.

process_item_set(From, To, #xmlel{attrs = Attrs, children = Els}) ->
    JID1 = jlib:binary_to_jid(xml:get_attr_s(<<"jid">>, Attrs)),
    #jid{user = User, luser = LUser, lserver = LServer} = From,
    case JID1 of
	error ->
	    ok;
	_ ->
	    JID = {JID1#jid.user, JID1#jid.server, JID1#jid.resource},
	    LJID = jlib:jid_tolower(JID1),
	    F = fun() ->
			Res = mnesia:read({roster, {LUser, LServer, LJID}}),
			Item = case Res of
				   [] ->
				       #roster{usj = {LUser, LServer, LJID},
					       us = {LUser, LServer},
					       jid = JID};
				   [I] ->
				       I#roster{jid = JID,
						name = <<>>,
						groups = [],
						xs = []}
			       end,
			Item1 = process_item_attrs(Item, Attrs),
			Item2 = process_item_els(Item1, Els),
			case Item2#roster.subscription of
			    remove ->
				mnesia:delete({roster, {LUser, LServer, LJID}});
			    _ ->
				mnesia:write(Item2)
			end,
			%% If the item exist in shared roster, take the
			%% subscription information from there:
			Item3 = ejabberd_hooks:run_fold(roster_process_item,
							LServer, Item2, [LServer]),
			case roster_version_on_db(LServer) of
				true -> mnesia:write(#roster_version{us = {LUser, LServer}, version = sha:sha(term_to_binary(now()))});
				false -> ok
			end,
			{Item, Item3}
		end,
	    case mnesia:transaction(F) of
		{atomic, {OldItem, Item}} ->
		    push_item(User, LServer, To, Item),
		    case Item#roster.subscription of
			remove ->
			    send_unsubscribing_presence(From, OldItem),
			    ok;
			_ ->
			    ok
		    end;
		E ->
		    ?DEBUG("ROSTER: roster item set error: ~p~n", [E]),
		    ok
	    end
    end;
process_item_set(_From, _To, _) ->
    ok.

process_item_attrs(Item, [{Attr, Val} | Attrs]) ->
    case Attr of
	<<"jid">> ->
	    case jlib:binary_to_jid(Val) of
		error ->
		    process_item_attrs(Item, Attrs);
		JID1 ->
		    JID = {JID1#jid.user, JID1#jid.server, JID1#jid.resource},
		    process_item_attrs(Item#roster{jid = JID}, Attrs)
	    end;
	<<"name">> ->
	    process_item_attrs(Item#roster{name = Val}, Attrs);
	<<"subscription">> ->
	    case Val of
		<<"remove">> ->
		    process_item_attrs(Item#roster{subscription = remove},
				       Attrs);
		_ ->
		    process_item_attrs(Item, Attrs)
	    end;
	<<"ask">> ->
	    process_item_attrs(Item, Attrs);
	_ ->
	    process_item_attrs(Item, Attrs)
    end;
process_item_attrs(Item, []) ->
    Item.


process_item_els(Item, [XE = #xmlel{name = Name, attrs = Attrs,
                                    children = SEls} | Els]) ->
    case Name of
	<<"group">> ->
	    Groups = [xml:get_cdata(SEls) | Item#roster.groups],
	    process_item_els(Item#roster{groups = Groups}, Els);
	_ ->
	    case xml:get_attr_s(<<"xmlns">>, Attrs) of
		<<>> ->
		    process_item_els(Item, Els);
		_ ->
		    XEls = [XE | Item#roster.xs],
		    process_item_els(Item#roster{xs = XEls}, Els)
	    end
    end;
process_item_els(Item, [#xmlcdata{} | Els]) ->
    process_item_els(Item, Els);
process_item_els(Item, []) ->
    Item.


push_item(User, Server, From, Item) ->
    ejabberd_sm:route(jlib:make_jid(<<>>, <<>>, <<>>),
		      jlib:make_jid(User, Server, <<>>),
		      #xmlel{name = <<"broadcast">>,
		             children = [{item,
			                  Item#roster.jid,
			                  Item#roster.subscription}]}),
    case roster_versioning_enabled(Server) of
	true ->
		push_item_version(Server, User, From, Item, roster_version(Server, User));
	false ->
	    lists:foreach(fun(Resource) ->
			  push_item(User, Server, Resource, From, Item)
		  end, ejabberd_sm:get_user_resources(User, Server))
    end.

% TODO: don't push to those who didn't load roster
push_item(User, Server, Resource, From, Item) ->
    push_item(User, Server, Resource, From, Item, not_found).

push_item(User, Server, Resource, From, Item, RosterVersion) ->
    ejabberd_hooks:run(roster_push, Server, [From, Item]),
    ExtraAttrs = case RosterVersion of
	not_found -> [];
	_ -> [{<<"ver">>, RosterVersion}]
    end,
    ResIQ = #iq{type = set, xmlns = ?NS_ROSTER,
		id = list_to_binary("push" ++ randoms:get_string()),
		sub_el = [#xmlel{name = <<"query">>,
			         attrs = [{<<"xmlns">>, ?NS_ROSTER}|ExtraAttrs],
			         children = [item_to_xml(Item)]}]},
    ejabberd_router:route(
      From,
      jlib:make_jid(User, Server, Resource),
      jlib:iq_to_xml(ResIQ)).

%% @doc Roster push, calculate and include the version attribute.
%% TODO: don't push to those who didn't load roster
push_item_version(Server, User, From, Item, RosterVersion)  ->
    lists:foreach(fun(Resource) ->
			  push_item(User, Server, Resource, From, Item, RosterVersion)
		end, ejabberd_sm:get_user_resources(User, Server)).

get_subscription_lists(_, User, Server) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    US = {LUser, LServer},
    JID = jlib:make_jid(User, Server, <<>>),
    case mnesia:dirty_index_read(roster, US, #roster.us) of
	Items when is_list(Items) ->
	    fill_subscription_lists(JID, Items, [], [], []);
	_ ->
	    {[], [], []}
    end.

fill_subscription_lists(JID, [I | Is], F, T, P) ->
    J = element(3, I#roster.usj),
    NewP = case I#roster.ask of
               Ask when Ask == in;
                        Ask == both ->
                   Message = I#roster.askmessage,
                   Status  = if is_binary(Message) ->
                                     Message;
                                true ->
                                     <<>>
                             end,
                   [#xmlel{name = <<"presence">>,
                           attrs = [{<<"from">>, jlib:jid_to_binary(I#roster.jid)},
                                    {<<"to">>, jlib:jid_to_binary(JID)},
                                    {<<"type">>, <<"subscribe">>}],
                           children = [#xmlel{name = <<"status">>,
                                              children = [#xmlcdata{content = Status}]}]} | P];
               _ ->
                   P
           end,
    case I#roster.subscription of
	both ->
	    fill_subscription_lists(JID, Is, [J | F], [J | T], NewP);
	from ->
	    fill_subscription_lists(JID, Is, [J | F], T, NewP);
	to ->
	    fill_subscription_lists(JID, Is, F, [J | T], NewP);
	_ ->
	    fill_subscription_lists(JID, Is, F, T, NewP)
    end;
fill_subscription_lists(_JID, [], F, T, P) ->
    {F, T, P}.

ask_to_pending(subscribe) -> out;
ask_to_pending(unsubscribe) -> none;
ask_to_pending(Ask) -> Ask.



in_subscription(_, User, Server, JID, Type, Reason) ->
    process_subscription(in, User, Server, JID, Type, Reason).

out_subscription(User, Server, JID, Type) ->
    process_subscription(out, User, Server, JID, Type, <<>>).

process_subscription(Direction, User, Server, JID1, Type, Reason) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    US = {LUser, LServer},
    LJID = jlib:jid_tolower(JID1),
    F = fun() ->
		Item = case mnesia:read({roster, {LUser, LServer, LJID}}) of
			   [] ->
			       JID = {JID1#jid.user,
				      JID1#jid.server,
				      JID1#jid.resource},
			       #roster{usj = {LUser, LServer, LJID},
				       us = US,
				       jid = JID};
			   [I] ->
			       I
		       end,
		NewState = case Direction of
			       out ->
				   out_state_change(Item#roster.subscription,
						    Item#roster.ask,
						    Type);
			       in ->
				   in_state_change(Item#roster.subscription,
						   Item#roster.ask,
						   Type)
			   end,
		AutoReply = case Direction of
				out ->
				    none;
				in ->
				    in_auto_reply(Item#roster.subscription,
						  Item#roster.ask,
						  Type)
			    end,
		AskMessage = case NewState of
				 {_, both} -> Reason;
				 {_, in}   -> Reason;
				 _         -> <<>>
			     end,
		case NewState of
		    none ->
			{none, AutoReply};
		    {none, none} when Item#roster.subscription == none,
		                      Item#roster.ask == in ->
			mnesia:delete({roster, {LUser, LServer, LJID}}),
			{none, AutoReply};
		    {Subscription, Pending} ->
			NewItem = Item#roster{subscription = Subscription,
					      ask = Pending,
					      askmessage = AskMessage},
			mnesia:write(NewItem),
			case roster_version_on_db(LServer) of
				true -> mnesia:write(#roster_version{us = {LUser, LServer}, version = sha:sha(term_to_binary(now()))});
				false -> ok
			end,
			{{push, NewItem}, AutoReply}
		end
	end,
    case mnesia:transaction(F) of
	{atomic, {Push, AutoReply}} ->
	    case AutoReply of
		none ->
		    ok;
		_ ->
		    T = case AutoReply of
			    subscribed -> <<"subscribed">>;
			    unsubscribed -> <<"unsubscribed">>
			end,
		    ejabberd_router:route(
		      jlib:make_jid(User, Server, <<>>), JID1,
		      #xmlel{name = <<"presence">>, attrs = [{<<"type">>, T}]})
	    end,
	    case Push of
		{push, Item} ->
		    if
			Item#roster.subscription == none,
			Item#roster.ask == in ->
			    ok;
			true ->
			    push_item(User, Server,
				      jlib:make_jid(User, Server, <<>>), Item)
		    end,
		    true;
		none ->
		    false
	    end;
	_ ->
	    false
    end.

%% in_state_change(Subscription, Pending, Type) -> NewState
%% NewState = none | {NewSubscription, NewPending}
-ifdef(ROSTER_GATEWAY_WORKAROUND).
-define(NNSD, {to, none}).
-define(NISD, {to, in}).
-else.
-define(NNSD, none).
-define(NISD, none).
-endif.

in_state_change(none, none, subscribe)    -> {none, in};
in_state_change(none, none, subscribed)   -> ?NNSD;
in_state_change(none, none, unsubscribe)  -> none;
in_state_change(none, none, unsubscribed) -> none;
in_state_change(none, out,  subscribe)    -> {none, both};
in_state_change(none, out,  subscribed)   -> {to, none};
in_state_change(none, out,  unsubscribe)  -> none;
in_state_change(none, out,  unsubscribed) -> {none, none};
in_state_change(none, in,   subscribe)    -> none;
in_state_change(none, in,   subscribed)   -> ?NISD;
in_state_change(none, in,   unsubscribe)  -> {none, none};
in_state_change(none, in,   unsubscribed) -> none;
in_state_change(none, both, subscribe)    -> none;
in_state_change(none, both, subscribed)   -> {to, in};
in_state_change(none, both, unsubscribe)  -> {none, out};
in_state_change(none, both, unsubscribed) -> {none, in};
in_state_change(to,   none, subscribe)    -> {to, in};
in_state_change(to,   none, subscribed)   -> none;
in_state_change(to,   none, unsubscribe)  -> none;
in_state_change(to,   none, unsubscribed) -> {none, none};
in_state_change(to,   in,   subscribe)    -> none;
in_state_change(to,   in,   subscribed)   -> none;
in_state_change(to,   in,   unsubscribe)  -> {to, none};
in_state_change(to,   in,   unsubscribed) -> {none, in};
in_state_change(from, none, subscribe)    -> none;
in_state_change(from, none, subscribed)   -> {both, none};
in_state_change(from, none, unsubscribe)  -> {none, none};
in_state_change(from, none, unsubscribed) -> none;
in_state_change(from, out,  subscribe)    -> none;
in_state_change(from, out,  subscribed)   -> {both, none};
in_state_change(from, out,  unsubscribe)  -> {none, out};
in_state_change(from, out,  unsubscribed) -> {from, none};
in_state_change(both, none, subscribe)    -> none;
in_state_change(both, none, subscribed)   -> none;
in_state_change(both, none, unsubscribe)  -> {to, none};
in_state_change(both, none, unsubscribed) -> {from, none}.

out_state_change(none, none, subscribe)    -> {none, out};
out_state_change(none, none, subscribed)   -> none;
out_state_change(none, none, unsubscribe)  -> none;
out_state_change(none, none, unsubscribed) -> none;
out_state_change(none, out,  subscribe)    -> {none, out}; %% We need to resend query (RFC3921, section 9.2)
out_state_change(none, out,  subscribed)   -> none;
out_state_change(none, out,  unsubscribe)  -> {none, none};
out_state_change(none, out,  unsubscribed) -> none;
out_state_change(none, in,   subscribe)    -> {none, both};
out_state_change(none, in,   subscribed)   -> {from, none};
out_state_change(none, in,   unsubscribe)  -> none;
out_state_change(none, in,   unsubscribed) -> {none, none};
out_state_change(none, both, subscribe)    -> none;
out_state_change(none, both, subscribed)   -> {from, out};
out_state_change(none, both, unsubscribe)  -> {none, in};
out_state_change(none, both, unsubscribed) -> {none, out};
out_state_change(to,   none, subscribe)    -> none;
out_state_change(to,   none, subscribed)   -> {both, none};
out_state_change(to,   none, unsubscribe)  -> {none, none};
out_state_change(to,   none, unsubscribed) -> none;
out_state_change(to,   in,   subscribe)    -> none;
out_state_change(to,   in,   subscribed)   -> {both, none};
out_state_change(to,   in,   unsubscribe)  -> {none, in};
out_state_change(to,   in,   unsubscribed) -> {to, none};
out_state_change(from, none, subscribe)    -> {from, out};
out_state_change(from, none, subscribed)   -> none;
out_state_change(from, none, unsubscribe)  -> none;
out_state_change(from, none, unsubscribed) -> {none, none};
out_state_change(from, out,  subscribe)    -> none;
out_state_change(from, out,  subscribed)   -> none;
out_state_change(from, out,  unsubscribe)  -> {from, none};
out_state_change(from, out,  unsubscribed) -> {none, out};
out_state_change(both, none, subscribe)    -> none;
out_state_change(both, none, subscribed)   -> none;
out_state_change(both, none, unsubscribe)  -> {from, none};
out_state_change(both, none, unsubscribed) -> {to, none}.

in_auto_reply(from, none, subscribe)    -> subscribed;
in_auto_reply(from, out,  subscribe)    -> subscribed;
in_auto_reply(both, none, subscribe)    -> subscribed;
in_auto_reply(none, in,   unsubscribe)  -> unsubscribed;
in_auto_reply(none, both, unsubscribe)  -> unsubscribed;
in_auto_reply(to,   in,   unsubscribe)  -> unsubscribed;
in_auto_reply(from, none, unsubscribe)  -> unsubscribed;
in_auto_reply(from, out,  unsubscribe)  -> unsubscribed;
in_auto_reply(both, none, unsubscribe)  -> unsubscribed;
in_auto_reply(_,    _,    _)  ->           none.


remove_user(User, Server) when is_list(User), is_list(Server) ->
    remove_user(list_to_binary(User), list_to_binary(Server));
remove_user(User, Server) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    US = {LUser, LServer},
    send_unsubscription_to_rosteritems(LUser, LServer),
    F = fun() ->
		lists:foreach(fun(R) ->
				      mnesia:delete_object(R)
			      end,
			      mnesia:index_read(roster, US, #roster.us))
        end,
    mnesia:transaction(F).

%% For each contact with Subscription:
%% Both or From, send a "unsubscribed" presence stanza;
%% Both or To, send a "unsubscribe" presence stanza.
send_unsubscription_to_rosteritems(LUser, LServer) ->
    RosterItems = get_user_roster([], {LUser, LServer}),
    From = jlib:make_jid({LUser, LServer, <<>>}),
    lists:foreach(fun(RosterItem) ->
			  send_unsubscribing_presence(From, RosterItem)
		  end,
		  RosterItems).

%% @spec (From::jid(), Item::roster()) -> ok
send_unsubscribing_presence(From, Item) ->
    IsTo = case Item#roster.subscription of
	       both -> true;
	       to -> true;
	       _ -> false
	   end,
    IsFrom = case Item#roster.subscription of
		 both -> true;
		 from -> true;
		 _ -> false
	     end,
    if IsTo ->
	    send_presence_type(
	      jlib:jid_remove_resource(From),
	      jlib:make_jid(Item#roster.jid), <<"unsubscribe">>);
       true -> ok
    end,
    if IsFrom ->
	    send_presence_type(
	      jlib:jid_remove_resource(From),
	      jlib:make_jid(Item#roster.jid), <<"unsubscribed">>);
       true -> ok
    end,
    ok.

send_presence_type(From, To, Type) ->
    ejabberd_router:route(
      From, To,
      #xmlel{name = <<"presence">>, attrs = [{<<"type">>, Type}]}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

set_items(User, Server, SubEl) ->
    #xmlel{children = Els} = SubEl,
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    F = fun() ->
		lists:foreach(fun(El) ->
				      process_item_set_t(LUser, LServer, El)
			      end, Els)
	end,
    mnesia:transaction(F).

process_item_set_t(LUser, LServer, #xmlel{attrs = Attrs,
                                          children = Els}) ->
    JID1 = jlib:binary_to_jid(xml:get_attr_s(<<"jid">>, Attrs)),
    case JID1 of
	error ->
	    ok;
	_ ->
	    JID = {JID1#jid.user, JID1#jid.server, JID1#jid.resource},
	    LJID = {JID1#jid.luser, JID1#jid.lserver, JID1#jid.lresource},
	    Item = #roster{usj = {LUser, LServer, LJID},
			   us = {LUser, LServer},
			   jid = JID},
	    Item1 = process_item_attrs_ws(Item, Attrs),
	    Item2 = process_item_els(Item1, Els),
	    case Item2#roster.subscription of
		remove ->
		    mnesia:delete({roster, {LUser, LServer, LJID}});
		_ ->
		    mnesia:write(Item2)
	    end
    end;
process_item_set_t(_LUser, _LServer, _) ->
    ok.

process_item_attrs_ws(Item, [{Attr, Val} | Attrs]) ->
    case Attr of
	<<"jid">> ->
	    case jlib:binary_to_jid(Val) of
		error ->
		    process_item_attrs_ws(Item, Attrs);
		JID1 ->
		    JID = {JID1#jid.user, JID1#jid.server, JID1#jid.resource},
		    process_item_attrs_ws(Item#roster{jid = JID}, Attrs)
	    end;
	<<"name">> ->
	    process_item_attrs_ws(Item#roster{name = Val}, Attrs);
	<<"subscription">> ->
	    case Val of
		<<"remove">> ->
		    process_item_attrs_ws(Item#roster{subscription = remove},
					  Attrs);
		<<"none">> ->
		    process_item_attrs_ws(Item#roster{subscription = none},
					  Attrs);
		<<"both">> ->
		    process_item_attrs_ws(Item#roster{subscription = both},
					  Attrs);
		<<"from">> ->
		    process_item_attrs_ws(Item#roster{subscription = from},
					  Attrs);
		<<"to">> ->
		    process_item_attrs_ws(Item#roster{subscription = to},
					  Attrs);
		_ ->
		    process_item_attrs_ws(Item, Attrs)
	    end;
	<<"ask">> ->
	    process_item_attrs_ws(Item, Attrs);
	_ ->
	    process_item_attrs_ws(Item, Attrs)
    end;
process_item_attrs_ws(Item, []) ->
    Item.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_jid_info(_, User, Server, JID) ->
    LUser = jlib:nodeprep(User),
    LJID = jlib:jid_tolower(JID),
    LServer = jlib:nameprep(Server),
    case catch mnesia:dirty_read(roster, {LUser, LServer, LJID}) of
	[#roster{subscription = Subscription, groups = Groups}] ->
	    {Subscription, Groups};
	_ ->
	    LRJID = jlib:jid_tolower(jlib:jid_remove_resource(JID)),
	    if
		LRJID == LJID ->
		    {none, []};
		true ->
		    case catch mnesia:dirty_read(
				 roster, {LUser, LServer, LRJID}) of
			[#roster{subscription = Subscription,
				 groups = Groups}] ->
			    {Subscription, Groups};
			_ ->
			    {none, []}
		    end
	    end
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


update_table() ->
    Fields = record_info(fields, roster),
    case mnesia:table_info(roster, attributes) of
	Fields ->
	    ok;
	[uj, user, jid, name, subscription, ask, groups, xattrs, xs] ->
	    convert_table1(Fields);
	[usj, us, jid, name, subscription, ask, groups, xattrs, xs] ->
	    convert_table2(Fields);
	_ ->
	    ?INFO_MSG("Recreating roster table", []),
	    mnesia:transform_table(roster, ignore, Fields)
    end.


%% Convert roster table to support virtual host
convert_table1(Fields) ->
    ?INFO_MSG("Virtual host support: converting roster table from "
	      "{uj, user, jid, name, subscription, ask, groups, xattrs, xs} format", []),
    Host = ?MYNAME,
    {atomic, ok} = mnesia:create_table(
		     mod_roster_tmp_table,
		     [{disc_only_copies, [node()]},
		      {type, bag},
		      {local_content, true},
		      {record_name, roster},
		      {attributes, record_info(fields, roster)}]),
    mnesia:del_table_index(roster, user),
    mnesia:transform_table(roster, ignore, Fields),
    F1 = fun() ->
		 mnesia:write_lock_table(mod_roster_tmp_table),
		 mnesia:foldl(
		   fun(#roster{usj = {U, JID}, us = U} = R, _) ->
			   mnesia:dirty_write(
			     mod_roster_tmp_table,
			     R#roster{usj = {U, Host, JID},
				      us = {U, Host}})
		   end, ok, roster)
	 end,
    mnesia:transaction(F1),
    mnesia:clear_table(roster),
    F2 = fun() ->
		 mnesia:write_lock_table(roster),
		 mnesia:foldl(
		   fun(R, _) ->
			   mnesia:dirty_write(R)
		   end, ok, mod_roster_tmp_table)
	 end,
    mnesia:transaction(F2),
    mnesia:delete_table(mod_roster_tmp_table).


%% Convert roster table: xattrs fields become
convert_table2(Fields) ->
    ?INFO_MSG("Converting roster table from "
	      "{usj, us, jid, name, subscription, ask, groups, xattrs, xs} format", []),
    mnesia:transform_table(roster, ignore, Fields).


webadmin_page(_, Host,
	      #request{us = _US,
		       path = ["user", U, "roster"],
		       q = Query,
		       lang = Lang} = _Request) ->
    Res = user_roster(U, Host, Query, Lang),
    {stop, Res};

webadmin_page(Acc, _, _) -> Acc.

user_roster(User, Server, Query, Lang) ->
    US = {jlib:nodeprep(User), jlib:nameprep(Server)},
    Items1 = mnesia:dirty_index_read(roster, US, #roster.us),
    Res = user_roster_parse_query(User, Server, Items1, Query),
    Items = mnesia:dirty_index_read(roster, US, #roster.us),
    SItems = lists:sort(Items),
    FItems =
	case SItems of
	    [] ->
		[?CT("None")];
	    _ ->
		[?XE("table",
		     [?XE("thead",
			  [?XE("tr",
			       [?XCT("td", "Jabber ID"),
				?XCT("td", "Nickname"),
				?XCT("td", "Subscription"),
				?XCT("td", "Pending"),
				?XCT("td", "Groups")
			       ])]),
		      ?XE("tbody",
			  lists:map(
			    fun(R) ->
				    Groups =
					lists:flatmap(
					  fun(Group) ->
						  [?C(Group), ?BR]
					  end, R#roster.groups),
				    Pending = ask_to_pending(R#roster.ask),
				    TDJID = build_contact_jid_td(R#roster.jid),
				    ?XE("tr",
					[TDJID,
					 ?XAC("td", [{"class", "valign"}],
					      R#roster.name),
					 ?XAC("td", [{"class", "valign"}],
					      atom_to_list(R#roster.subscription)),
					 ?XAC("td", [{"class", "valign"}],
					      atom_to_list(Pending)),
					 ?XAE("td", [{"class", "valign"}], Groups),
					 if
					     Pending == in ->
						 ?XAE("td", [{"class", "valign"}],
						      [?INPUTT("submit",
							       "validate" ++
							       ejabberd_web_admin:term_to_id(R#roster.jid),
							       "Validate")]);
					     true ->
						 ?X("td")
					 end,
					 ?XAE("td", [{"class", "valign"}],
					      [?INPUTT("submit",
						       "remove" ++
						       ejabberd_web_admin:term_to_id(R#roster.jid),
						       "Remove")])])
			    end, SItems))])]
	end,
    [?XC("h1", ?T("Roster of ") ++ us_to_list(US))] ++
	case Res of
	    ok -> [?XREST("Submitted")];
	    error -> [?XREST("Bad format")];
	    nothing -> []
	end ++
	[?XAE("form", [{"action", ""}, {"method", "post"}],
	      FItems ++
	      [?P,
	       ?INPUT("text", "newjid", ""), ?C(" "),
	       ?INPUTT("submit", "addjid", "Add Jabber ID")
	      ])].

build_contact_jid_td(RosterJID) ->
    %% Convert {U, S, R} into {jid, U, S, R, U, S, R}:
    ContactJID = jlib:make_jid(RosterJID),
    JIDURI = case {ContactJID#jid.luser, ContactJID#jid.lserver} of
        {<<>>, _} -> <<>>;
		 {CUser, CServer} ->
		     case lists:member(CServer, ?MYHOSTS) of
             false -> <<>>;
             true  -> <<"/admin/server/", CServer/binary,
                        "/user/", CUser/binary, "/">>
		     end
	     end,
    case JIDURI of
    <<>> ->
	    ?XAC("td", [{"class", "valign"}], jlib:jid_to_binary(RosterJID));
	URI when is_binary(URI) ->
	    ?XAE("td", [{"class", "valign"}], [?AC(JIDURI, jlib:jid_to_binary(RosterJID))])
    end.

user_roster_parse_query(User, Server, Items, Query) ->
    case lists:keysearch("addjid", 1, Query) of
	{value, _} ->
	    case lists:keysearch("newjid", 1, Query) of
		{value, {_, undefined}} ->
		    error;
		{value, {_, SJID}} ->
		    case jlib:binary_to_jid(SJID) of
			JID when is_record(JID, jid) ->
			    user_roster_subscribe_jid(User, Server, JID),
			    ok;
			error ->
			    error
		    end;
		false ->
		    error
	    end;
	false ->
	    case catch user_roster_item_parse_query(
			 User, Server, Items, Query) of
		submitted ->
		    ok;
		{'EXIT', _Reason} ->
		    error;
		_ ->
		    nothing
	    end
    end.


user_roster_subscribe_jid(User, Server, JID) ->
    out_subscription(User, Server, JID, subscribe),
    UJID = jlib:make_jid(User, Server, <<>>),
    ejabberd_router:route(
      UJID, JID, #xmlel{name = <<"presence">>,
                        attrs = [{<<"type">>, <<"subscribe">>}]}).

user_roster_item_parse_query(User, Server, Items, Query) ->
    lists:foreach(
      fun(R) ->
	      JID = R#roster.jid,
	      case lists:keysearch(
		     "validate" ++ ejabberd_web_admin:term_to_id(JID), 1, Query) of
		  {value, _} ->
		      JID1 = jlib:make_jid(JID),
		      out_subscription(
			User, Server, JID1, subscribed),
		      UJID = jlib:make_jid(User, Server, <<>>),
		      ejabberd_router:route(
			UJID, JID1, #xmlel{name = <<"presence">>,
				           attrs = [{<<"type">>, <<"subscribed">>}]}),
		      throw(submitted);
		  false ->
		      case lists:keysearch(
			     "remove" ++ ejabberd_web_admin:term_to_id(JID), 1, Query) of
			  {value, _} ->
			      UJID = jlib:make_jid(User, Server, <<>>),
			      process_iq(
				UJID, UJID,
				#iq{type = set,
				    sub_el = #xmlel{name = <<"query">>,
					            attrs = [{<<"xmlns">>, ?NS_ROSTER}],
					            children = [#xmlel{name = <<"item">>,
						                       attrs = [{<<"jid">>, jlib:jid_to_binary(JID)},
						                                {<<"subscription">>, <<"remove">>}]}]}}),
			      throw(submitted);
			  false ->
			      ok
		      end
	      end
      end, Items),
    nothing.

us_to_list({User, Server}) ->
    jlib:jid_to_binary({User, Server, <<>>}).

webadmin_user(Acc, _User, _Server, Lang) ->
    Acc ++ [?XE("h3", [?ACT("roster/", "Roster")])].

