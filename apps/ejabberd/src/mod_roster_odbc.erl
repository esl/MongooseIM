%%%----------------------------------------------------------------------
%%% File    : mod_roster_odbc.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Roster management
%%% Created : 15 Dec 2004 by Alexey Shchepin <alexey@process-one.net>
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

-module(mod_roster_odbc).
-author('alexey@process-one.net').

-behaviour(gen_mod).

-export([start/2, stop/1,
         process_iq/3,
         process_local_iq/3,
         get_user_roster/2,
         get_subscription_lists/3,
         get_in_pending_subscriptions/3,
         in_subscription/6,
         out_subscription/4,
         set_items/3,
         remove_user/2,
         get_jid_info/4,
         webadmin_page/3,
         webadmin_user/4,
         get_versioning_feature/2,
         roster_versioning_enabled/1]).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("mod_roster.hrl").
-include("ejabberd_http.hrl").
-include("ejabberd_web_admin.hrl").


start(Host, Opts) ->
    IQDisc = gen_mod:get_opt(iqdisc, Opts, one_queue),
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
    ejabberd_hooks:add(resend_subscription_requests_hook, Host,
                       ?MODULE, get_in_pending_subscriptions, 50),
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
    ejabberd_hooks:delete(resend_subscription_requests_hook, Host,
                          ?MODULE, get_in_pending_subscriptions, 50),
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

%% Returns a list that may contain an xmlelement with the XEP-237 feature if it's enabled.
get_versioning_feature(Acc, Host) ->
    case roster_versioning_enabled(Host) of
        true ->
            Feature = {xmlelement,
                       <<"ver">>,
                       [{<<"xmlns">>, ?NS_ROSTER_VER}],
                       [{xmlelement, <<"optional">>, [], []}]},
            [Feature | Acc];
        false -> []
    end.

roster_version(LServer ,LUser) ->
	US = {LUser, LServer},
	case roster_version_on_db(LServer) of
		true ->
			case odbc_queries:get_roster_version(ejabberd_odbc:escape(LServer),
                                                 ejabberd_odbc:escape(LUser)) of
				{selected, ["version"], [{Version}]} -> Version;
				{selected, ["version"], []} -> not_found
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
                    case odbc_queries:get_roster_version(ejabberd_odbc:escape(LServer),
                                                         ejabberd_odbc:escape(LUser)) of
                        {selected, ["version"], [{RequestedVersion}]} ->
                            {false, false};
                        {selected, ["version"], [{NewVersion}]} ->
                            {lists:map(fun item_to_xml/1,
                                       ejabberd_hooks:run_fold(roster_get, To#jid.lserver, [], [US])), NewVersion};
                        {selected, ["version"], []} ->
                            RosterVersion = sha:sha(term_to_binary(now())),
                            {atomic, {updated,1}} = odbc_queries:sql_transaction(LServer,
                                                                                 fun() ->
                                                                                         odbc_queries:set_roster_version(ejabberd_odbc:escape(LUser), RosterVersion)
                                                                                 end),

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
                                          {Items, false} -> [{xmlelement, <<"query">>, [{<<"xmlns">>, ?NS_ROSTER}], Items}];
                                          {Items, Version} -> [{xmlelement, <<"query">>, [{<<"xmlns">>, ?NS_ROSTER},
                                                                                          {<<"ver">>, Version}], Items}]
                                      end}
    catch
    	_:_ ->
            IQ#iq{type = error, sub_el = [SubEl, ?ERR_INTERNAL_SERVER_ERROR]}
    end.

get_user_roster(Acc, {LUser, LServer}) ->
    Items = get_roster(LUser, LServer),
    lists:filter(fun(#roster{subscription = none, ask = in}) ->
                         false;
                    (_) ->
                         true
                 end, Items) ++ Acc.

get_roster(LUser, LServer) ->
    Username = ejabberd_odbc:escape(LUser),
    case catch odbc_queries:get_roster(LServer, Username) of
        {selected, ["username", "jid", "nick", "subscription", "ask",
                    "askmessage", "server", "subscribe", "type"],
         Items} when is_list(Items) ->
            JIDGroups = case catch odbc_queries:get_roster_jid_groups(LServer, Username) of
                            {selected, ["jid","grp"], JGrps}
                              when is_list(JGrps) ->
                                JGrps;
                            _ ->
                                []
                        end,
            RItems = lists:flatmap(
                       fun(I) ->
                               case raw_to_record(LServer, I) of
                                   %% Bad JID in database:
                                   error ->
                                       [];
                                   R ->
                                       SJID = jlib:jid_to_binary(R#roster.jid),
                                       Groups = lists:flatmap(
                                                  fun({S, G}) when S == SJID ->
                                                          [G];
                                                     (_) ->
                                                          []
                                                  end, JIDGroups),
                                       [R#roster{groups = Groups}]
                               end
                       end, Items),
            RItems;
        _ ->
            []
    end.


item_to_xml(Item) ->
    Attrs1 = [{"jid", jlib:jid_to_binary(Item#roster.jid)}],
    Attrs2 = case Item#roster.name of
                 <<"">> ->
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
    Attrs = case ask_to_pending(Item#roster.ask) of
                out ->
                    [{<<"ask">>, <<"subscribe">>} | Attrs3];
                both ->
                    [{<<"ask">>, <<"subscribe">>} | Attrs3];
                _ ->
                    Attrs3
            end,
    SubEls = lists:map(fun(G) ->
                               {xmlelement, <<"group">>, [], [{xmlcdata, G}]}
                       end, Item#roster.groups),
    {xmlelement, <<"item">>, Attrs, SubEls}.

process_iq_set(From, To, #iq{sub_el = SubEl} = IQ) ->
    {xmlelement, _Name, _Attrs, Els} = SubEl,
    #jid{lserver = LServer} = From,
    ejabberd_hooks:run(roster_set, LServer, [From, To, SubEl]),
    lists:foreach(fun(El) -> process_item_set(From, To, El) end, Els),
    IQ#iq{type = result, sub_el = []}.

process_item_set(From, To, {xmlelement, _Name, Attrs, Els}) ->
    JID1 = jlib:binary_to_jid(xml:get_attr_s(<<"jid">>, Attrs)),
    #jid{user = User, luser = LUser, lserver = LServer} = From,
    case JID1 of
        error ->
            ok;
        _ ->
            LJID = jlib:jid_tolower(JID1),
            Username = ejabberd_odbc:escape(LUser),
            SJID = ejabberd_odbc:escape(jlib:jid_to_binary(LJID)),
            F = fun() ->
                        {selected,
                         ["username", "jid", "nick", "subscription",
                          "ask", "askmessage", "server", "subscribe", "type"],
                         Res} = odbc_queries:get_roster_by_jid(LServer, Username, SJID),
                        Item = case Res of
                                   [] ->
                                       #roster{usj = {LUser, LServer, LJID},
                                               us = {LUser, LServer},
                                               jid = LJID};
                                   [I] ->
                                       R = raw_to_record(LServer, I),
                                       case R of
                                           %% Bad JID in database:
                                           error ->
                                               #roster{usj = {LUser, LServer, LJID},
                                                       us = {LUser, LServer},
                                                       jid = LJID};
                                           _ ->
                                               R#roster{
                                                 usj = {LUser, LServer, LJID},
                                                 us = {LUser, LServer},
                                                 jid = LJID,
                                                 name = <<"">>}
                                       end
                               end,
                        Item1 = process_item_attrs(Item, Attrs),
                        Item2 = process_item_els(Item1, Els),
                        case Item2#roster.subscription of
                            remove ->
                                odbc_queries:del_roster(LServer, Username, SJID);
                            _ ->
                                ItemVals = record_to_string(Item2),
                                ItemGroups = groups_to_string(Item2),
                                odbc_queries:update_roster(LServer, Username, SJID, ItemVals, ItemGroups)
                        end,
                        %% If the item exist in shared roster, take the
                        %% subscription information from there:
                        Item3 = ejabberd_hooks:run_fold(roster_process_item,
                                                        LServer, Item2, [LServer]),
                        case roster_version_on_db(LServer) of
                            true -> odbc_queries:set_roster_version(ejabberd_odbc:escape(LUser), sha:sha(term_to_binary(now())));
                            false -> ok
                        end,
                        {Item, Item3}
                end,
            case odbc_queries:sql_transaction(LServer, F) of
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

process_item_attrs(Item, [{<<"jid">>, Val} | Attrs]) ->
    case jlib:binary_to_jid(Val) of
        error ->
            process_item_attrs(Item, Attrs);
        JID1 ->
            JID = {JID1#jid.luser, JID1#jid.lserver, JID1#jid.lresource},
            process_item_attrs(Item#roster{jid = JID}, Attrs)
    end;
process_item_attrs(Item, [{<<"name">>, Val} | Attrs]) ->
    process_item_attrs(Item#roster{name = Val}, Attrs);
process_item_attrs(Item, [{<<"subscription">>, <<"remove">>} | Attrs]) ->
    process_item_attrs(Item#roster{subscription = remove}, Attrs);
process_item_attrs(Item, [_ | Attrs]) ->
    process_item_attrs(Item, Attrs);
process_item_attrs(Item, []) ->
    Item.

process_item_els(Item, [{xmlelement, <<"group">>, _Attrs, SEls} | Els]) ->
    Groups = [xml:get_cdata(SEls) | Item#roster.groups],
    process_item_els(Item#roster{groups = Groups}, Els);
process_item_els(Item, [{xmlelement, _, _, _} | Els]) ->
    process_item_els(Item, Els);
process_item_els(Item, [{xmlcdata, _} | Els]) ->
    process_item_els(Item, Els);
process_item_els(Item, []) ->
    Item.

push_item(User, Server, From, Item) ->
    ejabberd_sm:route(jlib:make_jid(<<"">>, <<"">>, <<"">>),
                      jlib:make_jid(User, Server, <<"">>),
                      {xmlelement, <<"broadcast">>, [],
                       [{item,
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

%% TODO: don't push to those who not load roster
push_item(User, Server, Resource, From, Item) ->
    ejabberd_hooks:run(roster_push, Server, [From, Item]),
    ResIQ = #iq{type = set, xmlns = ?NS_ROSTER,
                id = list_to_binary("push" ++ randoms:get_string()),
                sub_el = [{xmlelement, <<"query">>,
                           [{<<"xmlns">>, ?NS_ROSTER}],
                           [item_to_xml(Item)]}]},
    ejabberd_router:route(
      From,
      jlib:make_jid(User, Server, Resource),
      jlib:iq_to_xml(ResIQ)).

%% @doc Roster push, calculate and include the version attribute.
%% TODO: don't push to those who didn't load roster
push_item_version(Server, User, From, Item, RosterVersion)  ->
    lists:foreach(fun(Resource) ->
                          push_item_version(User, Server, Resource, From, Item, RosterVersion)
                  end, ejabberd_sm:get_user_resources(User, Server)).

push_item_version(User, Server, Resource, From, Item, RosterVersion) ->
    IQPush = #iq{type = 'set', xmlns = ?NS_ROSTER,
                 id = list_to_binary("push" ++ randoms:get_string()),
                 sub_el = [{xmlelement, <<"query">>,
                            [{<<"xmlns">>, ?NS_ROSTER},
                             {<<"ver">>, RosterVersion}],
                            [item_to_xml(Item)]}]},
    ejabberd_router:route(
      From,
      jlib:make_jid(User, Server, Resource),
      jlib:iq_to_xml(IQPush)).

get_subscription_lists(_, User, Server) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    JID = jlib:make_jid(User, Server, <<>>),
    Username = ejabberd_odbc:escape(LUser),
    case catch odbc_queries:get_roster(LServer, Username) of
        {selected, ["username", "jid", "nick", "subscription", "ask",
                    "askmessage", "server", "subscribe", "type"],
         Items} when is_list(Items) ->
            fill_subscription_lists(JID, LServer, Items, [], [], []);
        _ ->
            {[], [], []}
    end.

fill_subscription_lists(JID, LServer, [IRaw | Is], F, T, P) ->
    I = raw_to_record(LServer, IRaw),
    J = element(3, I#roster.usj),
    NewP = case I#roster.ask of
        Ask when Ask == in; Ask == both ->
            Message = I#roster.askmessage,
            Status  = if is_binary(Message) -> Message;
                           true -> <<>>
                          end,
            [{xmlelement, <<"presence">>,
              [{<<"from">>, jlib:jid_to_binary(I#roster.jid)},
               {<<"to">>, jlib:jid_to_binary(JID)},
               {<<"type">>, <<"subscribe">>}],
              [{xmlelement, <<"status">>, [],
                [{xmlcdata, Status}]}]} | P];
        _ -> 
             P
        end,
    case I#roster.subscription of
    both ->
        fill_subscription_lists(JID, LServer, Is, [J | F], [J | T], NewP);
    from ->
        fill_subscription_lists(JID, LServer, Is, [J | F], T, NewP);
    to ->
        fill_subscription_lists(JID, LServer, Is, F, [J | T], NewP);
    _ ->
        fill_subscription_lists(JID, LServer, Is, F, T, NewP)
    end;
fill_subscription_lists(_JID, _LServer, [], F, T, P) ->
   {F, T, P}.

ask_to_pending(subscribe) -> out;
ask_to_pending(unsubscribe) -> none;
ask_to_pending(Ask) -> Ask.

in_subscription(_, User, Server, JID, Type, Reason) ->
    process_subscription(in, User, Server, JID, Type, Reason).

out_subscription(User, Server, JID, Type) ->
    process_subscription(out, User, Server, JID, Type, []).

process_subscription(Direction, User, Server, JID1, Type, Reason) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    LJID = jlib:jid_tolower(JID1),
    Username = ejabberd_odbc:escape(LUser),
    SJID = ejabberd_odbc:escape(jlib:jid_to_binary(LJID)),
    F = fun() ->
                Item =
                    case odbc_queries:get_roster_by_jid(LServer, Username, SJID) of
                        {selected,
                         ["username", "jid", "nick", "subscription", "ask",
                          "askmessage", "server", "subscribe", "type"],
                         [I]} ->
                            %% raw_to_record can return error, but
                            %% jlib_to_string would fail before this point
                            R = raw_to_record(LServer, I),
                            Groups =
                                case odbc_queries:get_roster_groups(LServer, Username, SJID) of
                                    {selected, ["grp"], JGrps} when is_list(JGrps) ->
                                        [JGrp || {JGrp} <- JGrps];
                                    _ ->
                                        []
                                end,
                            R#roster{groups = Groups};
                        {selected,
                         ["username", "jid", "nick", "subscription", "ask",
                          "askmessage", "server", "subscribe", "type"],
                         []} ->
                            #roster{usj = {LUser, LServer, LJID},
                                    us = {LUser, LServer},
                                    jid = LJID}
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
                        odbc_queries:del_roster(LServer, Username, SJID),
                        {none, AutoReply};
                    {Subscription, Pending} ->
                        NewItem = Item#roster{subscription = Subscription,
                                              ask = Pending,
                                              askmessage = AskMessage},
                        ItemVals = record_to_string(NewItem),
                        odbc_queries:roster_subscribe(LServer, Username, SJID, ItemVals),
                        case roster_version_on_db(LServer) of
                            true -> odbc_queries:set_roster_version(ejabberd_odbc:escape(LUser), sha:sha(term_to_binary(now())));
                            false -> ok
                        end,
                        {{push, NewItem}, AutoReply}
                end
        end,
    case odbc_queries:sql_transaction(LServer, F) of
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
                      {xmlelement, <<"presence">>, [{<<"type">>, T}], []})
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


remove_user(User, Server) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    Username = ejabberd_odbc:escape(LUser),
    send_unsubscription_to_rosteritems(LUser, LServer),
    odbc_queries:del_user_roster_t(LServer, Username),
    ok.

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
      {xmlelement, <<"presence">>,
       [{<<"type">>, Type}],
       []}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

set_items(User, Server, SubEl) ->
    {xmlelement, _Name, _Attrs, Els} = SubEl,
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    catch odbc_queries:sql_transaction(
            LServer,
            lists:flatmap(fun(El) ->
                                  process_item_set_t(LUser, LServer, El)
                          end, Els)).

process_item_set_t(LUser, LServer, {xmlelement, _Name, Attrs, Els}) ->
    JID1 = jlib:binary_to_jid(xml:get_attr_s(<<"jid">>, Attrs)),
    case JID1 of
        error ->
            [];
        _ ->
            LJID = {JID1#jid.luser, JID1#jid.lserver, JID1#jid.lresource},
            Username = ejabberd_odbc:escape(LUser),
            SJID = ejabberd_odbc:escape(jlib:jid_to_binary(LJID)),
            Item = #roster{usj = {LUser, LServer, LJID},
                           us = {LUser, LServer},
                           jid = LJID},
            Item1 = process_item_attrs_ws(Item, Attrs),
            Item2 = process_item_els(Item1, Els),
            case Item2#roster.subscription of
                remove ->
                    odbc_queries:del_roster_sql(Username, SJID);
                _ ->
                    ItemVals = record_to_string(Item1),
                    ItemGroups = groups_to_string(Item2),
                    odbc_queries:update_roster_sql(Username, SJID, ItemVals, ItemGroups)
            end
    end;
process_item_set_t(_LUser, _LServer, _) ->
    [].

process_item_attrs_ws(Item, [{<<"jid">>, Val} | Attrs]) ->
    case jlib:binary_to_jid(Val) of
        error ->
            process_item_attrs_ws(Item, Attrs);
        JID1 ->
            JID = {JID1#jid.luser, JID1#jid.lserver, JID1#jid.lresource},
            process_item_attrs_ws(Item#roster{jid = JID}, Attrs)
    end;
process_item_attrs_ws(Item, [{<<"name">>, Val} | Attrs]) ->
    process_item_attrs_ws(Item#roster{name = Val}, Attrs);
process_item_attrs_ws(Item, [{<<"subscription">>, <<"remove">>} | Attrs]) ->
    process_item_attrs_ws(Item#roster{subscription = remove}, Attrs);
process_item_attrs_ws(Item, [{<<"subscription">>, <<"none">>} | Attrs]) ->
    process_item_attrs_ws(Item#roster{subscription = none}, Attrs);
process_item_attrs_ws(Item, [{<<"subscription">>, <<"both">>} | Attrs]) ->
    process_item_attrs_ws(Item#roster{subscription = both}, Attrs);
process_item_attrs_ws(Item, [{<<"subscription">>, <<"from">>} | Attrs]) ->
    process_item_attrs_ws(Item#roster{subscription = from}, Attrs);
process_item_attrs_ws(Item, [{<<"subscription">>, <<"to">>} | Attrs]) ->
    process_item_attrs_ws(Item#roster{subscription = to}, Attrs);
process_item_attrs_ws(Item, [_ | Attrs]) ->
    process_item_attrs_ws(Item, Attrs);
process_item_attrs_ws(Item, []) ->
    Item.

get_in_pending_subscriptions(Ls, User, Server) ->
    JID = jlib:make_jid(User, Server, <<"">>),
    LUser = JID#jid.luser,
    LServer = JID#jid.lserver,
    Username = ejabberd_odbc:escape(LUser),
    case catch odbc_queries:get_roster(LServer, Username) of
        {selected, ["username", "jid", "nick", "subscription", "ask",
                    "askmessage", "server", "subscribe", "type"],
         Items} when is_list(Items) ->
    	    Ls ++ lists:map(
                    fun(R) ->
                            Message = R#roster.askmessage,
                            {xmlelement, <<"presence">>,
                             [{<<"from">>, jlib:jid_to_binary(R#roster.jid)},
                              {<<"to">>, jlib:jid_to_binary(JID)},
                              {<<"type">>, <<"subscribe">>}],
                             [{xmlelement, <<"status">>, [],
                               [{xmlcdata, Message}]}]}
                    end,
                    lists:flatmap(
                      fun(I) ->
                              case raw_to_record(LServer, I) of
                                  %% Bad JID in database:
                                  error ->
                                      [];
                                  R ->
                                      case R#roster.ask of
                                          in   -> [R];
                                          both -> [R];
                                          _ -> []
                                      end
                              end
                      end,
                      Items));
        _ ->
            Ls
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_jid_info(_, User, Server, JID) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    LJID = jlib:jid_tolower(JID),
    Username = ejabberd_odbc:escape(LUser),
    SJID = ejabberd_odbc:escape(jlib:jid_to_binary(LJID)),
    case catch odbc_queries:get_subscription(LServer, Username, SJID) of
        {selected, ["subscription"], [{SSubscription}]} ->
            Subscription = case SSubscription of
                               "B" -> both;
                               "T" -> to;
                               "F" -> from;
                               _ -> none
                           end,
            Groups = case catch odbc_queries:get_rostergroup_by_jid(LServer, Username, SJID) of
                         {selected, ["grp"], JGrps} when is_list(JGrps) ->
                             [JGrp || {JGrp} <- JGrps];
                         _ ->
                             []
                     end,
            {Subscription, Groups};
        _ ->
            LRJID = jlib:jid_tolower(jlib:jid_remove_resource(JID)),
            if
                LRJID == LJID ->
                    {none, []};
                true ->
                    SRJID = ejabberd_odbc:escape(jlib:jid_to_binary(LRJID)),
                    case catch odbc_queries:get_subscription(LServer, Username, SRJID) of
                        {selected, ["subscription"], [{SSubscription}]} ->
                            Subscription = case SSubscription of
                                               "B" -> both;
                                               "T" -> to;
                                               "F" -> from;
                                               _ -> none
                                           end,
                            Groups = case catch odbc_queries:get_rostergroup_by_jid(LServer, Username, SRJID) of
                                         {selected, ["grp"], JGrps} when is_list(JGrps) ->
                                             [JGrp || {JGrp} <- JGrps];
                                         _ ->
                                             []
                                     end,
                            {Subscription, Groups};
                        _ ->
                            {none, []}
                    end
            end
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

raw_to_record(LServer, {User, BJID, Nick, BSubscription, BAsk, AskMessage,
                        _Server, _Subscribe, _Type}) ->
    case jlib:binary_to_jid(BJID) of
        error ->
            error;
        JID ->
            LJID = jlib:jid_tolower(JID),
            Subscription = case BSubscription of
                               <<"B">> -> both;
                               <<"T">> -> to;
                               <<"F">> -> from;
                               _ -> none
                           end,
            Ask = case BAsk of
                      <<"S">> -> subscribe;
                      <<"U">> -> unsubscribe;
                      <<"B">> -> both;
                      <<"O">> -> out;
                      <<"I">> -> in;
                      _ -> none
                  end,
            #roster{usj = {User, LServer, LJID},
                    us = {User, LServer},
                    jid = LJID,
                    name = Nick,
                    subscription = Subscription,
                    ask = Ask,
                    askmessage = AskMessage}
    end.

record_to_string(#roster{us = {User, _Server},
                         jid = JID,
                         name = Name,
                         subscription = Subscription,
                         ask = Ask,
                         askmessage = AskMessage}) ->
    Username = ejabberd_odbc:escape(User),
    SJID = ejabberd_odbc:escape(jlib:jid_to_binary(jlib:jid_tolower(JID))),
    Nick = ejabberd_odbc:escape(Name),
    SSubscription = case Subscription of
                        both -> "B";
                        to   -> "T";
                        from -> "F";
                        none -> "N"
                    end,
    SAsk = case Ask of
               subscribe   -> "S";
               unsubscribe -> "U";
               both	   -> "B";
               out	   -> "O";
               in	   -> "I";
               none	   -> "N"
           end,
    SAskMessage = ejabberd_odbc:escape(AskMessage),
    [Username, SJID, Nick, SSubscription, SAsk, SAskMessage, "N", "", "item"].

groups_to_string(#roster{us = {User, _Server},
                         jid = JID,
                         groups = Groups}) ->
    Username = ejabberd_odbc:escape(User),
    SJID = ejabberd_odbc:escape(jlib:jid_to_binary(jlib:jid_tolower(JID))),

    %% Empty groups do not need to be converted to string to be inserted in
    %% the database
    lists:foldl(
      fun([], Acc) -> Acc;
         (Group, Acc) ->
              G = ejabberd_odbc:escape(Group),
              [[Username, SJID, G]|Acc] end, [], Groups).

webadmin_page(_, Host,
              #request{us = _US,
                       path = ["user", U, "roster"],
                       q = Query,
                       lang = Lang} = _Request) ->
    Res = user_roster(U, Host, Query, Lang),
    {stop, Res};

webadmin_page(Acc, _, _) -> Acc.

user_roster(User, Server, Query, Lang) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    US = {LUser, LServer},
    Items1 = get_roster(LUser, LServer),
    Res = user_roster_parse_query(User, Server, Items1, Query),
    Items = get_roster(LUser, LServer),
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
                 {"", _} -> "";
                 {CUser, CServer} ->
                     case lists:member(CServer, ?MYHOSTS) of
                         false -> "";
                         true -> "/admin/server/" ++ CServer ++ "/user/" ++ CUser ++ "/"
                     end
             end,
    case JIDURI of
        [] ->
            ?XAC("td", [{"class", "valign"}], jlib:jid_to_binary(RosterJID));
        URI when is_list(URI) ->
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
      UJID, JID, {xmlelement, "presence", [{"type", "subscribe"}], []}).

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
                        UJID, JID1, {xmlelement, "presence",
                                     [{"type", "subscribed"}], []}),
                      throw(submitted);
                  false ->
                      case lists:keysearch(
                             "remove" ++ ejabberd_web_admin:term_to_id(JID), 1, Query) of
                          {value, _} ->
                              UJID = jlib:make_jid(User, Server, <<>>),
                              process_iq(
                                UJID, UJID,
                                #iq{type = set,
                                    sub_el = {xmlelement, "query",
                                              [{"xmlns", ?NS_ROSTER}],
                                              [{xmlelement, "item",
                                                [{"jid", jlib:jid_to_binary(JID)},
                                                 {"subscription", "remove"}],
                                                []}]}}),
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
