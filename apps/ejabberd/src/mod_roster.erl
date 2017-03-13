%%%----------------------------------------------------------------------
%%% File    : mod_roster.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Roster management
%%% Created : 11 Dec 2002 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2013   ProcessOne
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
-xep([{xep, 237}, {version, "1.3"}]).
-xep([{xep, 83}, {version, "1.0"}]).
-xep([{xep, 93}, {version, "1.2"}]).
-behaviour(gen_mod).

-export([start/2,
         stop/1,
         process_iq/3,
         process_local_iq/3,
         get_user_roster/2,
         get_subscription_lists/3,
         get_roster/2,
         in_subscription/6,
         out_subscription/5,
         set_items/3,
         remove_user/2,
         remove_user/3,
         get_jid_info/4,
         item_to_xml/1,
         get_versioning_feature/2,
         roster_versioning_enabled/1,
         roster_version/2,
         send_unsubscription_to_rosteritems/2]).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("mod_roster.hrl").

-export_type([roster/0]).

-type roster() :: #roster{}.

-callback init(Host, Opts) -> ok when
    Host :: ejabberd:server(),
    Opts :: list().
-callback transaction(LServer, F) -> {aborted, Reason} | {atomic, Result} when
    LServer :: ejabberd:lserver(),
    F :: fun(),
    Reason :: any(),
    Result :: any().
-callback read_roster_version(LUser, LServer) -> Result when
    LUser :: ejabberd:luser(),
    LServer :: ejabberd:lserver(),
    Result :: binary() | error.
-callback write_roster_version(LUser, LServer, InTransaction, Ver) -> Result when
    LUser :: ejabberd:luser(),
    LServer :: ejabberd:lserver(),
    InTransaction :: boolean(),
    Ver :: binary(),
    Result :: any().
-callback get_roster(LUser, LServer) -> Result when
    LUser :: ejabberd:luser(),
    LServer :: ejabberd:lserver(),
    Result :: [roster()].
-callback get_roster_by_jid_t(LUser, LServer, LJid) -> Result when
    LUser :: ejabberd:luser(),
    LServer :: ejabberd:lserver(),
    LJid :: ejabberd:simple_jid(),
    Result :: term().
-callback get_subscription_lists(Acc, LUser, LServer) -> Result when
    Acc :: term(),
    LUser :: ejabberd:luser(),
    LServer :: ejabberd:lserver(),
    Result :: term().
-callback roster_subscribe_t(LUser, LServer, LJid, SJid) -> Result when
    LUser :: ejabberd:luser(),
    LServer :: ejabberd:lserver(),
    LJid :: ejabberd:simple_jid(),
    SJid :: roster(),
    Result :: term().
-callback get_roster_by_jid_with_groups_t(LUser, LServer, LJid) -> Result when
    LUser :: ejabberd:luser(),
    LServer :: ejabberd:lserver(),
    LJid :: ejabberd:simple_jid(),
    Result :: term().
-callback remove_user(LUser, LServer) -> Result when
    LUser :: ejabberd:luser(),
    LServer :: ejabberd:lserver(),
    Result :: term().
-callback update_roster_t(LUser, LServer, LJid, Item) -> Result when
    LUser :: ejabberd:luser(),
    LServer :: ejabberd:lserver(),
    LJid :: ejabberd:simple_jid(),
    Item :: roster(),
    Result :: term().
-callback del_roster_t(LUser, LServer, LJid) -> Result when
    LUser :: ejabberd:luser(),
    LServer :: ejabberd:lserver(),
    LJid :: ejabberd:simple_jid(),
    Result :: term().
-callback read_subscription_and_groups(LUser, LServer, LJid) -> Result when
    LUser :: ejabberd:luser(),
    LServer :: ejabberd:lserver(),
    LJid :: ejabberd:simple_jid(),
    Result :: term().

-callback raw_to_record(LServer, Item) -> Result when
    LServer :: ejabberd:lserver(),
    Item :: term(),
    Result :: error | roster().


-define(BACKEND, mod_roster_backend).

start(Host, Opts) ->
    IQDisc = gen_mod:get_opt(iqdisc, Opts, one_queue),
    TrackedFuns = [read_roster_version,
                   write_roster_version,
                   get_roster,
                   get_roster_by_jid_t,
                   get_subscription_lists,
                   roster_subscribe_t,
                   get_roster_by_jid_with_groups_t,
                   update_roster_t,
                   del_roster_t,
                   read_subscription_and_groups
                   ],
    gen_mod:start_backend_module(?MODULE, Opts, TrackedFuns),
    ?BACKEND:init(Host, Opts),

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
    L = [R#roster{groups = lists:sort(Grs)} ||
         R = #roster{groups = Grs} <- Items],
    sha:sha1_hex(term_to_binary(lists:sort(L))).

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

roster_version(LServer, LUser) ->
    US = {LUser, LServer},
    case roster_version_on_db(LServer) of
        true ->
            case read_roster_version(LUser, LServer) of
                error -> not_found;
                V -> V
            end;
        false ->
            roster_hash(ejabberd_hooks:run_fold(roster_get, LServer, [], [US]))
    end.

read_roster_version(LUser, LServer) ->
    ?BACKEND:read_roster_version(LUser, LServer).

write_roster_version(LUser, LServer) ->
    write_roster_version(LUser, LServer, false).

write_roster_version_t(LUser, LServer) ->
    write_roster_version(LUser, LServer, true).

write_roster_version(LUser, LServer, InTransaction) ->
    Ver = sha:sha1_hex(term_to_binary(os:timestamp())),
    ?BACKEND:write_roster_version(LUser, LServer, InTransaction, Ver),
    Ver.

%% Load roster from DB only if neccesary.
%% It is neccesary if
%%     - roster versioning is disabled in server OR
%%     - roster versioning is not used by the client OR
%%     - roster versioning is used by server and client, BUT the server isn't storing versions on db OR
%%     - the roster version from client don't match current version.
process_iq_get(From, To, #iq{sub_el = SubEl} = IQ) ->
    LServer = From#jid.lserver,
    try
        AttrVer = xml:get_tag_attr(<<"ver">>, SubEl),
        VersioningEnabled = roster_versioning_enabled(LServer),
        VersionOnDb = roster_version_on_db(LServer),
        {ItemsToSend, VersionToSend} =
        get_user_roster_based_on_version(AttrVer, VersioningEnabled, VersionOnDb,
                                         From, To),
        IQ#iq{type = result,
              sub_el = create_sub_el(ItemsToSend, VersionToSend)}
    catch
        _:_ ->
            IQ#iq{type = error,
                  sub_el = [SubEl, ?ERR_INTERNAL_SERVER_ERROR]}
    end.

get_user_roster_based_on_version({value, RequestedVersion}, true, true,
                                 From, To) ->
    LUser = From#jid.luser,
    LServer = From#jid.lserver,
    US = {LUser, LServer},
    case read_roster_version(LUser, LServer) of
        error ->
            RosterVersion = write_roster_version(LUser, LServer),
            {lists:map(fun item_to_xml/1,
                       ejabberd_hooks:run_fold(roster_get,
                                               To#jid.lserver,
                                               [],
                                               [US])),
             RosterVersion};
        RequestedVersion ->
            {false, false};
        NewVersion ->
            {lists:map(fun item_to_xml/1,
                       ejabberd_hooks:run_fold(roster_get,
                                               To#jid.lserver,
                                               [],
                                               [US])),
             NewVersion}
    end;
get_user_roster_based_on_version({value, RequestedVersion}, true, false,
                                 From, To) ->
    RosterItems =
    ejabberd_hooks:run_fold(roster_get,
                            To#jid.lserver,
                            [],
                            [{From#jid.luser, From#jid.lserver}]),
    case roster_hash(RosterItems) of
        RequestedVersion ->
            {false, false};
        New ->
            {lists:map(fun item_to_xml/1,
                       RosterItems),
             New}
    end;
get_user_roster_based_on_version(_, _, _, From, To) ->
    {lists:map(fun item_to_xml/1,
               ejabberd_hooks:run_fold(roster_get,
                                       To#jid.lserver,
                                       [],
                                       [{From#jid.luser, From#jid.lserver}])),
     false}.

create_sub_el(false, false) ->
    [];
create_sub_el(Items, false) ->
    [#xmlel{name = <<"query">>,
            attrs = [{<<"xmlns">>, ?NS_ROSTER}],
            children = Items}];
create_sub_el(Items, Version) ->
    [#xmlel{name = <<"query">>,
            attrs = [{<<"xmlns">>, ?NS_ROSTER},
                     {<<"ver">>, Version}],
            children = Items}].

get_user_roster(Acc, {LUser, LServer}) ->
    lists:filter(fun (#roster{subscription = none, ask = in}) ->
                         false;
                     (_) ->
                         true
                 end, get_roster(LUser, LServer)) ++ Acc.

get_roster(LUser, LServer) ->
    ?BACKEND:get_roster(LUser, LServer).

item_to_xml(Item) ->
    Attrs1 = [{<<"jid">>,
               jid:to_binary(Item#roster.jid)}],
    Attrs2 = case Item#roster.name of
                 <<"">> -> Attrs1;
                 Name -> [{<<"name">>, Name} | Attrs1]
             end,
    Attrs3 = case Item#roster.subscription of
                 none -> [{<<"subscription">>, <<"none">>} | Attrs2];
                 from -> [{<<"subscription">>, <<"from">>} | Attrs2];
                 to -> [{<<"subscription">>, <<"to">>} | Attrs2];
                 both -> [{<<"subscription">>, <<"both">>} | Attrs2];
                 remove -> [{<<"subscription">>, <<"remove">>} | Attrs2]
             end,
    Attrs4 = case ask_to_pending(Item#roster.ask) of
                 out -> [{<<"ask">>, <<"subscribe">>} | Attrs3];
                 both -> [{<<"ask">>, <<"subscribe">>} | Attrs3];
                 _ -> Attrs3
             end,
    SubEls1 = lists:map(fun (G) ->
                                #xmlel{name = <<"group">>, attrs = [],
                                       children = [{xmlcdata, G}]}
                        end,
                        Item#roster.groups),
    SubEls = SubEls1 ++ Item#roster.xs,
    #xmlel{name = <<"item">>, attrs = Attrs4,
           children = SubEls}.

get_roster_by_jid_t(LUser, LServer, LJID) ->
    ?BACKEND:get_roster_by_jid_t(LUser, LServer, LJID).

process_iq_set(#jid{lserver = LServer} = From, To, #iq{sub_el = SubEl} = IQ) ->
    #xmlel{children = Els} = SubEl,
    ejabberd_hooks:run(roster_set, LServer, [From, To, SubEl]),
    lists:foreach(fun(El) -> process_item_set(From, To, El) end, Els),
    IQ#iq{type = result, sub_el = []}.

process_item_set(From, To, #xmlel{attrs = Attrs} = El) ->
    JID1 = jid:from_binary(xml:get_attr_s(<<"jid">>, Attrs)),
    do_process_item_set(JID1, From, To, El);
process_item_set(_From, _To, _) -> ok.

do_process_item_set(error, _, _, _) -> ok;
do_process_item_set(JID1,
                    #jid{user = User, luser = LUser, lserver = LServer} = From,
                    To,
                    #xmlel{attrs = Attrs, children = Els}) ->
    LJID = jid:to_lower(JID1),
    F = fun () ->
                Item = get_roster_by_jid_t(LUser, LServer, LJID),
                Item1 = process_item_attrs(Item, Attrs),
                Item2 = process_item_els(Item1, Els),
                case Item2#roster.subscription of
                    remove -> del_roster_t(LUser, LServer, LJID);
                    _ -> update_roster_t(LUser, LServer, LJID, Item2)
                end,
                Item3 = ejabberd_hooks:run_fold(roster_process_item,
                                                LServer, Item2,
                                                [LServer]),
                case roster_version_on_db(LServer) of
                    true -> write_roster_version_t(LUser, LServer);
                    false -> ok
                end,
                {Item, Item3}
        end,
    case transaction(LServer, F) of
        {atomic, {OldItem, Item}} ->
            push_item(User, LServer, To, Item),
            case Item#roster.subscription of
                remove ->
                    send_unsubscribing_presence(From, OldItem), ok;
                _ -> ok
            end;
        E ->
            ?DEBUG("ROSTER: roster item set error: ~p~n", [E]), ok
    end.

process_item_attrs(Item, [{<<"jid">>, Val} | Attrs]) ->
    case jid:from_binary(Val) of
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

process_item_els(Item,
                 [#xmlel{name = Name, attrs = Attrs, children = SEls}
                  | Els]) ->
    case Name of
        <<"group">> ->
            Groups = [xml:get_cdata(SEls) | Item#roster.groups],
            process_item_els(Item#roster{groups = Groups}, Els);
        _ ->
            case xml:get_attr_s(<<"xmlns">>, Attrs) of
                <<"">> -> process_item_els(Item, Els);
                _ ->
                    XEls = [#xmlel{name = Name, attrs = Attrs,
                                   children = SEls}
                            | Item#roster.xs],
                    process_item_els(Item#roster{xs = XEls}, Els)
            end
    end;
process_item_els(Item, [{xmlcdata, _} | Els]) ->
    process_item_els(Item, Els);
process_item_els(Item, []) -> Item.

push_item(User, Server, From, Item) ->
    ejabberd_sm:route(jid:make(<<"">>, <<"">>, <<"">>),
                      jid:make(User, Server, <<"">>),
                      {broadcast, {item, Item#roster.jid, Item#roster.subscription}}),
    case roster_versioning_enabled(Server) of
        true ->
            push_item_version(Server, User, From, Item,
                              roster_version(Server, jid:nodeprep(User)));
        false ->
            lists:foreach(fun (Resource) ->
                                  push_item(User, Server, Resource, From, Item)
                          end,
                          ejabberd_sm:get_user_resources(User, Server))
    end.

push_item(User, Server, Resource, From, Item) ->
    ejabberd_hooks:run(roster_push, Server, [From, Item]),
    push_item(User, Server, Resource, From, Item, not_found).

push_item(User, Server, Resource, From, Item, RosterVersion) ->
    ExtraAttrs = case RosterVersion of
                     not_found -> [];
                     _ -> [{<<"ver">>, RosterVersion}]
                 end,
    ResIQ = #iq{type = set, xmlns = ?NS_ROSTER,
                %% @doc Roster push, calculate and include the version attribute.
                %% TODO: don't push to those who didn't load roster
                id = list_to_binary("push" ++ randoms:get_string()),
                sub_el =
                [#xmlel{name = <<"query">>,
                        attrs = [{<<"xmlns">>, ?NS_ROSTER} | ExtraAttrs],
                        children = [item_to_xml(Item)]}]},
    ejabberd_router:route(From,
                          jid:make(User, Server, Resource),
                          jlib:iq_to_xml(ResIQ)).

push_item_version(Server, User, From, Item,
                  RosterVersion) ->
    lists:foreach(fun (Resource) ->
                          push_item(User, Server, Resource, From, Item,
                                    RosterVersion)
                  end,
                  ejabberd_sm:get_user_resources(User, Server)).

get_subscription_lists(Acc, User, Server) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Server),
    Items = ?BACKEND:get_subscription_lists(Acc, LUser, LServer),
    JID = jid:make(User, Server, <<>>),
    SubLists = fill_subscription_lists(JID, LServer, Items, [], [], []),
    mongoose_acc:put(subscription_lists, SubLists, Acc).


fill_subscription_lists(JID, LServer, [#roster{} = I | Is], F, T, P) ->
    J = element(3, I#roster.usj),

    NewP = build_pending(I, JID, P),

    case I#roster.subscription of
        both ->
            fill_subscription_lists(JID, LServer, Is, [J | F], [J | T], NewP);
        from ->
            fill_subscription_lists(JID, LServer, Is, [J | F], T, NewP);
        to -> fill_subscription_lists(JID, LServer, Is, F, [J | T], NewP);
        _ -> fill_subscription_lists(JID, LServer, Is, F, T, NewP)
    end;
fill_subscription_lists(JID, LServer, [RawI | Is], F, T, P) ->
    I = ?BACKEND:raw_to_record(LServer, RawI),
    case I of
        %% Bad JID in database:
        error -> fill_subscription_lists(JID, LServer, Is, F, T, P);
        _ -> fill_subscription_lists(JID, LServer, [I | Is], F, T, P)
    end;
fill_subscription_lists(_, _LServer, [], F, T, P) -> {F, T, P}.

build_pending(#roster{ask = Ask} = I, JID, P)
  when Ask == in; Ask == both ->
    Message = I#roster.askmessage,
    Status  = if is_binary(Message) -> Message;
                 true -> <<>>
              end,
    StatusEl = #xmlel{
                  name = <<"status">>,
                  children = [#xmlcdata{content = Status}]},
    El = #xmlel{
            name = <<"presence">>,
            attrs = [{<<"from">>, jid:to_binary(I#roster.jid)},
                     {<<"to">>, jid:to_binary(JID)},
                     {<<"type">>, <<"subscribe">>}],
            children = [StatusEl]},
    [El | P];
build_pending(_, _, P) ->
    P.


ask_to_pending(subscribe) -> out;
ask_to_pending(unsubscribe) -> none;
ask_to_pending(Ask) -> Ask.

roster_subscribe_t(LUser, LServer, LJID, Item) ->
    ?BACKEND:roster_subscribe_t(LUser, LServer, LJID, Item).

transaction(LServer, F) ->
    ?BACKEND:transaction(LServer, F).

in_subscription(_, User, Server, JID, Type, Reason) ->
    process_subscription(in, User, Server, JID, Type,
        Reason).

out_subscription(Acc, User, Server, JID, Type) ->
    process_subscription(out, User, Server, JID, Type, <<"">>),
    Acc.

get_roster_by_jid_with_groups_t(LUser, LServer, LJID) ->
    ?BACKEND:get_roster_by_jid_with_groups_t(LUser, LServer, LJID).

process_subscription(Direction, User, Server, JID1, Type, Reason) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Server),
    LJID = jid:to_lower(JID1),
    F = fun () ->
                Item = get_roster_by_jid_with_groups_t(LUser, LServer,
                                                       LJID),
                NewState = case Direction of
                               out ->
                                   out_state_change(Item#roster.subscription,
                                                    Item#roster.ask, Type);
                               in ->
                                   in_state_change(Item#roster.subscription,
                                                   Item#roster.ask, Type)
                           end,
                AutoReply = case Direction of
                                out -> none;
                                in ->
                                    in_auto_reply(Item#roster.subscription,
                                                  Item#roster.ask, Type)
                            end,
                AskMessage = case NewState of
                                 {_, both} -> Reason;
                                 {_, in} -> Reason;
                                 _ -> <<"">>
                             end,
                case NewState of
                    none -> {none, AutoReply};
                    {none, none}
                      when Item#roster.subscription == none,
                           Item#roster.ask == in ->
                        del_roster_t(LUser, LServer, LJID), {none, AutoReply};
                    {Subscription, Pending} ->
                        NewItem = Item#roster{subscription = Subscription,
                                              ask = Pending,
                                              askmessage =
                                              iolist_to_binary(AskMessage)},
                        roster_subscribe_t(LUser, LServer, LJID, NewItem),
                        case roster_version_on_db(LServer) of
                            true -> write_roster_version_t(LUser, LServer);
                            false -> ok
                        end,
                        {{push, NewItem}, AutoReply}
                end
        end,
    case transaction(LServer, F) of
        {atomic, {Push, AutoReply}} ->
            case AutoReply of
                none -> ok;
                _ ->
                    T = case AutoReply of
                            subscribed -> <<"subscribed">>;
                            unsubscribed -> <<"unsubscribed">>
                        end,
                    ejabberd_router:route(jid:make(User, Server,
                                                   <<"">>),
                                          JID1,
                                          #xmlel{name = <<"presence">>,
                                                 attrs = [{<<"type">>, T}],
                                                 children = []})
            end,
            case Push of
                {push, Item} ->
                    if Item#roster.subscription == none,
                       Item#roster.ask == in ->
                           ok;
                       true ->
                           push_item(User, Server,
                                     jid:make(User, Server, <<"">>), Item)
                    end,
                    true;
                none -> false
            end;
        _ -> false
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

in_state_change(none, none, subscribe) -> {none, in};
in_state_change(none, none, subscribed) -> ?NNSD;
in_state_change(none, none, unsubscribe) -> none;
in_state_change(none, none, unsubscribed) -> none;
in_state_change(none, out, subscribe) -> {none, both};
in_state_change(none, out, subscribed) -> {to, none};
in_state_change(none, out, unsubscribe) -> none;
in_state_change(none, out, unsubscribed) -> {none, none};
in_state_change(none, in, subscribe) -> none;
in_state_change(none, in, subscribed) -> ?NISD;
in_state_change(none, in, unsubscribe) -> {none, none};
in_state_change(none, in, unsubscribed) -> none;
in_state_change(none, both, subscribe) -> none;
in_state_change(none, both, subscribed) -> {to, in};
in_state_change(none, both, unsubscribe) -> {none, out};
in_state_change(none, both, unsubscribed) -> {none, in};
in_state_change(to, none, subscribe) -> {to, in};
in_state_change(to, none, subscribed) -> none;
in_state_change(to, none, unsubscribe) -> none;
in_state_change(to, none, unsubscribed) -> {none, none};
in_state_change(to, in, subscribe) -> none;
in_state_change(to, in, subscribed) -> none;
in_state_change(to, in, unsubscribe) -> {to, none};
in_state_change(to, in, unsubscribed) -> {none, in};
in_state_change(from, none, subscribe) -> none;
in_state_change(from, none, subscribed) -> {both, none};
in_state_change(from, none, unsubscribe) -> {none, none};
in_state_change(from, none, unsubscribed) -> none;
in_state_change(from, out, subscribe) -> none;
in_state_change(from, out, subscribed) -> {both, none};
in_state_change(from, out, unsubscribe) -> {none, out};
in_state_change(from, out, unsubscribed) -> {from, none};
in_state_change(both, none, subscribe) -> none;
in_state_change(both, none, subscribed) -> none;
in_state_change(both, none, unsubscribe) -> {to, none};
in_state_change(both, none, unsubscribed) -> {from, none}.

out_state_change(none, none, subscribe) -> {none, out};
out_state_change(none, none, subscribed) -> none;
out_state_change(none, none, unsubscribe) -> none;
out_state_change(none, none, unsubscribed) -> none;
out_state_change(none, out, subscribe) ->{none, out}; %% We need to resend query (RFC3921, section 9.2)
out_state_change(none, out, subscribed) -> none;
out_state_change(none, out, unsubscribe) -> {none, none};
out_state_change(none, out, unsubscribed) -> none;
out_state_change(none, in, subscribe) -> {none, both};
out_state_change(none, in, subscribed) -> {from, none};
out_state_change(none, in, unsubscribe) -> none;
out_state_change(none, in, unsubscribed) -> {none, none};
out_state_change(none, both, subscribe) -> none;
out_state_change(none, both, subscribed) -> {from, out};
out_state_change(none, both, unsubscribe) -> {none, in};
out_state_change(none, both, unsubscribed) -> {none, out};
out_state_change(to, none, subscribe) -> none;
out_state_change(to, none, subscribed) -> {both, none};
out_state_change(to, none, unsubscribe) -> {none, none};
out_state_change(to, none, unsubscribed) -> none;
out_state_change(to, in, subscribe) -> none;
out_state_change(to, in, subscribed) -> {both, none};
out_state_change(to, in, unsubscribe) -> {none, in};
out_state_change(to, in, unsubscribed) -> {to, none};
out_state_change(from, none, subscribe) -> {from, out};
out_state_change(from, none, subscribed) -> none;
out_state_change(from, none, unsubscribe) -> none;
out_state_change(from, none, unsubscribed) -> {none, none};
out_state_change(from, out, subscribe) -> none;
out_state_change(from, out, subscribed) -> none;
out_state_change(from, out, unsubscribe) -> {from, none};
out_state_change(from, out, unsubscribed) -> {none, out};
out_state_change(both, none, subscribe) -> none;
out_state_change(both, none, subscribed) -> none;
out_state_change(both, none, unsubscribe) -> {from, none};
out_state_change(both, none, unsubscribed) -> {to, none}.

in_auto_reply(from, none, subscribe) -> subscribed;
in_auto_reply(from, out, subscribe) -> subscribed;
in_auto_reply(both, none, subscribe) -> subscribed;
in_auto_reply(none, in, unsubscribe) -> unsubscribed;
in_auto_reply(none, both, unsubscribe) -> unsubscribed;
in_auto_reply(to, in, unsubscribe) -> unsubscribed;
in_auto_reply(from, none, unsubscribe) -> unsubscribed;
in_auto_reply(from, out, unsubscribe) -> unsubscribed;
in_auto_reply(both, none, unsubscribe) -> unsubscribed;
in_auto_reply(_, _, _) -> none.

%% #rh
remove_user(Acc, User, Server) ->
    R = remove_user(User, Server),
    ?OK_OR_LOG(R),
    Acc.

remove_user(User, Server) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Server),
    send_unsubscription_to_rosteritems(LUser, LServer),
    ?BACKEND:remove_user(LUser, LServer).

%% For each contact with Subscription:
%% Both or From, send a "unsubscribed" presence stanza;
%% Both or To, send a "unsubscribe" presence stanza.
send_unsubscription_to_rosteritems(LUser, LServer) ->
    RosterItems = get_user_roster([], {LUser, LServer}),
    From = jid:make({LUser, LServer, <<"">>}),
    lists:foreach(fun (RosterItem) ->
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
           send_presence_type(jid:to_bare(From),
                              jid:make(Item#roster.jid),
                              <<"unsubscribe">>);
       true -> ok
    end,
    if IsFrom ->
           send_presence_type(jid:to_bare(From),
                              jid:make(Item#roster.jid),
                              <<"unsubscribed">>);
       true -> ok
    end,
    ok.

send_presence_type(From, To, Type) ->
    ejabberd_router:route(From, To,
                          #xmlel{name = <<"presence">>,
                                 attrs = [{<<"type">>, Type}], children = []}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

set_items(User, Server, SubEl) ->
    #xmlel{children = Els} = SubEl,
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Server),
    F = fun () ->
                lists:foreach(fun (El) ->
                                      process_item_set_t(LUser, LServer, El)
                              end,
                              Els)
        end,
    transaction(LServer, F).

update_roster_t(LUser, LServer, LJID, Item) ->
    ?BACKEND:update_roster_t(LUser, LServer, LJID, Item).

del_roster_t(LUser, LServer, LJID) ->
    ?BACKEND:del_roster_t(LUser, LServer, LJID).

process_item_set_t(LUser, LServer,
                   #xmlel{attrs = Attrs, children = Els}) ->
    JID1 = jid:from_binary(xml:get_attr_s(<<"jid">>, Attrs)),
    case JID1 of
        error -> ok;
        _ ->
            JID = {JID1#jid.user, JID1#jid.server,
                   JID1#jid.resource},
            LJID = {JID1#jid.luser, JID1#jid.lserver,
                    JID1#jid.lresource},
            Item = #roster{usj = {LUser, LServer, LJID},
                           us = {LUser, LServer}, jid = JID},
            Item1 = process_item_attrs_ws(Item, Attrs),
            Item2 = process_item_els(Item1, Els),
            case Item2#roster.subscription of
                remove -> del_roster_t(LUser, LServer, LJID);
                _ -> update_roster_t(LUser, LServer, LJID, Item2)
            end
    end;
process_item_set_t(_LUser, _LServer, _) -> ok.

process_item_attrs_ws(Item, [{<<"jid">>, Val} | Attrs]) ->
    case jid:from_binary(Val) of
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

read_subscription_and_groups(User, Server, LJID) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Server),
    ?BACKEND:read_subscription_and_groups(LUser, LServer, LJID).

get_jid_info(_, User, Server, JID) ->
    LJID = jid:to_lower(JID),
    case read_subscription_and_groups(User, Server, LJID) of
        {Subscription, Groups} -> {Subscription, Groups};
        error ->
            LRJID = jid:to_lower(jid:to_bare(JID)),
            if LRJID == LJID -> {none, []};
               true ->
                   case read_subscription_and_groups(User, Server, LRJID)
                   of
                       {Subscription, Groups} -> {Subscription, Groups};
                       error -> {none, []}
                   end
            end
    end.
