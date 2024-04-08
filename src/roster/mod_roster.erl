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
%%% Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
%%%
%%%----------------------------------------------------------------------

%%% @doc Roster management.
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
-behaviour(mongoose_module_metrics).

-export([start/2,
         stop/1,
         hooks/1,
         config_spec/0,
         supported_features/0,
         process_iq/5,
         get_roster_entry/4,
         item_to_map/1,
         set_roster_entry/5,
         remove_from_roster/3,
         item_to_xml/1
        ]).

% Hook handlers
-export([
         get_user_roster/3,
         in_subscription/3,
         out_subscription/3,
         get_subscription_lists/3,
         get_jid_info/3,
         remove_user/3,
         remove_domain/3,
         get_versioning_feature/3,
         get_personal_data/3
        ]).

-export([set_items/3,
         transaction/2,
         process_subscription_t/6,
         get_user_rosters_length/2]). % for testing

-export([config_metrics/1]).

-ignore_xref([get_user_rosters_length/2,
              item_to_xml/1,
              process_subscription_t/6,
              set_items/3,
              transaction/2
              ]).

-include("mongoose.hrl").
-include("jlib.hrl").
-include("mod_roster.hrl").
-include("mongoose_config_spec.hrl").

-type roster() :: #roster{}.

-type sub_presence() :: subscribe | subscribed | unsubscribe | unsubscribed.

-type subscription_state() :: none | from | to | both | remove.

-type get_user_roster_strategy() :: db_versioning | hash_versioning | no_versioning.

%% Types used in the backend API

-type contact() :: jid:ljid().
-type transaction_state() :: in_transaction | no_transaction.
-type entry_format() :: full | short.
-type version() :: binary().

-export_type([roster/0, sub_presence/0, subscription_state/0]).
-export_type([contact/0, transaction_state/0, entry_format/0, version/0]).

%%--------------------------------------------------------------------
%% gdpr callback
%%--------------------------------------------------------------------

-spec get_personal_data(Acc, Params, Extra) -> {ok, Acc} when
      Acc :: gdpr:personal_data(),
      Params :: #{jid := jid:jid()},
      Extra :: gen_hook:extra().
get_personal_data(Acc, #{jid := #jid{luser = LUser, lserver = LServer}}, #{host_type := HostType}) ->
    Schema = ["jid", "name", "subscription", "ask", "groups", "askmessage", "xs"],
    Records = get_roster(HostType, LUser, LServer),
    SerializedRecords = lists:map(fun roster_record_to_gdpr_entry/1, Records),
    {ok, [{roster, Schema, SerializedRecords} | Acc]}.

-spec roster_record_to_gdpr_entry(roster()) -> gdpr:entry().
roster_record_to_gdpr_entry(#roster{ jid = JID, name = Name,
                                     subscription = Subscription, ask = Ask, groups = Groups,
                                     askmessage = AskMessage, xs = XS }) ->
    [
     jid:to_binary(JID),
     Name,
     atom_to_binary(Subscription, utf8),
     atom_to_binary(Ask, utf8),
     string:join([ unicode:characters_to_list(G) || G <- Groups ], ", "),
     AskMessage,
     << (exml:to_binary(X)) || X <- XS >>
    ].

%%--------------------------------------------------------------------
%% mod_roster's callbacks
%%--------------------------------------------------------------------

-spec start(mongooseim:host_type(), gen_mod:module_opts()) -> any().
start(HostType, Opts = #{iqdisc := IQDisc}) ->
    mod_roster_backend:init(HostType, Opts),
    gen_iq_handler:add_iq_handler_for_domain(HostType, ?NS_ROSTER, ejabberd_sm,
                                             fun ?MODULE:process_iq/5, #{}, IQDisc).

-spec stop(mongooseim:host_type()) -> any().
stop(HostType) ->
    gen_iq_handler:remove_iq_handler_for_domain(HostType, ?NS_ROSTER, ejabberd_sm).

-spec config_spec() -> mongoose_config_spec:config_section().
config_spec() ->
    #section{
       items = #{<<"iqdisc">> => mongoose_config_spec:iqdisc(),
                 <<"versioning">> => #option{type = boolean},
                 <<"store_current_id">> => #option{type = boolean},
                 <<"backend">> => #option{type = atom,
                                          validate = {module, mod_roster}}
                },
       defaults = #{<<"iqdisc">> => one_queue,
                    <<"versioning">> => false,
                    <<"store_current_id">> => false,
                    <<"backend">> => mnesia}
      }.

supported_features() -> [dynamic_domains].

hooks(HostType) ->
    [{roster_get, HostType, fun ?MODULE:get_user_roster/3, #{}, 50},
     {roster_in_subscription, HostType, fun ?MODULE:in_subscription/3, #{}, 50},
     {roster_out_subscription, HostType, fun ?MODULE:out_subscription/3, #{}, 50},
     {roster_get_subscription_lists, HostType, fun ?MODULE:get_subscription_lists/3, #{}, 50},
     {roster_get_jid_info, HostType, fun ?MODULE:get_jid_info/3, #{}, 50},
     {remove_user, HostType, fun ?MODULE:remove_user/3, #{}, 50},
     {remove_domain, HostType, fun ?MODULE:remove_domain/3, #{}, 50},
     {anonymous_purge_hook, HostType, fun ?MODULE:remove_user/3, #{}, 50},
     {roster_get_versioning_feature, HostType, fun ?MODULE:get_versioning_feature/3, #{}, 50},
     {get_personal_data, HostType, fun ?MODULE:get_personal_data/3, #{}, 50}].

-spec process_iq(mongoose_acc:t(), jid:jid(), jid:jid(), jlib:iq(), map()) ->
    {mongoose_acc:t(), jlib:iq()}.
process_iq(Acc, From = #jid{lserver = LServer, luser = LUser},
           To = #jid{lserver = LServer, luser = LUser}, IQ, _Extra) ->
    process_local_iq(Acc, From, To, IQ);
process_iq(Acc, _From, _To, IQ = #iq{lang = Lang, sub_el = SubEl}, _Extra) ->
    %% Error type 'forbidden' is specified in Section 2.3.3 of RFC6121 for iq set.
    %% The behaviour is unspecified for iq get, but it makes sense to handle them consistently.
    ErrorMsg = <<"It is forbidden to query the roster of another user">>,
    ErrorEl = mongoose_xmpp_errors:forbidden(Lang, ErrorMsg),
    {Acc, IQ#iq{type = error, sub_el = [SubEl, ErrorEl]}}.

-spec process_local_iq(mongoose_acc:t(), jid:jid(), jid:jid(), jlib:iq()) ->
    {mongoose_acc:t(), jlib:iq()}.
process_local_iq(Acc, From, To, #iq{type = Type} = IQ) ->
    HostType = mongoose_acc:host_type(Acc),
    IQReply = case Type of
                  set -> process_iq_set(HostType, From, To, IQ);
                  get -> process_iq_get(HostType, From, To, IQ)
              end,
    {Acc, IQReply}.

roster_hash(Items) ->
    L = [R#roster{groups = lists:sort(Grs)} ||
         R = #roster{groups = Grs} <- Items],
    mongoose_bin:encode_crypto(term_to_binary(lists:sort(L))).

-spec roster_versioning_enabled(mongooseim:host_type()) -> boolean().
roster_versioning_enabled(HostType) ->
    gen_mod:get_module_opt(HostType, ?MODULE, versioning, false).

-spec roster_version_on_db(mongooseim:host_type()) -> boolean().
roster_version_on_db(HostType) ->
    gen_mod:get_module_opt(HostType, ?MODULE, store_current_id, false).

%% Returns a list that may contain an xmlel with the XEP-237 feature if it's enabled.
-spec get_versioning_feature(Acc, Params, Extra) -> {ok, Acc} when
    Acc :: [exml:element()],
    Params :: map(),
    Extra :: gen_hook:extra().
get_versioning_feature(Acc, _, #{host_type := HostType}) ->
    NewAcc = case roster_versioning_enabled(HostType) of
        true ->
            Feature = #xmlel{name = <<"ver">>,
                             attrs = [{<<"xmlns">>, ?NS_ROSTER_VER}]},
            [Feature | Acc];
        false -> []
    end,
    {ok, NewAcc}.

roster_version(HostType, #jid{luser = LUser, lserver = LServer} = JID) ->
    case roster_version_on_db(HostType) of
        true ->
            case read_roster_version(HostType, LUser, LServer) of
                error -> not_found;
                V -> V
            end;
        false ->
            R = get_roster_old(HostType, JID),
            roster_hash(R)
    end.

%% Load roster from DB only if neccesary.
%% It is neccesary if
%%     - roster versioning is disabled in server OR
%%     - roster versioning is not used by the client OR
%%     - roster versioning is used by server and client,
%%       BUT the server isn't storing versions on db OR
%%     - the roster version from client don't match current version.
-spec process_iq_get(mongooseim:host_type(), jid:jid(), jid:jid(), jlib:iq()) -> jlib:iq().
process_iq_get(HostType, From, To, #iq{sub_el = SubEl} = IQ) ->
    AttrVer = exml_query:attr(SubEl, <<"ver">>), %% type binary() | undefined
    VersioningRequested = is_binary(AttrVer),
    VersioningEnabled = roster_versioning_enabled(HostType),
    VersionOnDb = roster_version_on_db(HostType),
    Strategy = choose_get_user_roster_strategy(VersioningRequested, VersioningEnabled, VersionOnDb),
    {ItemsToSend, VersionToSend} =
        get_user_roster_based_on_version(HostType, Strategy, AttrVer, From, To),
    IQ#iq{type = result, sub_el = create_sub_el(ItemsToSend, VersionToSend)}.

-spec choose_get_user_roster_strategy(VersioningRequested :: boolean(),
                                      VersioningEnabled :: boolean(),
                                      VersionOnDb :: boolean()) ->
    get_user_roster_strategy().
choose_get_user_roster_strategy(true, true, true) -> db_versioning;
choose_get_user_roster_strategy(true, true, false) -> hash_versioning;
choose_get_user_roster_strategy(_, _, _) -> no_versioning.

get_user_roster_based_on_version(HostType, db_versioning, RequestedVersion, From, To) ->
    get_user_roster_db_versioning(HostType, RequestedVersion, From, To);
get_user_roster_based_on_version(HostType, hash_versioning, RequestedVersion, From, To) ->
    get_user_roster_hash_versioning(HostType, RequestedVersion, From, To);
get_user_roster_based_on_version(HostType, no_versioning, _RequestedVersion, From, To) ->
    get_user_roster_no_versioning(HostType, From, To).

get_user_roster_db_versioning(HostType, RequestedVersion, From, To)
    when is_binary(RequestedVersion) ->
    LUser = From#jid.luser,
    LServer = From#jid.lserver,
    case read_roster_version(HostType, LUser, LServer) of
        error ->
            RosterVersion = write_roster_version(HostType, LUser, LServer),
            {lists:map(fun item_to_xml/1,
                       get_roster_old(HostType, To#jid.lserver, From)),
             RosterVersion};
        RequestedVersion ->
            {false, false};
        NewVersion ->
            {lists:map(fun item_to_xml/1,
                       get_roster_old(HostType, To#jid.lserver, From)),
             NewVersion}
    end.

get_user_roster_hash_versioning(HostType, RequestedVersion, From, To)
    when is_binary(RequestedVersion) ->
    RosterItems = get_roster_old(HostType, To#jid.lserver, From),
    case roster_hash(RosterItems) of
        RequestedVersion ->
            {false, false};
        New ->
            {lists:map(fun item_to_xml/1, RosterItems), New}
    end.

get_user_roster_no_versioning(HostType, From, To) ->
    {lists:map(fun item_to_xml/1,
               get_roster_old(HostType, To#jid.lserver, From)),
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

-spec get_user_roster(Acc, Params, Extra) -> {ok, Acc} when
    Acc :: [roster()],
    Params :: #{show_full_roster := boolean(), jid := jid:jid()},
    Extra :: gen_hook:extra().
get_user_roster(Items, #{show_full_roster := false, jid := JID}, #{host_type := HostType}) ->
    NewItems = do_get_user_roster(HostType, JID),
    {ok, Items ++ NewItems};
get_user_roster(Items, #{show_full_roster := true, jid := JID}, #{host_type := HostType}) ->
    NewItems = do_get_user_full_roster(HostType, JID),
    {ok, Items ++ NewItems}.

-spec do_get_user_full_roster(mongooseim:host_type(), jid:jid()) -> [roster()].
do_get_user_full_roster(HostType, #jid{luser = LUser, lserver = LServer}) ->
    get_roster(HostType, LUser, LServer).

-spec do_get_user_roster(mongooseim:host_type(), jid:jid()) -> [roster()].
do_get_user_roster(HostType, #jid{luser = LUser, lserver = LServer}) ->
    Roster = get_roster(HostType, LUser, LServer),
    Fun = fun(#roster{subscription = S, ask = A}) -> not (none =:= S andalso in =:= A) end,
    lists:filter(Fun, Roster).

-spec item_to_xml(roster()) -> exml:element().
item_to_xml(Item) ->
    Attrs0 = [{<<"jid">>, jid:to_binary(Item#roster.jid)},
              {<<"subscription">>, subs_to_binary(Item#roster.subscription)}],
    Attrs1 = maybe_append_ask(Attrs0, Item),
    Attrs2 = maybe_append_name(Attrs1, Item),
    Fold = fun(G, Acc) -> [group_el(G) | Acc] end,
    SubEls = lists:foldl(Fold, Item#roster.xs, Item#roster.groups),
    #xmlel{name = <<"item">>, attrs = Attrs2, children = SubEls}.

-spec maybe_append_name([exml:attr()], roster()) -> [exml:attr()].
maybe_append_name(Attrs, #roster{name = <<>>}) ->
    Attrs;
maybe_append_name(Attrs, #roster{name = Name}) ->
    [{<<"name">>, Name} | Attrs].

-spec maybe_append_ask([exml:attr()], roster()) -> [exml:attr()].
maybe_append_ask(Attrs, #roster{ask = Ask}) when Ask =:= subscribe; Ask =:= out; Ask =:= both ->
    [{<<"ask">>, <<"subscribe">>} | Attrs];
maybe_append_ask(Attrs, _) ->
    Attrs.

-spec group_el(binary()) -> exml:element().
group_el(Name) ->
    #xmlel{name = <<"group">>, children = [#xmlcdata{content = Name}]}.

-spec process_iq_set(mongooseim:host_type(), jid:jid(), jid:jid(), jlib:iq()) -> jlib:iq().
process_iq_set(HostType, From, To, #iq{sub_el = SubEl} = IQ) ->
    #xmlel{children = Els} = SubEl,
    mongoose_hooks:roster_set(HostType, From, To, SubEl),
    lists:foreach(fun(El) -> process_item_set(HostType, From, To, El) end, Els),
    IQ#iq{type = result, sub_el = []}.

-spec process_item_set(mongooseim:host_type(), jid:jid(), jid:jid(), exml:element()) -> ok.
process_item_set(HostType, From, To, El) ->
    Jid = jid:from_binary(exml_query:attr(El, <<"jid">>)),
    do_process_item_set(HostType, From, To, El, Jid).

-spec do_process_item_set(
        mongooseim:host_type(), jid:jid(), jid:jid(), exml:element(), error | jid:jid()) -> ok.
do_process_item_set(_, _, _, _, error) -> ok;
do_process_item_set(HostType, From, To, #xmlel{attrs = Attrs, children = Els}, JID1) ->
    MakeItem2 = fun(Item) ->
                    Item1 = process_item_attrs(Item, Attrs),
                    process_item_els(Item1, Els)
                end,
    set_roster_item(HostType, JID1, From, To, MakeItem2),
    ok.

%% @doc this is run when a roster item is to be added, updated or removed
%% the interface of this func could probably be a bit simpler
-spec set_roster_item(mongooseim:host_type(),
                      ContactJID :: jid:jid(),
                      From :: jid:jid(),
                      To :: jid:jid(),
                      fun((roster()) -> roster())) -> ok | {error, any()}.
set_roster_item(HostType, ContactJID, From, To, MakeItem) ->
    ContactLJID = jid:to_lower(ContactJID),
    F = fun() -> set_roster_item_t(HostType, From, ContactLJID, MakeItem) end,
    case transaction(HostType, F) of
        {atomic, does_not_exist} ->
            {error, does_not_exist};
        {atomic, {OldItem, NewItem}} ->
            push_item(HostType, From, To, NewItem),
            case NewItem#roster.subscription of
                remove ->
                    send_unsubscribing_presence(From, OldItem),
                    ok;
                _ -> ok
            end;
        E ->
            ?LOG_ERROR(#{what => roster_set_item_failed, reason => E}),
            {error, E}
    end.

-spec set_roster_item_t(mongooseim:host_type(), jid:jid(), contact(),
                        fun((roster()) -> roster())) ->
          does_not_exist | {roster(), roster()}.
set_roster_item_t(HostType, UserJid = #jid{lserver = LServer},
                  ContactLJID, MakeItem) ->
    InitialItem = get_roster_entry_t(HostType, UserJid, ContactLJID, short),
    Item1 = case InitialItem of
                does_not_exist -> new_roster_item(UserJid, ContactLJID);
                Item -> Item
            end,
    Item2 = MakeItem(Item1),
    case {InitialItem, Item2} of
        {does_not_exist, #roster{subscription = remove}} ->
            %% Cannot remove a non-existing item
            does_not_exist;
        _ ->
            case Item2#roster.subscription of
                remove -> del_roster_t(HostType, UserJid, ContactLJID);
                _ -> update_roster_t(HostType, Item2)
            end,
            Item3 = mongoose_hooks:roster_process_item(HostType, LServer, Item2),
            case roster_version_on_db(HostType) of
                true -> write_roster_version_t(HostType, UserJid);
                false -> ok
            end,
            {Item1, Item3}
    end.

-spec new_roster_item(jid:jid(), jid:simple_jid()) -> roster().
new_roster_item(UserJid, ContactLJID) ->
    #roster{usj = {jid:to_lus(UserJid), ContactLJID},
            us = jid:to_lus(UserJid),
            jid = ContactLJID}.

process_item_attrs(Item, [{<<"jid">>, Val} | Attrs]) ->
    case jid:from_binary(Val) of
        error ->
            process_item_attrs(Item, Attrs);
        JID1 ->
            JID = jid:to_lower(JID1),
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

process_item_els(Item, [#xmlel{name = Name} = El | Els]) ->
    case Name of
        <<"group">> ->
            Groups = [exml_query:cdata(El) | Item#roster.groups],
            process_item_els(Item#roster{groups = Groups}, Els);
        _ ->
            case exml_query:attr(El, <<"xmlns">>, <<>>) of
                <<>> -> process_item_els(Item, Els);
                _ ->
                    XEls = [El | Item#roster.xs],
                    process_item_els(Item#roster{xs = XEls}, Els)
            end
    end;
process_item_els(Item, [#xmlcdata{} | Els]) ->
    process_item_els(Item, Els);
process_item_els(Item, []) -> Item.

push_item(HostType, JID, From, Item) ->
    broadcast_item(JID, Item#roster.jid, Item#roster.subscription),
    case roster_versioning_enabled(HostType) of
        true ->
            push_item_version(JID, From, Item, roster_version(HostType, JID));
        false ->
            lists:foreach(fun(Resource) ->
                                  push_item_without_version(HostType, JID, Resource, From, Item)
                          end,
                          ejabberd_sm:get_user_resources(JID))
    end.

-spec broadcast_item(jid:jid(), contact(), subscription_state()) -> ok.
broadcast_item(#jid{luser = LUser, lserver = LServer}, ContactJid, Subscription) ->
    Item = {item, ContactJid, Subscription},
    UserPids = ejabberd_sm:get_user_present_pids(LUser, LServer),
    lists:foreach(fun({_, Pid}) -> mongoose_c2s:cast(Pid, ?MODULE, Item) end, UserPids).

push_item_without_version(HostType, JID, Resource, From, Item) ->
    mongoose_hooks:roster_push(HostType, From, Item),
    push_item_final(jid:replace_resource(JID, Resource), From, Item, not_found).

push_item_version(JID, From, Item, RosterVersion) ->
    lists:foreach(fun(Resource) ->
                          push_item_final(jid:replace_resource(JID, Resource),
                                          From, Item, RosterVersion)
                  end, ejabberd_sm:get_user_resources(JID)).

push_item_final(JID, From, Item, RosterVersion) ->
    ExtraAttrs = case RosterVersion of
                     not_found -> [];
                     _ -> [{<<"ver">>, RosterVersion}]
                 end,
    ResIQ = #iq{type = set, xmlns = ?NS_ROSTER,
                %% @doc Roster push, calculate and include the version attribute.
                %% TODO: don't push to those who didn't load roster
                id = <<"push", (mongoose_bin:gen_from_crypto())/binary>>,
                sub_el =
                [#xmlel{name = <<"query">>,
                        attrs = [{<<"xmlns">>, ?NS_ROSTER} | ExtraAttrs],
                        children = [item_to_xml(Item)]}]},
    ejabberd_router:route(From, JID, jlib:iq_to_xml(ResIQ)).

-spec get_subscription_lists(Acc, Params, Extra) -> {ok, Acc} when
    Acc :: mongoose_acc:t(),
    Params :: #{jid := jid:jid()},
    Extra :: gen_hook:extra().
get_subscription_lists(Acc, #{jid := #jid{luser = LUser, lserver = LServer} = JID}, _) ->
    Items = mod_roster_backend:get_subscription_lists(Acc, LUser, LServer),
    SubLists = fill_subscription_lists(JID, LServer, Items, [], [], []),
    {ok, mongoose_acc:set(roster, subscription_lists, SubLists, Acc)}.

fill_subscription_lists(JID, LServer, [#roster{} = I | Is], F, T, P) ->
    J = I#roster.jid,
    NewP = build_pending(I, JID, P),
    case I#roster.subscription of
        both ->
            fill_subscription_lists(JID, LServer, Is, [J | F], [J | T], NewP);
        from ->
            fill_subscription_lists(JID, LServer, Is, [J | F], T, NewP);
        to -> fill_subscription_lists(JID, LServer, Is, F, [J | T], NewP);
        _ -> fill_subscription_lists(JID, LServer, Is, F, T, NewP)
    end;
fill_subscription_lists(_, _LServer, [], F, T, P) -> {F, T, P}.

build_pending(#roster{ask = Ask} = I, JID, P)
  when Ask == in; Ask == both ->
    Status = case I#roster.askmessage of
                 Message when is_binary(Message) -> Message;
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

-spec subs_to_binary(subscription_state()) -> binary().
subs_to_binary(none) -> <<"none">>;
subs_to_binary(from) -> <<"from">>;
subs_to_binary(to) -> <<"to">>;
subs_to_binary(both) -> <<"both">>;
subs_to_binary(remove) -> <<"remove">>.

-spec in_subscription(Acc, Params, Extra) -> {ok, Acc} when
    Acc :: mongoose_acc:t(),
    Params :: #{to := jid:jid(),
                from := jid:jid(),
                type := sub_presence(),
                reason := iodata()},
    Extra :: gen_hook:extra().
in_subscription(Acc,
                #{to := ToJID, from := FromJID, type := Type, reason := Reason},
                #{host_type := HostType}) ->
    Res = process_subscription(HostType, in, ToJID, FromJID, Type, Reason),
    {ok, mongoose_acc:set(hook, result, Res, Acc)}.

-spec out_subscription(Acc, Params, Extra) -> {ok, Acc} when
    Acc :: mongoose_acc:t(),
    Params :: #{to := jid:jid(),
                from := jid:jid(),
                type := sub_presence()},
    Extra :: gen_hook:extra().
out_subscription(Acc,
                #{to := ToJID, from := FromJID, type := Type},
                #{host_type := HostType}) ->
    Res = process_subscription(HostType, out, FromJID, ToJID, Type, <<>>),
    {ok, mongoose_acc:set(hook, result, Res, Acc)}.

-spec process_subscription(mongooseim:host_type(), in | out, jid:jid(), jid:jid(),
                           sub_presence(), iodata()) ->
          boolean().
process_subscription(HostType, Direction, JID, ContactJID, Type, Reason) ->
    TransactionFun =
        fun() ->
                process_subscription_t(HostType, Direction, JID, ContactJID, Type, Reason)
        end,
    case transaction(HostType, TransactionFun) of
        {atomic, {Push, AutoReply}} ->
            case AutoReply of
                none -> ok;
                _ ->
                    PresenceStanza = #xmlel{name = <<"presence">>,
                                            attrs = [{<<"type">>, autoreply_to_type(AutoReply)}],
                                            children = []},
                    ejabberd_router:route(JID, ContactJID, PresenceStanza)
            end,
            case Push of
                {push, #roster{subscription = none, ask = in}} ->
                    true;
                {push, Item} ->
                    push_item(HostType, JID, JID, Item),
                    true;
                none -> false
            end;
        _ -> false
    end.

autoreply_to_type(subscribed) -> <<"subscribed">>;
autoreply_to_type(unsubscribed) -> <<"unsubscribed">>.

-spec process_subscription_t(mongooseim:host_type(), in | out, jid:jid(), jid:jid(),
                             sub_presence(), iodata()) ->
          {Push :: none | {push, roster()},
           AutoReply :: none | subscribed | unsubscribed}.
process_subscription_t(HostType, Direction, JID, ContactJID, Type, Reason) ->
    US = jid:to_lus(JID),
    ContactLJID = jid:to_lower(ContactJID),
    Item = case get_roster_entry_t(HostType, JID, ContactLJID, full) of
               does_not_exist ->
                   #roster{usj = {US, ContactLJID},
                           us = US,
                           jid = ContactLJID};
               R -> R
           end,
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
            del_roster_t(HostType, JID, ContactLJID), {none, AutoReply};
        {Subscription, Pending} ->
            NewItem = Item#roster{subscription = Subscription,
                                  ask = Pending,
                                  askmessage = iolist_to_binary(AskMessage)},
            roster_subscribe_t(HostType, NewItem),
            case roster_version_on_db(HostType) of
                true -> write_roster_version_t(HostType, JID);
                false -> ok
            end,
            {{push, NewItem}, AutoReply}
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
out_state_change(none, out, subscribe) ->
    {none, out}; %% We need to resend query (RFC3921, section 9.2)
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

get_user_rosters_length(HostType, JID) ->
    length(get_roster_old(HostType, JID)).

-spec remove_user(Acc, Params, Extra) -> {ok, Acc} when
      Acc :: mongoose_acc:t(),
      Params :: #{jid := jid:jid()},
      Extra :: gen_hook:extra().
remove_user(Acc, #{jid := #jid{luser = LUser, lserver = LServer} = JID}, #{host_type := HostType}) ->
    Acc1 = try_send_unsubscription_to_rosteritems(Acc, JID),
    F = fun() -> mod_roster_backend:remove_user_t(HostType, LUser, LServer) end,
    case transaction(HostType, F) of
        {atomic, ok} ->
            ok;
        Result ->
            ?LOG_ERROR(#{what => remove_user_transaction_failed, reason => Result})
    end,
    {ok, Acc1}.

-spec try_send_unsubscription_to_rosteritems(mongoose_acc:t(), jid:jid()) ->
    mongoose_acc:t().
try_send_unsubscription_to_rosteritems(Acc, JID) ->
    try
        send_unsubscription_to_rosteritems(Acc, JID)
    catch
        E:R:S ->
            ?LOG_WARNING(#{what => roster_unsubcribe_failed,
                           class => E, reason => R, stacktrace => S}),
            Acc
    end.

%% For each contact with Subscription:
%% Both or From, send a "unsubscribed" presence stanza;
%% Both or To, send a "unsubscribe" presence stanza.
-spec send_unsubscription_to_rosteritems(mongoose_acc:t(), jid:jid()) -> mongoose_acc:t().
send_unsubscription_to_rosteritems(Acc, JID) ->
    RosterItems = do_get_user_roster(mongoose_acc:host_type(Acc), JID),
    lists:foreach(fun(RosterItem) ->
                          send_unsubscribing_presence(JID, RosterItem)
                  end, RosterItems),
    Acc.

-spec send_unsubscribing_presence(From :: jid:jid(), Item :: roster()) -> ok | mongoose_acc:t().
send_unsubscribing_presence(From, #roster{ subscription = Subscription } = Item) ->
    BareFrom = jid:to_bare(From),
    ContactJID = jid:make_noprep(Item#roster.jid),
    IsTo = Subscription == both orelse Subscription == to,
    IsFrom = Subscription == both orelse Subscription == from,
    case IsTo of
        true -> send_presence_type(BareFrom, ContactJID, <<"unsubscribe">>);
        _ -> ok
    end,
    case IsFrom of
        true -> send_presence_type(BareFrom, ContactJID, <<"unsubscribed">>);
        _ -> ok
    end.

send_presence_type(From, To, Type) ->
    ejabberd_router:route(From, To,
                          #xmlel{name = <<"presence">>,
                                 attrs = [{<<"type">>, Type}], children = []}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec remove_domain(Acc, Params, Extra) -> {ok , Acc} when
      Acc :: mongoose_domain_api:remove_domain_acc(),
      Params :: #{domain := jid:lserver()},
      Extra :: gen_hook:extra().
remove_domain(Acc, #{domain := Domain}, #{host_type := HostType}) ->
    case mongoose_lib:is_exported(mod_roster_backend, remove_domain_t, 2) of
         true ->
            F = fun() -> mod_roster_backend:remove_domain_t(HostType, Domain) end,
            case transaction(HostType, F) of
                {atomic, ok} ->
                    ok;
                Result ->
                    ?LOG_ERROR(#{what => remove_domain_transaction_failed,
                                 reason => Result})
            end;
        false ->
            ok
    end,
    {ok, Acc}.

-spec set_items(mongooseim:host_type(), jid:jid(), exml:element()) -> ok | {error, any()}.
set_items(HostType, Jid, SubEl) ->
    F = fun() -> set_items_t(HostType, Jid, SubEl) end,
    case transaction(HostType, F) of
        {atomic, _} ->
            ok;
        Result ->
            {error, Result}
    end.

set_items_t(HostType, JID, #xmlel{children = Els}) ->
    lists:foreach(fun(El) ->
                          process_item_set_t(HostType, JID, El)
                  end, Els).

%% @doc add a contact to roster, or update
-spec set_roster_entry(mongooseim:host_type(), jid:jid(), jid:jid(), binary(), [binary()]) ->
          ok | {error, any()}.
set_roster_entry(HostType, UserJid, ContactJid, Name, Groups) ->
    UpdateF = fun(Item) -> Item#roster{name = Name, groups = Groups} end,
    set_roster_item(HostType, ContactJid, UserJid, UserJid, UpdateF).

%% @doc remove from roster - in practice it means changing subscription state to 'remove'
-spec remove_from_roster(mongooseim:host_type(), UserJid :: jid:jid(), ContactJid :: jid:jid()) ->
          ok | {error, any()}.
remove_from_roster(HostType, UserJid, ContactJid) ->
    UpdateF = fun(Item) -> Item#roster{subscription = remove} end,
    set_roster_item(HostType, ContactJid, UserJid, UserJid, UpdateF).

process_item_set_t(HostType, JID,
                   #xmlel{attrs = Attrs, children = Els} = El) ->
    case jid:from_binary(exml_query:attr(El, <<"jid">>)) of
        error -> ok;
        JID1 ->
            LJID = jid:to_lower(JID1),
            Item = #roster{usj = {jid:to_lus(JID), LJID},
                           us = jid:to_lus(JID), jid = LJID},
            Item1 = process_item_attrs_ws(Item, Attrs),
            Item2 = process_item_els(Item1, Els),
            case Item2#roster.subscription of
                remove -> del_roster_t(HostType, JID, LJID);
                _ -> update_roster_t(HostType, Item2)
            end
    end;
process_item_set_t(_HostType, _Jid, _) -> ok.

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

-spec get_jid_info(Acc, Params, Extra) -> {ok, Acc} when
    Acc :: {subscription_state(), [binary()]},
    Params :: #{to := jid:jid(), remote := jid:jid() | jid:simple_jid()},
    Extra :: gen_hook:extra().
get_jid_info(_, #{to := ToJID, remote := JID}, #{host_type := HostType}) ->
    ToRosterEntry = get_roster_entry(HostType, ToJID, JID, full),
    RemoteRosterEntryGetter = fun() -> get_roster_entry(HostType, ToJID, jid:to_bare(jid:to_lower(JID)), full) end,
    NewAcc = determine_subscription_state(ToRosterEntry, RemoteRosterEntryGetter),
    {ok, NewAcc}.

-spec determine_subscription_state(RosterEntry, RosterEntryGetter) -> SubscriptionState when
    RosterEntry :: roster() | does_not_exist | error,
    RosterEntryGetter :: fun(() -> RosterEntry) | undefined,
    SubscriptionState :: {subscription_state(), [binary()]}.
determine_subscription_state(error, _) -> {none, []};
determine_subscription_state(does_not_exist, undefined) -> {none, []};
determine_subscription_state(does_not_exist, Getter) -> determine_subscription_state(Getter(), undefined);
determine_subscription_state(R, _) -> {R#roster.subscription, R#roster.groups}.

get_roster_old(HostType, #jid{lserver = LServer} = JID) ->
    get_roster_old(HostType, LServer, JID).

get_roster_old(HostType, DestServer, JID) ->
    A = mongoose_acc:new(#{ location => ?LOCATION,
                            lserver => DestServer,
                            host_type => HostType,
                            element => undefined }),
    mongoose_hooks:roster_get(A, JID, false).

-spec item_to_map(roster()) -> map().
item_to_map(#roster{} = Roster) ->
    ContactJid = jid:make_noprep(jid:to_bare(Roster#roster.jid)),
    ContactName = Roster#roster.name,
    Subs = Roster#roster.subscription,
    Groups = Roster#roster.groups,
    Ask = Roster#roster.ask,
    #{jid => ContactJid, name => ContactName, subscription => Subs,
      groups => Groups, ask => Ask}.

-spec config_metrics(mongooseim:host_type()) -> [{gen_mod:opt_key(), gen_mod:opt_value()}].
config_metrics(HostType) ->
    mongoose_module_metrics:opts_for_module(HostType, ?MODULE, [backend]).

%% Backend API wrappers

-spec transaction(mongooseim:host_type(), fun(() -> any())) ->
    {aborted, any()} | {atomic, any()} | {error, any()}.
transaction(HostType, F) ->
    mod_roster_backend:transaction(HostType, F).

-spec read_roster_version(mongooseim:host_type(), jid:luser(), jid:lserver()) ->
    binary() | error.
read_roster_version(HostType, LUser, LServer) ->
    mod_roster_backend:read_roster_version(HostType, LUser, LServer).

-spec write_roster_version(mongooseim:host_type(), jid:luser(), jid:lserver()) -> version().
write_roster_version(HostType, LUser, LServer) ->
    write_roster_version(HostType, LUser, LServer, no_transaction).

-spec write_roster_version_t(mongooseim:host_type(), jid:jid()) -> version().
write_roster_version_t(HostType, #jid{luser = LUser, lserver = LServer}) ->
    write_roster_version(HostType, LUser, LServer, in_transaction).

-spec write_roster_version(mongooseim:host_type(), jid:luser(), jid:lserver(),
                           transaction_state()) -> version().
write_roster_version(HostType, LUser, LServer, TransactionState) ->
    Ver = mongoose_bin:encode_crypto(term_to_binary(os:timestamp())),
    mod_roster_backend:write_roster_version(HostType, LUser, LServer, TransactionState, Ver),
    Ver.

-spec get_roster(mongooseim:host_type(), jid:luser(), jid:lserver()) -> [roster()].
get_roster(HostType, LUser, LServer) ->
    mod_roster_backend:get_roster(HostType, LUser, LServer).

-spec get_roster_entry(mongooseim:host_type(), jid:jid(), contact(), entry_format()) ->
           roster() | does_not_exist | error.
get_roster_entry(HostType, #jid{luser = LUser, lserver = LServer}, LJid, Format) ->
    mod_roster_backend:get_roster_entry(HostType, LUser, LServer, LJid, no_transaction, Format).

-spec get_roster_entry_t(mongooseim:host_type(), jid:jid(), contact(), entry_format()) ->
          roster() | does_not_exist | error.
get_roster_entry_t(HostType, #jid{luser = LUser, lserver = LServer}, LJid, Format) ->
    mod_roster_backend:get_roster_entry(HostType, LUser, LServer, LJid, in_transaction, Format).

-spec roster_subscribe_t(mongooseim:host_type(), roster()) -> ok.
roster_subscribe_t(HostType, Item) ->
    mod_roster_backend:roster_subscribe_t(HostType, Item).

-spec update_roster_t(mongooseim:host_type(), roster()) -> ok.
update_roster_t(HostType, Item) ->
    mod_roster_backend:update_roster_t(HostType, Item).

-spec del_roster_t(mongooseim:host_type(), jid:jid(), contact()) -> ok.
del_roster_t(HostType, #jid{luser = LUser, lserver = LServer}, LJID) ->
    mod_roster_backend:del_roster_t(HostType, LUser, LServer, LJID).
