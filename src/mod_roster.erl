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
-behaviour(mongoose_module_metrics).

-export([start/2,
         stop/1,
         config_spec/0,
         process_iq/4,
         process_local_iq/4,
         get_roster_entry/2,
         get_roster_entry/3,
         get_roster/2,
         item_to_map/1,
         set_items/2,
         set_roster_entry/4,
         remove_from_roster/2,
         item_to_xml/1
        ]).

% Main hooks
-export([
         get_user_roster/2,
         in_subscription/5,
         out_subscription/4,
         get_subscription_lists/2,
         get_jid_info/3,
         remove_user/2, % for tests
         remove_user/3,
         get_versioning_feature/2,
         get_personal_data/3
        ]).

% Deprecated Hooks
-deprecated({in_subscription, 6, eventually}).
-deprecated({out_subscription, 5, eventually}).
-deprecated({get_subscription_lists, 3, eventually}).
-deprecated({get_jid_info, 4, eventually}).
-export([
         % get_user_roster/2,
             % This means that the clause of get_user_roster/2
             % accepting a tuple of binaries is to be deprecated.
         in_subscription/6,
         out_subscription/5,
         get_subscription_lists/3,
         get_jid_info/4
        ]).

-export([remove_test_user/2,
         transaction/2,
         process_subscription_transaction/5,
         get_user_rosters_length/1]). % for testing

-export([config_metrics/1]).

-include("mongoose.hrl").
-include("jlib.hrl").
-include("mod_roster.hrl").
-include("mongoose_config_spec.hrl").

-export_type([roster/0, sub_presence/0]).

-type roster() :: #roster{}.

-type sub_presence() :: subscribe | subscribed | unsubscribe | unsubscribed.

-type subscription_state() :: none  | from | to | both | remove.

-type get_user_roster_strategy() :: db_versioning | hash_versioning | no_versioning.

-callback init(Host, Opts) -> ok when
    Host :: jid:server(),
    Opts :: list().
-callback transaction(LServer, F) -> {aborted, Reason} | {atomic, Result} when
    LServer :: jid:lserver(),
    F :: fun(),
    Reason :: any(),
    Result :: any().
-callback read_roster_version(LUser, LServer) -> Result when
    LUser :: jid:luser(),
    LServer :: jid:lserver(),
    Result :: binary() | error.
-callback write_roster_version(LUser, LServer, InTransaction, Ver) -> Result when
    LUser :: jid:luser(),
    LServer :: jid:lserver(),
    InTransaction :: boolean(),
    Ver :: binary(),
    Result :: any().
-callback get_roster(LUser, LServer) -> Result when
    LUser :: jid:luser(),
    LServer :: jid:lserver(),
    Result :: [roster()].
-callback get_subscription_lists(Acc, LUser, LServer) -> Result when
    Acc :: term(),
    LUser :: jid:luser(),
    LServer :: jid:lserver(),
    Result :: term().
-callback roster_subscribe_t(LUser, LServer, LJid, SJid) -> Result when
    LUser :: jid:luser(),
    LServer :: jid:lserver(),
    LJid :: jid:simple_jid(),
    SJid :: roster(),
    Result :: term().
-callback remove_user(LUser, LServer) -> Result when
    LUser :: jid:luser(),
    LServer :: jid:lserver(),
    Result :: term().
-callback update_roster_t(LUser, LServer, LJid, Item) -> Result when
    LUser :: jid:luser(),
    LServer :: jid:lserver(),
    LJid :: jid:simple_jid(),
    Item :: roster(),
    Result :: term().
-callback del_roster_t(LUser, LServer, LJid) -> Result when
    LUser :: jid:luser(),
    LServer :: jid:lserver(),
    LJid :: jid:simple_jid(),
    Result :: term().
-callback get_roster_entry(LUser, LServer, Jid) -> Result when
    LUser :: jid:luser(),
    LServer :: jid:lserver(),
    Jid :: jid:simple_jid() | jid:ljid() |jid:jid(),
    Result :: roster() | does_not_exist | error.
-callback get_roster_entry(LUser, LServer, Jid, full) -> Result when
    LUser :: jid:luser(),
    LServer :: jid:lserver(),
    Jid :: jid:simple_jid() | jid:ljid() |jid:jid(),
    Result :: roster() | does_not_exist | error.
-callback get_roster_entry_t(LUser, LServer, Jid) -> Result when
    LUser :: jid:luser(),
    LServer :: jid:lserver(),
    Jid :: jid:simple_jid() | jid:ljid() |jid:jid(),
    Result :: roster() | does_not_exist | error.
-callback get_roster_entry_t(LUser, LServer, Jid, full) -> Result when
    LUser :: jid:luser(),
    LServer :: jid:lserver(),
    Jid :: jid:simple_jid() | jid:ljid() |jid:jid(),
    Result :: roster() | does_not_exist | error.

-callback raw_to_record(LServer, Item) -> Result when
    LServer :: jid:lserver(),
    Item :: term(),
    Result :: error | roster().

%====================================================================
% Deprecated API
%====================================================================
 -define(FUNCTION_STRING,
            ?MODULE_STRING ++ ":" ++
               atom_to_list(?FUNCTION_NAME) ++ "/" ++
                  integer_to_list(?FUNCTION_ARITY)).
 -define(DEPRECATE_FUNCTION,
         mongoose_deprecations:log(
           {?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY},
           #{what => roster_function_deprecated,
             text => <<"The function ", (list_to_binary(?FUNCTION_STRING))/binary,
                       " is deprecated, please use the #jid{} equivalent instead">>},
           [{log_level, warning}])).
 in_subscription(Acc, U, S, JID, Type, Reason) ->
     ?DEPRECATE_FUNCTION,
     in_subscription(Acc, jid:make(U, S, <<>>), JID, Type, Reason).
 out_subscription(Acc, U, S, JID, Type) ->
     ?DEPRECATE_FUNCTION,
     out_subscription(Acc, jid:make(U, S, <<>>), JID, Type).
 get_subscription_lists(Acc, U, S) ->
     ?DEPRECATE_FUNCTION,
     get_subscription_lists(Acc, jid:make(U, S, <<>>)).
 get_jid_info(Acc, U, S, JID) ->
     ?DEPRECATE_FUNCTION,
     get_jid_info(Acc, jid:make(U, S, <<>>), JID).
%====================================================================
% Deprecated API
%====================================================================

%%--------------------------------------------------------------------
%% gdpr callback
%%--------------------------------------------------------------------

-spec get_personal_data(gdpr:personal_data(), mongooseim:host_type(), jid:jid()) -> gdpr:personal_data().
get_personal_data(Acc, _HostType, #jid{ luser = LUser, lserver = LServer }) ->
    Schema = ["jid", "name", "subscription", "ask", "groups", "askmessage", "xs"],
    Records = mod_roster_backend:get_roster(LUser, LServer),
    SerializedRecords = lists:map(fun roster_record_to_gdpr_entry/1, Records),
    [{roster, Schema, SerializedRecords} | Acc].

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
     << <<(exml:to_binary(X))>> || X <- XS >>
    ].

%%--------------------------------------------------------------------
%% mod_roster's callbacks
%%--------------------------------------------------------------------

start(Host, Opts) ->
    IQDisc = gen_mod:get_opt(iqdisc, Opts, one_queue),
    TrackedFuns = [read_roster_version,
                   write_roster_version,
                   get_roster,
                   get_roster_entry,
                   get_roster_entry_t,
                   get_subscription_lists,
                   roster_subscribe_t,
                   update_roster_t,
                   del_roster_t
                   ],
    gen_mod:start_backend_module(?MODULE, Opts, TrackedFuns),
    mod_roster_backend:init(Host, Opts),

    ejabberd_hooks:add(hooks(Host)),

    gen_iq_handler:add_iq_handler(ejabberd_sm, Host, ?NS_ROSTER,
                                  ?MODULE, process_iq, IQDisc).

stop(Host) ->
    ejabberd_hooks:delete(hooks(Host)),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host, ?NS_ROSTER).

-spec config_spec() -> mongoose_config_spec:config_section().
config_spec() ->
    #section{
       items = #{<<"iqdisc">> => mongoose_config_spec:iqdisc(),
                 <<"versioning">> => #option{type = boolean},
                 <<"store_current_id">> => #option{type = boolean},
                 <<"backend">> => #option{type = atom,
                                          validate = {module, mod_roster}},
                 <<"riak">> => riak_config_spec()
                }
      }.

riak_config_spec() ->
    #section{items = #{<<"bucket_type">> => #option{type = binary,
                                                    validate = non_empty},
                       <<"version_bucket_type">> => #option{type = binary,
                                                            validate = non_empty}},
             format = none
            }.

hooks(Host) ->
    [{roster_get, Host, ?MODULE, get_user_roster, 50},
     {roster_in_subscription, Host, ?MODULE, in_subscription, 50},
     {roster_out_subscription, Host, ?MODULE, out_subscription, 50},
     {roster_get_subscription_lists, Host, ?MODULE, get_subscription_lists, 50},
     {roster_get_jid_info, Host, ?MODULE, get_jid_info, 50},
     {remove_user, Host, ?MODULE, remove_user, 50},
     {anonymous_purge_hook, Host, ?MODULE, remove_user, 50},
     {roster_get_versioning_feature, Host, ?MODULE, get_versioning_feature, 50},
     {get_personal_data, Host, ?MODULE, get_personal_data, 50}].

get_roster_entry(#jid{luser = LUser, lserver = LServer}, Jid) ->
    mod_roster_backend:get_roster_entry(LUser, LServer, jid_arg_to_lower(Jid)).

get_roster_entry(#jid{luser = LUser, lserver = LServer}, Jid, full) ->
    mod_roster_backend:get_roster_entry(LUser, LServer, jid_arg_to_lower(Jid), full).

get_roster_entry_t(#jid{luser = LUser, lserver = LServer}, Jid, full) ->
    mod_roster_backend:get_roster_entry_t(LUser, LServer, jid_arg_to_lower(Jid), full).

-spec jid_arg_to_lower(JID :: jid:simple_jid() | jid:jid() | binary()) ->
    error | jid:simple_jid().
jid_arg_to_lower(Jid) when is_binary(Jid) ->
    RJid = jid:from_binary(Jid),
    jid:to_lower(RJid);
jid_arg_to_lower(Jid) ->
    jid:to_lower(Jid).

-spec process_iq(jid:jid(), jid:jid(), mongoose_acc:t(), jlib:iq()) ->
    {mongoose_acc:t(), jlib:iq()}.
process_iq(From, To, Acc, IQ) ->
    #iq{sub_el = SubEl} = IQ,
    #jid{lserver = LServer} = From,
    case lists:member(LServer, ?MYHOSTS) of
        true ->
            process_local_iq(From, To, Acc, IQ);
        _ ->
            {Acc, IQ#iq{type = error, sub_el = [SubEl, mongoose_xmpp_errors:item_not_found()]}}
    end.

-spec process_local_iq(jid:jid(), jid:jid(), mongoose_acc:t(), jlib:iq()) ->
    {mongoose_acc:t(), jlib:iq()}.
process_local_iq(From, To, Acc, #iq{type = Type} = IQ) ->
    case Type of
        set ->
            {Acc, process_iq_set(From, To, IQ)};
        get ->
            {Acc, process_iq_get(From, To, IQ)}
    end.

roster_hash(Items) ->
    L = [R#roster{groups = lists:sort(Grs)} ||
         R = #roster{groups = Grs} <- Items],
    sha:sha1_hex(term_to_binary(lists:sort(L))).

-spec roster_versioning_enabled(jid:lserver()) -> boolean().
roster_versioning_enabled(Host) ->
    gen_mod:get_module_opt(Host, ?MODULE, versioning, false).

-spec roster_version_on_db(jid:lserver()) -> boolean().
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

roster_version(#jid{luser = LUser, lserver = LServer} = JID) ->
    case roster_version_on_db(LServer) of
        true ->
            case read_roster_version(LUser, LServer) of
                error -> not_found;
                V -> V
            end;
        false ->
            R = get_roster_old(JID),
            roster_hash(R)
    end.

read_roster_version(LUser, LServer) ->
    mod_roster_backend:read_roster_version(LUser, LServer).

write_roster_version(LUser, LServer) ->
    write_roster_version(LUser, LServer, false).

write_roster_version_t(LUser, LServer) ->
    write_roster_version(LUser, LServer, true).

write_roster_version(LUser, LServer, InTransaction) ->
    Ver = sha:sha1_hex(term_to_binary(os:timestamp())),
    mod_roster_backend:write_roster_version(LUser, LServer, InTransaction, Ver),
    Ver.

%% Load roster from DB only if neccesary.
%% It is neccesary if
%%     - roster versioning is disabled in server OR
%%     - roster versioning is not used by the client OR
%%     - roster versioning is used by server and client,
%%       BUT the server isn't storing versions on db OR
%%     - the roster version from client don't match current version.
-spec process_iq_get(jid:jid(), jid:jid(), jlib:iq()) -> jlib:iq().
process_iq_get(From, To, IQ) ->
    mongoose_iq:try_to_handle_iq(From, To, IQ, fun do_process_iq_get/3).

-spec do_process_iq_get(jid:jid(), jid:jid(), jlib:iq()) -> jlib:iq().
do_process_iq_get(#jid{lserver = LServer} = From, To, #iq{sub_el = SubEl} = IQ) ->
    AttrVer = exml_query:attr(SubEl, <<"ver">>), %% type binary() | undefined
    VersioningRequested = is_binary(AttrVer),
    VersioningEnabled = roster_versioning_enabled(LServer),
    VersionOnDb = roster_version_on_db(LServer),
    Strategy = choose_get_user_roster_strategy(VersioningRequested, VersioningEnabled, VersionOnDb),
    {ItemsToSend, VersionToSend} = get_user_roster_based_on_version(Strategy, AttrVer, From, To),
    IQ#iq{type = result, sub_el = create_sub_el(ItemsToSend, VersionToSend)}.

-spec choose_get_user_roster_strategy(VersioningRequested :: boolean(),
                                      VersioningEnabled :: boolean(),
                                      VersionOnDb :: boolean()) ->
    get_user_roster_strategy().
choose_get_user_roster_strategy(true, true, true) -> db_versioning;
choose_get_user_roster_strategy(true, true, false) -> hash_versioning;
choose_get_user_roster_strategy(_, _, _) -> no_versioning.

get_user_roster_based_on_version(db_versioning, RequestedVersion, From, To) ->
    get_user_roster_db_versioning(RequestedVersion, From, To);
get_user_roster_based_on_version(hash_versioning, RequestedVersion, From, To) ->
    get_user_roster_hash_versioning(RequestedVersion, From, To);
get_user_roster_based_on_version(no_versioning, _RequestedVersion, From, To) ->
    get_user_roster_no_versioning(From, To).

get_user_roster_db_versioning(RequestedVersion, From, To)
    when is_binary(RequestedVersion) ->
    LUser = From#jid.luser,
    LServer = From#jid.lserver,
    case read_roster_version(LUser, LServer) of
        error ->
            RosterVersion = write_roster_version(LUser, LServer),
            {lists:map(fun item_to_xml/1,
                       get_roster_old(To#jid.server, From)),
             RosterVersion};
        RequestedVersion ->
            {false, false};
        NewVersion ->
            {lists:map(fun item_to_xml/1,
                       get_roster_old(To#jid.server, From)),
             NewVersion}
    end.

get_user_roster_hash_versioning(RequestedVersion, From, To)
    when is_binary(RequestedVersion) ->
    RosterItems = get_roster_old(To#jid.lserver, From),
    case roster_hash(RosterItems) of
        RequestedVersion ->
            {false, false};
        New ->
            {lists:map(fun item_to_xml/1, RosterItems), New}
    end.

get_user_roster_no_versioning(From, To) ->
    {lists:map(fun item_to_xml/1,
               get_roster_old(To#jid.lserver, From)),
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

-spec get_user_roster(mongoose_acc:t(),
                      jid:jid() | {jid:luser(), jid:lserver()}) ->
    mongoose_acc:t().
get_user_roster(Acc, #jid{luser = LUser, lserver = LServer}) ->
    case mongoose_acc:get(roster, show_full_roster, false, Acc) of
        true ->
            Roster = get_roster(LUser, LServer),
            mongoose_acc:append(roster, items, Roster, Acc);
        _ ->
            Roster = lists:filter(fun (#roster{subscription = none, ask = in}) ->
                                          false;
                                      (_) ->
                                          true
                                  end, get_roster(LUser, LServer)),
            mongoose_acc:append(roster, items, Roster, Acc)
    end;
get_user_roster(Acc, {U, S}) when is_binary(U), is_binary(S) ->
    ?DEPRECATE_FUNCTION,
    get_user_roster(Acc, jid:make(U, S, <<>>)).

-spec get_roster(jid:luser(), jid:lserver()) -> [roster()].
get_roster(LUser, LServer) ->
    mod_roster_backend:get_roster(LUser, LServer).

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

-spec process_iq_set(jid:jid(), jid:jid(), jlib:iq()) -> jlib:iq().
process_iq_set(#jid{lserver = LServer} = From, To, #iq{sub_el = SubEl} = IQ) ->
    #xmlel{children = Els} = SubEl,
    mongoose_hooks:roster_set(LServer, From, To, SubEl),
    lists:foreach(fun(El) -> process_item_set(From, To, El) end, Els),
    IQ#iq{type = result, sub_el = []}.

-spec process_item_set(jid:jid(), jid:jid(), exml:element()) -> ok.
process_item_set(From, To, #xmlel{attrs = Attrs} = El) ->
    JID1 = jid:from_binary(xml:get_attr_s(<<"jid">>, Attrs)),
    do_process_item_set(JID1, From, To, El);
process_item_set(_From, _To, _) -> ok.

-spec do_process_item_set(error | jid:jid(), jid:jid(), jid:jid(), exml:element()) -> ok.
do_process_item_set(error, _, _, _) -> ok;
do_process_item_set(#jid{} = JID1, #jid{} = From, #jid{} = To,
                    #xmlel{attrs = Attrs, children = Els}) ->
    MakeItem2 = fun(Item) ->
                    Item1 = process_item_attrs(Item, Attrs),
                    process_item_els(Item1, Els)
                end,
    set_roster_item(JID1, From, To, MakeItem2).

%% @doc this is run when a roster item is to be added, updated or removed
%% the interface of this func could probably be a bit simpler
-spec set_roster_item(JID1 :: jid:jid(),
                      From :: jid:jid(),
                      To :: jid:jid(),
                      MakeItem2 :: fun( (roster()) -> roster())) -> ok.
set_roster_item(JID1, #jid{luser = LUser, lserver = LServer} = From, To, MakeItem2) ->
    LJID = jid:to_lower(JID1),
    F = fun() ->
                Item = case get_roster_entry(From, LJID) of
                           does_not_exist ->
                               #roster{usj = {LUser, LServer, LJID},
                                       us = {LUser, LServer},
                                       jid = LJID};
                           I -> I
                       end,

                Item2 = MakeItem2(Item),
                case Item2#roster.subscription of
                    remove -> del_roster_t(LUser, LServer, LJID);
                    _ -> update_roster_t(LUser, LServer, LJID, Item2)
                end,
                Item3 = mongoose_hooks:roster_process_item(LServer, Item2),
                case roster_version_on_db(LServer) of
                    true -> write_roster_version_t(LUser, LServer);
                    false -> ok
                end,
                {Item, Item3}
        end,
    case transaction(LServer, F) of
        {atomic, {OldItem, NewItem}} ->
            push_item(From, To, NewItem),
            case NewItem#roster.subscription of
                remove ->
                    send_unsubscribing_presence(From, OldItem), ok;
                _ -> ok
            end;
        E ->
            ?LOG_ERROR(#{what => roster_set_item_failed, reason => E}), ok
    end.

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

push_item(#jid{lserver = LServer} = JID, From, Item) ->
    ejabberd_sm:route(jid:make_noprep(<<>>, <<>>, <<>>), JID,
                      {broadcast, {item, Item#roster.jid, Item#roster.subscription}}),
    case roster_versioning_enabled(LServer) of
        true ->
            push_item_version(JID, From, Item, roster_version(JID));
        false ->
            lists:foreach(fun(Resource) ->
                                  push_item_without_version(JID, Resource, From, Item)
                          end,
                          ejabberd_sm:get_user_resources(JID))
    end.

push_item_without_version(#jid{lserver = Server} = JID, Resource, From, Item) ->
    mongoose_hooks:roster_push(Server, From, Item),
    push_item_final(jid:replace_resource(JID, Resource), From, Item, not_found).

push_item_version(JID, From, Item, RosterVersion) ->
    lists:foreach(fun(Resource) ->
                          push_item_final(jid:replace_resource(JID, Resource), From, Item, RosterVersion)
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

-spec get_subscription_lists(Acc :: mongoose_acc:t(), JID :: jid:jid()) ->
    mongoose_acc:t().
get_subscription_lists(Acc, #jid{luser = LUser, lserver = LServer} = JID) ->
    Items = mod_roster_backend:get_subscription_lists(Acc, LUser, LServer),
    SubLists = fill_subscription_lists(JID, LServer, Items, [], [], []),
    mongoose_acc:set(roster, subscription_lists, SubLists, Acc).


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
    I = mod_roster_backend:raw_to_record(LServer, RawI),
    case I of
        %% Bad JID in database:
        error -> fill_subscription_lists(JID, LServer, Is, F, T, P);
        _ -> fill_subscription_lists(JID, LServer, [I | Is], F, T, P)
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


ask_to_pending(subscribe) -> out;
ask_to_pending(unsubscribe) -> none;
ask_to_pending(Ask) -> Ask.

roster_subscribe_t(LUser, LServer, LJID, Item) ->
    mod_roster_backend:roster_subscribe_t(LUser, LServer, LJID, Item).

transaction(LServer, F) ->
    mod_roster_backend:transaction(LServer, F).

-spec in_subscription(Acc:: mongoose_acc:t(),
                      ToJID :: jid:jid(),
                      FromJID :: jid:jid(),
                      Type :: sub_presence(),
                      Reason :: any()) ->
    mongoose_acc:t().
in_subscription(Acc, ToJID, FromJID, Type, Reason) ->
    Res = process_subscription(in, ToJID, FromJID, Type, Reason),
    mongoose_acc:set(hook, result, Res, Acc).

-spec out_subscription(Acc:: mongoose_acc:t(),
                       FromJID :: jid:jid(),
                       ToJID :: jid:jid(),
                       Type :: sub_presence()) ->
    mongoose_acc:t().
out_subscription(Acc, FromJID, ToJID, Type) ->
    Res = process_subscription(out, FromJID, ToJID, Type, <<>>),
    mongoose_acc:set(hook, result, Res, Acc).

process_subscription(Direction, FromJID, ToJID, Type, Reason) ->
    LServer = case FromJID of
                  #jid{lserver = LS} -> LS;
                  error -> error
              end,
    TransactionFun =
        fun() -> process_subscription_transaction(Direction, FromJID, ToJID, Type, Reason) end,
    case transaction(LServer, TransactionFun) of
        {atomic, {Push, AutoReply}} ->
            case AutoReply of
                none -> ok;
                _ ->
                    PresenceStanza = #xmlel{name = <<"presence">>,
                                            attrs = [{<<"type">>, autoreply_to_type(AutoReply)}],
                                            children = []},
                    ejabberd_router:route(FromJID, ToJID, PresenceStanza)
            end,
            case Push of
                {push, #roster{subscription = none, ask = in}} ->
                    true;
                {push, Item} ->
                    push_item(FromJID, FromJID, Item),
                    true;
                none -> false
            end;
        _ -> false
    end.

autoreply_to_type(subscribed) -> <<"subscribed">>;
autoreply_to_type(unsubscribed) -> <<"unsubscribed">>.

process_subscription_transaction(Direction, JID, JID1, Type, Reason) ->
    #jid{luser = LUser, lserver = LServer} = JID,
    LJID = jid:to_lower(JID1),
    Item = case get_roster_entry_t(JID, JID1, full) of
               does_not_exist ->
                   #roster{usj = {LUser, LServer, LJID},
                           us = {LUser, LServer},
                           jid = LJID};
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

remove_test_user(User, Server) ->
    mod_roster_backend:remove_user(User, Server).

get_user_rosters_length(JID) ->
    length(get_roster_old(JID)).

%% Used only by tests
remove_user(User, Server) ->
    Acc = mongoose_acc:new(#{location => ?LOCATION,
                             lserver => <<_/binary>> = jid:nameprep(Server),
                             element => undefined}),
    remove_user(Acc, User, Server).

remove_user(Acc, User, Server) ->
    case jid:make(User, Server, <<>>) of
        #jid{luser = LUser, lserver = LServer} = JID ->
            Acc1 = try_send_unsubscription_to_rosteritems(Acc, JID),
            mod_roster_backend:remove_user(LUser, LServer),
            Acc1;
        error ->
            Acc
    end.

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
send_unsubscription_to_rosteritems(Acc, JID) ->
    Acc1 = get_user_roster(Acc, JID),
    RosterItems = mongoose_acc:get(roster, items, [], Acc1),
    lists:foreach(fun(RosterItem) ->
                          send_unsubscribing_presence(JID, RosterItem)
                  end, RosterItems),
    Acc1.

%% @spec (From::jid(), Item::roster()) -> any()
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

set_items(#jid{luser = LUser, lserver = LServer}, SubEl) ->
    #xmlel{children = Els} = SubEl,
    F = fun () ->
                lists:foreach(fun(El) ->
                                      process_item_set_t(LUser, LServer, El)
                              end, Els)
        end,
    transaction(LServer, F).

%% @doc add a contact to roster, or update
-spec set_roster_entry(jid:jid(), jid:jid() | error, binary(), [binary()]) -> ok | error.
set_roster_entry(UserJid, ContactJid, Name, Groups) ->
    set_roster_entry(UserJid, ContactJid, Name, Groups, unchanged).

-spec set_roster_entry(UserJid ::jid:jid(),
                       ContactJid :: jid:jid(),
                       Name :: binary() | unchanged,
                       Groups :: [binary()] | unchanged,
                       NewSubscription :: remove | unchanged) -> ok | error.
set_roster_entry(UserJid, ContactJid, Name, Groups, NewSubscription) ->
    MakeItem = fun(Item) ->
                       modify_roster_item(Item, Name, Groups, NewSubscription)
               end,
    set_roster_item(ContactJid, UserJid, UserJid, MakeItem).

modify_roster_item(Item, Name, Groups, NewSubscription) ->
    Item1 = case Name of
                unchanged -> Item;
                _ -> Item#roster{name = Name}
            end,
    Item2 = case Groups of
                unchanged -> Item1;
                _ -> Item#roster{groups = Groups}
            end,
    case NewSubscription of
        unchanged -> Item2;
        _ -> Item2#roster{subscription = NewSubscription}
    end.

%% @doc remove from roster - in practice it means changing
%% subscription state to 'remove'
-spec remove_from_roster(UserJid ::jid:jid(), ContactJid :: jid:jid()) -> ok | error.
remove_from_roster(UserJid, ContactJid) ->
    set_roster_entry(UserJid, ContactJid, unchanged, unchanged, remove).

update_roster_t(LUser, LServer, LJID, Item) ->
    mod_roster_backend:update_roster_t(LUser, LServer, LJID, Item).

del_roster_t(LUser, LServer, LJID) ->
    mod_roster_backend:del_roster_t(LUser, LServer, LJID).

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

-spec get_jid_info(_ :: term(),
                   ToJID :: jid:jid(),
                   JID ::jid:jid() | jid:ljid()) -> {subscription_state(), [binary()]}.
get_jid_info(_, ToJID, JID) ->
    case get_roster_entry(ToJID, JID, full) of
        error -> {none, []};
        does_not_exist ->
            LRJID = jid:to_bare(jid:to_lower(JID)),
            case get_roster_entry(ToJID, LRJID, full) of
                error -> {none, []};
                does_not_exist -> {none, []};
                R -> {R#roster.subscription, R#roster.groups}
            end;
        Re -> {Re#roster.subscription, Re#roster.groups}
    end.

get_roster_old(#jid{lserver = LServer} = JID) ->
    get_roster_old(LServer, JID).

get_roster_old(DestServer, JID) ->
    %% TODO host type should be passed to this function
    {ok, HostType} = mongoose_domain_api:get_domain_host_type(DestServer),
    A = mongoose_acc:new(#{ location => ?LOCATION,
                            lserver => DestServer,
                            host_type => HostType,
                            element => undefined }),
    A2 = mongoose_hooks:roster_get(A, JID),
    mongoose_acc:get(roster, items, [], A2).

-spec item_to_map(roster()) -> map().
item_to_map(#roster{} = Roster) ->
    ContactJid = jid:make_noprep(jid:to_bare(Roster#roster.jid)),
    ContactName = Roster#roster.name,
    Subs = Roster#roster.subscription,
    Groups = Roster#roster.groups,
    Ask = Roster#roster.ask,
    #{jid => ContactJid, name => ContactName, subscription => Subs,
      groups => Groups, ask => Ask}.

config_metrics(Host) ->
    OptsToReport = [{backend, mnesia}], %list of tuples {option, defualt_value}
    mongoose_module_metrics:opts_for_module(Host, ?MODULE, OptsToReport).
