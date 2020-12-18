%%%----------------------------------------------------------------------
%%% File    : mod_roster_rdbms.erl
%%% Author  : Michał Piotrowski <michal.piotrowski@erlang-solutions.com>
%%% Purpose : mod_roster_rdbms rdbms backend
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2014   ProcessOne
%%% MongooseIM, Copyright (C) 2015      Erlang Solutions Ltd.
%%%
%%%----------------------------------------------------------------------

-module(mod_roster_rdbms).
-include("mod_roster.hrl").
-include("jlib.hrl").
-include("mongoose.hrl").

-behaviour(mod_roster).

%% API
-export([init/2,
         transaction/2,
         read_roster_version/2,
         write_roster_version/4,
         get_roster/2,
         get_roster_entry/3,
         get_roster_entry/4,
         get_roster_entry_t/3,
         get_roster_entry_t/4,
         get_subscription_lists/3,
         roster_subscribe_t/4,
         update_roster_t/4,
         remove_user/2,
         del_roster_t/3
         ]).

-export([raw_to_record/2]).

-spec init(jid:server(), list()) -> ok.
init(_Host, _Opts) ->
    ok.

-spec transaction(LServer :: jid:lserver(), F :: fun()) ->
    {aborted, Reason :: any()} | {atomic, Result :: any()}.
transaction(LServer, F) ->
    mongoose_rdbms:sql_transaction(LServer, F).

-spec read_roster_version(jid:luser(), jid:lserver())
-> binary() | error.
read_roster_version(LUser, LServer) ->
    Username = mongoose_rdbms:escape_string(LUser),
    case get_roster_version(LServer, Username)
    of
        {selected, [{Version}]} -> Version;
        {selected, []} -> error
    end.

write_roster_version(LUser, LServer, InTransaction, Ver) ->
    Username = mongoose_rdbms:escape_string(LUser),
    EVer = mongoose_rdbms:escape_string(Ver),
    case InTransaction of
        true ->
            set_roster_version(Username, EVer);
        _ ->
            rdbms_queries:sql_transaction(LServer,
                                          fun () ->
                                                  set_roster_version(Username, EVer)
                                          end)
    end.

get_roster(LUser, LServer) ->
    Username = mongoose_rdbms:escape_string(LUser),
    try get_roster_sql(LServer, Username) of
        {selected, Items} when is_list(Items) ->
            {selected, JIDGroups} = get_roster_jid_groups(LServer, Username),
            GroupsDict = lists:foldl(fun ({J, G}, Acc) ->
                                             dict:append(J, G, Acc)
                                     end,
                                     dict:new(), JIDGroups),
            RItems = lists:flatmap(fun (I) ->
                                          raw_to_record_with_group(LServer, I, GroupsDict)
                                   end,
                                   Items),
            RItems;
        _ -> []
    catch Class:Reason:StackTrace ->
        ?LOG_ERROR(#{what => get_roster_failed, class => Class, reason => Reason,
                     stacktrace => StackTrace, user => LUser, host => LServer}),
        []
    end.

raw_to_record_with_group(LServer, I, GroupsDict) ->
    case raw_to_record(LServer, I) of
        %% Bad JID in database:
        error -> [];
        R ->
            SJID = jid:to_binary(R#roster.jid),
            Groups = case dict:find(SJID, GroupsDict) of
                         {ok, Gs} -> Gs;
                         error -> []
                     end,
            [R#roster{groups = Groups}]
    end.

get_roster_entry(LUser, LServer, LJID) ->
    do_get_roster_entry(LUser, LServer, LJID, get_roster_by_jid).

get_roster_entry_t(LUser, LServer, LJID) ->
    do_get_roster_entry(LUser, LServer, LJID, get_roster_by_jid_t).

do_get_roster_entry(LUser, LServer, LJID, FuncName) ->
    Username = mongoose_rdbms:escape_string(LUser),
    SJID = mongoose_rdbms:escape_string(jid:to_binary(LJID)),
    {selected, Res} = case FuncName of
                          get_roster_by_jid ->
                              get_roster_by_jid(LServer, Username, SJID);
                          get_roster_by_jid_t ->
                              get_roster_by_jid_t(LServer, Username, SJID)
                      end,
    case Res of
        [] ->
            does_not_exist;
        [I] ->
            R = raw_to_record(LServer, I),
            case R of
                %% Bad JID in database:
                error ->
                    #roster{usj = {LUser, LServer, LJID},
                        us = {LUser, LServer}, jid = LJID};
                _ ->
                    R#roster{usj = {LUser, LServer, LJID},
                        us = {LUser, LServer}, jid = LJID}
            end
    end.

get_roster_entry(LUser, LServer, LJID, full) ->
    case get_roster_entry(LUser, LServer, LJID) of
        does_not_exist -> does_not_exist;
        Rentry ->
            case read_subscription_and_groups(LUser, LServer, LJID) of
                error -> error;
                {Subscription, Groups} ->
                    Rentry#roster{subscription = Subscription, groups = Groups}
            end
    end.

get_roster_entry_t(LUser, LServer, LJID, full) ->
    case get_roster_entry_t(LUser, LServer, LJID) of
        does_not_exist -> does_not_exist;
        Rentry ->
        case read_subscription_and_groups_t(LUser, LServer, LJID) of
            error -> error;
            {Subscription, Groups} ->
                Rentry#roster{subscription = Subscription, groups = Groups}
        end
    end.


get_subscription_lists(_, LUser, LServer) ->
    Username = mongoose_rdbms:escape_string(LUser),
    try get_roster_sql(LServer, Username) of
        {selected, Items} when is_list(Items) ->
            Items;
        Other ->
            ?LOG_ERROR(#{what => get_subscription_lists_failed, reason => Other,
                        user => LUser, host => LServer}),
            []
    catch Class:Reason:StackTrace ->
        ?LOG_ERROR(#{what => get_subscription_lists_failed, class => Class,
                     reason => Reason, stacktrace => StackTrace,
                     user => LUser, host => LServer}),
        []
    end.

roster_subscribe_t(LUser, LServer, LJID, Item) ->
    ItemVals = record_to_string(Item),
    Username = mongoose_rdbms:escape_string(LUser),
    SJID = mongoose_rdbms:escape_string(jid:to_binary(LJID)),
    roster_subscribe(LServer, Username, SJID, ItemVals).

remove_user(LUser, LServer) ->
    Username = mongoose_rdbms:escape_string(LUser),
    del_user_roster_t(LServer, Username),
    ok.

update_roster_t(LUser, LServer, LJID, Item) ->
    Username = mongoose_rdbms:escape_string(LUser),
    SJID = mongoose_rdbms:escape_string(jid:to_binary(LJID)),
    ItemVals = record_to_string(Item),
    ItemGroups = groups_to_string(Item),
    update_roster(LServer, Username, SJID, ItemVals, ItemGroups).

del_roster_t(LUser, LServer, LJID) ->
    Username = mongoose_rdbms:escape_string(LUser),
    SJID = mongoose_rdbms:escape_string(jid:to_binary(LJID)),
    del_roster(LServer, Username, SJID).

raw_to_record(LServer,
              {User, SJID, Nick, SSubscription, SAsk, SAskMessage,
               _SServer, _SSubscribe, _SType}) ->
    case jid:from_binary(SJID) of
        error -> error;
        JID ->
            LJID = jid:to_lower(JID),
            Subscription = case SSubscription of
                               <<"B">> -> both;
                               <<"T">> -> to;
                               <<"F">> -> from;
                               _ -> none
                           end,
            Ask = case SAsk of
                      <<"S">> -> subscribe;
                      <<"U">> -> unsubscribe;
                      <<"B">> -> both;
                      <<"O">> -> out;
                      <<"I">> -> in;
                      _ -> none
                  end,
            #roster{usj = {User, LServer, LJID},
                    us = {User, LServer}, jid = LJID, name = Nick,
                    subscription = Subscription, ask = Ask,
                    askmessage = SAskMessage}
    end.


read_subscription_and_groups(LUser, LServer, LJID) ->
    read_subscription_and_groups(LUser, LServer, LJID, get_subscription,
        get_rostergroup_by_jid).

read_subscription_and_groups_t(LUser, LServer, LJID) ->
    read_subscription_and_groups(LUser, LServer, LJID, get_subscription_t,
                                 get_rostergroup_by_jid_t).

read_subscription_and_groups(LUser, LServer, LJID, GSFunc, GRFunc) ->
    Username = mongoose_rdbms:escape_string(LUser),
    SJID = mongoose_rdbms:escape_string(jid:to_binary(LJID)),
    SubResult = case GSFunc of
              get_subscription ->
                  catch get_subscription(LServer, Username, SJID);
              get_subscription_t ->
                  catch get_subscription_t(LServer, Username, SJID)
                end,
    case SubResult of
        {selected, [{SSubscription}]} ->
            Subscription = case SSubscription of
                               <<"B">> -> both;
                               <<"T">> -> to;
                               <<"F">> -> from;
                               _ -> none
                           end,
            GRResult = case GRFunc of
                           get_rostergroup_by_jid ->
                               catch get_rostergroup_by_jid(LServer, Username, SJID);
                           get_rostergroup_by_jid_t ->
                               catch get_rostergroup_by_jid_t(LServer, Username, SJID)
                       end,
            Groups = case GRResult of
                         {selected, JGrps} when is_list(JGrps) ->
                             [JGrp || {JGrp} <- JGrps];
                         _ ->
                             ?LOG_ERROR(#{what => read_subscription_and_groups_failed,
                                          reason => GRResult, user => LUser,
                                          host => LServer}),
                             []
                     end,
            {Subscription, Groups};
        E ->
           ?LOG_ERROR(#{what => read_subscription_and_groups_failed,
                        reason => E, user => LUser, host => LServer}),
            error
    end.

%%==============================================================================
%% Helper functions
%%==============================================================================

record_to_string(#roster{us = {User, _Server},
                         jid = JID, name = Name, subscription = Subscription,
                         ask = Ask, askmessage = AskMessage}) ->
    Username = mongoose_rdbms:escape_string(User),
    SJID =
    mongoose_rdbms:escape_string(jid:to_binary(jid:to_lower(JID))),
    Nick = mongoose_rdbms:escape_string(Name),
    SSubscription = mongoose_rdbms:escape_string(case Subscription of
                        both -> <<"B">>;
                        to -> <<"T">>;
                        from -> <<"F">>;
                        none -> <<"N">>
                    end),
    SAsk = mongoose_rdbms:escape_string(case Ask of
               subscribe -> <<"S">>;
               unsubscribe -> <<"U">>;
               both -> <<"B">>;
               out -> <<"O">>;
               in -> <<"I">>;
               none -> <<"N">>
           end),
    SAskMessage = mongoose_rdbms:escape_string(AskMessage),
    [Username, SJID, Nick, SSubscription, SAsk, SAskMessage,
     mongoose_rdbms:escape_string(<<"N">>),
     mongoose_rdbms:escape_string(<<"">>),
     mongoose_rdbms:escape_string(<<"item">>)].

groups_to_string(#roster{us = {User, _Server},
                         jid = JID, groups = Groups}) ->
    Username = mongoose_rdbms:escape_string(User),
    SJID = mongoose_rdbms:escape_string(jid:to_binary(jid:to_lower(JID))),
    lists:foldl(fun (<<"">>, Acc) -> Acc;
                    (Group, Acc) ->
                        G = mongoose_rdbms:escape_string(Group),
                        [[Username, SJID, G] | Acc]
                end,
                [], Groups).


get_roster_version(LServer, LUser) ->
    mongoose_rdbms:sql_query(
      LServer,
      [<<"select version from roster_version "
         "where username=">>, mongoose_rdbms:use_escaped_string(LUser)]).

set_roster_version(LUser, Version) ->
    mongoose_rdbms:update_t(
      <<"roster_version">>,
      [<<"username">>, <<"version">>],
      [LUser, Version],
      [<<"username = ">>, mongoose_rdbms:use_escaped_string(LUser)]).

get_roster_sql(LServer, Username) ->
    mongoose_rdbms:sql_query(
      LServer,
      [<<"select username, jid, nick, subscription, ask, "
         "askmessage, server, subscribe, type from rosterusers "
         "where username=">>, mongoose_rdbms:use_escaped_string(Username)]).

get_roster_jid_groups(LServer, Username) ->
    mongoose_rdbms:sql_query(
      LServer,
      [<<"select jid, grp from rostergroups "
         "where username=">>, mongoose_rdbms:use_escaped_string(Username)]).

del_user_roster_t(LServer, Username) ->
    mongoose_rdbms:sql_transaction(
      LServer,
      fun() ->
              mongoose_rdbms:sql_query_t(
                [<<"delete from rosterusers "
                   "where username=">>, mongoose_rdbms:use_escaped_string(Username)]),
              mongoose_rdbms:sql_query_t(
                [<<"delete from rostergroups "
                   "where username=">>, mongoose_rdbms:use_escaped_string(Username)])
      end).


q_get_roster(Username, SJID) ->
    [<<"select username, jid, nick, subscription, "
    "ask, askmessage, server, subscribe, type from rosterusers "
    "where username=">>, mongoose_rdbms:use_escaped_string(Username), <<" "
    "and jid=">>, mongoose_rdbms:use_escaped_string(SJID)].

get_roster_by_jid(LServer, Username, SJID) ->
    mongoose_rdbms:sql_query(LServer, q_get_roster(Username, SJID)).

get_roster_by_jid_t(_LServer, Username, SJID) ->
    mongoose_rdbms:sql_query_t(q_get_roster(Username, SJID)).

q_get_rostergroup(Username, SJID) ->
    [<<"select grp from rostergroups "
    "where username=">>, mongoose_rdbms:use_escaped_string(Username), <<" "
    "and jid=">>, mongoose_rdbms:use_escaped_string(SJID)].

get_rostergroup_by_jid(LServer, Username, SJID) ->
    mongoose_rdbms:sql_query(LServer, q_get_rostergroup(Username, SJID)).

get_rostergroup_by_jid_t(_LServer, Username, SJID) ->
    mongoose_rdbms:sql_query_t(q_get_rostergroup(Username, SJID)).

del_roster(_LServer, Username, SJID) ->
    mongoose_rdbms:sql_query_t(
      [<<"delete from rosterusers "
         "where username=">>, mongoose_rdbms:use_escaped_string(Username), <<" "
         "and jid=">>, mongoose_rdbms:use_escaped_string(SJID)]),
    mongoose_rdbms:sql_query_t(
      [<<"delete from rostergroups "
         "where username=">>, mongoose_rdbms:use_escaped_string(Username), <<" "
         "and jid=">>, mongoose_rdbms:use_escaped_string(SJID)]).

update_roster(_LServer, Username, SJID, ItemVals, ItemGroups) ->
    mongoose_rdbms:update_t(<<"rosterusers">>,
             [<<"username">>, <<"jid">>, <<"nick">>, <<"subscription">>, <<"ask">>,
              <<"askmessage">>, <<"server">>, <<"subscribe">>, <<"type">>],
             ItemVals,
             [<<"username=">>, mongoose_rdbms:use_escaped_string(Username),
              <<" and jid=">>, mongoose_rdbms:use_escaped_string(SJID)]),
    mongoose_rdbms:sql_query_t(
      [<<"delete from rostergroups "
         "where username=">>, mongoose_rdbms:use_escaped_string(Username), <<" "
         "and jid=">>, mongoose_rdbms:use_escaped_string(SJID)]),
    lists:foreach(fun(ItemGroup) ->
                          mongoose_rdbms:sql_query_t(
                            [<<"insert into rostergroups(username, jid, grp) "
                               "values (">>, mongoose_rdbms:join_escaped(ItemGroup), ");"])
                  end,
                  ItemGroups).

roster_subscribe(_LServer, Username, SJID, ItemVals) ->
    mongoose_rdbms:update_t(<<"rosterusers">>,
             [<<"username">>, <<"jid">>, <<"nick">>, <<"subscription">>, <<"ask">>,
              <<"askmessage">>, <<"server">>, <<"subscribe">>, <<"type">>],
             ItemVals,
             [<<"username=">>, mongoose_rdbms:use_escaped_string(Username),
              <<" and jid=">>, mongoose_rdbms:use_escaped_string(SJID)]).

q_get_subscription(Username, SJID) ->
    [<<"select subscription from rosterusers "
    "where username=">>, mongoose_rdbms:use_escaped_string(Username), <<" "
    "and jid=">>, mongoose_rdbms:use_escaped_string(SJID)].

get_subscription(LServer, Username, SJID) ->
    mongoose_rdbms:sql_query( LServer, q_get_subscription(Username, SJID)).

get_subscription_t(_LServer, Username, SJID) ->
    mongoose_rdbms:sql_query_t(q_get_subscription(Username, SJID)).

