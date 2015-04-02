%%%----------------------------------------------------------------------
%%% File    : mod_roster_odbc.erl
%%% Author  : Michał Piotrowski <michal.piotrowski@erlang-solutions.com>
%%% Purpose : mod_roster_odbc odbc backend (XEP-0012)
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2014   ProcessOne
%%% MongooseIM, Copyright (C) 2015      Erlang Solutions Ltd.
%%%
%%%----------------------------------------------------------------------

-module(mod_roster_odbc).

-include("mod_roster.hrl").
-include("jlib.hrl").

%% API
-export([init/2,
         read_roster_version/2,
         write_roster_version/4,
         get_roster/2,
         get_roster_by_jid_t/3,
         get_subscription_lists/3,
         roster_subscribe_t/4,
         get_roster_by_jid_with_groups_t/3,
         update_roster_t/4,
         remove_user/2,
         del_roster_t/3,
         read_subscription_and_groups/3]).

-export([raw_to_record/2]).

-spec init(ejabberd:server(), list()) -> no_return().
init(_Host, _Opts) ->
    ok.

-spec read_roster_version(ejabberd:luser(), ejabberd:lserver())
-> binary() | error.
read_roster_version(LUser, LServer) ->
    Username = ejabberd_odbc:escape(LUser),
    case odbc_queries:get_roster_version(LServer, Username)
    of
        {selected, [<<"version">>], [{Version}]} -> Version;
        {selected, [<<"version">>], []} -> error
    end.

write_roster_version(LUser, LServer, InTransaction, Ver) ->
    Username = ejabberd_odbc:escape(LUser),
    EVer = ejabberd_odbc:escape(Ver),
    if InTransaction ->
           odbc_queries:set_roster_version(Username, EVer);
       true ->
           odbc_queries:sql_transaction(LServer,
                                        fun () ->
                                                odbc_queries:set_roster_version(Username,
                                                                                EVer)
                                        end)
    end.

get_roster(LUser, LServer) ->
    Username = ejabberd_odbc:escape(LUser),
    case catch odbc_queries:get_roster(LServer, Username) of
        {selected,
         [<<"username">>, <<"jid">>, <<"nick">>,
          <<"subscription">>, <<"ask">>, <<"askmessage">>,
          <<"server">>, <<"subscribe">>, <<"type">>],
         Items}
          when is_list(Items) ->
            JIDGroups = case catch
                             odbc_queries:get_roster_jid_groups(LServer,
                                                                Username)
                        of
                            {selected, [<<"jid">>, <<"grp">>], JGrps}
                              when is_list(JGrps) ->
                                JGrps;
                            _ -> []
                        end,
            GroupsDict = lists:foldl(fun ({J, G}, Acc) ->
                                             dict:append(J, G, Acc)
                                     end,
                                     dict:new(), JIDGroups),
            RItems = lists:flatmap(fun (I) ->
                                           case raw_to_record(LServer, I) of
                                               %% Bad JID in database:
                                               error -> [];
                                               R ->
                                                   SJID =
                                                   jlib:jid_to_binary(R#roster.jid),
                                                   Groups = case dict:find(SJID,
                                                                           GroupsDict)
                                                            of
                                                                {ok, Gs} -> Gs;
                                                                error -> []
                                                            end,
                                                   [R#roster{groups = Groups}]
                                           end
                                   end,
                                   Items),
            RItems;
        _ -> []
    end.

get_roster_by_jid_t(LUser, LServer, LJID) ->
    Username = ejabberd_odbc:escape(LUser),
    SJID = ejabberd_odbc:escape(jlib:jid_to_binary(LJID)),
    {selected,
     [<<"username">>, <<"jid">>, <<"nick">>,
      <<"subscription">>, <<"ask">>, <<"askmessage">>,
      <<"server">>, <<"subscribe">>, <<"type">>],
     Res} =
    odbc_queries:get_roster_by_jid(LServer, Username, SJID),
    case Res of
        [] ->
            #roster{usj = {LUser, LServer, LJID},
                    us = {LUser, LServer}, jid = LJID};
        [I] ->
            R = raw_to_record(LServer, I),
            case R of
                %% Bad JID in database:
                error ->
                    #roster{usj = {LUser, LServer, LJID},
                            us = {LUser, LServer}, jid = LJID};
                _ ->
                    R#roster{usj = {LUser, LServer, LJID},
                             us = {LUser, LServer}, jid = LJID, name = <<"">>}
            end
    end.

get_subscription_lists(_, LUser, LServer) ->
    Username = ejabberd_odbc:escape(LUser),
    case catch odbc_queries:get_roster(LServer, Username) of
        {selected,
         [<<"username">>, <<"jid">>, <<"nick">>,
          <<"subscription">>, <<"ask">>, <<"askmessage">>,
          <<"server">>, <<"subscribe">>, <<"type">>],
         Items}
          when is_list(Items) ->
            Items;
        _ -> []
    end.

roster_subscribe_t(LUser, LServer, LJID, Item) ->
    ItemVals = record_to_string(Item),
    Username = ejabberd_odbc:escape(LUser),
    SJID = ejabberd_odbc:escape(jlib:jid_to_binary(LJID)),
    odbc_queries:roster_subscribe(LServer, Username, SJID,
                                  ItemVals).

get_roster_by_jid_with_groups_t(LUser, LServer, LJID) ->
    Username = ejabberd_odbc:escape(LUser),
    SJID = ejabberd_odbc:escape(jlib:jid_to_binary(LJID)),
    case odbc_queries:get_roster_by_jid(LServer, Username,
                                        SJID)
    of
        {selected,
         [<<"username">>, <<"jid">>, <<"nick">>,
          <<"subscription">>, <<"ask">>, <<"askmessage">>,
          <<"server">>, <<"subscribe">>, <<"type">>],
         [I]} ->
            R = raw_to_record(LServer, I),
            Groups = case odbc_queries:get_roster_groups(LServer,
                                                         Username, SJID)
                     of
                         {selected, [<<"grp">>], JGrps} when is_list(JGrps) ->
                             [JGrp || [JGrp] <- JGrps];
                         _ -> []
                     end,
            R#roster{groups = Groups};
        {selected,
         [<<"username">>, <<"jid">>, <<"nick">>,
          <<"subscription">>, <<"ask">>, <<"askmessage">>,
          <<"server">>, <<"subscribe">>, <<"type">>],
         []} ->
            #roster{usj = {LUser, LServer, LJID},
                    us = {LUser, LServer}, jid = LJID}
    end.

remove_user(LUser, LServer) ->
    Username = ejabberd_odbc:escape(LUser),
    mod_roster:send_unsubscription_to_rosteritems(LUser, LServer),
    odbc_queries:del_user_roster_t(LServer, Username),
    ok.

update_roster_t(LUser, LServer, LJID, Item) ->
    Username = ejabberd_odbc:escape(LUser),
    SJID = ejabberd_odbc:escape(jlib:jid_to_binary(LJID)),
    ItemVals = record_to_string(Item),
    ItemGroups = groups_to_string(Item),
    odbc_queries:update_roster(LServer, Username, SJID, ItemVals, ItemGroups).

del_roster_t(LUser, LServer, LJID) ->
    Username = ejabberd_odbc:escape(LUser),
    SJID = ejabberd_odbc:escape(jlib:jid_to_binary(LJID)),
    odbc_queries:del_roster(LServer, Username, SJID).

raw_to_record(LServer,
              {User, SJID, Nick, SSubscription, SAsk, SAskMessage,
               _SServer, _SSubscribe, _SType}) ->
    case jlib:binary_to_jid(SJID) of
        error -> error;
        JID ->
            LJID = jlib:jid_tolower(JID),
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
    Username = ejabberd_odbc:escape(LUser),
    SJID = ejabberd_odbc:escape(jlib:jid_to_binary(LJID)),
    case catch odbc_queries:get_subscription(LServer,
                                             Username, SJID)
    of
        {selected, [<<"subscription">>], [{SSubscription}]} ->
            Subscription = case SSubscription of
                               <<"B">> -> both;
                               <<"T">> -> to;
                               <<"F">> -> from;
                               _ -> none
                           end,
            Groups = case catch
                          odbc_queries:get_rostergroup_by_jid(LServer, Username,
                                                              SJID)
                     of
                         {selected, [<<"grp">>], JGrps} when is_list(JGrps) ->
                             [JGrp || [JGrp] <- JGrps];
                         _ -> []
                     end,
            {Subscription, Groups};
        _ -> error
    end.

%%==============================================================================
%% Helper functions
%%==============================================================================

record_to_string(#roster{us = {User, _Server},
                         jid = JID, name = Name, subscription = Subscription,
                         ask = Ask, askmessage = AskMessage}) ->
    Username = ejabberd_odbc:escape(User),
    SJID =
    ejabberd_odbc:escape(jlib:jid_to_binary(jlib:jid_tolower(JID))),
    Nick = ejabberd_odbc:escape(Name),
    SSubscription = case Subscription of
                        both -> <<"B">>;
                        to -> <<"T">>;
                        from -> <<"F">>;
                        none -> <<"N">>
                    end,
    SAsk = case Ask of
               subscribe -> <<"S">>;
               unsubscribe -> <<"U">>;
               both -> <<"B">>;
               out -> <<"O">>;
               in -> <<"I">>;
               none -> <<"N">>
           end,
    SAskMessage = ejabberd_odbc:escape(AskMessage),
    [Username, SJID, Nick, SSubscription, SAsk, SAskMessage,
     <<"N">>, <<"">>, <<"item">>].

groups_to_string(#roster{us = {User, _Server},
                         jid = JID, groups = Groups}) ->
    Username = ejabberd_odbc:escape(User),
    SJID =
    ejabberd_odbc:escape(jlib:jid_to_binary(jlib:jid_tolower(JID))),
    lists:foldl(fun (<<"">>, Acc) -> Acc;
                    (Group, Acc) ->
                        G = ejabberd_odbc:escape(Group),
                        [[Username, SJID, G] | Acc]
                end,
                [], Groups).
