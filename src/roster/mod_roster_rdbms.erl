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

-behaviour(mod_roster_backend).

%% API
-export([init/2,
         transaction/2,
         read_roster_version/3,
         write_roster_version/5,
         get_roster/3,
         get_roster_entry/6,
         get_subscription_lists/3,
         roster_subscribe_t/2,
         update_roster_t/2,
         del_roster_t/4,
         remove_user_t/3,
         remove_domain_t/2]).

%% mod_roster backend API

-spec init(mongooseim:host_type(), gen_mod:module_opts()) -> ok.
init(HostType, _Opts) ->
    prepare_queries(HostType),
    ok.

-spec transaction(mongooseim:host_type(), fun(() -> any())) ->
    {aborted, any()} | {atomic, any()} | {error, any()}.
transaction(HostType, F) ->
    mongoose_rdbms:sql_transaction(HostType, F).

-spec read_roster_version(mongooseim:host_type(), jid:luser(), jid:lserver()) -> binary() | error.
read_roster_version(HostType, LUser, LServer) ->
    case mongoose_rdbms:execute_successfully(HostType, roster_version_get, [LServer, LUser]) of
        {selected, [{Version}]} -> Version;
        {selected, []} -> error
    end.

-spec write_roster_version(mongooseim:host_type(), jid:luser(), jid:lserver(),
                           mod_roster:transaction_state(), mod_roster:version()) -> ok.
write_roster_version(HostType, LUser, LServer, _TransactionState, Ver) ->
    version_upsert(HostType, LUser, LServer, Ver),
    ok.

-spec get_roster(mongooseim:host_type(), jid:luser(), jid:lserver()) -> [mod_roster:roster()].
get_roster(HostType, LUser, LServer) ->
    {selected, Rows} = execute_roster_get(HostType, LUser, LServer),
    {selected, GroupRows} = execute_roster_group_get(HostType, LUser, LServer),
    decode_roster_rows(LServer, LUser, Rows, GroupRows).

-spec get_roster_entry(mongooseim:host_type(), jid:luser(), jid:lserver(), mod_roster:contact(),
                       mod_roster:transaction_state(), mod_roster:entry_format()) ->
    mod_roster:roster() | does_not_exist.
get_roster_entry(HostType, LUser, LServer, LJID, _TransactionState, full) ->
    BinJID = jid:to_binary(LJID),
    case execute_roster_get_by_jid(HostType, LUser, LServer, BinJID) of
        {selected, []} ->
            does_not_exist;
        {selected, [Row]} ->
            Groups = get_groups_by_jid(HostType, LUser, LServer, BinJID),
            row_to_record(LServer, LUser, Row, #{BinJID => Groups})
    end;
get_roster_entry(HostType, LUser, LServer, LJID, _TransactionState, short) ->
    BinJID = jid:to_binary(LJID),
    case execute_roster_get_by_jid(HostType, LUser, LServer, BinJID) of
        {selected, []} ->
            does_not_exist;
        {selected, [Row]} ->
            row_to_record(LServer, LUser, Row, #{})
    end.

-spec get_subscription_lists(mongoose_acc:t(), jid:luser(), jid:lserver()) -> [mod_roster:roster()].
get_subscription_lists(Acc, LUser, LServer) ->
    HostType = mongoose_acc:host_type(Acc),
    {selected, Rows} = execute_roster_get(HostType, LUser, LServer),
    [row_to_record(LServer, LUser, Row, #{}) || Row <- Rows].

-spec roster_subscribe_t(mongooseim:host_type(), mod_roster:roster()) -> ok.
roster_subscribe_t(HostType, Item) ->
    RosterRow = record_to_row(Item),
    roster_upsert(HostType, RosterRow),
    ok.

-spec update_roster_t(mongooseim:host_type(), mod_roster:roster()) -> ok.
update_roster_t(HostType, Item) ->
    RosterRow = [LServer, LUser, BinJID | _] = record_to_row(Item),
    GroupRows = groups_to_rows(Item),
    roster_upsert(HostType, RosterRow),
    mongoose_rdbms:execute_successfully(HostType, roster_group_delete_by_jid,
                                        [LServer, LUser, BinJID]),
    [mongoose_rdbms:execute_successfully(HostType, roster_group_insert, GroupRow)
     || GroupRow <- GroupRows],
    ok.

-spec del_roster_t(mongooseim:host_type(), jid:luser(), jid:lserver(), mod_roster:contact()) -> ok.
del_roster_t(HostType, LUser, LServer, LJID) ->
    BinJID = jid:to_binary(LJID),
    mongoose_rdbms:execute_successfully(
      HostType, roster_delete_by_jid, [LServer, LUser, BinJID]),
    mongoose_rdbms:execute_successfully(
      HostType, roster_group_delete_by_jid, [LServer, LUser, BinJID]),
    ok.

-spec remove_user_t(mongooseim:host_type(), jid:luser(), jid:lserver()) -> ok.
remove_user_t(HostType, LUser, LServer) ->
    mongoose_rdbms:execute_successfully(HostType, roster_delete, [LServer, LUser]),
    mongoose_rdbms:execute_successfully(HostType, roster_group_delete, [LServer, LUser]),
    ok.

-spec remove_domain_t(mongooseim:host_type(), jid:lserver()) -> ok.
remove_domain_t(HostType, Domain) ->
    mongoose_rdbms:execute_successfully(HostType, rosterusers_remove_domain, [Domain]),
    mongoose_rdbms:execute_successfully(HostType, rostergroups_remove_domain, [Domain]),
    mongoose_rdbms:execute_successfully(HostType, roster_version_remove_domain, [Domain]),
    ok.

%% Query preparation

prepare_queries(HostType) ->
    mongoose_rdbms:prepare(roster_group_insert, rostergroups, [server, username, jid, grp],
                           <<"INSERT INTO rostergroups(server, username, jid, grp) "
                             "VALUES (?, ?, ?, ?)">>),
    mongoose_rdbms:prepare(roster_version_get, roster_version, [server, username],
                           <<"SELECT version FROM roster_version "
                             "WHERE server = ? AND username = ?">>),
    mongoose_rdbms:prepare(roster_get, rosterusers, [server, username],
                           <<"SELECT ", (roster_fields())/binary,
                             " FROM rosterusers WHERE server = ? AND username = ?">>),
    mongoose_rdbms:prepare(roster_get_by_jid, rosterusers, [server, username, jid],
                           <<"SELECT ", (roster_fields())/binary,
                             " FROM rosterusers WHERE server = ? AND username = ? AND jid = ?">>),
    mongoose_rdbms:prepare(roster_group_get, rostergroups, [server, username],
                           <<"SELECT jid, grp FROM rostergroups WHERE server = ? AND username = ?">>),
    mongoose_rdbms:prepare(roster_group_get_by_jid, rostergroups, [server, username, jid],
                           <<"SELECT grp FROM rostergroups "
                             "WHERE server = ? AND username = ? AND jid = ?">>),
    mongoose_rdbms:prepare(roster_delete, rosterusers, [server, username],
                           <<"DELETE FROM rosterusers WHERE server = ? AND username = ?">>),
    mongoose_rdbms:prepare(roster_group_delete, rostergroups, [server, username],
                           <<"DELETE FROM rostergroups WHERE server = ? AND username = ?">>),
    mongoose_rdbms:prepare(roster_delete_by_jid, rosterusers, [server, username, jid],
                           <<"DELETE FROM rosterusers"
                             " WHERE server = ? AND username = ? AND jid = ?">>),
    mongoose_rdbms:prepare(roster_group_delete_by_jid, rostergroups, [server, username, jid],
                           <<"DELETE FROM rostergroups"
                             " WHERE server = ? AND username = ? AND jid = ?">>),
    mongoose_rdbms:prepare(rosterusers_remove_domain, rosterusers, [server],
                           <<"DELETE FROM rosterusers WHERE server = ?">>),
    mongoose_rdbms:prepare(rostergroups_remove_domain, rostergroups, [server],
                           <<"DELETE FROM rostergroups WHERE server = ?">>),
    mongoose_rdbms:prepare(roster_version_remove_domain, roster_version, [server],
                           <<"DELETE FROM roster_version WHERE server = ?">>),
    prepare_roster_upsert(HostType),
    prepare_version_upsert(HostType),
    ok.

prepare_roster_upsert(HostType) ->
    Fields = [<<"nick">>, <<"subscription">>, <<"ask">>, <<"askmessage">>],
    Filter = [<<"server">>, <<"username">>, <<"jid">>],
    rdbms_queries:prepare_upsert(HostType, roster_upsert, rosterusers,
                                 Filter ++ Fields, Fields, Filter).

prepare_version_upsert(HostType) ->
    Fields = [<<"version">>],
    Filter = [<<"server">>, <<"username">>],
    rdbms_queries:prepare_upsert(HostType, roster_version_upsert, roster_version,
                                 Filter ++ Fields, Fields, Filter).

%% Query Helpers

-spec execute_roster_get(mongooseim:host_type(), jid:luser(), jid:lserver()) ->
          mongoose_rdbms:query_result().
execute_roster_get(HostType, LUser, LServer) ->
    mongoose_rdbms:execute_successfully(HostType, roster_get, [LServer, LUser]).

-spec execute_roster_group_get(mongooseim:host_type(), jid:luser(), jid:lserver()) ->
          mongoose_rdbms:query_result().
execute_roster_group_get(HostType, LUser, LServer) ->
    mongoose_rdbms:execute_successfully(HostType, roster_group_get, [LServer, LUser]).

-spec execute_roster_get_by_jid(mongooseim:host_type(), jid:luser(), jid:lserver(), jid:literal_jid()) ->
          mongoose_rdbms:query_result().
execute_roster_get_by_jid(HostType, LUser, LServer, BinJID) ->
    mongoose_rdbms:execute_successfully(HostType, roster_get_by_jid, [LServer, LUser, BinJID]).

-spec execute_roster_get_groups_by_jid(mongooseim:host_type(), jid:luser(), jid:lserver(), jid:literal_jid()) ->
    mongoose_rdbms:query_result().
execute_roster_get_groups_by_jid(HostType, LUser, LServer, BinJID) ->
    mongoose_rdbms:execute_successfully(HostType, roster_group_get_by_jid, [LServer, LUser, BinJID]).

-spec roster_upsert(mongooseim:host_type(), list()) -> mongoose_rdbms:query_result().
roster_upsert(HostType, [LServer, LUser, BinJID | Rest] = RosterRow) ->
    InsertParams = RosterRow,
    UpdateParams = Rest,
    UniqueKeyValues = [LServer, LUser, BinJID],
    {updated, _} = rdbms_queries:execute_upsert(HostType, roster_upsert,
                                                InsertParams, UpdateParams, UniqueKeyValues).

-spec version_upsert(mongooseim:host_type(), jid:luser(), jid:lserver(), mod_roster:version()) ->
          mongoose_rdbms:query_result().
version_upsert(HostType, LUser, LServer, Version) ->
    InsertParams = [LServer, LUser, Version],
    UpdateParams = [Version],
    UniqueKeyValues = [LServer, LUser],
    {updated, _} = rdbms_queries:execute_upsert(HostType, roster_version_upsert,
                                                InsertParams, UpdateParams, UniqueKeyValues).

-spec get_groups_by_jid(mongooseim:host_type(), jid:luser(), jid:lserver(), jid:literal_jid()) ->
          [binary()].
get_groups_by_jid(HostType, LUser, LServer, BinJID) ->
    {selected, Rows} = execute_roster_get_groups_by_jid(HostType, LUser, LServer, BinJID),
    [Group || {Group} <- Rows].

%%==============================================================================
%% Helper functions
%%==============================================================================

decode_subscription($B) -> both;
decode_subscription($T) -> to;
decode_subscription($F) -> from;
decode_subscription($N) -> none.

encode_subscription(both) -> <<"B">>;
encode_subscription(to)   -> <<"T">>;
encode_subscription(from) -> <<"F">>;
encode_subscription(none) -> <<"N">>.

decode_ask($S) -> subscribe;
decode_ask($U) -> unsubscribe;
decode_ask($B) -> both;
decode_ask($O) -> out;
decode_ask($I) -> in;
decode_ask($N) -> none.

encode_ask(subscribe) -> <<"S">>;
encode_ask(unsubscribe) -> <<"U">>;
encode_ask(both) -> <<"B">>;
encode_ask(out)  -> <<"O">>;
encode_ask(in)   -> <<"I">>;
encode_ask(none) -> <<"N">>.

record_to_row(#roster{us = {LUser, LServer},
                      jid = JID, name = Nick, subscription = Subscription,
                      ask = Ask, askmessage = AskMessage}) ->
    BinJID = jid:to_binary(jid:to_lower(JID)),
    ExtSubscription = encode_subscription(Subscription),
    ExtAsk = encode_ask(Ask),
    [LServer, LUser, BinJID, Nick, ExtSubscription, ExtAsk, AskMessage].

groups_to_rows(#roster{us = {LUser, LServer}, jid = JID, groups = Groups}) ->
    BinJID = jid:to_binary(jid:to_lower(JID)),
    lists:foldl(fun (<<>>, Acc) -> Acc;
                    (Group, Acc) -> [[LServer, LUser, BinJID, Group] | Acc]
                end, [], Groups).

%% We don't care about `server, subscribe, type' fields
roster_fields() ->
    <<"jid, nick, subscription, ask, askmessage">>.

%% Decode fields from `roster_fields()' into a record
row_to_record(LServer, LUser,
              {BinJID, Nick, ExtSubscription, ExtAsk, AskMessage}, GroupsPerJID) ->
    JID = jid:from_binary_noprep(BinJID), %% We trust the DB has correct jids
    LJID = jid:to_lower(JID), %% Convert to tuple {U,S,R}
    Subscription = decode_subscription(mongoose_rdbms:character_to_integer(ExtSubscription)),
    Ask = decode_ask(mongoose_rdbms:character_to_integer(ExtAsk)),
    US = {LUser, LServer},
    USJ = {US, LJID},
    Groups = maps:get(BinJID, GroupsPerJID, []),
    #roster{usj = USJ, us = US, jid = LJID, name = Nick,
            subscription = Subscription, ask = Ask, groups = Groups, askmessage = AskMessage}.

decode_roster_rows(LServer, LUser, Rows, JIDGroups) ->
    GroupsPerJID = group_per_jid(JIDGroups),
    [row_to_record(LServer, LUser, Row, GroupsPerJID) || Row <- Rows].

group_per_jid(Pairs) ->
    F = fun ({Jid, Group}, Acc) ->
                case Acc of
                    #{Jid := Groups} ->
                        Acc#{Jid := [Group | Groups]};
                    _ ->
                        Acc#{Jid => [Group]}
                end
        end,
    lists:foldl(F, #{}, Pairs).
