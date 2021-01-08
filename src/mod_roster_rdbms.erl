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
-include("mongoose_logger.hrl").

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
init(Host, _Opts) ->
    prepare_queries(Host),
    ok.

prepare_queries(Host) ->
    mongoose_rdbms:prepare(roster_group_insert, rostergroups, [username, jid, grp],
                           <<"INSERT INTO rostergroups(username, jid, grp) "
                             "VALUES (?, ?, ?)">>),
    mongoose_rdbms:prepare(roster_version_get, roster_version, [username],
                           <<"SELECT version FROM roster_version "
                             "WHERE username=?">>),
    mongoose_rdbms:prepare(roster_get, rosterusers, [username],
        <<"SELECT ", (roster_fields())/binary,
           " FROM rosterusers WHERE username=?">>),
    mongoose_rdbms:prepare(roster_get_by_jid, rostergroups, [username, jid],
        <<"SELECT ", (roster_fields())/binary,
           " FROM rosterusers WHERE username=? AND jid=?">>),
    mongoose_rdbms:prepare(roster_group_get, rostergroups, [username],
        <<"SELECT jid, grp FROM rostergroups WHERE username=?">>),
    mongoose_rdbms:prepare(roster_group_get_by_jid, rostergroups, [username, jid],
        <<"SELECT grp FROM rostergroups "
          "WHERE username=? AND jid=?">>),
    mongoose_rdbms:prepare(roster_delete, rosterusers, [username],
                           <<"DELETE FROM rosterusers WHERE username=?">>),
    mongoose_rdbms:prepare(roster_group_delete, rostergroups, [username],
                           <<"DELETE FROM rostergroups WHERE username=?">>),
    mongoose_rdbms:prepare(roster_delete_by_jid, rosterusers, [username, jid],
                           <<"DELETE FROM rosterusers WHERE username=? AND jid=?">>),
    mongoose_rdbms:prepare(roster_group_delete_by_jid, rostergroups, [username, jid],
                           <<"DELETE FROM rostergroups WHERE username=? AND jid=?">>),
    prepare_roster_upsert(Host),
    prepare_version_upsert(Host),
    ok.


%% We don't care about `server, subscribe, type' fields
roster_fields() ->
    <<"username, jid, nick, subscription, ask, askmessage">>.

prepare_roster_upsert(Host) ->
    Fields = [<<"nick">>, <<"subscription">>, <<"ask">>,
              <<"askmessage">>, <<"server">>, <<"subscribe">>, <<"type">>],
    Filter = [<<"username">>, <<"jid">>],
    rdbms_queries:prepare_upsert(Host, roster_upsert, rosterusers,
                                 Filter ++ Fields, Fields, Filter).

prepare_version_upsert(Host) ->
    Fields = [<<"version">>],
    Filter = [<<"username">>],
    rdbms_queries:prepare_upsert(Host, roster_version_upsert, roster_version,
                                 Filter ++ Fields, Fields, Filter).

%% Query Helpers

execute_roster_get(LServer, LUser) ->
    mongoose_rdbms:execute_successfully(LServer, roster_get, [LUser]).

roster_upsert(Host, LUser, BinJID, RosterRow) ->
    [_LUser, _BinJID|Rest] = RosterRow,
    InsertParams = RosterRow,
    UpdateParams = Rest,
    UniqueKeyValues = [LUser, BinJID],
    rdbms_queries:execute_upsert(Host, roster_upsert, InsertParams, UpdateParams, UniqueKeyValues).

version_upsert(Host, LUser, Version) ->
    InsertParams = [LUser, Version],
    UpdateParams = [Version],
    UniqueKeyValues = [LUser],
    rdbms_queries:execute_upsert(Host, roster_version_upsert, InsertParams, UpdateParams, UniqueKeyValues).

%% API functions

-spec transaction(LServer :: jid:lserver(), F :: fun()) ->
    {aborted, Reason :: any()} | {atomic, Result :: any()}.
transaction(LServer, F) ->
    mongoose_rdbms:sql_transaction(LServer, F).

-spec read_roster_version(jid:luser(), jid:lserver()) -> binary() | error.
read_roster_version(LUser, LServer) ->
    case mongoose_rdbms:execute_successfully(LServer, roster_version_get, [LUser]) of
        {selected, [{Version}]} -> Version;
        {selected, []} -> error
    end.

write_roster_version(LUser, LServer, _InTransaction, Ver) ->
    version_upsert(LServer, LUser, Ver).

get_roster(LUser, LServer) ->
    {selected, Rows} = execute_roster_get(LServer, LUser),
    {selected, GroupRows} = mongoose_rdbms:execute_successfully(LServer, roster_group_get, [LUser]),
    decode_roster_rows(LServer, Rows, GroupRows).

get_roster_entry_t(LUser, LServer, LJID) ->
    %% `mongoose_rdbms:execute' automatically detects if we are in a transaction or not.
    get_roster_entry(LUser, LServer, LJID).

get_roster_entry(LUser, LServer, LJID) ->
    BinJID = jid:to_binary(LJID),
    {selected, Rows} = mongoose_rdbms:execute_successfully(LServer, roster_get_by_jid, [LUser, BinJID]),
    case Rows of
        [] -> does_not_exist;
        [Row] -> row_to_record(LServer, Row)
    end.

get_roster_entry_t(LUser, LServer, LJID, full) ->
    get_roster_entry(LUser, LServer, LJID, full).

%% full means we should query for groups too
get_roster_entry(LUser, LServer, LJID, full) ->
    case get_roster_entry(LUser, LServer, LJID) of
        does_not_exist -> does_not_exist;
        Rec ->
            Groups = get_groups_by_jid(LUser, LServer, LJID),
            record_with_groups(Rec, Groups)
    end.

get_subscription_lists(_, LUser, LServer) ->
    {selected, Rows} = execute_roster_get(LServer, LUser),
    [row_to_record(LServer, Row) || Row <- Rows].

roster_subscribe_t(LUser, LServer, LJID, Item) ->
    BinJID = jid:to_binary(LJID),
    RosterRow = record_to_row(Item),
    roster_upsert(LServer, LUser, BinJID, RosterRow).

remove_user(LUser, LServer) ->
    F = fun() ->
            mongoose_rdbms:execute_successfully(LServer, roster_delete, [LUser]),
            mongoose_rdbms:execute_successfully(LServer, roster_group_delete, [LUser])
        end,
    mongoose_rdbms:sql_transaction(LServer, F),
    ok.

update_roster_t(LUser, LServer, LJID, Item) ->
    BinJID = jid:to_binary(LJID),
    RosterRow = record_to_row(Item),
    GroupRows = groups_to_rows(Item),
    roster_upsert(LServer, LUser, BinJID, RosterRow),
    mongoose_rdbms:execute_successfully(LServer, roster_group_delete_by_jid, [LUser, BinJID]),
    [mongoose_rdbms:execute_successfully(LServer, roster_group_insert, GroupRow)
     || GroupRow <- GroupRows],
    ok.

del_roster_t(LUser, LServer, LJID) ->
    BinJID = jid:to_binary(LJID),
    mongoose_rdbms:execute_successfully(LServer, roster_delete_by_jid, [LUser, BinJID]),
    mongoose_rdbms:execute_successfully(LServer, roster_group_delete_by_jid, [LUser, BinJID]).

get_groups_by_jid(LUser, LServer, LJID) ->
    BinJID = jid:to_binary(LJID),
    {selected, Rows} = mongoose_rdbms:execute_successfully(
                         LServer, roster_group_get_by_jid, [LUser, BinJID]),
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

record_to_row(#roster{us = {LUser, _Server},
                      jid = JID, name = Nick, subscription = Subscription,
                      ask = Ask, askmessage = AskMessage}) ->
    BinJID = jid:to_binary(jid:to_lower(JID)),
    ExtSubscription = encode_subscription(Subscription),
    ExtAsk = encode_ask(Ask),
    [LUser, BinJID, Nick, ExtSubscription, ExtAsk, AskMessage,
     <<"N">>, <<>>, <<"item">>].

groups_to_rows(#roster{us = {LUser, _LServer}, jid = JID, groups = Groups}) ->
    BinJID = jid:to_binary(jid:to_lower(JID)),
    lists:foldl(fun (<<>>, Acc) -> Acc;
                    (Group, Acc) -> [[LUser, BinJID, Group] | Acc]
                end, [], Groups).

%% We must not leak our external RDBMS format representation into MongooseIM
raw_to_record(_LServer, Rec) -> Rec.

%% Decode fields from `roster_fields()' into a record
row_to_record(LServer,
              {User, BinJID, Nick, ExtSubscription, ExtAsk, AskMessage}) ->
    JID = parse_jid(BinJID),
    LJID = jid:to_lower(JID), %% Convert to tuple {U,S,R}
    Subscription = decode_subscription(mongoose_rdbms:character_to_integer(ExtSubscription)),
    Ask = decode_ask(mongoose_rdbms:character_to_integer(ExtAsk)),
    USJ = {User, LServer, LJID},
    US = {User, LServer},
    #roster{usj = USJ, us = US, jid = LJID, name = Nick,
            subscription = Subscription, ask = Ask, askmessage = AskMessage}.

row_to_binary_jid(Row) -> element(2, Row).

record_with_groups(Rec, Groups) ->
    Rec#roster{groups = Groups}.

%% We require all DB jids to be parsable.
%% They should be lowered too.
parse_jid(BinJID) ->
    case jid:from_binary(BinJID) of
        error ->
            error(#{what => parse_jid_failed, jid => BinJID});
        JID ->
            JID
    end.

decode_roster_rows(LServer, Rows, JIDGroups) ->
    GroupsDict = pairs_to_dict(JIDGroups),
    [raw_to_record_with_group(LServer, Row, GroupsDict) || Row <- Rows].

pairs_to_dict(Pairs) ->
    F = fun ({K, V}, Acc) -> dict:append(K, V, Acc) end,
    lists:foldl(F, dict:new(), Pairs).

raw_to_record_with_group(LServer, Row, GroupsDict) ->
    Rec = row_to_record(LServer, Row),
    BinJID = row_to_binary_jid(Row),
    Groups = dict_get(BinJID, GroupsDict, []),
    record_with_groups(Rec, Groups).

dict_get(K, Dict, Default) ->
    case dict:find(K, Dict) of
        {ok, Values} -> Values;
        error -> Default
    end.
