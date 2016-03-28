%%%----------------------------------------------------------------------
%%% File    : mod_roster_odbc.erl
%%% Author  : Michael Uvarov <michael.uvarov@erlang-solutions.com>
%%% Purpose : mod_roster cassandra backend
%%%
%%%
%%% MongooseIM, Copyright (C) 2016      Erlang Solutions Ltd.
%%%
%%%----------------------------------------------------------------------

-module(mod_roster_cassandra).
-include("mod_roster.hrl").
-include("jlib.hrl").
-include("ejabberd.hrl").

-behaviour(mod_roster).
-behaviour(mongoose_cassandra).

%% API
-export([init/2,
         transaction/2,
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
         read_subscription_and_groups/3,
         raw_to_record/2]).

-export([prepared_queries/0]).

prepared_queries() ->
    [{read_roster_version_query, "SELECT version FROM roster_version WHERE username = ?"},
     {write_roster_version_query, "INSERT INTO roster_version (username, version) VALUES (?, ?)"},
     {write_roster_item_query, "INSERT INTO rosterusers (username, jid, nick, subscription, ask, askmessage, groups) VALUES (?,?,?,?,?,?,?)"},
     {get_roster_query, "SELECT jid, nick, subscription, ask, askmessage, groups FROM rosterusers WHERE username = ?"},
     {get_roster_item_query, "SELECT nick, subscription, ask, askmessage, groups FROM rosterusers WHERE username = ? AND jid = ?"},
     {read_subscription_and_groups_query, "SELECT subscription, groups FROM rosterusers WHERE username = ? AND jid = ?"},
     {remove_roster_item_query, "DELETE FROM rosterusers WHERE username = ? AND jid = ?"},
     {remove_roster_query, "DELETE FROM rosterusers WHERE username = ?"},
     {remove_roster_version_query, "DELETE FROM roster_version WHERE username = ?"}].



-spec init(ejabberd:server(), list()) -> ok.
init(_Host, Opts) ->
    compile_params_module(Opts),
    ok.

-spec transaction(LServer :: ejabberd:lserver(), F :: fun()) ->
    {aborted, Reason :: any()} | {atomic, Result :: any()}.
transaction(_LServer, F) ->
    try F() of
        Result ->
            {atomic, Result}
    catch
        _:Reason -> {aborted, Reason}
    end.

-spec read_roster_version(ejabberd:luser(), ejabberd:lserver())
-> binary() | error.
read_roster_version(LUser, LServer) ->
    UserJID = jid:make(LUser, LServer, <<>>),
    PoolName = pool_name(LServer, LUser),
    Params = [LUser],
    Res = mongoose_cassandra_worker:cql_query_pool(PoolName, UserJID, ?MODULE, read_roster_version_query, Params),
    case Res of
        {ok, [[Version]]} ->
            Version;
        {ok, []} ->
            error;
        {error, Other} ->
            ?ERROR_MSG("issue=\"read_roster_version failed\", user=~ts, reason=~1000p",
                       [LUser, Other]),
            error
    end.

write_roster_version(LUser, LServer, _InTransaction, Ver) ->
    UserJID = jid:make(LUser, LServer, <<>>),
    PoolName = pool_name(LServer, LUser),
    Params = [LUser, Ver],
    Res = mongoose_cassandra_worker:cql_query_pool(PoolName, UserJID, ?MODULE, write_roster_version_query, Params),
    case Res of
        {ok, []} ->
            ok;
        {error, Other} ->
            ?ERROR_MSG("issue=\"write_roster_version failed\", user=~ts, reason=~1000p",
                       [LUser, Other]),
            error
    end.

get_roster(LUser, LServer) ->
    UserJID = jid:make(LUser, LServer, <<>>),
    PoolName = pool_name(LServer, LUser),
    Params = [LUser],
    Res = mongoose_cassandra_worker:cql_query_pool(PoolName, UserJID, ?MODULE, get_roster_query, Params),
    case Res of
        {ok, []} ->
            [];
        {ok, [_|_]=Rows} ->
            roster_rows_to_items(UserJID, Rows);
        {error, Other} ->
            ?ERROR_MSG("issue=\"read_roster_version failed\", user=~ts, reason=~1000p",
                       [LUser, Other]),
            []
    end.

get_roster_by_jid_t(LUser, LServer, LJID) ->
    UserJID = jid:make(LUser, LServer, <<>>),
    PoolName = pool_name(LServer, LUser),
    ContactJID = jid:to_binary(LJID),
    Params = [LUser, ContactJID],
    Res = mongoose_cassandra_worker:cql_query_pool(PoolName, UserJID, ?MODULE, get_roster_item_query, Params),
    case Res of
        {ok, []} ->
            #roster{usj = {LUser, LServer, LJID},
                    us = {LUser, LServer}, jid = LJID};
        {ok, [Row]} ->
            Row2 = [ContactJID|Row],
            Item = roster_row_to_item(UserJID, Row2),
            case Item of
                error ->
                    #roster{usj = {LUser, LServer, LJID},
                            us = {LUser, LServer}, jid = LJID};
                _ ->
                    Item
            end;
        {error, Other} ->
            ?ERROR_MSG("issue=\"read_roster_version failed\", user=~ts, reason=~1000p",
                       [LUser, Other]),
            []
    end.

get_subscription_lists(_, LUser, LServer) ->
    get_roster(LUser, LServer).

roster_subscribe_t(LUser, LServer, LJID, Item) ->
    update_roster_t(LUser, LServer, LJID, Item).

get_roster_by_jid_with_groups_t(LUser, LServer, LJID) ->
    get_roster_by_jid_t(LUser, LServer, LJID).

remove_user(LUser, LServer) ->
    UserJID = jid:make(LUser, LServer, <<>>),
    PoolName = pool_name(LServer, LUser),
    Params = [LUser],
    Queries = [{remove_roster_query, Params},
               {remove_roster_version_query, Params}],
    %% don't use unlogged batches here because Cassandra does not allow to do them
    %% to different partitions (one case is operations on several different tables).
    %% not_batch or logged
    Res = mongoose_cassandra_worker:cql_batch_pool(PoolName, UserJID, ?MODULE, Queries, not_batch),
    handle_empty_result(Res, remove_user).

update_roster_t(LUser, LServer, _LJID, Item) ->
    UserJID = jid:make(LUser, LServer, <<>>),
    PoolName = pool_name(LServer, LUser),
    Params = record_to_string(Item),
    Res = mongoose_cassandra_worker:cql_query_pool(PoolName, UserJID, ?MODULE, write_roster_item_query, Params),
    handle_empty_result(Res, update_roster_t).

del_roster_t(LUser, LServer, LJID) ->
    UserJID = jid:make(LUser, LServer, <<>>),
    PoolName = pool_name(LServer, LUser),
    SJID = jid:to_binary(LJID),
    Params = [LUser, SJID],
    Res = mongoose_cassandra_worker:cql_query_pool(PoolName, UserJID, ?MODULE, remove_roster_item_query, Params),
    handle_empty_result(Res, del_roster_t).

read_subscription_and_groups(LUser, LServer, LJID) ->
    UserJID = jid:make(LUser, LServer, <<>>),
    PoolName = pool_name(LServer, LUser),
    ContactJID = jid:to_binary(LJID),
    Params = [LUser, ContactJID],
    Res = mongoose_cassandra_worker:cql_query_pool(PoolName, UserJID, ?MODULE, read_subscription_and_groups_query, Params),
    case Res of
        {ok, []} ->
            error;
        {ok, [[SSubscription, SGroups]]} ->
            Subscription = case SSubscription of
                               <<"B">> -> both;
                               <<"T">> -> to;
                               <<"F">> -> from;
                               _ -> none
                           end,
            Groups = cassandra_set_to_list(SGroups),
            {Subscription, Groups};
        {error, Other} ->
            ?ERROR_MSG("issue=\"read_roster_version failed\", user=~ts, reason=~1000p",
                       [LUser, Other]),
            []
    end.

raw_to_record(_, Item) -> Item.

%%==============================================================================
%% Helper functions
%%==============================================================================

record_to_string(#roster{us = {User, _Server},
                         jid = JID, name = Nick, subscription = Subscription,
                         ask = Ask, askmessage = AskMessage, groups = Groups}) ->
    SJID = jid:to_binary(jid:to_lower(JID)),
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
    SGroups = list_to_cassandra_set(Groups),
    [User, SJID, Nick, SSubscription, SAsk, SAskMessage, SGroups].

roster_rows_to_items(UserJID, Rows) ->
    skip_errors([roster_row_to_item(UserJID, Row) || Row <- Rows]).

skip_errors(Items) ->
    [Item || Item <- Items, Item =/= error].

roster_row_to_item(#jid{luser=User, lserver=LServer},
                   [Jid, Nick, SSubscription, SAsk, AskMessage, SGroups]) ->
    case jid:from_binary(Jid) of
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
            Groups = cassandra_set_to_list(SGroups),
            #roster{usj = {User, LServer, LJID},
                    us = {User, LServer}, jid = LJID, name = Nick,
                    subscription = Subscription, ask = Ask,
                    askmessage = AskMessage,
                    groups = Groups}
    end.

handle_empty_result({ok, _}, _Pos) -> ok;
handle_empty_result({error, Other}, Pos) ->
    ?ERROR_MSG("issue=\"handle_empty_result failed\", position=~p, reason=~1000p",
               [Pos, Other]),
    {error, Other};
handle_empty_result(Other, Pos) -> handle_empty_result({error, Other}, Pos).

%% seestar related
cassandra_set_to_list(null) ->
    [];
cassandra_set_to_list(Set) when is_list(Set) ->
    Set;
cassandra_set_to_list(Set) ->
    case sets:is_set(Set) of
        true ->
            sets:to_list(Set);
        false ->
            ?ERROR_MSG("issue=\"cassandra_set_to_list failed\", reason=bad_set, argument=~p",
                       [Set]),
            []
    end.

list_to_cassandra_set(List) ->
    sets:from_list(List).

%% ----------------------------------------------------------------------
%% Dynamic params module

compile_params_module(Params) ->
    CodeStr = params_helper(Params),
    {Mod, Code} = dynamic_compile:from_string(CodeStr),
    code:load_binary(Mod, "mod_roster_cassandra_params.erl", Code).

params_helper(Params) ->
    binary_to_list(iolist_to_binary(io_lib:format(
        "-module(mod_roster_cassandra_params).~n"
        "-compile(export_all).~n"
        "pool_name() -> ~p.~n",
        [proplists:get_value(pool_name, Params, default)
        ]))).

-spec pool_name(binary(), binary()) -> term().
pool_name(_LServer, _LUser) ->
    mod_roster_cassandra_params:pool_name().
