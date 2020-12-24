%%%----------------------------------------------------------------------
%%% File    : service_admin_extra_migration.erl
%%% Author  : Viacheslav Katsuba <v.katsuba.dev@gmail.com>
%%% Purpose : Migration Mnesia to RDBMS
%%% Created : 11 Dec 2019 by Viacheslav Katsuba <v.katsuba.dev@gmail.com>
%%%
%%% MongooseIM, Copyright (C) 2020 Erlang Solutions Ltd.
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

-module(service_admin_extra_migration).

-export([commands/0, migrate/3, get_data/2]).

-export([
    migrate_pubsub_nodes/1,
    migrate_pubsub_affiliations/1,
    migrate_pubsub_subscriptions/1,
    migrate_pubsub_items/1,
    migrate_vcard/1,
    migrate_vcard_search/1,
    migrate_users/1,
    migrate_event_pusher_push_subscription/1,
    migrate_rosterusers/1,
    migrate_roster_version/1,
    migrate_rostergroups/1,
    migrate_last/1,
    migrate_private_storage/1,
    migrate_offline_message/1,
    migrate_muc_light_rooms/1
]).

%%% ==================================================================
%%% Includes
%%% ==================================================================

-include("jlib.hrl").
-include("pubsub.hrl").
-include("mongoose.hrl").
-include("mod_last.hrl").
-include("mod_vcard.hrl").
-include("mod_roster.hrl").
-include("mod_offline.hrl").
-include("ejabberd_commands.hrl").

%%% ==================================================================
%%% API
%%% ==================================================================

%% -------------------------------------------------------------------
%% @doc
%% Commands of migration CTL
%% @end
%% -------------------------------------------------------------------
-spec commands() -> [ejabberd_commands:cmd()].

commands() ->
    [#ejabberd_commands{
        name = migrate,
        tags = [migration],
        desc = "Migration from Mnesia to MySQL",
        module = ?MODULE,
        function = migrate,
        args = [{from, binary}, {to, binary}, {act, binary}],
        result = {res, restuple}
    }].

%% -------------------------------------------------------------------
%% @doc
%% Migrate from DB to DB
%% @end
%% -------------------------------------------------------------------
-spec migrate(From :: binary(), To :: binary(), Act :: binary()) -> Result :: tuple().

migrate(<<"mnesia">>, <<"rdbms">>, Act) when is_binary(Act) ->
    OP = ejabberd_config:get_local_option_or_default(outgoing_pools, []),
    case [X || {X, _, _, _, _} <- OP, X =:= rdbms] of
        [rdbms] ->
            case try_migrate(Act) of
                {ok, Resp} ->
                    {ok, Resp};
                {error, Reason} ->
                    {exists, Reason}
            end;
        [rdbms|_] ->
            {exists, "Detected multi 'rdbms' configuration. Need to use one 'rdbms' configuration for correct migration"};
        _ ->
            {exists, "Looks like that the MySQL is not configured. Check 'mongooseim.cfg'"}
  end;

migrate(From, To, _) ->
    {exists, io_lib:format("Migration from '~s' to '~s' is not supported", [From, To])}.

%% -------------------------------------------------------------------
%% @doc
%% @private
%% Trying do migrate from DB to DB
%% @end
%% -------------------------------------------------------------------
-spec try_migrate(Act :: binary() | tuple()) -> Result :: tuple().

try_migrate(Act) when is_binary(Act) ->
    case Act of
        <<"all">> ->
            Acts = [<<"pubsub_nodes">>, <<"pubsub_affiliations">>, <<"pubsub_subscriptions">>, <<"pubsub_items">>,
                    <<"vcard">>, <<"vcard_search">>, <<"users">>, <<"event_pusher_push_subscription">>,
                    <<"rosterusers">>, <<"roster_version">>, <<"rostergroups">>, <<"last">>, <<"private_storage">>,
                    <<"offline_message">>, <<"muc_light_rooms">>],
            _ = [try_migrate(X) || X <- Acts],
            {ok, io_lib:format("Completed the migration of the tables: ~p", [Acts])};
        <<"pubsub_nodes">> ->
            try_migrate({Act, pubsub_node});
        Act when Act =:= <<"pubsub_affiliations">>; Act =:= <<"pubsub_subscriptions">> ->
            try_migrate({Act, pubsub_state});
        <<"pubsub_items">> ->
            try_migrate({Act, pubsub_item});
        <<"vcard">> ->
            try_migrate({Act, vcard});
        <<"vcard_search">> ->
            try_migrate({Act, vcard_search});
        <<"users">> ->
            try_migrate({Act, passwd});
        <<"event_pusher_push_subscription">> ->
            try_migrate({Act, push_subscription});
        Act when Act =:= <<"rosterusers">>; Act =:= <<"rostergroups">> ->
            try_migrate({Act, roster});
        <<"roster_version">> ->
            try_migrate({Act, roster_version});
        <<"last">> ->
            try_migrate({Act, last_activity});
        <<"private_storage">> ->
            try_migrate({Act, private_storage});
        <<"offline_message">> ->
            try_migrate({Act, offline_msg});
        <<"muc_light_rooms">> ->
            try_migrate({Act, muc_light_room});
        Act ->
            {error, io_lib:format("Migrate dont support act: ~p", [binary_to_list(Act)])}
    end;

try_migrate({Act, Table}) when is_atom(Table) ->
    try_migrate({Act, Table, get_data(first, Table)});

try_migrate({Act, Table, TableData}) ->
    case TableData of
        {ok, [_] = Data, '$end_of_table'} ->
            ?MODULE:(list_to_existing_atom("migrate_" ++ binary_to_list(Act)))(Data),
            {ok, io_lib:format("Completed the migration of the table: '~s'", [Act])};
        {ok, _, '$end_of_table'} ->
            {ok, io_lib:format("Completed the migration of the table: '~s'", [Act])};
        {ok, Data, NextKey} ->
            ?MODULE:(list_to_existing_atom("migrate_" ++ binary_to_list(Act)))(Data),
            try_migrate({Act, Table, get_data(next, {Table, NextKey})});
        Error ->
            Error
    end.

%% -------------------------------------------------------------------
%% @private
%% @doc
%% Helper for migrate "pubsub_node" from Mnesia to SQL DB
%% @end
%% -------------------------------------------------------------------
-spec migrate_pubsub_nodes(Data :: list()) -> Result :: ok.

migrate_pubsub_nodes([]) ->
    ok;

migrate_pubsub_nodes([H|_]) ->
    {Host, ID} = H#pubsub_node.nodeid,
    Owners = jiffy:encode([jid:to_binary(O) || O <- H#pubsub_node.owners]),
    Ops = jiffy:encode({H#pubsub_node.options}),
    Cols = ["p_key", "name", "type", "owners", "options"], % "nidx"
    Vals = [Host, ID, H#pubsub_node.type, Owners, Ops],    % H#pubsub_node.id
    Q = ["INSERT INTO pubsub_nodes ", expand_sql_vals(Cols, Vals), ";"],
    case mongoose_rdbms:sql_query(?MYNAME, Q) of
        {error, Reason} ->
            ?WARNING_MSG("The SQL for 'pubsub_node' has error: ~p for values ~p", [Reason, Vals]);
        _ ->
            ok
    end.

%% -------------------------------------------------------------------
%% @private
%% @doc
%% Helper for migrate "pubsub_affiliations" from Mnesia to SQL DB
%% @end
%% -------------------------------------------------------------------
-spec migrate_pubsub_affiliations(Data :: list()) -> Result :: ok.

migrate_pubsub_affiliations([]) ->
    ok;

migrate_pubsub_affiliations([H|_]) ->
    {SJID, Nidx} = H#pubsub_state.stateid,
    JidBIn = jid:to_binary(SJID),
    JID = jid:from_binary(JidBIn),
    case {JID#jid.luser, H#pubsub_state.affiliation} of
        {Luser, Aff} when Aff =:= none; Luser =:= <<>> ->
            ok;
        _ ->
            JidBIn = jid:to_binary(SJID),
            JID = jid:from_binary(JidBIn),
            Cols = ["nidx", "luser", "lserver", "aff"],
            Vals = [Nidx, JID#jid.luser, JID#jid.lserver, mod_pubsub_db_rdbms:aff2int(H#pubsub_state.affiliation)],
            Q = ["INSERT INTO pubsub_affiliations ", expand_sql_vals(Cols, Vals), ";"],
            case mongoose_rdbms:sql_query(?MYNAME, Q) of
                {error, Reason} ->
                    ?WARNING_MSG("The SQL for 'pubsub_affiliations' has error: ~p for values ~p", [Reason, Vals]);
                _ ->
                    ok
            end
    end.

%% -------------------------------------------------------------------
%% @private
%% @doc
%% Helper for migrate "pubsub_subscriptions" from Mnesia to SQL DB
%% @end
%% -------------------------------------------------------------------
-spec migrate_pubsub_subscriptions(Data :: list()) -> Result :: ok.

migrate_pubsub_subscriptions([]) ->
    ok;

migrate_pubsub_subscriptions([H|_]) ->
    case H#pubsub_state.subscriptions of
        [{Type, SubID}] when Type =:= none; Type =:= pending; Type =:= subscribed ->
            {SJID, Nidx} = H#pubsub_state.stateid,
            JidBIn = jid:to_binary(SJID),
            JID = jid:from_binary(JidBIn),
            Cols = ["nidx", "luser", "lserver", "lresource", "type", "sub_id", "options"],
            Vals = [Nidx, JID#jid.luser, JID#jid.lserver, JID#jid.lresource,
                    escape(mod_pubsub_db_rdbms:sub2int(Type)), SubID, "\"{}\""],
            Q = ["INSERT INTO pubsub_subscriptions ", expand_sql_vals(Cols, Vals), ";"],
            case mongoose_rdbms:sql_query(?MYNAME, Q) of
                {error, Reason} ->
                    ?WARNING_MSG("The SQL for 'pubsub_subscriptions' has error: ~p for values ~p", [Reason, Vals]);
                _ ->
                    ok
            end;
        _ ->
            ok
    end.

%% -------------------------------------------------------------------
%% @private
%% @doc
%% Helper for migrate "pubsub_items" from Mnesia to SQL DB
%% @end
%% -------------------------------------------------------------------
-spec migrate_pubsub_items(Data :: list()) -> Result :: ok.

migrate_pubsub_items([]) ->
    ok;

migrate_pubsub_items([H|_]) ->
    {IID, NodeIdx} = H#pubsub_item.itemid,
    {CTime, CJID} = H#pubsub_item.creation,
    Payload = H#pubsub_item.payload,
    CT = to_unixtime(CTime),
    JidBIn = jid:to_binary(CJID),
    JID = jid:from_binary(JidBIn),
    XMLB = exml:to_binary(#xmlel{name = <<"item">>, children = Payload}),
    Cols = ["nidx", "itemid", "created_luser", "created_lserver", "created_at",
            "modified_luser", "modified_lserver", "modified_lresource", "modified_at", "payload"],
    Vals = [NodeIdx, IID, JID#jid.luser, JID#jid.lserver, CT, JID#jid.lserver,
            JID#jid.lserver, JID#jid.lresource, CT, {XMLB, escape_binary}],
    Q = ["INSERT INTO pubsub_items ", expand_sql_vals(Cols, Vals), ";"],
    case mongoose_rdbms:sql_query(?MYNAME, Q) of
        {error, Reason} ->
            ?WARNING_MSG("The SQL for 'pubsub_items' has error: ~p for values ~p", [Reason, Vals]);
        _ ->
            ok
    end.

%% -------------------------------------------------------------------
%% @private
%% @doc
%% Helper for migrate "vcard" from Mnesia to SQL DB
%% @end
%% -------------------------------------------------------------------
-spec migrate_vcard(Data :: list()) -> Result :: ok.

migrate_vcard([]) ->
    ok;

migrate_vcard([H|_]) ->
    {Luser, Lserver} = H#vcard.us,
    Cols = ["username", "server", "vcard"],
    Vals = [Luser, Lserver, exml:to_binary(H#vcard.vcard)],
    Q = ["INSERT INTO vcard ", expand_sql_vals(Cols, Vals), ";"],
    case mongoose_rdbms:sql_query(?MYNAME, Q) of
        {error, Reason} ->
            ?WARNING_MSG("The SQL for 'vcard' has error: ~p for values ~p", [Reason, Vals]);
        _ ->
            ok
    end.

%% -------------------------------------------------------------------
%% @private
%% @doc
%% Helper for migrate "vcard_search" from Mnesia to SQL DB
%% @end
%% -------------------------------------------------------------------
-spec migrate_vcard_search(Data :: list()) -> Result :: ok.

migrate_vcard_search([]) ->
    ok;

migrate_vcard_search([H|_]) ->
    {Luser, Lserver} = H#vcard_search.us,
    Cols = ["username", "lusername", "server", "fn", "lfn", "family", "lfamily", "given",
            "lgiven", "middle", "lmiddle", "nickname", "lnickname", "bday", "lbday", "ctry",
            "lctry", "locality", "llocality", "email", "lemail", "orgname", "lorgname", "orgunit",
            "lorgunit"],
    Vals = [Luser, H#vcard_search.luser, Lserver, H#vcard_search.fn, H#vcard_search.lfn,
            H#vcard_search.family, H#vcard_search.lfamily, H#vcard_search.given, H#vcard_search.lgiven,
            H#vcard_search.middle, H#vcard_search.lmiddle, H#vcard_search.nickname, H#vcard_search.lnickname,
            H#vcard_search.bday, H#vcard_search.lbday, H#vcard_search.ctry, H#vcard_search.lctry,
            H#vcard_search.locality, H#vcard_search.llocality, H#vcard_search.email, H#vcard_search.lemail,
            H#vcard_search.orgname, H#vcard_search.lorgname, H#vcard_search.orgunit, H#vcard_search.lorgunit],
    Q = ["INSERT INTO vcard_search ", expand_sql_vals(Cols, Vals), ";"],
    case mongoose_rdbms:sql_query(?MYNAME, Q) of
        {error, Reason} ->
            ?WARNING_MSG("The SQL for 'vcard_search' has error: ~p for values ~p", [Reason, Vals]);
        _ ->
            ok
    end.

%% -------------------------------------------------------------------
%% @private
%% @doc
%% Helper for migrate "users" from Mnesia to SQL DB
%% @end
%% -------------------------------------------------------------------
-spec migrate_users(Data :: list()) -> Result :: ok.

migrate_users([]) ->
    ok;

migrate_users([H|_]) ->
    Password = case ejabberd_auth_internal:get_password(H) of
        P when is_binary(P) ->
            P;
        P ->
            mongoose_scram:serialize(P)
    end,
    {Luser, _} = ejabberd_auth_internal:get_us(H),
    Cols = ["username", "password"],
    Vals = [Luser, Password],
    Q = ["INSERT INTO users ", expand_sql_vals(Cols, Vals), ";"],
    case mongoose_rdbms:sql_query(?MYNAME, Q) of
        {error, Reason} ->
            ?WARNING_MSG("The SQL for 'users' has error: ~p for values ~p", [Reason, Vals]);
        _ ->
            ok
    end.

%% -------------------------------------------------------------------
%% @private
%% @doc
%% Helper for migrate "event_pusher_push_subscription;" from Mnesia to SQL DB
%% @end
%% -------------------------------------------------------------------
-spec migrate_event_pusher_push_subscription(Data :: list()) -> Result :: ok.

migrate_event_pusher_push_subscription([]) ->
    ok;

migrate_event_pusher_push_subscription([H|_]) ->
    {Luser, Lserver} = mod_event_pusher_push_mnesia:get_user_jid(H),
    Cols = ["owner_jid", "node", "pubsub_jid", "form", "created_at"],
    Vals = [<<Luser/binary, "@", Lserver/binary>>, mod_event_pusher_push_mnesia:get_pubsub_node(H),
            jid:to_binary(mod_event_pusher_push_mnesia:get_pubsub_jid(H)),
            jiffy:encode({mod_event_pusher_push_mnesia:get_form(H)}),
            integer_to_binary(os:system_time(micro_seconds))],
    Q = ["INSERT INTO event_pusher_push_subscription ", expand_sql_vals(Cols, Vals), ";"],
    case mongoose_rdbms:sql_query(?MYNAME, Q) of
        {error, Reason} ->
            ?WARNING_MSG("The SQL for 'event_pusher_push_subscription' has error: ~p for values ~p", [Reason, Vals]);
        _ ->
            ok
    end.

%% -------------------------------------------------------------------
%% @private
%% @doc
%% Helper for migrate "rosterusers" from Mnesia to SQL DB
%% @end
%% -------------------------------------------------------------------
-spec migrate_rosterusers(Data :: list()) -> Result :: ok.

migrate_rosterusers([]) ->
    ok;

migrate_rosterusers([H|_]) ->
    {Luser, _} = H#roster.us,
    Cols = ["username", "jid", "nick", "askmessage", "subscription", "ask", "server", "subscribe", "type"],
    Vals = [Luser, jid:to_binary(H#roster.jid), H#roster.name, H#roster.askmessage,
            subscription_to_char(H#roster.subscription), ask_to_char(H#roster.ask),
            "", "", "item"], % @TODO what should be "server" - char(1)??? and what should be "subscribe" and "type"
    Q = ["INSERT INTO rosterusers ", expand_sql_vals(Cols, Vals), ";"],
    case mongoose_rdbms:sql_query(?MYNAME, Q) of
        {error, Reason} ->
            ?WARNING_MSG("The SQL for 'rosterusers' has error: ~p for values ~p", [Reason, Vals]);
        _ ->
            ok
    end.

%% -------------------------------------------------------------------
%% @private
%% @doc
%% Helper for migrate "roster_version" from Mnesia to SQL DB
%% @end
%% -------------------------------------------------------------------
-spec migrate_roster_version(Data :: list()) -> Result :: ok.

migrate_roster_version([]) ->
    ok;

migrate_roster_version([H|_]) ->
    {Luser, _} = H#roster_version.us,
    Cols = ["username", "version"],
    Vals = [Luser, H#roster_version.version],
    Q = ["INSERT INTO roster_version ", expand_sql_vals(Cols, Vals), ";"],
    case mongoose_rdbms:sql_query(?MYNAME, Q) of
        {error, Reason} ->
            ?WARNING_MSG("The SQL for 'roster_version' has error: ~p for values ~p", [Reason, Vals]);
        _ ->
            ok
    end.

%% -------------------------------------------------------------------
%% @private
%% @doc
%% Helper for migrate "rostergroups" from Mnesia to SQL DB
%% @end
%% -------------------------------------------------------------------
-spec migrate_rostergroups(Data :: list()) -> Result :: ok.

migrate_rostergroups([]) ->
    ok;

migrate_rostergroups([H|_]) ->
    {Luser, _} = H#roster.us,
    Cols = ["username", "jid", "grp"],
    Vals = [Luser, jid:to_binary(H#roster.jid)],
    _ = [begin
        Q = ["INSERT INTO rostergroups ", expand_sql_vals(Cols, Vals ++ [Group]), ";"],
        case mongoose_rdbms:sql_query(?MYNAME, Q) of
            {error, Reason} ->
                ?WARNING_MSG("The SQL for 'rostergroups' has error: ~p for values ~p", [Reason, Vals ++ Group]);
            _ ->
                ok
        end
    end || Group <-  H#roster.groups],
    ok.

%% -------------------------------------------------------------------
%% @private
%% @doc
%% Helper for migrate "last" from Mnesia to SQL DB
%% @end
%% -------------------------------------------------------------------
-spec migrate_last(Data :: list()) -> Result :: ok.

migrate_last([]) ->
    ok;

migrate_last([H|_]) ->
    {Luser, _} = H#last_activity.us,
    Cols = ["username", "seconds", "state"],
    Vals = [Luser, H#last_activity.timestamp, H#last_activity.status],
    Q = ["INSERT INTO last ", expand_sql_vals(Cols, Vals), ";"],
    case mongoose_rdbms:sql_query(?MYNAME, Q) of
        {error, Reason} ->
            ?WARNING_MSG("The SQL for 'last' has error: ~p for values ~p", [Reason, Vals]);
        _ ->
            ok
    end.

%% -------------------------------------------------------------------
%% @private
%% @doc
%% Helper for migrate "private_storage" from Mnesia to SQL DB
%% @end
%% -------------------------------------------------------------------
-spec migrate_private_storage(Data :: list()) -> Result :: ok.

migrate_private_storage([]) ->
    ok;

migrate_private_storage([H|_]) ->
    {LUser, _, NS} = mod_private_mnesia:get_usns(H),
    Cols = ["username", "namespace", "data"],
    Vals = [LUser, NS, exml:to_binary(mod_private_mnesia:get_xml(H))],
    Q = ["INSERT INTO private_storage ", expand_sql_vals(Cols, Vals), ";"],
    case mongoose_rdbms:sql_query(?MYNAME, Q) of
        {error, Reason} ->
            ?WARNING_MSG("The SQL for 'private_storage' has error: ~p for values ~p", [Reason, Vals]);
        _ ->
            ok
    end.

%% -------------------------------------------------------------------
%% @private
%% @doc
%% Helper for migrate "offline_message" from Mnesia to SQL DB
%% @end
%% -------------------------------------------------------------------
-spec migrate_offline_message(Data :: list()) -> Result :: ok.

migrate_offline_message([]) ->
    ok;

migrate_offline_message([H|_]) ->
    {LUser, LServer} = H#offline_msg.us,
    Cols = ["timestamp", "server", "username", "from_jid", "packet"],
    Vals = [integer_to_binary(to_unixtime(H#offline_msg.timestamp)), LServer,
            LUser, jid:to_binary(H#offline_msg.from), exml:to_binary(H#offline_msg.packet)],
    Q = ["INSERT INTO offline_message ", expand_sql_vals(Cols, Vals), ";"],
    case mongoose_rdbms:sql_query(?MYNAME, Q) of
        {error, Reason} ->
            ?WARNING_MSG("The SQL for 'offline_message' has error: ~p for values ~p", [Reason, Vals]);
        _ ->
            ok
    end.

%% -------------------------------------------------------------------
%% @private
%% @doc
%% Helper for migrate "muc_light_rooms" from Mnesia to SQL DB
%% @end
%% -------------------------------------------------------------------
-spec migrate_muc_light_rooms(Data :: list()) -> Result :: ok.

migrate_muc_light_rooms([]) ->
    ok;

migrate_muc_light_rooms([H|_]) ->
    {LUser, LServer} = mod_muc_light_db_mnesia:get_room(H),
    ID = case mongoose_rdbms:sql_query(?MYNAME, "SELECT MAX(ID) from muc_light_rooms;") of
        {selected, [{null}]} ->
            1;
        {selected, [{N}]} when is_integer(N) ->
            N + 1;
        {selected, [{N}]} when is_binary(N) ->
            list_to_integer(binary_to_list(N)) + 1
    end,
    Cols = ["luser", "lserver", "version"],                                  % ---"id"
    Vals = [LUser, LServer, mod_muc_light_db_mnesia:get_room_version(H)],    % --- ID
    Q = ["INSERT INTO muc_light_rooms ", expand_sql_vals(Cols, Vals), ";"],
    case mongoose_rdbms:sql_query(?MYNAME, Q) of
        {error, Reason} ->
            ?WARNING_MSG("The SQL for 'muc_light_rooms' has error: ~p for values ~p", [Reason, Vals]);
        _ ->
            migrate_muc_light_config(mod_muc_light_db_mnesia:get_room_config(H), ID),
            _ = [migrate_muc_light_occupants(ID, U, S, Aff) || {{U, S}, Aff} <- mod_muc_light_db_mnesia:get_room_aff_users(H)],
            ok
    end.

%% -------------------------------------------------------------------
%% @private
%% @doc
%% Helper for migrate "muc_light_config" from Mnesia to SQL DB
%% For "muc_light_config" need to know "room_id", by this reason this function used inside of migrate_muc_light_rooms/1
%% @end
%% -------------------------------------------------------------------
-spec migrate_muc_light_config(Data :: list(), RoomID :: integer()) -> ok.

migrate_muc_light_config([{_, Name}, {_, Subject}], RoomID) ->
    Cols = ["room_id", "opt", "val"],
    Vals = [RoomID, Name, Subject],
    Q = ["INSERT INTO muc_light_config ", expand_sql_vals(Cols, Vals), ";"],
    case mongoose_rdbms:sql_query(?MYNAME, Q) of
        {error, Reason} ->
            ?WARNING_MSG("The SQL for 'muc_light_config' has error: ~p for values ~p", [Reason, Vals]);
        _ ->
            ok
    end.

%% -------------------------------------------------------------------
%% @private
%% @doc
%% Helper for migrate "muc_light_occupants" from Mnesia to SQL DB
%% For "muc_light_occupants" need to know "room_id", by this reason this function used inside of migrate_muc_light_rooms/1
%% @end
%% -------------------------------------------------------------------
-spec migrate_muc_light_occupants(ID :: integer(), U :: binary(), S :: binary(), A :: atom()) -> ok.

migrate_muc_light_occupants(ID, LUser, LServer, Aff) ->
    AffDB = case Aff of
        owner ->
            1;
        member ->
            2;
        _ ->
            0
    end,
    case AffDB of
        0 ->
            ok;
        AffDB ->
            Cols = ["room_id", "luser", "lserver", "aff"],
            Vals = [ID, LUser, LServer, AffDB],
            Q = ["INSERT INTO muc_light_occupants ", expand_sql_vals(Cols, Vals), ";"],
            case mongoose_rdbms:sql_query(?MYNAME, Q) of
                {error, Reason} ->
                    ?WARNING_MSG("The SQL for 'migrate_muc_light_occupants' has error: ~p for values ~p", [Reason, Vals]);
                _ ->
                    ok
            end
    end.

%% -------------------------------------------------------------------
%% @private
%% @doc
%% convert time for DB
%% @end
%% -------------------------------------------------------------------
-spec to_unixtime(Data :: tuple()) -> Result :: integer().

to_unixtime({Mega, Sec, Micro}) ->
    Mega * 1000000 * 1000000 + Sec * 1000000 + Micro.

%% -------------------------------------------------------------------
%% @private
%% @doc
%% Getting data from table of Mnesia by table name
%% @end
%% -------------------------------------------------------------------
-spec get_data(Step :: atom(), Name :: atom() | tuple()) -> Result :: {ok, any(), any()} | {error, list()}.

get_data(first, Name) ->
    try
        Key = mnesia:dirty_first(Name),
        {ok, mnesia:dirty_read(Name, Key), mnesia:dirty_next(Name, Key)}
    catch Class:Exception ->
        {error, io_lib:format("Migration call: [~p:~p/1] catched ~p:~p", [?MODULE, get_data, Class, Exception])}
    end;

get_data(next, {Name, Key}) ->
    {ok, mnesia:dirty_read(Name, Key), mnesia:dirty_next(Name, Key)}.

%% -------------------------------------------------------------------
%% @private
%% @doc
%% Generate SQL values: "(field_1, field_1) VALUES ('bear', 'one')"
%% @end
%% -------------------------------------------------------------------
-spec expand_sql_vals(Cols :: list(), Vals :: list()) -> Q :: list().

expand_sql_vals(Cols, Vals) ->
    EVals = [escape(V) || V <- Vals],
    ["(", join(Cols, ", "), ") VALUES (", join(EVals, ", "), ")"].

%% -------------------------------------------------------------------
%% @private
%% @doc
%% Join lists by separator
%% @end
%% -------------------------------------------------------------------
-spec join(list(), list()) -> list().

join([], _Sep) ->
    [];

join([H|T], Sep) ->
    [H, [[Sep, X] || X <- T]].

%% -------------------------------------------------------------------
%% @private
%% @doc
%% escape helper
%% @end
%% -------------------------------------------------------------------
-spec escape(Data :: any()) -> Res :: binary() | list() | any().

escape({Data, Escape}) ->
    mongoose_rdbms_odbc:Escape(Data);

escape(Data) when is_integer(Data) ->
    integer_to_binary(Data);

escape(Data) when is_binary(Data); is_list(Data) ->
    [$', mongoose_rdbms:escape_characters(Data), $'];

escape(Data) ->
    Data.

%% -------------------------------------------------------------------
%% @private
%% @doc
%% Subscription to char
%% @end
%% -------------------------------------------------------------------
-spec subscription_to_char(Data :: atom()) -> Result :: binary().

subscription_to_char(Subs) ->
    case Subs of
        both ->
            <<"B">>;
        to ->
            <<"T">>;
        from ->
            <<"F">>;
        remove ->
            <<"R">>;
        none ->
            <<"N">>
    end.

%% -------------------------------------------------------------------
%% @private
%% @doc
%% Ask to char
%% @end
%% -------------------------------------------------------------------
-spec ask_to_char(Data :: atom()) -> Result :: binary().

ask_to_char(Ask) ->
    case Ask of
        subscribe ->
            <<"S">>;
        unsubscribe ->
            <<"U">>;
        both ->
            <<"B">>;
        out ->
            <<"O">>;
        in ->
            <<"I">>;
        _ ->
            <<"N">>
    end.
