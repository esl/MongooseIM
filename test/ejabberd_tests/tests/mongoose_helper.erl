-module(mongoose_helper).

%% API

-export([auth_modules/0]).

-export([total_offline_messages/0,
         total_active_users/0,
         total_privacy_items/0,
         total_private_items/0,
         total_vcard_items/0,
         total_roster_items/0]).

-export([clear_last_activity/2]).

-define(RPC(M,F,A), escalus_ejabberd:rpc(M, F, A)).

-spec auth_modules() -> [atom()].
auth_modules() ->
    Hosts = escalus_ejabberd:rpc(ejabberd_config, get_global_option, [hosts]),
    lists:flatmap(
        fun(Host) ->
            escalus_ejabberd:rpc(ejabberd_auth, auth_modules, [Host])
        end, Hosts).

-spec total_offline_messages() -> integer() | false.
total_offline_messages() ->
    generic_count(mod_offline_backend).

-spec total_active_users() -> integer() | false.
total_active_users() ->
    generic_count(mod_last_backend).

-spec total_privacy_items() -> integer() | false.
total_privacy_items() ->
    generic_count(mod_privacy_backend).

-spec total_private_items() -> integer() | false.
total_private_items() ->
    generic_count(mod_private_backend).

-spec total_vcard_items() -> integer() | false.
total_vcard_items() ->
    generic_count(mod_vcard_backend).

-spec total_roster_items() -> integer() | false.
total_roster_items() ->
    Domain = escalus_ct:get_config(ejabberd_domain),
    RosterMnesia = ?RPC(gen_mod, is_loaded, [Domain, mod_roster]),
    RosterODBC = ?RPC(gen_mod, is_loaded, [Domain, mod_roster_odbc]),
    case {RosterMnesia, RosterODBC} of
        {true, _} ->
            generic_count_backend(mod_roster_mnesia);
        {_, true} ->
            generic_count_backend(mod_roster_odbc);
        _ ->
            false
    end.

%% Need to clear last_activity after carol (connected over BOSH)
%% It is possible that from time to time the unset_presence_hook,
%% for user connected over BOSH, is called after user removal.
%% This happens when the BOSH session is closed (on server side)
%% after user's removal
%% In such situation the last info is set back
-spec clear_last_activity(list(), atom() | binary() | [atom() | binary()]) -> no_return().
clear_last_activity(Config, User) ->
    S = escalus_config:get_config(ejabberd_domain, Config),
    case catch escalus_ejabberd:rpc(gen_mod, is_loaded, [S, mod_last]) of
        true ->
            do_clear_last_activity(Config, User);
        _ ->
            ok
    end.

do_clear_last_activity(Config, User) when is_atom(User)->
    [U, S, _P] = escalus_users:get_usp(Config, carol),
    escalus_ejabberd:rpc(mod_last, remove_user, [U, S]);
do_clear_last_activity(_Config, User) when is_binary(User) ->
    U = escalus_utils:get_username(User),
    S = escalus_utils:get_server(User),
    escalus_ejabberd:rpc(mod_last, remove_user, [U, S]);
do_clear_last_activity(Config, Users) when is_list(Users) ->
    lists:foreach(fun(User) -> do_clear_last_activity(Config, User) end, Users).

get_backend(Module) ->
  case ?RPC(Module, backend, []) of
    {badrpc, _Reason} -> false;
    Backend -> Backend
  end.

generic_count(Module) ->
    case get_backend(Module) of
        false -> %% module disabled
            false;
        B when is_atom(B) ->
            generic_count_backend(B)
    end.

generic_count_backend(mod_offline_mnesia) -> count_wildpattern(offline_msg);
generic_count_backend(mod_offline_odbc) -> count_odbc(<<"offline_message">>);
generic_count_backend(mod_offline_riak) -> count_riak(<<"offline">>);
generic_count_backend(mod_last_mnesia) -> count_wildpattern(last_activity);
generic_count_backend(mod_last_odbc) -> count_odbc(<<"last">>);
generic_count_backend(mod_last_riak) -> count_riak(<<"last">>);
generic_count_backend(mod_privacy_mnesia) -> count_wildpattern(privacy);
generic_count_backend(mod_privacy_odbc) -> count_odbc(<<"privacy_list">>);
generic_count_backend(mod_privacy_cassandra) -> count_cassandra(privacy_list);
generic_count_backend(mod_private_mnesia) -> count_wildpattern(private_storage);
generic_count_backend(mod_private_odbc) -> count_odbc(<<"private_storage">>);
generic_count_backend(mod_private_mysql) -> count_odbc(<<"private_storage">>);
generic_count_backend(mod_private_riak) -> count_riak(<<"private">>);
generic_count_backend(mod_private_cassandra) -> count_cassandra(private_storage);
generic_count_backend(mod_vcard_mnesia) -> count_wildpattern(vcard);
generic_count_backend(mod_vcard_odbc) -> count_odbc(<<"vcard">>);
generic_count_backend(mod_vcard_riak) -> count_riak(<<"vcard">>);
generic_count_backend(mod_vcard_ldap) ->
    D = escalus_ct:get_config(ejabberd_domain),
    %% number of vcards in ldap is the same as number of users
    ?RPC(ejabberd_auth_ldap, get_vh_registered_users_number, [D]);
generic_count_backend(mod_roster_mnesia) -> count_wildpattern(roster);
generic_count_backend(mod_roster_riak) ->
    count_riak(<<"rosters">>),
    count_riak(<<"roster_versions">>);
generic_count_backend(mod_roster_cassandra) -> count_cassandra(rosterusers);
generic_count_backend(mod_roster_odbc) -> count_odbc(<<"rosterusers">>).

count_wildpattern(Table) ->
    Pattern = ?RPC(mnesia, table_info, [Table, wild_pattern]),
    length(?RPC(mnesia, dirty_match_object, [Pattern])).

count_odbc(Table) ->
    {selected, _, [{N}]} =
        ?RPC(ejabberd_odbc,sql_query, [<<"localhost">>,[<<"select count(*) from ", Table/binary, " ;">>]]),
    count_to_integer(N).

count_to_integer(N) when is_binary(N) ->
    list_to_integer(binary_to_list(N));
count_to_integer(N) when is_integer(N)->
    N.

count_riak(BucketType) ->
    {ok, Buckets} = ?RPC(mongoose_riak, list_buckets, [BucketType]),
    BucketKeys = [?RPC(mongoose_riak, list_keys, [{BucketType, Bucket}]) || Bucket <- Buckets],
    length(lists:flatten(BucketKeys)).

count_cassandra(Table) ->
    %% Only default pool is supported
    ?RPC(mongoose_cassandra_worker, total_count_query, [default, Table]).
