-module(mongoose_helper).

%% API

-export([is_rdbms_enabled/1]).

-export([auth_modules/0]).

-export([total_offline_messages/0,
         total_offline_messages/1,
         total_active_users/0,
         total_privacy_items/0,
         total_private_items/0,
         total_vcard_items/0,
         total_roster_items/0]).

-export([clear_last_activity/2,
         clear_caps_cache/1]).

-export([kick_everyone/0]).
-export([ensure_muc_clean/0]).
-export([successful_rpc/3]).
-export([logout_user/2]).
-export([wait_until/2, wait_until/3, wait_for_user/3]).

-import(distributed_helper, [mim/0,
                             require_rpc_nodes/1,
                             rpc/4]).

-spec is_rdbms_enabled(Host :: binary()) -> boolean().
is_rdbms_enabled(Host) ->
    case rpc(mim(), mongoose_rdbms, sql_transaction, [Host, fun erlang:yield/0]) of
        {atomic, _} -> true;
        _ -> false
    end.

-spec auth_modules() -> [atom()].
auth_modules() ->
    Hosts = rpc(mim(), ejabberd_config, get_global_option, [hosts]),
    lists:flatmap(
        fun(Host) ->
            rpc(mim(), ejabberd_auth, auth_modules, [Host])
        end, Hosts).

-spec total_offline_messages() -> integer() | false.
total_offline_messages() ->
    generic_count(mod_offline_backend).

-spec total_offline_messages({binary(), binary()}) -> integer() | false.
total_offline_messages(User) ->
    generic_count(mod_offline_backend, User).

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
    generic_count(mod_roster_backend).

%% Need to clear last_activity after carol (connected over BOSH)
%% It is possible that from time to time the unset_presence_hook,
%% for user connected over BOSH, is called after user removal.
%% This happens when the BOSH session is closed (on server side)
%% after user's removal
%% In such situation the last info is set back
-spec clear_last_activity(list(), atom() | binary() | [atom() | binary()]) -> no_return().
clear_last_activity(Config, User) ->
    S = ct:get_config({hosts, mim, domain}),
    case catch rpc(mim(), gen_mod, is_loaded, [S, mod_last]) of
        true ->
            do_clear_last_activity(Config, User);
        _ ->
            ok
    end.

do_clear_last_activity(Config, User) when is_atom(User)->
    [U, S, _P] = escalus_users:get_usp(Config, carol),
    Acc = new_mongoose_acc({?MODULE, ?FUNCTION_NAME, ?LINE}, S),
    successful_rpc(mod_last, remove_user, [Acc, U, S]);
do_clear_last_activity(_Config, User) when is_binary(User) ->
    U = escalus_utils:get_username(User),
    S = escalus_utils:get_server(User),
    Acc = new_mongoose_acc({?MODULE, ?FUNCTION_NAME, ?LINE}, S),
    successful_rpc(mod_last, remove_user, [Acc, U, S]);
do_clear_last_activity(Config, Users) when is_list(Users) ->
    lists:foreach(fun(User) -> do_clear_last_activity(Config, User) end, Users).

new_mongoose_acc(Location, Server) ->
    successful_rpc(mongoose_acc, new, [#{ location => Location,
                                          lserver => Server,
                                          element => undefined }]).

clear_caps_cache(CapsNode) ->
    ok = rpc(mim(), mod_caps, delete_caps, [CapsNode]).

get_backend(Module) ->
  case rpc(mim(), Module, backend, []) of
    {badrpc, _Reason} -> false;
    Backend -> Backend
  end.

generic_count(mod_offline_backend, {User, Server}) ->
    rpc(mim(), mod_offline_backend, count_offline_messages, [User, Server, 100]).


generic_count(Module) ->
    case get_backend(Module) of
        false -> %% module disabled
            false;
        B when is_atom(B) ->
            generic_count_backend(B)
    end.

generic_count_backend(mod_offline_mnesia) -> count_wildpattern(offline_msg);
generic_count_backend(mod_offline_rdbms) -> count_rdbms(<<"offline_message">>);
generic_count_backend(mod_offline_riak) -> count_riak(<<"offline">>);
generic_count_backend(mod_last_mnesia) -> count_wildpattern(last_activity);
generic_count_backend(mod_last_rdbms) -> count_rdbms(<<"last">>);
generic_count_backend(mod_last_riak) -> count_riak(<<"last">>);
generic_count_backend(mod_privacy_mnesia) -> count_wildpattern(privacy);
generic_count_backend(mod_privacy_rdbms) -> count_rdbms(<<"privacy_list">>);
generic_count_backend(mod_privacy_riak) -> count_riak(<<"privacy_lists">>);
generic_count_backend(mod_private_mnesia) -> count_wildpattern(private_storage);
generic_count_backend(mod_private_rdbms) -> count_rdbms(<<"private_storage">>);
generic_count_backend(mod_private_mysql) -> count_rdbms(<<"private_storage">>);
generic_count_backend(mod_private_riak) -> count_riak(<<"private">>);
generic_count_backend(mod_vcard_mnesia) -> count_wildpattern(vcard);
generic_count_backend(mod_vcard_rdbms) -> count_rdbms(<<"vcard">>);
generic_count_backend(mod_vcard_riak) -> count_riak(<<"vcard">>);
generic_count_backend(mod_vcard_ldap) ->
    D = ct:get_config({hosts, mim, domain}),
    %% number of vcards in ldap is the same as number of users
    rpc(mim(), ejabberd_auth_ldap, get_vh_registered_users_number, [D]);
generic_count_backend(mod_roster_mnesia) -> count_wildpattern(roster);
generic_count_backend(mod_roster_riak) ->
    count_riak(<<"rosters">>),
    count_riak(<<"roster_versions">>);
generic_count_backend(mod_roster_rdbms) -> count_rdbms(<<"rosterusers">>).

count_wildpattern(Table) ->
    Pattern = rpc(mim(), mnesia, table_info, [Table, wild_pattern]),
    length(rpc(mim(), mnesia, dirty_match_object, [Pattern])).


count_rdbms(Table) ->
    {selected, [{N}]} =
        rpc(mim(), mongoose_rdbms, sql_query,
            [<<"localhost">>, [<<"select count(*) from ", Table/binary, " ;">>]]),
    count_to_integer(N).

count_to_integer(N) when is_binary(N) ->
    list_to_integer(binary_to_list(N));
count_to_integer(N) when is_integer(N)->
    N.

count_riak(BucketType) ->
    {ok, Buckets} = rpc(mim(), mongoose_riak, list_buckets, [BucketType]),
    BucketKeys = [rpc(mim(), mongoose_riak, list_keys, [{BucketType, Bucket}]) || Bucket <- Buckets],
    length(lists:flatten(BucketKeys)).

kick_everyone() ->
    [rpc(mim(), ejabberd_c2s, stop, [Pid]) || Pid <- get_session_pids()],
    wait_for_session_count(0).

wait_for_session_count(Expected) ->
    wait_until(fun() -> length(get_session_specs()) end, Expected, #{name => session_count}).

get_session_specs() ->
    rpc(mim(), supervisor, which_children, [ejabberd_c2s_sup]).

get_session_pids() ->
    [element(2, X) || X <- get_session_specs()].


ensure_muc_clean() ->
    stop_online_rooms(),
    forget_persistent_rooms().

stop_online_rooms() ->
    Host = ct:get_config({hosts, mim, domain}),
    Supervisor = rpc(mim(), gen_mod, get_module_proc, [Host, ejabberd_mod_muc_sup]),
    SupervisorPid = rpc(mim(), erlang, whereis, [Supervisor]),
    rpc(mim(), erlang, exit, [SupervisorPid, kill]),
    rpc(mim(), mnesia, clear_table, [muc_online_room]),
    ok.

forget_persistent_rooms() ->
    rpc(mim(), mnesia, clear_table, [muc_room]),
    rpc(mim(), mnesia, clear_table, [muc_registered]),
    ok.

-spec successful_rpc(atom(), atom(), list()) -> term().
successful_rpc(Module, Function, Args) ->
    case rpc(mim(), Module, Function, Args) of
        {badrpc, Reason} ->
            ct:fail({badrpc, Module, Function, Args, Reason});
        Result ->
            Result
    end.

%% This function is a version of escalus_client:stop/2
%% that ensures that c2s process is dead.
%% This allows to avoid race conditions.
logout_user(Config, User) ->
    Resource = escalus_client:resource(User),
    Username = escalus_client:username(User),
    Server = escalus_client:server(User),
    Result = successful_rpc(ejabberd_sm, get_session_pid,
                            [Username, Server, Resource]),
    case Result of
        none ->
            %% This case can be a side effect of some error, you should
            %% check your test when you see the message.
            ct:pal("issue=user_not_registered jid=~ts@~ts/~ts",
                   [Username, Server, Resource]),
            escalus_client:stop(Config, User);
        Pid when is_pid(Pid) ->
            MonitorRef = erlang:monitor(process, Pid),
            escalus_client:stop(Config, User),
            %% Wait for pid to die
            receive
                {'DOWN', MonitorRef, _, _, _} ->
                    ok
                after 10000 ->
                    ct:pal("issue=c2s_still_alive "
                            "jid=~ts@~ts/~ts pid=~p",
                           [Username, Server, Resource, Pid]),
                    ct:fail({logout_user_failed, {Username, Resource, Pid}})
            end
    end.

%% @doc Waits `TimeLeft` for `Fun` to return `ExpectedValue`
%% If the result of `Fun` matches `ExpectedValue`, returns {ok, ExpectedValue}
%% If no value is returned or the result doesn't match `ExpectedValue`, returns one of the following:
%% {Name, History}, if Opts as #{name => Name} is passed
%% {timeout, History}, otherwise

wait_until(Fun, ExpectedValue) ->
    wait_until(Fun, ExpectedValue, #{}).

%% Example: wait_until(fun () -> ... end, SomeVal, #{time_left => timer:seconds(2)})
wait_until(Fun, ExpectedValue, Opts) ->
    Defaults = #{time_left => timer:seconds(5),
                 sleep_time => 100,
                 history => [],
                 name => timeout},
    do_wait_until(Fun, ExpectedValue, maps:merge(Defaults, Opts)).

do_wait_until(_Fun, _ExpectedValue, #{
                                      time_left := TimeLeft,
                                      history := History,
                                      name := Name
                                     }) when TimeLeft =< 0 ->
    error({Name, lists:reverse(History)});

do_wait_until(Fun, ExpectedValue, Opts) ->
    try Fun() of
        ExpectedValue ->
            {ok, ExpectedValue};
        OtherValue ->
            wait_and_continue(Fun, ExpectedValue, OtherValue, Opts)
    catch Error:Reason ->
            wait_and_continue(Fun, ExpectedValue, {Error, Reason}, Opts)
    end.

wait_and_continue(Fun, ExpectedValue, FunResult, #{time_left := TimeLeft,
                                                   sleep_time := SleepTime,
                                                   history := History} = Opts) ->
    timer:sleep(SleepTime),
    do_wait_until(Fun, ExpectedValue, Opts#{time_left => TimeLeft - SleepTime,
                                            history => [FunResult | History]}).

wait_for_user(Config, User, LeftTime) ->
    mongoose_helper:wait_until(fun() -> 
                                escalus_users:verify_creation(escalus_users:create_user(Config, User)) 
                               end, ok,
							   #{
                                 sleep_time => 400, 
                                 left_time => LeftTime, 
                                 name => 'escalus_users:create_user'
                                }).

