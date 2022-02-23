-module(mongoose_helper).

-include_lib("kernel/include/logger.hrl").
-include_lib("common_test/include/ct.hrl").

%% API

-export([is_rdbms_enabled/1,
         get_backend_mnesia_rdbms_riak/1,
         backend_for_module/2,
         mnesia_or_rdbms_backend/0,
         get_backend_name/2]).

-export([auth_modules/0]).

-export([total_offline_messages/0,
         total_offline_messages/1,
         total_active_users/0,
         total_privacy_items/0,
         total_private_items/0,
         total_vcard_items/0,
         total_roster_items/0,
         generic_count/1]).

-export([clear_last_activity/2,
         clear_caps_cache/1]).

-export([kick_everyone/0]).
-export([ensure_muc_clean/0]).
-export([successful_rpc/3, successful_rpc/4, successful_rpc/5]).
-export([logout_user/2, logout_user/3]).
-export([enable_carbons/1, disable_carbons/1]).
-export([wait_until/2, wait_until/3, wait_for_user/3]).

-export([inject_module/1, inject_module/2, inject_module/3]).
-export([make_jid/2]).
-export([make_jid/3]).
-export([make_jid_noprep/3]).
-export([get_session_pid/1]).
-export([get_session_pid/2]).
-export([get_session_info/2]).
-export([wait_for_route_message_count/2]).
-export([wait_for_pid_to_die/1]).
-export([supports_sasl_module/1]).
-export([auth_opts_with_password_format/1]).
-export([get_listeners/2]).
-export([restart_listener/2]).
-export([should_minio_be_running/1]).
-export([new_mongoose_acc/1]).
-export([print_debug_info_for_module/1]).
-export([backup_and_set_config/2, backup_and_set_config_option/3, change_config_option/3]).
-export([restore_config/1, restore_config_option/2]).
-export([wait_for_n_offline_messages/2]).
-export([wait_for_c2s_state_name/2, get_c2s_state_name/1]).

-import(distributed_helper, [mim/0, rpc/4]).

-spec is_rdbms_enabled(HostType :: binary()) -> boolean().
is_rdbms_enabled(HostType) ->
    case rpc(mim(), mongoose_rdbms, sql_transaction, [HostType, fun erlang:yield/0]) of
        {atomic, _} -> true;
        _ -> false
    end.

-spec mnesia_or_rdbms_backend() -> atom().
mnesia_or_rdbms_backend() ->
    Host = ct:get_config({hosts, mim, domain}),
    case is_rdbms_enabled(Host) of
        true -> rdbms;
        false -> mnesia
    end.

get_backend_mnesia_rdbms_riak(HostType) ->
    case {is_rdbms_enabled(HostType), mam_helper:is_riak_enabled(HostType)} of
        {false, false} -> mnesia;
        {true, false} -> rdbms;
        {false, true} -> riak
    end.

backend_for_module(Module, Backend) ->
    [{Module, [{backend, Backend}]}].

-spec auth_modules() -> [atom()].
auth_modules() ->
    Hosts = rpc(mim(), mongoose_config, get_opt, [hosts]),
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
    Acc = new_mongoose_acc(?LOCATION, S),
    successful_rpc(mod_last, remove_user, [Acc, U, S]);
do_clear_last_activity(_Config, User) when is_binary(User) ->
    U = escalus_utils:get_username(User),
    S = escalus_utils:get_server(User),
    Acc = new_mongoose_acc(?LOCATION, S),
    successful_rpc(mod_last, remove_user, [Acc, U, S]);
do_clear_last_activity(Config, Users) when is_list(Users) ->
    lists:foreach(fun(User) -> do_clear_last_activity(Config, User) end, Users).

new_mongoose_acc(Server) ->
    new_mongoose_acc(?LOCATION, Server).

new_mongoose_acc(Location, Server) ->
    {ok, HostType} = rpc(mim(), mongoose_domain_core, get_host_type, [Server]),
    successful_rpc(mongoose_acc, new, [#{ location => Location,
                                          lserver => Server,
                                          host_type => HostType,
                                          element => undefined }]).

clear_caps_cache(CapsNode) ->
    ok = rpc(mim(), mod_caps, delete_caps, [CapsNode]).

get_backend(HostType, Module) ->
    try rpc(mim(), mongoose_backend, get_backend_module, [HostType, Module])
    catch
        error:{badrpc, _Reason} -> false
    end.

get_backend_name(HostType, Module) ->
    rpc(mim(), mongoose_backend, get_backend_name, [HostType, Module]).

generic_count(mod_offline_backend, {LUser, LServer}) ->
    HostType = domain_helper:host_type(),
    rpc(mim(), mod_offline_backend, count_offline_messages, [HostType, LUser, LServer, 100]).

generic_count(Module) ->
    lists:sum([generic_count_per_host_type(HT, Module) || HT <- domain_helper:host_types()]).

generic_count_per_host_type(HostType, Module) ->
    case get_backend(HostType, Module) of
        false -> %% module disabled
            0;
        B when is_atom(B) ->
            generic_count_backend(B)
    end.

generic_count_backend(mod_smart_markers_rdbms) -> count_rdbms(<<"smart_markers">>);
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
            [domain_helper:host_type(), [<<"select count(*) from ", Table/binary, " ;">>]]),
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
    HostType = domain_helper:host_type(),
    Supervisor = rpc(mim(), gen_mod, get_module_proc, [HostType, ejabberd_mod_muc_sup]),
    SupervisorPid = rpc(mim(), erlang, whereis, [Supervisor]),
    case is_pid(SupervisorPid) of
        true -> ok;
        false -> ct:fail({ejabberd_mod_muc_sup_not_found, Supervisor, HostType})
    end,
    rpc(mim(), erlang, exit, [SupervisorPid, kill]),
    rpc(mim(), mnesia, clear_table, [muc_online_room]),
    ok.

forget_persistent_rooms() ->
    %% To avoid `binary_to_existing_atom(<<"maygetmemberlist">>, utf8)' failing
    rpc(mim(), mod_muc_room, module_info, []),
    HostType = domain_helper:host_type(),
    {ok, Rooms} = rpc(mim(), mod_muc_backend, get_rooms, [HostType, muc_helper:muc_host()]),
    lists:foreach(
     fun({muc_room, {RoomName, MucHost}, _Opts}) ->
             rpc(mim(), mod_muc, forget_room, [HostType, MucHost, RoomName])
     end,
     Rooms).

-spec successful_rpc(M :: atom(), F :: atom(), A :: list()) -> term().
successful_rpc(Module, Function, Args) ->
    successful_rpc(mim(), Module, Function, Args).

-spec successful_rpc(Spec, M, F, A) -> term() when
      Spec :: distributed_helper:rpc_spec(),
      M :: module(),
      F :: atom(),
      A :: list().
successful_rpc(#{} = Spec, Module, Function, Args) ->
    successful_rpc(Spec, Module, Function, Args, timer:seconds(5)).

-spec successful_rpc(Spec, M, F, A, timeout()) -> term() when
      Spec :: distributed_helper:rpc_spec(),
      M :: module(),
      F :: atom(),
      A :: list().
successful_rpc(#{} = Spec, Module, Function, Args, Timeout) ->
    case rpc(Spec#{timeout => Timeout}, Module, Function, Args) of
        {badrpc, Reason} ->
            ct:fail({badrpc, Module, Function, Args, Reason});
        Result ->
            Result
    end.

enable_carbons(Clients) when is_list(Clients) ->
    lists:foreach(fun enable_carbons/1, Clients);
enable_carbons(Client) ->
    IqSet = escalus_stanza:carbons_enable(),
    escalus_client:send(Client, IqSet),
    Result = escalus_client:wait_for_stanza(Client),
    escalus:assert(is_iq, [<<"result">>], Result).

disable_carbons(Clients) when is_list(Clients) ->
    lists:foreach(fun disable_carbons/1, Clients);
disable_carbons(Client) ->
    IqSet = escalus_stanza:carbons_disable(),
    escalus_client:send(Client, IqSet),
    Result = escalus_client:wait_for_stanza(Client),
    escalus:assert(is_iq, [<<"result">>], Result).

logout_user(Config, User) ->
    Node = distributed_helper:mim(),
    logout_user(Config, User, Node).

%% This function is a version of escalus_client:stop/2
%% that ensures that c2s process is dead.
%% This allows to avoid race conditions.
logout_user(Config, User, Node) ->
    Resource = escalus_client:resource(User),
    Username = escalus_client:username(User),
    Server = escalus_client:server(User),
    Result = get_session_pid(User, Node),
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
    Defaults = #{validator => fun(NewValue) -> ExpectedValue =:= NewValue end,
                 expected_value => ExpectedValue,
                 time_left => timer:seconds(5),
                 sleep_time => 100,
                 history => [],
                 name => timeout},
    do_wait_until(Fun, maps:merge(Defaults, Opts)).

do_wait_until(_Fun, #{expected_value := ExpectedValue,
                      time_left := TimeLeft,
                      history := History,
                      name := Name}) when TimeLeft =< 0 ->
    error({Name, ExpectedValue, simplify_history(lists:reverse(History), 1)});

do_wait_until(Fun, #{validator := Validator} = Opts) ->
    try Fun() of
        Value -> case Validator(Value) of
                     true -> {ok, Value};
                     _ -> wait_and_continue(Fun, Value, Opts)
                 end
    catch Error:Reason:Stacktrace ->
              wait_and_continue(Fun, {Error, Reason, Stacktrace}, Opts)
    end.

simplify_history([H|[H|_]=T], Times) ->
    simplify_history(T, Times + 1);
simplify_history([H|T], Times) ->
    [{times, Times, H}|simplify_history(T, 1)];
simplify_history([], 1) ->
    [].

wait_and_continue(Fun, FunResult, #{time_left := TimeLeft,
                                    sleep_time := SleepTime,
                                    history := History} = Opts) ->
    timer:sleep(SleepTime),
    do_wait_until(Fun, Opts#{time_left => TimeLeft - SleepTime,
                             history => [FunResult | History]}).

wait_for_user(Config, User, LeftTime) ->
    wait_until(fun() ->
                       escalus_users:verify_creation(escalus_users:create_user(Config, User))
               end, ok,
               #{sleep_time => 400,
                 left_time => LeftTime,
                 name => 'escalus_users:create_user'}).

% Loads a module present in big tests into a MongooseIM node
-spec inject_module(Module :: module()) -> ok.
inject_module(Module) ->
    inject_module(Module, no_reload).

-spec inject_module(Module :: module(),
                    ReloadIfAlreadyLoaded :: no_reload | reload) -> ok | already_loaded.
inject_module(Module, ReloadIfAlreadyLoaded) ->
    inject_module(mim(), Module, ReloadIfAlreadyLoaded).

-spec inject_module(Node :: atom(),
                    Module :: module(),
                    ReloadIfAlreadyLoaded :: no_reload | reload) ->
    ok | already_loaded.
inject_module(Node, Module, no_reload) ->
    case successful_rpc(Node, code, is_loaded, [Module]) of
        false ->
            inject_module(Node, Module, reload);
        _ ->
            already_loaded
    end;
inject_module(Node, Module, reload) ->
    {Mod, Bin, File} = code:get_object_code(Module),
    successful_rpc(Node, code, load_binary, [Mod, File, Bin]).


make_jid_noprep(User, Server, Resource) ->
    jid:make_noprep(User, Server, Resource).

make_jid(User, Server) ->
    jid:make(User, Server, <<>>).

make_jid(User, Server, Resource) ->
    jid:make(User, Server, Resource).

get_session_pid(User) ->
    get_session_pid(User, mim()).

get_session_pid(User, Node) ->
    Resource = escalus_client:resource(User),
    Username = escalus_client:username(User),
    Server = escalus_client:server(User),
    JID = make_jid(Username, Server, Resource),
    successful_rpc(Node, ejabberd_sm, get_session_pid, [JID]).

get_session_info(RpcDetails, User) ->
    Username = escalus_client:username(User),
    Server = escalus_client:server(User),
    Resource = escalus_client:resource(User),
    JID = make_jid(Username, Server, Resource),
    {session, _, _, _, _, Info} = rpc(RpcDetails, ejabberd_sm, get_session, [JID]),
    Info.

wait_for_route_message_count(C2sPid, ExpectedCount) when is_pid(C2sPid), is_integer(ExpectedCount) ->
    wait_until(fun() -> count_route_messages(C2sPid) end, ExpectedCount, #{name => has_route_message}).

count_route_messages(C2sPid) when is_pid(C2sPid) ->
     {messages, Messages} = rpc:pinfo(C2sPid, messages),
     length([Route || Route <- Messages, is_tuple(Route), route =:= element(1, Route)]).

wait_for_pid_to_die(Pid) ->
    MonitorRef = erlang:monitor(process, Pid),
    %% Wait for pid to die
    receive
        {'DOWN', MonitorRef, _, _, _} ->
            ok
        after 10000 ->
            ct:fail({wait_for_pid_to_die_failed, Pid})
    end.

supports_sasl_module(Module) ->
    HostType = domain_helper:host_type(),
    rpc(mim(), ejabberd_auth, supports_sasl_module, [HostType, Module]).

auth_opts_with_password_format(Type) ->
    HostType = domain_helper:host_type(mim),
    AuthOpts = #{password := PassOpts} = rpc(mim(), mongoose_config, get_opt, [{auth, HostType}]),
    AuthOpts#{password := build_new_password_opts(Type, PassOpts)}.

build_new_password_opts(scram, PassOpts) ->
    PassOpts#{format => scram, scram_iterations => 64};
build_new_password_opts(Type, PassOpts) ->
    PassOpts#{format => Type}.

get_listeners(#{} = Spec, Pattern) ->
    Keys = maps:keys(Pattern),
    Listeners = rpc(Spec, mongoose_config, get_opt, [listen]),
    lists:filter(fun(Listener) -> maps:with(Keys, Listener) =:= Pattern end, Listeners).

%% 'port', 'ip_tuple' and 'proto' options need to stay unchanged for a successful restart
restart_listener(Spec, Listener) ->
    rpc(Spec, ejabberd_listener, stop_listener, [Listener]),
    rpc(Spec, ejabberd_listener, start_listener, [Listener]).

should_minio_be_running(Config) ->
    DBs = ct_helper:get_preset_var(Config, dbs, []),
    lists:member(minio, DBs).

%% It is useful to debug dynamic IQ handler registration
print_debug_info_for_module(Module) ->
    ModConfig = rpc(mim(), gen_mod, hosts_and_opts_with_module, [Module]),
    IqConfig = rpc(mim(), ets, tab2list, [sm_iqtable]),
    ct:pal("hosts_and_opts=~p~n iq_handlers=~p~n",
           [ModConfig, IqConfig]).

%% Backing up and changing configuration options - API

backup_and_set_config(Config, Options) ->
    Backup = get_config_backup(Config),
    NewBackup = maps:fold(fun(K, V, Bkp) ->
                                  do_backup_and_set_config_option(Bkp, K, V)
                          end, Backup, Options),
    set_config_backup(Config, NewBackup).

backup_and_set_config_option(Config, Option, NewValue) ->
    Backup = get_config_backup(Config),
    NewBackup = do_backup_and_set_config_option(Backup, Option, NewValue),
    set_config_backup(Config, NewBackup).

%% @doc Change a config option that is already backed up
change_config_option(Config, Option, NewValue) ->
    case maps:is_key(Option, get_config_backup(Config)) of
        true ->
            rpc(mim(), mongoose_config, set_opt, [Option, NewValue]);
        false ->
            error({"Cannot change an option that is not backed up", Option, NewValue})
    end.

restore_config(Config) ->
    Options = get_config_backup(Config),
    %% TODO replace with maps:foreach when dropping OTP 23
    maps:map(fun do_restore_config_option/2, Options),
    ok.

restore_config_option(Config, Option) ->
    Options = get_config_backup(Config),
    do_restore_config_option(Option, maps:get(Option, Options)).

%% Backing up and changing configuration options - helpers

get_config_backup(Config) ->
    proplists:get_value(config_backup, Config, #{}).

set_config_backup(Config, Backup) ->
    lists:keystore(config_backup, 1, Config, {config_backup, Backup}).

do_backup_and_set_config_option(ConfigBackup, Option, NewValue) ->
    OriginalValue = rpc(mim(), mongoose_config, lookup_opt, [Option]),
    rpc(mim(), mongoose_config, set_opt, [Option, NewValue]),
    %% If Option has already been backed up, keep the original value
    maps:merge(#{Option => OriginalValue}, ConfigBackup).

do_restore_config_option(Option, {ok, Value}) ->
    rpc(mim(), mongoose_config, set_opt, [Option, Value]);
do_restore_config_option(Option, {error, not_found}) ->
    rpc(mim(), mongoose_config, unset_opt, [Option]).

wait_for_n_offline_messages(Client, N) ->
    LUser = escalus_utils:jid_to_lower(escalus_client:username(Client)),
    LServer = escalus_utils:jid_to_lower(escalus_client:server(Client)),
    WaitFn = fun() -> total_offline_messages({LUser, LServer}) end,
    wait_until(WaitFn, N).

wait_for_c2s_state_name(C2SPid, NewStateName) ->
    wait_until(fun() -> get_c2s_state_name(C2SPid) end, NewStateName,
                #{name => get_c2s_state_name}).

get_c2s_state_name(C2SPid) when is_pid(C2SPid) ->
    SysStatus = rpc(mim(), sys, get_status, [C2SPid]),
    extract_state_name(SysStatus).

extract_state_name(SysStatus) ->
    {status, _Pid, {module, _},
     [_, _, _, _, [_, {data, FSMData} | _]]} = SysStatus,
    proplists:get_value("StateName", FSMData).
