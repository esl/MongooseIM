-module(mongoose_cleanup_SUITE).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").
-include("mongoose.hrl").

-compile([export_all, nowarn_export_all]).

-define(HOST, <<"localhost">>).

%% -----------------------------------------------------
%% CT callbacks
%% -----------------------------------------------------

all() ->
    [
     cleaner_runs_hook_on_nodedown,
     cleaner_runs_hook_on_nodedown_for_host_type,
     auth_anonymous,
     last,
     {group, cets},
     {group, mnesia}
    ].

groups() ->
    [{cets, [], backend_tests()},
     {mnesia, [], backend_tests()},
     {component, [], component_cases()},
     {muc, [], muc_cases()}].

backend_tests() ->
    [{group, component}, {group, muc}, bosh, stream_management, s2s].

component_cases() ->
    [component, component_from_other_node_remains].

muc_cases() ->
    [muc_node_cleanup_for_host_type, muc_room, muc_room_from_other_node_remains].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(jid),
    ok = mnesia:create_schema([node()]),
    ok = mnesia:start(),
    mongoose_config:set_opts(opts()),
    lists:foreach(fun setup_meck/1, meck_mods()),
    async_helper:start(Config, [{mim_ct_sup, start_link, [ejabberd_sup]},
                                {mongooseim_helper, start_link_loaded_hooks, []},
                                {mongoose_domain_sup, start_link, []},
                                {mongoose_instrument, start_link, []}]).

end_per_suite(Config) ->
    async_helper:stop_all(Config),
    lists:foreach(fun unload_meck/1, meck_mods()),
    mongoose_config:erase_opts(),
    mnesia:stop(),
    mnesia:delete_schema([node()]).

init_per_group(cets, Config) ->
    [{backend, cets} | start_cets_disco(Config)];
init_per_group(mnesia, Config) ->
    [{backend, mnesia} | Config];
init_per_group(component, Config) ->
    mongoose_config:set_opt(component_backend, ?config(backend, Config)),
    [{needs_component, true} | Config];
init_per_group(Group, Config) ->
    start_modules(Group, Config),
    Config.

end_per_group(cets, Config) ->
    stop_cets_disco(Config);
end_per_group(Group, Config) ->
    stop_modules(Group, Config).

init_per_testcase(s2s, Config) ->
    mongoose_config:set_opt(s2s_backend, ?config(backend, Config)),
    Config;
init_per_testcase(auth_anonymous, Config) ->
    mongoose_gen_auth:start(ejabberd_auth_anonymous, ?HOST),
    Config;
init_per_testcase(TestCase, Config) ->
    start_component_if_needed(?config(needs_component, Config)),
    start_modules(TestCase, Config),
    Config.

end_per_testcase(auth_anonymous, _Config) ->
    mongoose_gen_auth:stop(ejabberd_auth_anonymous, ?HOST);
end_per_testcase(TestCase, Config) ->
    stop_modules(TestCase, Config),
    stop_component_if_needed(?config(needs_component, Config)).

start_modules(GroupOrCase, Config) ->
    mongoose_modules:replace_modules(?HOST, [], required_modules(GroupOrCase, Config)).

stop_modules(GroupOrCase, Config) ->
    mongoose_modules:replace_modules(?HOST, maps:keys(required_modules(GroupOrCase, Config)), #{}).

opts() ->
    #{hosts => [?HOST],
      host_types => [],
      s2s_backend => mnesia,
      {auth, ?HOST} => config_parser_helper:extra_auth(),
      {modules, ?HOST} => #{},
      instrumentation => config_parser_helper:default_config([instrumentation])}.

meck_mods() ->
    [exometer, mod_bosh_socket, mongoose_bin, ejabberd_sm, ejabberd_local].

required_modules(muc, Config) ->
    required_module(mod_muc, #{online_backend => ?config(backend, Config), backend => mnesia});
required_modules(bosh, Config) ->
    required_module(mod_bosh, #{backend => ?config(backend, Config)});
required_modules(stream_management, Config) ->
    required_module(mod_stream_management, #{backend => ?config(backend, Config)});
required_modules(_GroupOrCase, _Config) ->
    #{}.

required_module(Module, ExtraOpts) ->
    #{Module => config_parser_helper:mod_config(Module, ExtraOpts)}.

start_component_if_needed(true) ->
    mongoose_router:start(),
    mongoose_component:start();
start_component_if_needed(_) ->
    ok.

stop_component_if_needed(true) ->
    mongoose_component:stop(),
    mongoose_router:stop();
stop_component_if_needed(_) ->
    ok.

%% -----------------------------------------------------
%% Tests
%% -----------------------------------------------------

cleaner_runs_hook_on_nodedown(_Config) ->
    meck:expect(gen_hook, error_running_hook, fun(_, _, _, _, _) -> ok end),
    {ok, Cleaner} = mongoose_cleaner:start_link(),
    gen_hook:add_handler(node_cleanup, global,
                         fun ?MODULE:notify_self_hook/3,
                         #{self => self()}, 50),
    FakeNode = fakename@fakehost,
    Cleaner ! {nodedown, FakeNode},
    receive
        {got_nodedown, FakeNode} -> ok
    after timer:seconds(1) ->
        ct:fail({timeout, got_nodedown})
    end,
    ?assertEqual(false, meck:called(gen_hook, error_running_hook,
                                    ['_', '_', '_', '_', '_'])).

cleaner_runs_hook_on_nodedown_for_host_type(_Config) ->
    HostType = ?HOST,
    {ok, Cleaner} = mongoose_cleaner:start_link(),
    gen_hook:add_handler(node_cleanup_for_host_type, HostType,
                         fun ?MODULE:notify_self_hook_for_host_type/3,
                         #{self => self()}, 50),
    FakeNode = fakename@fakehost,
    Cleaner ! {nodedown, FakeNode},
    receive
        {got_nodedown_for_host_type, FakeNode, HostType} -> ok
    after timer:seconds(1) ->
        ct:fail({timeout, got_nodedown})
    end.

notify_self_hook(Acc, #{node := Node}, #{self := Self}) ->
    Self ! {got_nodedown, Node},
    {ok, Acc}.

notify_self_hook_for_host_type(Acc, #{node := Node}, #{self := Self, host_type := HostType}) ->
    Self ! {got_nodedown_for_host_type, Node, HostType},
    {ok, Acc}.

auth_anonymous(_Config) ->
    HostType = ?HOST,
    {U, S, R, JID, SID} = get_fake_session(),
    Info = #{auth_module => cyrsasl_anonymous},
    ejabberd_auth_anonymous:register_connection(#{},
                                                #{sid => SID, jid => JID, info => Info},
                                                #{host_type => HostType}),
    true = ejabberd_auth_anonymous:does_user_exist(HostType, U, S),
    mongoose_hooks:session_cleanup(S, new_acc(S), U, R, SID),
    false = ejabberd_auth_anonymous:does_user_exist(HostType, U, S).

last(_Config) ->
    HostType = ?HOST,
    {U, S, R, JID, SID} = get_fake_session(),
    {started, ok} = start(HostType,
                          mod_last,
                          config_parser_helper:mod_config(mod_last, #{iqdisc => no_queue})),
    not_found = mod_last:get_last_info(HostType, U, S),
    Status1 = <<"status1">>,
    {ok, #{}} = mod_last:unset_presence(new_acc(S), #{jid => JID, status => Status1}, #{}),
    {ok, TS1, Status1} = mod_last:get_last_info(HostType, U, S),
    wait_helper:wait_until(
      fun() ->
              mongoose_hooks:session_cleanup(S, new_acc(S), U, R, SID),
              {ok, TS2, <<>>} = mod_last:get_last_info(HostType, U, S),
              TS2 - TS1 > 0
      end,
      true).

stream_management(_Config) ->
    HostType = ?HOST,
    {U, S, R, _JID, SID} = get_fake_session(),
    SMID = <<"123">>,
    mod_stream_management:register_smid(HostType, SMID, SID),
    {sid, SID} = mod_stream_management:get_sid(HostType, SMID),
    mongoose_hooks:session_cleanup(S, new_acc(S), U, R, SID),
    {error, smid_not_found} = mod_stream_management:get_sid(HostType, SMID).

s2s(_Config) ->
    ejabberd_s2s:start_link(),
    FromTo = {?HOST, <<"foreign">>},
    ejabberd_s2s:try_register(FromTo),
    Self = self(),
    [Self] = ejabberd_s2s:get_s2s_out_pids(FromTo),
    mongoose_hooks:node_cleanup(node()),
    [] = ejabberd_s2s:get_s2s_out_pids(FromTo).

bosh(_Config) ->
    SID = <<"sid">>,
    Self = self(),
    {error, _} = mod_bosh:get_session_socket(SID),
    mod_bosh:store_session(SID, Self),
    {ok, Self} = mod_bosh:get_session_socket(SID),
    mongoose_hooks:node_cleanup(node()),
    {error, _} = mod_bosh:get_session_socket(SID),
    ok.

component(_Config) ->
    Handler = fun() -> ok end,
    Domain = <<"cool.localhost">>,
    Node = some_node,
    {ok, _} = mongoose_component:register_component(Domain, Node, Handler, false, false),
    true = mongoose_component:has_component(Domain),
    #{mongoose_component := ok} = mongoose_hooks:node_cleanup(Node),
    [] = mongoose_component:dirty_get_all_components(all),
    false = mongoose_component:has_component(Domain),
    ok.

component_from_other_node_remains(_Config) ->
    Handler = fun() -> ok end,
    Domain = <<"cool.localhost">>,
    {ok, Comp} = mongoose_component:register_component(Domain, other_node, Handler, false, false),
    true = mongoose_component:has_component(Domain),
    #{mongoose_component := ok} = mongoose_hooks:node_cleanup(some_node),
    true = mongoose_component:has_component(Domain),
    mongoose_component:unregister_component(Comp),
    ok.

muc_node_cleanup_for_host_type(_Config) ->
    {ok, Pid} = mongoose_cleaner:start_link(),
    Pid ! {nodedown, 'badnode@localhost'},
    %% Check if the cleaner process is still running
    ok = gen_server:call(Pid, ping).

muc_room(_Config) ->
    HostType = ?HOST,
    MucHost = <<"muc.localhost">>,
    Pid = remote_pid(),
    Node = node(Pid),
    Room = <<"remote_room">>,
    ok = mod_muc_online_backend:register_room(HostType, MucHost, Room, Pid),
    ok = mod_muc_online_backend:node_cleanup(HostType, Node),
    {error, not_found} = mod_muc_online_backend:find_room_pid(HostType, MucHost, Room).

muc_room_from_other_node_remains(_Config) ->
    HostType = ?HOST,
    MucHost = <<"muc.localhost">>,
    Pid = self(),
    RemoteNode = node(remote_pid()),
    Room = <<"room_on_other_node">>,
    ok = mod_muc_online_backend:register_room(HostType, MucHost, Room, Pid),
    ok = mod_muc_online_backend:node_cleanup(HostType, RemoteNode),
    {ok, Pid} = mod_muc_online_backend:find_room_pid(HostType, MucHost, Room).

%% -----------------------------------------------------
%% Internal
%% -----------------------------------------------------

setup_meck(exometer) ->
    meck:new(exometer, [no_link]),
    meck:expect(exometer, info, fun(_, _) -> undefined end),
    meck:expect(exometer, new, fun(_, _) -> ok end),
    meck:expect(exometer, update, fun(_, _) -> ok end);
setup_meck(ejabberd_sm) ->
    meck:new(ejabberd_sm, [no_link]),
    meck:expect(ejabberd_sm, register_iq_handler,
                fun(_A1, _A2, _A3) -> ok end);
setup_meck(ejabberd_local) ->
    meck:new(ejabberd_local, [no_link]),
    meck:expect(ejabberd_local, register_iq_handler,
                fun(_A1, _A2, _A3) -> ok end);
setup_meck(mongoose_bin) ->
    meck:new(mongoose_bin, [passthrough, no_link]),
    meck:expect(mongoose_bin, gen_from_crypto, fun() -> <<"123456">> end);
setup_meck(mod_bosh_socket) ->
    meck:new(mod_bosh_socket, [passthrough, no_link]),
    meck:expect(mod_bosh_socket, start_supervisor, fun() -> {ok, self()} end).

unload_meck(Module) ->
    meck:validate(Module),
    meck:unload(Module).

-spec get_fake_session() ->
    {U :: binary(), S :: binary(), R :: binary(),
     JID :: jid:jid(), SID :: ejabberd_sm:sid()}.
get_fake_session() ->
    U = <<"someuser">>,
    S = ?HOST,
    R = <<"someresource">>,
    JID = jid:make(U, S, R),
    SID = {os:timestamp(), self()},
    {U, S, R, JID, SID}.

new_acc(Server) ->
    mongoose_acc:new(#{location => ?LOCATION,
                       lserver => Server,
                       host_type => ?HOST,
                       element => undefined}).

start(HostType, Module) ->
    start(HostType, Module, config_parser_helper:default_mod_config(Module)).

start(HostType, Module, Opts) ->
    mongoose_modules:ensure_started(HostType, Module, Opts).

disco_opts() ->
    #{name => mongoose_cets_discovery, disco_file => "does_not_exist.txt"}.

start_cets_disco(Config) ->
    {ok, Pid} = cets_discovery:start(disco_opts()),
    [{cets_disco, Pid} | Config].

stop_cets_disco(Config) ->
    case proplists:get_value(cets_disco, Config) of
        Pid when is_pid(Pid) ->
            exit(Pid, kill);
        _ ->
            ok
    end.

%% Pid 90 on cool_node@localhost
%% Made using:
%% erl -name cool_node@localhost
%% rp(term_to_binary(list_to_pid("<0.90.0>"))).
remote_pid_binary() ->
    <<131, 88, 100, 0, 19, 99, 111, 111, 108, 95, 110, 111, 100, 101, 64,
      108, 111, 99, 97, 108, 104, 111, 115, 116, 0, 0, 0, 90, 0, 0, 0, 0, 100,
      200, 255, 233>>.

remote_pid() ->
    binary_to_term(remote_pid_binary()).
