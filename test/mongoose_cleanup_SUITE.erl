-module(mongoose_cleanup_SUITE).

-include_lib("eunit/include/eunit.hrl").
-include("mongoose.hrl").

-export([all/0,
         init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2]).
-export([cleaner_runs_hook_on_nodedown/1, notify_self_hook/3]).
-export([auth_anonymous/1,
         last/1,
         stream_management/1,
         local/1,
         s2s/1,
         bosh/1
        ]).

-define(HOST, <<"localhost">>).
-define(NS_CC_2, <<"urn:xmpp:carbons:2">>).

%% -----------------------------------------------------
%% CT callbacks
%% -----------------------------------------------------

all() ->
    [
     cleaner_runs_hook_on_nodedown,
     auth_anonymous,
     last,
     stream_management,
     local,
     s2s,
     bosh
    ].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(jid),
    ok = mnesia:create_schema([node()]),
    ok = mnesia:start(),
    [mongoose_config:set_opt(Key, Value) || {Key, Value} <- opts()],
    mongoose_domain_api:init(),
    Config.

end_per_suite(Config) ->
    mongoose_domain_api:stop(),
    [mongoose_config:unset_opt(Key) || {Key, _Value} <- opts()],
    mnesia:stop(),
    mnesia:delete_schema([node()]),
    Config.

opts() ->
    [{hosts, [?HOST]},
     {host_types, []},
     {all_metrics_are_global, false}].

init_per_testcase(T, Config) ->
    {ok, _HooksServer} = gen_hook:start_link(),
    setup_meck(meck_mods(T)),
    Config.

end_per_testcase(T, Config) ->
    unload_meck(meck_mods(T)),
    Config.

meck_mods(bosh) -> [exometer, mod_bosh_socket];
meck_mods(s2s) -> [exometer, ejabberd_commands, mongoose_bin];
meck_mods(local) -> [exometer];
meck_mods(_) -> [exometer, ejabberd_sm, ejabberd_local].

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

notify_self_hook(Acc, #{node := Node}, #{self := Self}) ->
    Self ! {got_nodedown, Node},
    {ok, Acc}.

auth_anonymous(_Config) ->
    HostType = host_type(),
    {U, S, R, JID, SID} = get_fake_session(),
    ejabberd_auth_anonymous:start(HostType),
    Info = #{auth_module => cyrsasl_anonymous},
    ejabberd_auth_anonymous:register_connection(#{}, HostType, SID, JID, Info),
    true = ejabberd_auth_anonymous:does_user_exist(HostType, U, S),
    mongoose_hooks:session_cleanup(S, new_acc(S), U, R, SID),
    false = ejabberd_auth_anonymous:does_user_exist(HostType, U, S).

last(_Config) ->
    HostType = host_type(),
    {U, S, R, _JID, SID} = get_fake_session(),
    mod_last:start(HostType, [{backend, mnesia}, {iqdisc, no_queue}]),
    not_found = mod_last:get_last_info(HostType, U, S),
    Status1 = <<"status1">>,
    #{} = mod_last:on_presence_update(new_acc(S), U, S, R, Status1),
    {ok, TS1, Status1} = mod_last:get_last_info(HostType, U, S),
    async_helper:wait_until(
      fun() ->
              mongoose_hooks:session_cleanup(S, new_acc(S), U, R, SID),
              {ok, TS2, <<>>} = mod_last:get_last_info(HostType, U, S),
              TS2 - TS1 > 0
      end,
      true).

stream_management(_Config) ->
    HostType = host_type(),
    {U, S, R, _JID, SID} = get_fake_session(),
    mod_stream_management:start(HostType, []),
    SMID = <<"123">>,
    mod_stream_management:register_smid(HostType, SMID, SID),
    {sid, SID} = mod_stream_management:get_sid(HostType, SMID),
    mongoose_hooks:session_cleanup(S, new_acc(S), U, R, SID),
    {error, smid_not_found} = mod_stream_management:get_sid(HostType, SMID).

local(_Config) ->
    ejabberd_local:start_link(),
    Self = self(),
    SelfNotify = fun(_, _, _, Arg) -> Self ! Arg end,
    ID = <<"abc123">>,

    ejabberd_local:register_iq_response_handler(?HOST, ID, undefined, SelfNotify, 50),
    receive
        timeout -> ok
    after
        2000 -> ct:fail({timeout, valid_iq_timeout})
    end,

    ejabberd_local:register_iq_response_handler(?HOST, ID, undefined, SelfNotify, 2000),
    {ok, undefined, _F} = ejabberd_local:get_iq_callback(ID),
    mongoose_hooks:node_cleanup(node()),
    error = ejabberd_local:get_iq_callback(ID).

s2s(_Config) ->
    ejabberd_s2s:start_link(),
    FromTo = {?HOST, <<"foreign">>},
    ejabberd_s2s:try_register(FromTo),
    Self = self(),
    [Self] = ejabberd_s2s:get_connections_pids(FromTo),
    mongoose_hooks:node_cleanup(node()),
    [] = ejabberd_s2s:get_connections_pids(FromTo).

bosh(_Config) ->
    mod_bosh:start(?HOST, config_parser_helper:default_mod_config(mod_bosh)),
    SID = <<"sid">>,
    Self = self(),
    {error, _} = mod_bosh:get_session_socket(SID),
    mod_bosh:store_session(SID, Self),
    {ok, Self} = mod_bosh:get_session_socket(SID),
    mongoose_hooks:node_cleanup(node()),
    {error, _} = mod_bosh:get_session_socket(SID),
    ok.

%% -----------------------------------------------------
%% Internal
%% -----------------------------------------------------

setup_meck([exometer | R]) ->
    meck:new(exometer),
    meck:expect(exometer, info, fun(_, _) -> undefined end),
    meck:expect(exometer, new, fun(_, _) -> ok end),
    meck:expect(exometer, update, fun(_, _) -> ok end),
    setup_meck(R);
setup_meck([ejabberd_sm | R]) ->
    meck:new(ejabberd_sm),
    meck:expect(ejabberd_sm, register_iq_handler,
                fun(_A1, _A2, _A3) -> ok end),
    setup_meck(R);
setup_meck([ejabberd_local | R]) ->
    meck:new(ejabberd_local),
    meck:expect(ejabberd_local, register_iq_handler,
                fun(_A1, _A2, _A3) -> ok end),
    setup_meck(R);
setup_meck([ejabberd_commands | R]) ->
    meck:new(ejabberd_commands),
    meck:expect(ejabberd_commands, register_commands, fun(_) -> ok end),
    setup_meck(R);
setup_meck([mongoose_bin | R]) ->
    meck:new(mongoose_bin, [passthrough]),
    meck:expect(mongoose_bin, gen_from_crypto, fun() -> <<"123456">> end),
    setup_meck(R);
setup_meck([mod_bosh_socket | R]) ->
    meck:new(mod_bosh_socket, [passthrough]),
    meck:expect(mod_bosh_socket, start_supervisor, fun() -> {ok, self()} end),
    setup_meck(R);
setup_meck([]) ->
    ok.

unload_meck(Mods) ->
    [ {meck:validate(Mod), meck:unload(Mod)} ||
      Mod <- Mods ].

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
                       host_type => host_type(),
                       element => undefined}).

host_type() ->
    <<"test host type">>.
