-module(mongoose_cleanup_SUITE).

-include_lib("eunit/include/eunit.hrl").
-include("mongoose.hrl").

-export([all/0,
         init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2]).
-export([cleaner_runs_hook_on_nodedown/1]).
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
    {ok, _} = application:ensure_all_started(stringprep),
    ok = mnesia:create_schema([node()]),
    ok = mnesia:start(),
    Config.

end_per_suite(Config) ->
    mnesia:stop(),
    mnesia:delete_schema([node()]),
    Config.

init_per_testcase(T, Config) ->
    {ok, _HooksServer} = ejabberd_hooks:start_link(),
    setup_meck(meck_mods(T)),
    Config.

end_per_testcase(T, Config) ->
    unload_meck(meck_mods(T)),
    Config.

meck_mods(bosh) -> [exometer, mod_bosh_socket, ejabberd_config];
meck_mods(s2s) -> [exometer, ejabberd_commands, mongoose_bin, ejabberd_config];
meck_mods(local) -> [exometer, ejabberd_config];
meck_mods(_) -> [exometer, ejabberd_sm, ejabberd_local, ejabberd_config].

%% -----------------------------------------------------
%% Tests
%% -----------------------------------------------------

cleaner_runs_hook_on_nodedown(_Config) ->
    meck:expect(ejabberd_hooks, error_running_hook, fun(_, _, _) -> ok end),
    {ok, Cleaner} = mongoose_cleaner:start_link(),
    Self = self(),
    NotifySelf = fun (Acc, Node) -> Self ! {got_nodedown, Node}, Acc end,
    ejabberd_hooks:add(node_cleanup, global, undefined, NotifySelf, 50),

    FakeNode = fakename@fakehost,
    Cleaner ! {nodedown, FakeNode},

    receive
        {got_nodedown, FakeNode} -> ok
    after timer:seconds(1) ->
        ct:fail({timeout, got_nodedown})
    end,
    ?assertEqual(false, meck:called(ejabberd_hooks, error_running_hook, ['_','_','_'])).

auth_anonymous(_Config) ->
    ejabberd_auth_anonymous:start(?HOST),
    {U, S, R, JID, SID} = get_fake_session(),
    Info = [{auth_module, ejabberd_auth_anonymous}],
    ejabberd_auth_anonymous:register_connection(#{}, SID, JID, Info),
    true = ejabberd_auth_anonymous:anonymous_user_exist(U, S),
    ejabberd_hooks:run(session_cleanup, ?HOST, [U, S, R, SID]),
    false = ejabberd_auth_anonymous:anonymous_user_exist(U, S).

last(_Config) ->
    mod_last:start(?HOST, [{backend, mnesia}, {iqdisc, no_queue}]),
    {U, S, R, _JID, SID} = get_fake_session(),
    not_found = mod_last:get_last_info(U, S),
    Status1 = <<"status1">>,
    #{} = mod_last:on_presence_update(#{}, U, S, R, Status1),
    {ok, TS1, Status1} = mod_last:get_last_info(U, S),
    async_helper:wait_until(
      fun() ->
              ejabberd_hooks:run(session_cleanup, ?HOST, [U, S, R, SID]),
              {ok, TS2, <<>>} = mod_last:get_last_info(U, S),
              TS2 - TS1 > 0
      end,
      true).

stream_management(_Config) ->
    mod_stream_management:start(?HOST, []),
    {U, S, R, _JID, SID} = get_fake_session(),
    SMID = <<"123">>,
    mod_stream_management:register_smid(SMID, SID),
    {sid, SID} = mod_stream_management:get_sid(SMID),
    Acc = mongoose_acc:new(
            #{location => ?LOCATION,
              lserver => S,
              element => undefined}),
    ejabberd_hooks:run_fold(session_cleanup, ?HOST, Acc, [U, S, R, SID]),
    {error, smid_not_found} = mod_stream_management:get_sid(SMID).

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
    ejabberd_hooks:run(node_cleanup, [node()]),
    error = ejabberd_local:get_iq_callback(ID).

s2s(_Config) ->
    ejabberd_s2s:start_link(),
    FromTo = {?HOST, <<"foreign">>},
    ejabberd_s2s:try_register(FromTo),
    Self = self(),
    [Self] = ejabberd_s2s:get_connections_pids(FromTo),
    ejabberd_hooks:run(node_cleanup, [node()]),
    [] = ejabberd_s2s:get_connections_pids(FromTo).

bosh(_Config) ->
    mod_bosh:start(?HOST, []),
    SID = <<"sid">>,
    Self = self(),
    {error, _} = mod_bosh:get_session_socket(SID),
    mod_bosh:store_session(SID, Self),
    {ok, Self} = mod_bosh:get_session_socket(SID),
    ejabberd_hooks:run(node_cleanup, [node()]),
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
                fun(_A1, _A2, _A3, _A4, _A5) -> ok end),
    setup_meck(R);
setup_meck([ejabberd_local | R]) ->
    meck:new(ejabberd_local),
    meck:expect(ejabberd_local, register_iq_handler,
                fun(_A1, _A2, _A3, _A4, _A5) -> ok end),
    setup_meck(R);
setup_meck([ejabberd_config | R]) ->
    meck:new(ejabberd_config),
    meck:expect(ejabberd_config, get_global_option,
                fun
                    (hosts) -> [];
                    (_) -> undefined
                end),
    meck:expect(ejabberd_config, get_local_option, fun(_) -> undefined end),
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
    meck:new(mod_bosh_socket),
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

