-module(mongoose_cleanup_SUITE).

-export([all/0,
         init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2]).
-export([cleaner_runs_hook_on_nodedown/1]).
-export([auth_anonymous/1,
         carboncopy/1,
         last/1,
         stream_management/1
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
     carboncopy,
     last,
     stream_management
    ].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(p1_stringprep),
    mnesia:create_schema([node()]),
    mnesia:start(),
    Config.

end_per_suite(Config) ->
    mnesia:stop(),
    mnesia:delete_schema([node()]),
    Config.

init_per_testcase(_T, Config) ->
    {ok, _HooksServer} = ejabberd_hooks:start_link(),
    setup_meck(),
    Config.

end_per_testcase(_T, Config) ->
    unload_meck(),
    Config.

%% -----------------------------------------------------
%% Tests
%% -----------------------------------------------------

cleaner_runs_hook_on_nodedown(_Config) ->
    {ok, Cleaner} = mongoose_cleaner:start_link(),
    Self = self(),
    NotifySelf = fun (Node) -> Self ! {got_nodedown, Node} end,
    ejabberd_hooks:add(node_cleanup, global, undefined, NotifySelf, 50),
    
    FakeNode = fakename@fakehost,
    Cleaner ! {nodedown, FakeNode},
    
    receive
        {got_nodedown, FakeNode} -> ok
    after timer:seconds(1) ->
        ct:fail({timeout, got_nodedown})
    end.

auth_anonymous(_Config) ->
    ejabberd_auth_anonymous:start(?HOST),
    {U, S, R, JID, SID} = get_fake_session(),
    Info = [{auth_module, ejabberd_auth_anonymous}],
    ejabberd_auth_anonymous:register_connection(SID, JID, Info),
    true = ejabberd_auth_anonymous:anonymous_user_exist(U, S),
    ejabberd_hooks:run(session_cleanup, ?HOST, [U, S, R, SID]),
    false = ejabberd_auth_anonymous:anonymous_user_exist(U, S).

carboncopy(_Config) ->
    mod_carboncopy:start(?HOST, [{iqdisc, no_queue}]),
    {U, S, R, _JID, SID} = get_fake_session(),
    mod_carboncopy:enable(S, U, R, ?NS_CC_2),
    [{R, ?NS_CC_2}] = mod_carboncopy:resources_to_cc(U, S),
    ejabberd_hooks:run(session_cleanup, ?HOST, [U, S, R, SID]),
    [] = mod_carboncopy:resources_to_cc(U, S).

last(_Config) ->
    mod_last:start(?HOST, [{iqdisc, no_queue}]),
    {U, S, R, _JID, SID} = get_fake_session(),
    not_found = mod_last:get_last_info(U, S),
    Status1 = <<"status1">>,
    ok = mod_last:on_presence_update(U, S, R, Status1),
    {ok, TS1, Status1} = mod_last:get_last_info(U, S),
    timer:sleep(2000),
    ejabberd_hooks:run(session_cleanup, ?HOST, [U, S, R, SID]),
    {ok, TS2, <<>>} = mod_last:get_last_info(U, S),
    true = TS2 - TS1 > 0.

stream_management(_Config) ->
    mod_stream_management:start(?HOST, []),
    {U, S, R, _JID, SID} = get_fake_session(),
    SMID = <<"123">>,
    mod_stream_management:register_smid(SMID, SID),
    [SID] = mod_stream_management:get_sid(SMID),
    ejabberd_hooks:run(session_cleanup, ?HOST, [U, S, R, SID]),
    [] = mod_stream_management:get_sid(SMID).

%% -----------------------------------------------------
%% Internal
%% -----------------------------------------------------

setup_meck() ->
    meck:new(exometer),
    meck:expect(exometer, info, fun(_, _) -> undefined end),
    meck:expect(exometer, new, fun(_, _) -> ok end),
    meck:expect(exometer, update, fun(_, _) -> ok end),
    meck:new(ejabberd_sm),
    meck:expect(ejabberd_sm, register_iq_handler,
                fun(_A1, _A2, _A3, _A4, _A5) -> ok end),
    meck:new(ejabberd_local),
    meck:expect(ejabberd_local, register_iq_handler,
                fun(_A1, _A2, _A3, _A4, _A5) -> ok end).

unload_meck() ->
    [ {meck:validate(Mod), meck:unload(Mod)} ||
      Mod <- [exometer, ejabberd_sm, ejabberd_local] ].

-spec get_fake_session() ->
    {U :: binary(), S :: binary(), R :: binary(),
     JID :: ejabberd:jid(), SID :: ejabberd_sm:sid()}.
get_fake_session() ->
    U = <<"someuser">>,
    S = ?HOST,
    R = <<"someresource">>,
    JID = jlib:make_jid(U, S, R),
    SID = {os:timestamp(), self()},
    {U, S, R, JID, SID}.

