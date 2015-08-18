-module(mongooseim_cleaner_SUITE).
-compile([export_all]).

all() ->
    [sanity_check,
     cleaner_runs_hook_on_nodedown].

%%
%% Tests
%%

sanity_check(_) -> ok.

cleaner_runs_hook_on_nodedown(_) ->
    setup_meck(),
    %% given
    {ok, _HooksServer} = ejabberd_hooks:start_link(),
    {ok, Cleaner} = mongooseim_cleaner:start_link(),
    Self = self(),
    NotifySelf = fun (Node) -> Self ! {got_nodedown, Node} end,
    ejabberd_hooks:add(node_cleanup, global, undefined, NotifySelf, 50),
    %% when
    FakeNode = fakename@fakehost,
    Cleaner ! {nodedown, FakeNode},
    %% then
    receive
        {got_nodedown, FakeNode} -> ok
    after timer:seconds(1) ->
        ct:fail({timeout, got_nodedown})
    end,
    unload_meck().

%%
%% Internal
%%

setup_meck() ->
    meck:new(exometer),
    NodeCleanupMetric = [global, node_cleanup],
    meck:expect(exometer, info, fun(NodeCleanupMetric, _) -> undefined end),
    meck:expect(exometer, new, fun(NodeCleanupMetric, _) -> ok end),
    meck:expect(exometer, update, fun(NodeCleanupMetric, _) -> ok end).

unload_meck() ->
    meck:validate(exometer),
    meck:unload(exometer).