%% Helper module for an ETS table with an owner process

-module(ets_helper).

-compile([export_all, nowarn_export_all]).

%% API

new(Name) ->
    new(Name, []).

new(Name, ExtraOpts) ->
    TabOwner = spawn(?MODULE, init_tab, [self(), Name, ExtraOpts]),
    receive
        table_created -> ok
    after
        5000 ->
            exit(TabOwner, kill),
            error("Table owner did not respond")
    end.

delete(Name) ->
    TabOwner = ets:info(Name, owner),
    ets:delete(Name),
    TabOwner ! stop,
    ok.

%% Helpers

init_tab(Caller, Name, ExtraOpts) ->
    ets:new(Name, [named_table, public | ExtraOpts]),
    Caller ! table_created,
    receive stop -> ok end.
