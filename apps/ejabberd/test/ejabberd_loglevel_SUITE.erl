-module(ejabberd_loglevel_SUITE).
-compile([export_all]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(eq(Expected, Actual), ?assertEqual(Expected, Actual)).
-define(ne(A, B), ?assertNot(A == B)).

all() ->
    [init_test,
     set_get_loglevel,
     set_custom_loglevel].

end_per_testcase(_TestCase, _Config) ->
    application:stop(lager),
    ok.

%%
%% Tests
%%

init_test(_) ->
    ejabberd_loglevel:init().

set_get_loglevel(C) ->
    %% given
    ejabberd_loglevel_running(),
    Levels = [{0, none},
              {1, critical},
              {2, error},
              {3, warning},
              {4, info},
              {5, debug}],
    %% hint: [0, 1, ..., 5, none, critical, ..., debug]
    LevelsToTest = ([ {L, element(1, L)} || L <- Levels ] ++
                    [ {L, element(2, L)} || L <- Levels ]),
    %% when / then
    [ set_get_loglevel(C, lager_console_backend, Expected, Level)
      || {Expected, Level} <- LevelsToTest ].

set_get_loglevel(_, Backend, Expected, Level) ->
    %% when
    [{Backend, ok}] = ejabberd_loglevel:set(Level),
    %% then
    [{Backend, Expected}] = ejabberd_loglevel:get().

set_custom_loglevel(_) ->
    %% given
    {ok, Backend} = ejabberd_loglevel_running(),
    ExampleMod = ejabberd_c2s,
    ExampleLvl = info,
    %% when setting a custom log level for some module
    %% the operation succeeds
    [{Backend, ok}] = ejabberd_loglevel:set_custom(ExampleMod, ExampleLvl).

%%
%% Helpers
%%

ejabberd_loglevel_running() ->
    application:load(lager),
    Backend = lager_console_backend,
    application:set_env(lager, handlers, [{lager_console_backend, info}]),
    ejabberd_loglevel:init(),
    {ok, Backend}.
