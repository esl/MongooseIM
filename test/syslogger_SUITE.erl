-module(syslogger_SUITE).

-export([all/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_testcase/2,
         end_per_testcase/2]).

-export([
    syslogger_is_used_to_log/1,
    log_at_every_level/1,
    log_at_custom_level/1
]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(HANDLER_ID, sys_log).
-define(IDENT, "mongooseim").
-define(FACILITY, local0).

all() ->
    [syslogger_is_used_to_log,
     log_at_every_level,
     log_at_custom_level
    ].

init_per_suite(Config) ->
    LoggerConfig = logger:get_primary_config(),
    logger:set_primary_config(level, info),

    setup_meck(Config),
    ok = mongoose_logger_running(),

    [{logger_primary_config, LoggerConfig} | Config].

end_per_suite(Config) ->
    logger:remove_handler(?HANDLER_ID),
    logger:set_primary_config(?config(logger_primary_config, Config)),

    unload_meck(Config),
    ok.

init_per_testcase(_TC, Config) ->
    meck:reset(syslogger),
    Config.

end_per_testcase(_TC, _Config) ->
    ok.

%%
%% Tests
%%
syslogger_is_used_to_log(_Config) ->
    logger:log(error, "Something"),

    ?assert(meck:validate(syslogger)),
    ok = meck:wait(syslogger, log, [#{level => error,
                                           msg => {string, "Something"},
                                           meta => '_'},
        #{id => ?HANDLER_ID, ident => ?IDENT, facility => ?FACILITY}],
        timer:seconds(1)).

log_at_every_level(_Config) ->
    %% given
    [ begin
      %% when
          mongoose_logs:set_global_loglevel(LName),
          %% then
          log_at_level(LName)
      end || {_, LName} <- levels() ].

log_at_level(none) ->
    %% When log level `none` is set and we log on each possible level...
    [ logger:log(LevelName, "", []) || {_, LevelName} <- levels(), LevelName /= none ],
    %% ...then nothing ends up in the log file.
    %% (polling doesn't make sense in this one case, we have to sleep)
    Fun = fun() -> meck:history(syslogger) end,
    async_helper:wait_until(Fun, []);
log_at_level(LName) ->
    %% When current log level is L and we log on each possible level...
    [ logger:log(LevelName, "Syslogger testing") || {_, LevelName} <- levels(), LevelName /= none ],
    %% ...then for each sensible level (i.e. less or equal to current level)
    %% we get a call to syslogger.
    [ok = meck:wait(syslogger, log, [#{level => L,
                                        msg => {string, "Syslogger testing"},
                                        meta => '_'}, '_'], 100)
     || L <- levels_less_than_or_equal_to(LName)].

log_at_custom_level(_Config) ->
    %% given logging on custom log level for the helper module
    mongoose_logs:set_global_loglevel(critical),
    mongoose_logs:set_module_loglevel(mongooseim_loglevel_SUITE_helper, debug),
    %% when we log from here and from the helper module
    NotSupposedToBeLogged = "This should not be in the logs!",
    ShouldBeSuccessfullyLogged = "This MUST appear in the logs successfully",
    logger:log(debug, NotSupposedToBeLogged),
    mongooseim_loglevel_SUITE_helper:log(debug, ShouldBeSuccessfullyLogged),
    ok = meck:wait(syslogger, log, [#{level => debug,
                                      msg => {string, ShouldBeSuccessfullyLogged},
                                      meta => '_'}, '_'], 100),

    Fun = fun() -> meck:num_calls(syslogger, log, '_') end,
    async_helper:wait_until(Fun, 1).

%%
%% Helpers
%%

setup_meck(_Config) ->
    meck:new(syslogger, [no_link]),
    meck:expect(syslogger, adding_handler,
                fun(C) ->
                    {ok, C}
                end),
    meck:expect(syslogger, log,
                fun(_LogEvent, _Config) ->
                    ok
                end).

levels() ->
    [
        {-1, none},
        {0, emergency},
        {1, alert},
        {2, critical},
        {3, error},
        {4, warning},
        {5, notice},
        {6, info},
        {7, debug}
    ].

mongoose_logger_running() ->
    HandlerID = ?HANDLER_ID,
    HandlerModule = syslogger,
    HandlerConfig = #{formatter => {logger_formatter, #{single_line => true}},
                      config => {log_opts, [cons, pid, perror]},
                      ident => ?IDENT,
                      facility => ?FACILITY},
    ok = logger:add_handler(HandlerID, HandlerModule, HandlerConfig).

levels_less_than_or_equal_to(LName) ->
    {LNumber, LName} = lists:keyfind(LName, 2, levels()),
    ListOfNamesLessThanOrEqual = lists:filtermap(
        fun({LNum, Name}) when LNum =< LNumber-> {true, Name};
            (_) -> false end, levels()),
    lists:delete(none, ListOfNamesLessThanOrEqual).

unload_meck(_G) ->
    meck:unload(syslogger).
