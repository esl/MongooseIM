-module(syslogger_SUITE).

-export([all/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_testcase/2,
         end_per_testcase/2]).

-export([syslogger_is_used_to_log/1]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(HANDLER_ID, sys_log).
-define(IDENT, "mongooseim").
-define(FACILITY, local0).

all() ->
    [syslogger_is_used_to_log
    ].

init_per_suite(Config) ->
    LoggerConfig = logger:get_primary_config(),
    case logger:compare_levels(info, maps:get(level, LoggerConfig)) of
        lt -> logger:set_primary_config(level, info);
        _ -> ok
    end,

    setup_meck(),
    ok = mongoose_logger_running(),

    [{logger_primary_config, LoggerConfig} | Config].

end_per_suite(Config) ->
    logger:remove_handler(?HANDLER_ID),
    logger:set_primary_config(?config(logger_primary_config, Config)),

    unload_meck(),
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
    logger:log(error, "Something for syslogger"),

    ?assert(meck:validate(syslogger)),
    ok = meck:wait(syslogger, log, [#{level => error,
                                      msg => {string, "Something for syslogger"},
                                      meta => '_'},
                                    #{id => ?HANDLER_ID, ident => ?IDENT, facility => ?FACILITY}],
                   timer:seconds(1)).

%%
%% Helpers
%%

setup_meck() ->
    meck:new(syslogger, [no_link]),
    meck:expect(syslogger, adding_handler,
                fun(C) ->
                    {ok, C}
                end),
    meck:expect(syslogger, log,
                fun(_LogEvent, _Config) ->
                    ok
                end).

mongoose_logger_running() ->
    HandlerID = ?HANDLER_ID,
    HandlerModule = syslogger,
    HandlerConfig = #{level => info,
                      formatter => {logger_formatter, #{single_line => true}},
                      config => {log_opts, [cons, pid, perror]},
                      ident => ?IDENT,
                      facility => ?FACILITY},
    ok = logger:add_handler(HandlerID, HandlerModule, HandlerConfig).

unload_meck() ->
    meck:unload(syslogger).
