-module(shutdown_SUITE).

-compile([export_all, nowarn_export_all]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("exml/include/exml.hrl").

all() ->
    [{group, main}].

groups() ->
    [{main, [parallel], [shutdown]}].

init_per_suite(Config) ->
    escalus:init_per_suite(Config).

end_per_suite(Config) ->
    escalus:end_per_suite(Config).

init_per_group(_, Config) ->
    escalus:create_users(Config, escalus:get_users([geralt_s, alice])),
    Config.

end_per_group(_, Config) ->
    escalus:delete_users(Config, escalus:get_users([geralt_s, alice])),
    Config.

init_per_testcase(shutdown = TC, Config) ->
    logger_ct_backend:start(),
    escalus:init_per_testcase(TC, Config);
init_per_testcase(Name, Config) ->
    escalus:init_per_testcase(Name, Config).

end_per_testcase(shutdown = TC, Config) ->
    logger_ct_backend:stop(),
    escalus:end_per_testcase(TC, Config);
end_per_testcase(Name, Config) ->
    escalus:end_per_testcase(Name, Config).

shutdown(Config) ->
    UserSpec = escalus_users:get_userspec(Config, geralt_s),
    {ok, _Alice, _} = escalus_connection:start(UserSpec),
    logger_ct_backend:capture(error),
    ejabberd_node_utils:restart_application(mongooseim),
    logger_ct_backend:stop_capture(),
    FilterFun = fun(_, WMsg) -> re:run(WMsg, "event_not_registered") /= nomatch end,
    [] = logger_ct_backend:recv(FilterFun).
