-module(mnesia_db_SUITE).
-compile([export_all, nowarn_export_all]).

-include_lib("eunit/include/eunit.hrl").

local() ->
    #{node => node()}.

all() ->
      [mnesia_wait_for_tables].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_, C) ->
    mock_mnesia(),
    logger_ct_backend:start(local()),
    C.

end_per_testcase(_, _) ->
    meck:unload(),
    logger_ct_backend:stop(local()).

mock_mnesia() ->
    meck:new(mnesia, []),
    meck:expect(mnesia, system_info, fun(local_tables) -> [test_table_fast, test_table_slow] end),
    meck:expect(mnesia, table_info, fun(_, _) -> [] end),
    meck:expect(mnesia, wait_for_tables, fun(Tables, _Interval) ->
            case meck:num_calls(mnesia, wait_for_tables, '_') > 5 of
                true -> ok;
                false -> {timeout, Tables -- [test_table_fast]}
            end
        end),
    ok.

mnesia_wait_for_tables(_Config) ->
    logger_ct_backend:capture(warning, local()),
    mongoose_internal_databases:wait_for_mnesia(),
    logger_ct_backend:stop_capture(local()),
    FilterFun = fun(_, Msg) -> re:run(Msg, "what: mnesia_wait_for_tables_progress") /= nomatch end,
    6 = length(logger_ct_backend:recv(FilterFun)).
