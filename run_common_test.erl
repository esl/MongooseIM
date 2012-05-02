-module(run_common_test).
-export([ct/0]).

-define(CT_DIR, filename:join([".", "tests"])).
-define(CT_REPORT, filename:join([".", "ct_report"])).
-define(CT_CONFIG, "test.config").

ct() ->
    Result = ct:run_test([{config, [?CT_CONFIG]},
                          {dir, ?CT_DIR},
                          {logdir, ?CT_REPORT}]),
    case Result of
        {error, Reason} ->
            throw({ct_error, Reason});
        _ ->
            ok
    end,
    init:stop(0).
