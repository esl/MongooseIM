%% It is used in the backend modules
%% Requires ?MAIN_MODULE macro to be defined

-define(CALL_METRIC, [backends, ?MAIN_MODULE, calls, ?FUNCTION_NAME]).
-define(TIME_METRIC, [backends, ?MAIN_MODULE, ?FUNCTION_NAME]).

-define(CALL_TRACKED(HostType, Args),
    Backend = gen_mod:get_backend_module(HostType, ?MAIN_MODULE),
    mongoose_metrics:update(global, ?CALL_METRIC, 1),
    {Time, Result} = timer:tc(fun() -> Backend:(?FUNCTION_NAME) Args end),
    mongoose_metrics:update(global, ?TIME_METRIC, Time),
    Result).

-define(CALL(HostType, Args),
    Backend = gen_mod:get_backend_module(HostType, ?MAIN_MODULE),
    Backend:(?FUNCTION_NAME) Args).
