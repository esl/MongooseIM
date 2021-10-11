%% It is used in the backend modules
%% Requires macro to be defined:
%% - MAIN_MODULE
%% - DEFAULT_BACKEND_MODULE

-define(CALL_METRIC, [backends, ?MAIN_MODULE, calls, ?FUNCTION_NAME]).
-define(TIME_METRIC, [backends, ?MAIN_MODULE, ?FUNCTION_NAME]).
-define(BACKEND_MOD(HostType),
        backend_module:get_backend_module(HostType, ?MAIN_MODULE,
                                          ?DEFAULT_BACKEND_MODULE)).

-define(CALL_TRACKED(HostType, Args),
    Backend = ?BACKEND_MOD(HostType),
    mongoose_metrics:update(global, ?CALL_METRIC, 1),
    {Time, Result} = timer:tc(fun() -> Backend:(?FUNCTION_NAME) Args end),
    mongoose_metrics:update(global, ?TIME_METRIC, Time),
    Result).

-define(CALL(HostType, Args),
    Backend = ?BACKEND_MOD(HostType),
    Backend:(?FUNCTION_NAME) Args).
