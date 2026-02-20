%% @doc Helper module injected into MIM nodes for log_error_SUITE tests.
%% Provides functions to trigger error logs with predictable patterns.
-module(log_error_test_helper).

-export([log_error/2, log_error/3]).

%% @doc Log an error with the given 'what' atom and reason.
-spec log_error(atom(), term()) -> ok.
log_error(What, Reason) ->
    log_error(What, Reason, #{}).

%% @doc Log an error with 'what', reason, and extra fields.
-spec log_error(atom(), term(), map()) -> ok.
log_error(What, Reason, Extra) ->
    Report = maps:merge(#{what => What, reason => Reason}, Extra),
    %% Include MFA in metadata so pattern matching tests work
    Meta = #{mfa => {?MODULE, log_error, 2}},
    logger:error(Report, Meta).
