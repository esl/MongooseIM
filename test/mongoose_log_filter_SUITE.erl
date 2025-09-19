-module(mongoose_log_filter_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([all/0, init_per_testcase/2, end_per_testcase/2]).
-export([
    format_c2s_state_with_invalid_c2s_state/1,
    format_c2s_state_with_no_c2s_keys/1
]).

all() ->
    [
        format_c2s_state_with_invalid_c2s_state,
        format_c2s_state_with_no_c2s_keys
    ].

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

format_c2s_state_with_invalid_c2s_state(_Config) ->
    % Test the backward compatibility path with invalid c2s_state data
    % This tests the try-catch block that should prevent crashes
    Event = #{
        msg => {report, #{what => test_invalid, c2s_state => <<"not_a_record">>}}
    },
    
    Result = mongoose_log_filter:format_c2s_state_filter(Event, no_state),
    
    % The result should be unchanged when c2s_state contains invalid data
    ?assertEqual(Event, Result).

format_c2s_state_with_no_c2s_keys(_Config) ->
    % Test when there are no c2s_data or c2s_state keys
    Event = #{
        msg => {report, #{what => test_no_c2s}}
    },
    
    Result = mongoose_log_filter:format_c2s_state_filter(Event, no_state),
    
    % The result should be unchanged
    ?assertEqual(Event, Result).
