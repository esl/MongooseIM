%%%===================================================================
%%% @copyright (C) 2011, Erlang Solutions Ltd.
%%% @doc Suite for testing mod_offline* modules
%%% @end
%%%===================================================================

-module(offline_stub_SUITE).
-compile(export_all).

%%%===================================================================
%%% Suite configuration
%%%===================================================================

all() ->
    [with_mod_offline_stub,
     without_mod_offline_stub].

suite() ->
    escalus:suite().

%%%===================================================================
%%% Init & teardown
%%%===================================================================

init_per_suite(C) -> escalus:init_per_suite(C).
end_per_suite(C) -> escalus_fresh:clean(), escalus:end_per_suite(C).

init_per_testcase(Name, C) ->
    HostType = domain_helper:host_type(),
    Config = dynamic_modules:save_modules(HostType, C),
    dynamic_modules:ensure_stopped(HostType, [mod_offline]),
    case Name of
        with_mod_offline_stub ->
            dynamic_modules:ensure_modules(HostType, [{mod_offline_stub, []}]);
        _ -> ok
    end,
    escalus:init_per_testcase(Name, Config).

end_per_testcase(Name, C) ->
    HostType = domain_helper:host_type(),
    dynamic_modules:restore_modules(HostType, C),
    escalus:end_per_testcase(Name, C).

%%%===================================================================
%%% offline tests
%%%===================================================================

with_mod_offline_stub(Config) ->
    Story =
        fun(FreshConfig, Alice, Bob) ->
            logout(FreshConfig, Bob),
            escalus:send(Alice, escalus_stanza:chat_to(Bob, <<"msgtxt">>)),
            [] = escalus:wait_for_stanzas(Alice, 1, 1000)
        end,
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 1}], Story).

without_mod_offline_stub(Config) ->
    Story =
        fun(FreshConfig, Alice, Bob) ->
            logout(FreshConfig, Bob),
            escalus:send(Alice, escalus_stanza:chat_to(Bob, <<"msgtxt">>)),
            Err = escalus:wait_for_stanza(Alice),
            escalus:assert(is_error, [<<"cancel">>, <<"service-unavailable">>], Err)
        end,
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 1}], Story).

%%%===================================================================
%%% Helpers
%%%===================================================================
logout(Config, User) ->
    mongoose_helper:logout_user(Config, User).
