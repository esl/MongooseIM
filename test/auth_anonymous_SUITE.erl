-module(auth_anonymous_SUITE).
-compile([export_all, nowarn_export_all]).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("session.hrl").

all() -> [get_registered_users_pagination].

init_per_suite(Config) ->
    application:ensure_all_started(jid),
    application:ensure_all_started(exometer_core),
    mnesia:create_schema([node()]),
    mnesia:start(),

    %% Setup config for auth anonymous and sm backend
    mongoose_config:set_opts(#{
        {auth, host_type()} => #{methods => [anonymous], anonymous => #{backend => mnesia}},
        {sm_backend, host_type()} => mnesia,
        instrumentation => #{}
    }),

    mongoose_instrument:start_link(),
    gen_hook:start_link(),

    %% Initialize SM backend (this sets up mongoose_backend routing)
    ejabberd_sm_backend:init(#{}),

    %% Start auth anonymous
    ejabberd_auth_anonymous:start(host_type()),
    Config.

end_per_suite(_Config) ->
    catch ejabberd_auth_anonymous:stop(host_type()),
    catch gen_hook:stop(),
    catch mongoose_instrument:stop(),
    mongoose_config:erase_opts(),
    mnesia:stop(),
    mnesia:delete_schema([node()]),
    application:stop(exometer_core).

get_registered_users_pagination(_C) ->
    Domain = <<"anon.com">>,
    Users = [<<"u1">>, <<"u2">>, <<"u3">>, <<"u4">>, <<"u5">>],

    %% Register sessions directly in mnesia backend
    lists:foreach(fun(U) ->
        SID = {now(), self()},
        Session = #session{sid = SID, usr = {U, Domain, <<"res">>}, us = {U, Domain}, priority = 1, info = []},
        ejabberd_sm_mnesia:set_session(U, Domain, <<"res">>, Session)
    end, Users),

    %% Verify pagination
    %% Limit
    ?assertEqual([{<<"u1">>, Domain}, {<<"u2">>, Domain}],
                 ejabberd_auth_anonymous:get_registered_users(host_type(), Domain, #{limit => 2})),

    %% Offset
    ?assertEqual([{<<"u3">>, Domain}, {<<"u4">>, Domain}, {<<"u5">>, Domain}],
                 ejabberd_auth_anonymous:get_registered_users(host_type(), Domain, #{offset => 2})),

    %% Limit + Offset
    ?assertEqual([{<<"u2">>, Domain}, {<<"u3">>, Domain}],
                 ejabberd_auth_anonymous:get_registered_users(host_type(), Domain, #{limit => 2, offset => 1})),

    %% Cleanup
    lists:foreach(fun(U) ->
        %% We can't easily delete by user without SID in mnesia backend API,
        %% but since we are in a test suite, we can just leave them or clear table if needed.
        ok
    end, Users).

host_type() -> <<"test_host_type">>.
