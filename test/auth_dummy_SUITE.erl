%%==============================================================================
%% Copyright 2014 Erlang Solutions Ltd.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%==============================================================================

-module(auth_dummy_SUITE).
-compile([export_all, nowarn_export_all]).
-author('kacper.mentel@erlang-solutions.com').

-include_lib("common_test/include/ct.hrl").

-define(DOMAIN, <<"localhost">>).
-define(HOST_TYPE, <<"some host type">>).

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() -> [
    authorize,
    ejabberd_auth_interfaces,
    supports_dynamic_domains
].

init_per_suite(C) ->
    {ok, _} = application:ensure_all_started(jid),
    AuthOpts = #{methods => [dummy],
                 dummy => #{base_time => 5, variance => 10}},
    mongoose_config:set_opts(#{{auth, ?HOST_TYPE} => AuthOpts}),
    C.

end_per_suite(_C) ->
    mongoose_config:erase_opts().

%%--------------------------------------------------------------------
%% Authentication tests
%%--------------------------------------------------------------------

authorize(_Config) ->
    Creds = mongoose_credentials:new(?DOMAIN, ?HOST_TYPE, #{}),
    {ok, Creds2} = ejabberd_auth_dummy:authorize(Creds),
    ejabberd_auth_dummy = mongoose_credentials:get(Creds2, auth_module).

ejabberd_auth_interfaces(_Config) ->
    [meck:new(M, Opts) || {M, Opts} <-
        [{mongoose_domain_api, []}, {ejabberd_auth_dummy, [passthrough]},
         {mongoose_metrics, []}]],

    meck:expect(mongoose_domain_api, get_domain_host_type,
                fun(?DOMAIN) -> {ok, ?HOST_TYPE} end),
    meck:expect(mongoose_metrics, update, fun(_, _, _) -> ok end),

    Creds = mongoose_credentials:new(?DOMAIN, ?HOST_TYPE, #{}),
    {ok, Creds2} = ejabberd_auth:authorize(Creds),
    ejabberd_auth_dummy = mongoose_credentials:get(Creds2, auth_module),

    UserName = <<"any_user">>, Password = <<"any_pasword">>,
    JID = jid:make(UserName, ?DOMAIN, <<"any_resource">>),
    true = ejabberd_auth:check_password(JID, Password),
    Args1 = [?HOST_TYPE, UserName, ?DOMAIN, Password],
    1 = meck:num_calls(ejabberd_auth_dummy, check_password, Args1),

    Digest = <<"any_digest">>, DigestGen = fun(_) -> <<"">> end,
    false = ejabberd_auth:check_password(JID, Password, Digest, DigestGen),
    Args2 = [?HOST_TYPE, UserName, ?DOMAIN, Password, Digest, DigestGen],
    1 = meck:num_calls(ejabberd_auth_dummy, check_password, Args2).

supports_dynamic_domains(_) ->
    true = ejabberd_auth:does_method_support(dummy, dynamic_domains),
    false = ejabberd_auth:does_method_support(invalid_method, dynamic_domains),
    false = ejabberd_auth:does_method_support(dummy, invalid_feature).
