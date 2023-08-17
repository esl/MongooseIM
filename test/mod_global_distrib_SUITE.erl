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

-module(mod_global_distrib_SUITE).
-compile([export_all, nowarn_export_all]).
-author('piotr.nosek@erlang-solutions.com').

-include_lib("exml/include/exml.hrl").
-include("mongoose.hrl").
-include("jlib.hrl").

-import(config_parser_helper, [mod_config/2, config/2]).

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [{group, hook_handlers}].

groups() ->
    [
     {hook_handlers, [], hook_handlers_tests()}
    ].

hook_handlers_tests() ->
    [
        missing_struct_in_message_from_user,
        missing_struct_in_message_from_component
    ].

suite() ->
    [].

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(jid),
    {ok, _} = application:ensure_all_started(cache_tab),
    Config.

end_per_suite(Config) ->
    Config.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

init_per_testcase(_CaseName, Config) ->
    set_meck(),
    mongoose_config:set_opts(opts()),
    mongoose_domain_sup:start_link(),
    mim_ct_sup:start_link(ejabberd_sup),
    mongooseim_helper:start_link_loaded_hooks(),
    mongoose_modules:start(),
    Config.

end_per_testcase(_CaseName, Config) ->
    mongoose_modules:stop(),
    mongoose_config:erase_opts(),
    unset_meck(),
    Config.

opts() ->
    maps:from_list([{hosts, hosts()},
                    {host_types, []},
                    {all_metrics_are_global, false} |
                    [{{modules, HostType}, modules(HostType)} || HostType <- hosts()]]).

hosts() ->
    [global_host(), local_host()].

modules(HostType) ->
    gen_mod_deps:resolve_deps(HostType, #{mod_global_distrib => module_opts()}).

module_opts() ->
    mod_config(mod_global_distrib, #{global_host => global_host(),
                                     local_host => local_host(),
                                     connections => connection_opts()}).

connection_opts() ->
    config([modules, mod_global_distrib, connections],
           #{endpoints => [],
             resolved_endpoints => [],
             advertised_endpoints => []}).

%%--------------------------------------------------------------------
%% Hook handlers tests
%%--------------------------------------------------------------------


%% missing_struct_ tests verify the behaviour of packet_to_component handler,
%% which is supposed to update the mapping of the sender in Redis and cache.
%% In case of routing between nodes in single cluster AND routers being reordered
%% with component routers at the beginning of the chain, this hook must not fail
%% despite lack of global_distrib structure in Acc.
missing_struct_in_message_from_user(_Config) ->
    From = jid:make(<<"user">>, global_host(), <<"resource">>),
    {Acc, _To} = fake_acc_to_component(From),
    % The handler must not crash and return unchanged Acc
    {ok, Acc} = mod_global_distrib_mapping:packet_to_component(Acc, #{from => From}, #{}).

%% Update logic has two separate paths: when a packet is sent by a user or by another
%% component. This test covers the latter.
missing_struct_in_message_from_component(_Config) ->
    From = jid:make(<<"">>, <<"from_service.", (global_host())/binary>>, <<"">>),
    {Acc, _To} = fake_acc_to_component(From),
    % The handler must not crash and return unchanged Acc
    {ok, Acc} = mod_global_distrib_mapping:packet_to_component(Acc, #{from => From}, #{}).

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

global_host() ->
    <<"localhost">>.

local_host() ->
    <<"localhost.bis">>.

-spec fake_acc_to_component(From :: jid:jid()) -> {Acc :: mongoose_acc:t(), To :: jid:jid()}.
fake_acc_to_component(From) ->
    To = jid:make(<<"">>, <<"to_service.localhost">>, <<"">>),
    FromBin = jid:to_binary(From),
    ToBin = jid:to_binary(To),
    BodyEl = #xmlel{
                name = <<"body">>,
                children = [#xmlcdata{ content = <<"hooks test">> }]
               },
    Packet = #xmlel{
                name = <<"message">>,
                attrs = [{<<"from">>, FromBin}, {<<"to">>, ToBin}, {<<"type">>, <<"chat">>}],
                children = [BodyEl]
               },
    {mongoose_acc:new(#{ location => ?LOCATION,
                         lserver => From#jid.lserver,
                         host_type => From#jid.lserver,
                         element => Packet }), To}.

%%--------------------------------------------------------------------
%% Meck
%%--------------------------------------------------------------------

set_meck() ->
    meck:new(mongoose_metrics, [stub_all]),
    meck:new(mod_global_distrib_mapping_backend, [stub_all]),
    %% Simulate missing entries and inserts into Redis
    meck:expect(mod_global_distrib_mapping_backend, get_session, fun(_) -> error end),
    meck:expect(mod_global_distrib_mapping_backend, get_domain, fun(_) -> error end).

unset_meck() ->
    meck:unload(mod_global_distrib_mapping_backend),
    meck:unload(mongoose_metrics).
