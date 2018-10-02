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
-compile(export_all).
-author('piotr.nosek@erlang-solutions.com').

-include_lib("common_test/include/ct.hrl").
-include_lib("exml/include/exml.hrl").
-include("mongoose.hrl").
-include("jlib.hrl").

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
    {ok, _} = application:ensure_all_started(stringprep),
    Config.

end_per_suite(Config) ->
    Config.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

init_per_testcase(_CaseName, Config) ->
    set_meck(),
    fake_start(),
    Config.

end_per_testcase(_CaseName, Config) ->
    fake_stop(),
    unset_meck(),
    Config.

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
    {Acc, To} = fake_acc_to_component(From),
    % The handler must not crash and return unchanged Acc
    Acc = mod_global_distrib_mapping:packet_to_component(Acc, From, To).
        
%% Update logic has two separate paths: when a packet is sent by a user or by another
%% component. This test covers the latter.
missing_struct_in_message_from_component(_Config) ->
    From = jid:make(<<"">>, <<"from_service.", (global_host())/binary>>, <<"">>),
    {Acc, To} = fake_acc_to_component(From),
    % The handler must not crash and return unchanged Acc
    Acc = mod_global_distrib_mapping:packet_to_component(Acc, From, To).

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
                         element => Packet }), To}.

%%--------------------------------------------------------------------
%% Meck & fake zone
%%--------------------------------------------------------------------

set_meck() ->
    meck:new(ejabberd_config, []),
    meck:expect(ejabberd_config, get_global_option,
                fun(hosts) -> [global_host(), local_host()] end),
    meck:expect(ejabberd_config, get_local_option, fun(_) -> undefined end),

    meck:new(mongoose_metrics, []),
    meck:expect(mongoose_metrics, update, fun(_, _, _) -> ok end),

    meck:new(mod_global_distrib_mapping_backend, [non_strict]),
    %% Simulate missing entries and inserts into Redis
    meck:expect(mod_global_distrib_mapping_backend, get_session, fun(_) -> error end),
    meck:expect(mod_global_distrib_mapping_backend, put_session, fun(_) -> ok end),
    meck:expect(mod_global_distrib_mapping_backend, get_domain, fun(_) -> error end),
    meck:expect(mod_global_distrib_mapping_backend, put_domain, fun(_) -> ok end).

unset_meck() ->
    meck:unload(ejabberd_config),
    meck:unload(mod_global_distrib_mapping_backend),
    meck:unload(mongoose_metrics).

%% These functions fake modules startup in order to populate ETS with options.
%% It is impossible to meck 'ets' modules and mecking mod_global_distrib_utils:opt/2
%% does not fully work as well because maybe_update_mapping/2 function does local call
%% to opt/2, bypassing meck.
fake_start() ->
    lists:foreach(
      fun(Mod) ->
              mod_global_distrib_utils:start(Mod, global_host(),
                                             common_fake_opts(), fun() -> ok end)
      end, fake_start_stop_list()).

fake_stop() ->
    lists:foreach(
      fun(Mod) ->
              mod_global_distrib_utils:stop(Mod, global_host(), fun() -> ok end)
      end, fake_start_stop_list()).

fake_start_stop_list() ->
    [mod_global_distrib, mod_global_distrib_mapping].

common_fake_opts() ->
    [
     {global_host, global_host()},
     {local_host, local_host()}
    ].

fake_tabs_info() ->
    [ ets:info(T) || T <- fake_start_stop_list() ].

