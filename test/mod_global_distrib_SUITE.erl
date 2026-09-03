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
    [{group, hook_handlers},
     {group, acc_transfer}].

groups() ->
    [
     {hook_handlers, [], hook_handlers_tests()},
     {acc_transfer, [], acc_transfer_tests()}
    ].

hook_handlers_tests() ->
    [
        missing_struct_in_message_from_user,
        missing_struct_in_message_from_component
    ].

acc_transfer_tests() ->
    [
        prepared_acc_is_safe_decodable_on_a_foreign_node,
        unprepared_acc_is_not_safe_decodable_on_a_foreign_node,
        restore_reinstates_node_local_fields,
        permanent_metadata_survives_transfer,
        undecodable_payload_does_not_kill_the_worker
    ].

suite() ->
    [].

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(jid),
    {ok, _} = application:ensure_all_started(cache_tab),
    mongoose_config:set_opts(opts()),
    async_helper:start(Config, [{mongoose_instrument, start_link, []},
                                {mongooseim_helper, start_link_loaded_hooks, []}]).

end_per_suite(Config) ->
    mongoose_config:erase_opts(),
    async_helper:stop_all(Config).

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

init_per_testcase(_CaseName, Config) ->
    set_meck(),
    mongoose_domain_sup:start_link(),
    mim_ct_sup:start_link(ejabberd_sup),
    mongoose_modules:start(),
    Config.

end_per_testcase(_CaseName, Config) ->
    mongoose_modules:stop(),
    unset_meck(),
    Config.

opts() ->
    maps:from_list([{hosts, hosts()},
                    {host_types, []},
                    {instrumentation, config_parser_helper:default_config([instrumentation])} |
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
%% Acc transfer tests
%%--------------------------------------------------------------------

%% The property that actually matters. A local round trip would pass trivially,
%% because the local node's own name atom always exists; only a node that has
%% never seen the sender exercises the [safe] restriction on external pids/refs.
prepared_acc_is_safe_decodable_on_a_foreign_node(_Config) ->
    Bin = term_to_binary(mod_global_distrib_utils:prepare_acc_for_transfer(fake_acc())),
    {ok, DecodedAcc} = safe_decode_on_foreign_node(Bin),
    #{mongoose_acc := true, lserver := _} = DecodedAcc,
    false = maps:is_key(ref, DecodedAcc),
    false = maps:is_key(origin_pid, DecodedAcc).

%% Negative control: without the preparation step the very same payload is
%% rejected, which is why binary_to_term/2 [safe] cannot simply be switched on.
unprepared_acc_is_not_safe_decodable_on_a_foreign_node(_Config) ->
    Bin = term_to_binary(fake_acc()),
    {error, badarg} = safe_decode_on_foreign_node(Bin).

restore_reinstates_node_local_fields(_Config) ->
    Acc = fake_acc(),
    Prepared = mod_global_distrib_utils:prepare_acc_for_transfer(Acc),
    Restored = mod_global_distrib_utils:restore_acc_after_transfer(Prepared),
    #{ref := Ref, origin_pid := Pid, stanza := #{ref := StanzaRef}} = Restored,
    true = is_reference(Ref),
    true = is_pid(Pid),
    true = is_reference(StanzaRef),
    Self = self(),
    Self = Pid,
    %% the payload itself must come through untouched
    #{lserver := LServer, host_type := HostType, stanza := #{element := El}} = Acc,
    #{lserver := LServer, host_type := HostType, stanza := #{element := El}} = Restored.

%% Guards the assumption that mongoose_acc:strip/1 is safe to use here: the
%% routing TTL is stored with set_permanent/4 and must survive the round trip.
permanent_metadata_survives_transfer(_Config) ->
    Acc = mod_global_distrib:put_metadata(fake_acc(), ttl, 5),
    Prepared = mod_global_distrib_utils:prepare_acc_for_transfer(Acc),
    Restored = mod_global_distrib_utils:restore_acc_after_transfer(
                 binary_to_term(term_to_binary(Prepared))),
    {ok, 5} = mod_global_distrib:find_metadata(Restored, ttl).

undecodable_payload_does_not_kill_the_worker(_Config) ->
    Stamp = erlang:monotonic_time(),
    Garbage = <<"certainly not an erlang term">>,
    {noreply, state, _} = mod_global_distrib_worker:handle_cast(
                            {data, global_host(), 0, Stamp, Garbage}, state).

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

%% Decodes Bin with [safe] on a separate node that has never been connected to
%% this one, so this node's name atom does not exist there. The peer is driven
%% over standard_io precisely to avoid the Erlang distribution handshake, which
%% would create that atom on the peer and mask the failure we are testing for.
safe_decode_on_foreign_node(Bin) ->
    {ok, Peer, _Node} = peer:start(#{name => peer:random_name(),
                                     connection => standard_io,
                                     args => ["-pa" | code:get_path()]}),
    try
        %% A real peer runs the same release, so give it the same atoms. ?MODULE is
        %% in the list because the acc's origin_location names this suite, standing
        %% in for the MongooseIM module that would appear there in production.
        ok = peer:call(Peer, code, ensure_modules_loaded,
                       [[mongoose_acc, mongoose_c2s_acc, jid, exml, mod_global_distrib,
                         ?MODULE]]),
        try peer:call(Peer, erlang, binary_to_term, [Bin, [safe]]) of
            Decoded -> {ok, Decoded}
        catch
            error:{exception, badarg, _} -> {error, badarg};
            error:badarg -> {error, badarg}
        end
    after
        peer:stop(Peer)
    end.

fake_acc() ->
    From = jid:make(<<"user">>, global_host(), <<"resource">>),
    {Acc, _To} = fake_acc_to_component(From),
    Acc.

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
                attrs = #{<<"from">> => FromBin,
                          <<"to">> => ToBin,
                          <<"type">> => <<"chat">>},
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
    meck:new(mod_global_distrib_mapping_backend, [stub_all]),
    %% Simulate missing entries and inserts into Redis
    meck:expect(mod_global_distrib_mapping_backend, get_session, fun(_) -> error end),
    meck:expect(mod_global_distrib_mapping_backend, get_domain, fun(_) -> error end).

unset_meck() ->
    meck:unload(mod_global_distrib_mapping_backend).
