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
-module(connect_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("escalus/include/escalus.hrl").
-include_lib("escalus/include/escalus_xmlns.hrl").
-include_lib("exml/include/exml.hrl").
-include_lib("exml/include/exml_stream.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

-define(SECURE_USER, secure_joe).
-define(CERT_FILE, "priv/ssl/fake_server.pem").
-define(TLS_VERSIONS, ["tlsv1", "tlsv1.1", "tlsv1.2"]).
-define(NS_AUTH, <<"jabber:iq:auth">>).

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    AuthMods = mongoose_helper:auth_modules(),
    case lists:member(ejabberd_auth_external, AuthMods) of
        true ->
            {skip, "Conf reload doesn't work correctly with sample external auth"};
        _ ->
            [{group, negative},
             {group, pre_xmpp_1_0},
             {group, starttls},
             {group, tls}]
    end.

groups() ->
    [{negative, [], [invalid_host,
                     invalid_stream_namespace]},
     {pre_xmpp_1_0, [], [pre_xmpp_1_0_stream]},
     {starttls, test_cases()},
     {tls, generate_tls_vsn_tests()}].

test_cases() ->
    generate_tls_vsn_tests() ++
    [should_fail_with_sslv3,
     should_fail_to_authenticate_without_starttls].

suite() ->
    escalus:suite().

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    Config0 = escalus:init_per_suite([{escalus_user_db, {module, escalus_ejabberd, []}} | Config]),
    Config1 = ejabberd_node_utils:init(Config0),
    ejabberd_node_utils:backup_config_file(Config1),
    assert_cert_file_exists(),
    escalus:create_users(Config1, {by_name, [?SECURE_USER, alice]}).

end_per_suite(Config) ->
    escalus:delete_users(Config, {by_name, [?SECURE_USER, alice]}),
    restore_ejabberd_node(Config),
    escalus:end_per_suite(Config).

init_per_group(starttls, Config) ->
    config_ejabberd_node_tls(Config, fun mk_value_for_starttls_required_config_pattern/0),
    ejabberd_node_utils:restart_application(ejabberd),
    Config;
init_per_group(tls, Config) ->
    config_ejabberd_node_tls(Config, fun mk_value_for_tls_config_pattern/0),
    ejabberd_node_utils:restart_application(ejabberd),
    Users = proplists:get_value(escalus_users, Config, []),
    JoeSpec = lists:keydelete(starttls, 1, proplists:get_value(?SECURE_USER, Users)),
    JoeSpec2 = {?SECURE_USER, lists:keystore(ssl, 1, JoeSpec, {ssl, true})},
    NewUsers = lists:keystore(?SECURE_USER, 1, Users, JoeSpec2),
    lists:keystore(escalus_users, 1, Config, {escalus_users, NewUsers});
init_per_group(_, Config) ->
    Config.


end_per_group(_, Config) ->
    Config.

generate_tls_vsn_tests() ->
    [list_to_existing_atom("should_pass_with_" ++ VSN)
     || VSN <- ?TLS_VERSIONS].

%%--------------------------------------------------------------------
%% Tests
%%--------------------------------------------------------------------

invalid_host(Config) ->
    %% given
    Spec = escalus_users:get_userspec(Config, alice),
    %% when
    [Start, Error, End] = connect_to_invalid_host(Spec),
    %% then
    %% See RFC 6120 4.9.1.3 (http://xmpp.org/rfcs/rfc6120.html#streams-error-rules-host).
    %% Stream start from the server is required in this case.
    escalus:assert(is_stream_start, Start),
    escalus:assert(is_stream_error, [<<"host-unknown">>, <<>>], Error),
    escalus:assert(is_stream_end, End).

invalid_stream_namespace(Config) ->
    %% given
    Spec = escalus_users:get_userspec(Config, alice),
    %% when
    [Start, Error, End] = connect_with_invalid_stream_namespace(Spec),
    %% then
    escalus:assert(is_stream_start, Start),
    escalus:assert(is_stream_error, [<<"invalid-namespace">>, <<>>], Error),
    escalus:assert(is_stream_end, End).

pre_xmpp_1_0_stream(Config) ->
    %% given
    Spec = escalus_users:get_userspec(Config, alice),
    Steps = [
             %% when
             {?MODULE, start_stream_pre_xmpp_1_0},
             {?MODULE, failed_legacy_auth}
            ],
    %% ok, now do the plan from above
    {ok, Conn, _, _} = escalus_connection:start(Spec, Steps),
    escalus_connection:stop(Conn).

should_fail_with_sslv3(Config) ->
    %% GIVEN
    UserSpec0 = escalus_users:get_userspec(Config, ?SECURE_USER),
    UserSpec1 = set_secure_connection_protocol(UserSpec0, sslv3),
    %% WHEN
    try escalus_connection:start(UserSpec1) of
    %% THEN
        _ ->
            error(client_connected_using_sslv3)
    catch
        error:closed ->
            ok
    end.

should_pass_with_tlsv1(Config) ->
    should_pass_with_tls(tlsv1, Config).

'should_pass_with_tlsv1.1'(Config) ->
    should_pass_with_tls('tlsv1.1', Config).

'should_pass_with_tlsv1.2'(Config) ->
    should_pass_with_tls('tlsv1.2', Config).


should_pass_with_tls(Version, Config)->
    UserSpec0 = escalus_users:get_userspec(Config, ?SECURE_USER),
    UserSpec1 = set_secure_connection_protocol(UserSpec0, Version),

    %% WHEN
    Result = escalus_connection:start(UserSpec1),

    %% THEN
    ?assertMatch({ok, _, _, _}, Result).

should_fail_to_authenticate_without_starttls(Config) ->
    %% GIVEN
    UserSpec = escalus_users:get_userspec(Config, ?SECURE_USER),
    {Conn, Props, Features} = start_stream_with_compression(UserSpec),

    %% WHEN
    try escalus_session:authenticate(Conn, Props, Features) of
    %% THEN
        _ ->
            error(authentication_without_tls_suceeded)
    catch
        throw:{auth_failed, User, AuthReply} ->
            ?assertEqual(atom_to_binary(?SECURE_USER, utf8), User),
            escalus:assert(is_stream_error, [<<"policy-violation">>,
                                             <<"Use of STARTTLS required">>],
                           AuthReply)
    end.

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------

restore_ejabberd_node(Config) ->
    ejabberd_node_utils:restore_config_file(Config),
    ejabberd_node_utils:restart_application(ejabberd).

assert_cert_file_exists() ->
    ejabberd_node_utils:file_exists(?CERT_FILE) orelse
        ct:fail("cert file ~s not exists", [?CERT_FILE]).

config_ejabberd_node_tls(Config, Fun) ->
    ejabberd_node_utils:modify_config_file([Fun()],
                                           Config).

mk_value_for_tls_config_pattern() ->
    {tls_config, "{certfile, \"" ++ ?CERT_FILE ++ "\"}, tls,"}.

mk_value_for_starttls_required_config_pattern() ->
    {tls_config, "{certfile, \"" ++ ?CERT_FILE ++ "\"}, starttls_required,"}.

set_secure_connection_protocol(UserSpec, Version) ->
    [{ssl_opts, [{versions, [Version]}]} | UserSpec].

start_stream_with_compression(UserSpec) ->
    ConnetctionSteps = [start_stream, stream_features, maybe_use_compression],
    {ok, Conn, Props, Features} = escalus_connection:start(UserSpec,
                                                            ConnetctionSteps),
    {Conn, Props, Features}.

connect_to_invalid_host(Spec) ->
    {ok, Conn, _, _} = escalus_connection:start(Spec, [{?MODULE, connect_to_invalid_host}]),
    escalus:wait_for_stanzas(Conn, 3).

connect_to_invalid_host(Conn, UnusedProps, UnusedFeatures) ->
    escalus:send(Conn, escalus_stanza:stream_start(<<"hopefullynonexistentdomain">>,
                                                   ?NS_JABBER_CLIENT)),
    {Conn, UnusedProps, UnusedFeatures}.

connect_with_invalid_stream_namespace(Spec) ->
    F = fun (Conn, UnusedProps, UnusedFeatures) ->
                Start = stream_start_invalid_stream_ns(escalus_users:get_server([], Spec)),
                escalus:send(Conn, Start),
                {Conn, UnusedProps, UnusedFeatures}
        end,
    {ok, Conn, _, _} = escalus_connection:start(Spec, [F]),
    escalus:wait_for_stanzas(Conn, 3).

stream_start_invalid_stream_ns(To) ->
    stream_start(lists:keystore(stream_ns, 1, default_context(To),
                                {stream_ns, <<"obviously-invalid-namespace">>})).

stream_start_pre_xmpp_1_0(To) ->
    stream_start(lists:keystore(version, 1, default_context(To), {version, <<>>})).

default_context(To) ->
    [{version, <<"version='1.0'">>},
     {to, To},
     {stream_ns, ?NS_XMPP}].

stream_start(Context) ->
    %% Be careful! The closing slash here is a hack to enable implementation of from_template/2
    %% to parse the snippet properly. In standard XMPP <stream:stream> is just opening of an XML
    %% element, NOT A SELF CLOSING element.
    T = <<"<stream:stream {{version}} xml:lang='en' xmlns='jabber:client' "
          "               to='{{to}}' "
          "               xmlns:stream='{{stream_ns}}' />">>,
    %% So we rewrap the parsed contents from #xmlel{} to #xmlstreamstart{} here.
    #xmlel{name = Name, attrs = Attrs, children = []} = escalus_stanza:from_template(T, Context),
    #xmlstreamstart{name = Name, attrs = Attrs}.

username(Username) when is_binary(Username) ->
    #xmlel{name = <<"username">>,
           children = [exml:escape_cdata(Username)]}.

digest(Digest) when is_binary(Digest) ->
    #xmlel{name = <<"digest">>,
           children = [exml:escape_cdata(Digest)]}.

generate_digest(SID, Password) ->
    %% compute digest
    D = binary_to_list(SID) ++ binary_to_list(Password),
    sha(D).

digit_to_xchar(D) when (D >= 0) and (D < 10) ->
    D + 48;
digit_to_xchar(D) ->
    D + 87.

sha(Text) ->
    Bin = crypto:hash(sha256, Text),
    lists:reverse(ints_to_rxstr(binary_to_list(Bin), [])).

ints_to_rxstr([], Res) ->
    Res;
ints_to_rxstr([N | Ns], Res) ->
    ints_to_rxstr(Ns, [digit_to_xchar(N rem 16),
                       digit_to_xchar(N div 16) | Res]).

start_stream_pre_xmpp_1_0(Conn, Props, UnusedFeatures) ->
    escalus:send(Conn, stream_start_pre_xmpp_1_0(escalus_users:get_server([], Props))),
    #xmlstreamstart{attrs = StreamAttrs} = StreamStart = escalus:wait_for_stanza(Conn),
    escalus:assert(is_stream_start, StreamStart),
    {<<"id">>, StreamID} = lists:keyfind(<<"id">>, 1, StreamAttrs),
    {Conn, [{stream_id, StreamID} | Props], UnusedFeatures}.

failed_legacy_auth(Conn, Props, UnusedFeatures) ->
    {stream_id, StreamID} = lists:keyfind(stream_id, 1, Props),
    %ct:pal("id: ~p", [StreamID]),
    [Username, _, Password] = escalus_users:get_usp([], Props),
    Digest = list_to_binary(generate_digest(StreamID, Password)),
    AuthReq = escalus_stanza:iq_set(?NS_AUTH, [username(Username), digest(Digest)]),
    escalus:send(Conn, AuthReq),
    %ct:pal("la req: ~p", [AuthReq]),
    Response = escalus:wait_for_stanza(Conn),
    %ct:pal("la response: ~p", [Response]),
    %% This is the success case - we want to assert the error case.
    %escalus:assert(is_iq_result, Response),
    escalus:assert(is_error, [<<"modify">>, <<"not-acceptable">>], Response),
    {Conn, Props, UnusedFeatures}.
