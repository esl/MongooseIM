-module(ejabberd_c2s_SUITE_mocks).
-export([setup/0, teardown/0]).

setup() ->
    meck:new(ejabberd_sm),
    meck:expect(ejabberd_sm, make_new_sid,
                fun() -> {erlang:system_time(microsecond), self()} end),
    meck:expect(ejabberd_sm, close_session,
                fun(Acc, _SID, _JID, _Reason) -> Acc end),
    meck:expect(ejabberd_sm, open_session, fun(_, _, _, _) -> [] end),

    meck:new(ejabberd_socket),
    meck:expect(ejabberd_socket, close,
                fun(_) -> ok end),
    meck:expect(ejabberd_socket, send, fun(_, _) -> ok end),
    meck:expect(ejabberd_socket, get_sockmod, fun(_) -> gen_tcp end),
    meck:expect(ejabberd_socket, peername,
                fun(_) -> {ok, {{127, 0, 0, 0}, 50001}}  end),
    meck:expect(ejabberd_socket, get_peer_certificate,
                fun(_) -> no_peer_cert  end),
    meck:expect(ejabberd_socket, monitor,
                fun(_) -> ok  end),
    meck:expect(ejabberd_socket, change_shaper, fun(_, _) -> ok end),
    meck:expect(ejabberd_socket, get_socket, fun(_) -> ok end),

    meck:new(cyrsasl),
    meck:expect(cyrsasl, server_new, fun(_, _, _, _, _, _) -> saslstate end),
    meck:expect(cyrsasl, server_start, fun(_, _, _, _) -> {ok, dummy_creds} end),
    meck:expect(cyrsasl, listmech, fun(_) -> [] end),

    meck:new(mongoose_credentials),
    meck:expect(mongoose_credentials, new, fun(_, _) -> ok end),
    meck:expect(mongoose_credentials, get,
                fun(dummy_creds, sasl_success_response, undefined) ->
                    undefined end),
    meck:expect(mongoose_credentials, get, fun mcred_get/2),

    meck:new(gen_hook),
    meck:expect(gen_hook, run_fold, fun hookfold/4),

    [mongoose_config:set_opt(Key, Value) || {Key, Value} <- opts()],

    meck:expect(acl, match_rule, fun(_, _, _, _) -> allow end),

    meck:new(mongoose_bin, [passthrough]),
    meck:expect(mongoose_bin, gen_from_crypto, fun() -> <<"57">> end),

    meck:new(mongoose_metrics),
    meck:expect(mongoose_metrics, update, fun (_, _, _) -> ok end),

    meck:new(gen_mod),
    meck:expect(gen_mod, is_loaded, fun (_, _) -> true end),

    meck:new(mongoose_domain_api),
    meck:expect(mongoose_domain_api, get_domain_host_type, fun get_host_type/1).


teardown() ->
    [mongoose_config:unset_opt(Key) || {Key, _Value} <- opts()],
    meck:unload().

opts() ->
    [{max_fsm_queue, 100},
     {default_server_domain, <<"localhost">>},
     {language, <<"en">>}].

mcred_get(dummy_creds, username) -> <<"cosmic_hippo">>;
mcred_get(dummy_creds, auth_module) -> auuuthmodule.

hookfold(check_bl_c2s, _, _, _) -> {ok, false};
hookfold(roster_get_versioning_feature, _, _, _) -> {ok, []};
hookfold(roster_get_subscription_lists, _, A, _) -> {ok, A};
hookfold(privacy_get_user_list, _, A, _) -> {ok, A};
hookfold(session_opening_allowed_for_user, _, _, _) -> {ok, allow};
hookfold(c2s_stream_features, _, _, _) -> {ok, []};
hookfold(xmpp_send_element, _, A, _) -> {ok, A};
hookfold(privacy_check_packet, _, _, _) -> {ok, allow}.

get_host_type(<<"localhost">>) -> {ok, <<"localhost">>};
get_host_type(_) -> {error, not_found}.
