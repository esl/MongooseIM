-module(caps_helper).

-include_lib("escalus/include/escalus_xmlns.hrl").
-include_lib("exml/include/exml.hrl").
-include_lib("stdlib/include/assert.hrl").

-compile([export_all, nowarn_export_all]).

-import(distributed_helper, [mim/0, rpc/4]).

-type feature() :: binary().
-type host_type() :: binary().
-type caps() :: exml:element().
-type version() :: v1 | v2.

check_backend() ->
    case ct_helper:get_internal_database() of
        cets -> ok;
        mnesia -> {skip, "mod_caps has no mnesia backend"}
    end.

%% Stanza flow

-spec enable_new_caps(escalus:client(), [feature()], version()) -> caps().
enable_new_caps(Client, Features, Version) ->
    Caps = caps(Features, Version),
    send_presence_with_caps(Client, Caps),
    handle_requested_caps(Client, Caps, Features),
    receive_presence_with_caps(Client, Client, Caps),
    Caps.

-spec enable_caps(escalus:client(), [binary()], version()) -> caps().
enable_caps(Client, Features, Version) ->
    Caps = caps(Features, Version),
    send_presence_with_caps(Client, Caps),
    receive_presence_with_caps(Client, Client, Caps),
    Caps.

-spec send_presence_with_caps(escalus:client(), caps()) -> ok.
send_presence_with_caps(Client, Caps) ->
    Presence = escalus_stanza:presence(~"available", [Caps]),
    escalus:send(Client, Presence).

-spec receive_presence_with_caps(escalus:client(), escalus:client(), caps()) -> ok.
receive_presence_with_caps(Client, Sender, Caps) ->
    PresenceNotification = escalus:wait_for_stanza(Client),
    escalus:assert(is_presence, PresenceNotification),
    escalus:assert(is_stanza_from, [Sender], PresenceNotification),
    ?assertEqual(Caps, exml_query:subelement(PresenceNotification, ~"c")).

-spec handle_requested_caps(escalus:client(), caps(), [feature()]) -> ok.
handle_requested_caps(Client, Caps, Features) ->
    DiscoRequest = receive_caps_request(Client, Caps),
    send_caps_disco_result(Client, DiscoRequest, Features).

-spec receive_caps_request(escalus:client(), caps()) -> exml:element().
receive_caps_request(Client, Caps) ->
    DiscoRequest = escalus:wait_for_stanza(Client),
    escalus:assert(is_iq_with_ns, [?NS_DISCO_INFO], DiscoRequest),
    #xmlel{children = [#xmlel{attrs = #{~"node" := QueryNode}}]} = DiscoRequest,
    ?assertEqual(escalus_stanza:caps_to_node(Caps), QueryNode),
    DiscoRequest.

-spec send_caps_disco_result(escalus:client(), exml:element(), [feature()]) -> ok.
send_caps_disco_result(Client, DiscoRequest, Features) ->
    QueryEl = escalus_stanza:query_el(?NS_DISCO_INFO, feature_elems(Features)),
    DiscoResult = escalus_stanza:iq_result(DiscoRequest, [QueryEl]),
    escalus:send(Client, DiscoResult).

%% Assertions

-spec assert_caps(host_type(), escalus:client(), [feature()]) -> ok.
assert_caps(HostType, Client, Features) ->
    ExpectedFeatures = all_features(Features),
    ?assertEqual(ExpectedFeatures, get_client_features(HostType, Client)).

-spec assert_no_caps(host_type(), escalus:client()) -> ok.
assert_no_caps(HostType, Client) ->
    ?assertEqual([], get_client_features(HostType, Client)).

-spec wait_for_caps(host_type(), escalus:client(), [feature()]) -> ok.
wait_for_caps(HostType, Client, Features) ->
    ExpectedFeatures = all_features(Features),
    wait_helper:wait_until(fun() -> get_client_features(HostType, Client) end,
                           ExpectedFeatures, #{sleep_time => 200}),
    ok.

-spec wait_for_no_caps(host_type(), escalus:client()) -> ok.
wait_for_no_caps(HostType, Client) ->
    wait_helper:wait_until(fun() -> get_client_features(HostType, Client) end,
                           [], #{sleep_time => 200}),
    ok.

%% XML elements

feature_elems(Features) ->
    [escalus_stanza:identity(~"client", ~"pc", ~"Psi") |
     lists:map(fun escalus_stanza:feature/1, all_features(Features))].

-spec caps([feature()], version()) -> caps().
caps(Features, Version) ->
    Alg = alg(Version),
    escalus_stanza:caps(Alg, caps_hash(Features, Version, Alg), Version).

alg(v1) -> ~"sha-1";
alg(v2) -> ~"sha-256".

all_features(Features) ->
    [?NS_DISCO_INFO, ?NS_DISCO_ITEMS | Features].

caps_hash(Features, Version, Alg) ->
    rpc(mim(), mod_caps_hash, generate, [feature_elems(Features), Version, Alg]).

get_client_features(HostType, Client) ->
    Jid = jid:from_binary(escalus_client:full_jid(Client)),
    rpc(mim(), mod_caps, get_features, [HostType, Jid]).
