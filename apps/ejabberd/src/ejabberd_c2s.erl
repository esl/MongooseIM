%%%----------------------------------------------------------------------
%%% File    : ejabberd_c2s.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Serve C2S connection
%%% Created : 16 Nov 2002 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2011   ProcessOne
%%%
%%% This program is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation; either version 2 of the
%%% License, or (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with this program; if not, write to the Free Software
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
%%% 02111-1307 USA
%%%
%%%----------------------------------------------------------------------

-module(ejabberd_c2s).
-author('alexey@process-one.net').
-update_info({update, 0}).
%% External exports
-export([start/2,
         stop/1,
         start_link/2,
         send_text/2,
         send_element/2, % what for? doesn't seem to be used elsewhere
         socket_type/0,
         get_presence/1,
         get_aux_field/2,
         set_aux_field/3,
         del_aux_field/2,
         get_subscription/2,
         get_subscribed/1,
         send_filtered/5,
         broadcast/4,
         store_session_info/5]).

%% gen_fsm callbacks
-export([init/1,
         wait_for_stream/2,
         wait_for_auth/2,
         wait_for_feature_before_auth/2,
         wait_for_feature_after_auth/2,
         wait_for_session_or_sm/2,
         wait_for_sasl_response/2,
         session_established/2, session_established/3,
         resume_session/2, resume_session/3,
         handle_event/3,
         handle_sync_event/4,
         code_change/4,
         handle_info/3,
         terminate/3,
         print_state/1]).

-include("ejabberd.hrl").
-include("ejabberd_c2s.hrl").
-include("jlib.hrl").
-include_lib("exml/include/exml.hrl").
-xep([{xep, 18}, {version, "0.2"}]).
-behaviour(p1_fsm_old).

-export_type([broadcast/0]).

-type packet() :: {ejabberd:jid(), ejabberd:jid(), xmlel()}.

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

-spec start(_, list())
-> {'error', _} | {'ok', 'undefined' | pid()} | {'ok', 'undefined' | pid(), _}.
start(SockData, Opts) ->
    ?SUPERVISOR_START.


start_link(SockData, Opts) ->
    p1_fsm_old:start_link(ejabberd_c2s, [SockData, Opts],
                        fsm_limit_opts(Opts) ++ ?FSMOPTS).

socket_type() ->
    xml_stream.


%% @doc Return Username, Resource and presence information
get_presence(FsmRef) ->
    p1_fsm_old:sync_send_all_state_event(FsmRef, get_presence, 1000).


-spec get_aux_field(Key :: aux_key(),
                    State :: state()) -> 'error' | {'ok', aux_value()}.
get_aux_field(Key, #state{aux_fields = Opts}) ->
    case lists:keyfind(Key, 1, Opts) of
        {_, Val} ->
            {ok, Val};
        _ ->
            error
    end.


-spec set_aux_field(Key :: aux_key(),
                    Val :: aux_value(),
                    State :: state()) -> state().
set_aux_field(Key, Val, #state{aux_fields = Opts} = State) ->
    Opts1 = lists:keydelete(Key, 1, Opts),
    State#state{aux_fields = [{Key, Val}|Opts1]}.


-spec del_aux_field(Key :: aux_key(), State :: state()) -> aux_value().
del_aux_field(Key, #state{aux_fields = Opts} = State) ->
    Opts1 = lists:keydelete(Key, 1, Opts),
    State#state{aux_fields = Opts1}.


-spec get_subscription(From :: ejabberd:jid() | ejabberd:simple_jid(),
                       State :: state()) -> 'both' | 'from' | 'none' | 'to'.
get_subscription(From = #jid{}, StateData) ->
    get_subscription(jid:to_lower(From), StateData);
get_subscription(LFrom, StateData) ->
    LBFrom = setelement(3, LFrom, <<>>),
    F = is_subscribed_to_my_presence(LFrom, LBFrom, StateData),
    T = am_i_subscribed_to_presence(LFrom, LBFrom, StateData),
    case {F, T} of
        {true, true} -> both;
        {true, _} -> from;
        {_, true} -> to;
        _ -> none
    end.

send_filtered(FsmRef, Feature, From, To, Packet) ->
    FsmRef ! {send_filtered, Feature, From, To, Packet}.

broadcast(FsmRef, Type, From, Packet) ->
    FsmRef ! {broadcast, Type, From, Packet}.

stop(FsmRef) ->
    p1_fsm_old:send_event(FsmRef, closed).

store_session_info(FsmRef, User, Server, Resource, KV) ->
    FsmRef ! {store_session_info, User, Server, Resource, KV, self()}.

%%%----------------------------------------------------------------------
%%% Callback functions from gen_fsm
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, StateName, StateData}          |
%%          {ok, StateName, StateData, Timeout} |
%%          ignore                              |
%%          {stop, StopReason}
%%----------------------------------------------------------------------
init([{SockMod, Socket}, Opts]) ->
    Access = case lists:keyfind(access, 1, Opts) of
                 {_, A} -> A;
                 _ -> all
             end,
    Shaper = case lists:keyfind(shaper, 1, Opts) of
                 {_, S} -> S;
                 _ -> none
             end,
    XMLSocket =
    case lists:keyfind(xml_socket, 1, Opts) of
        {_, XS} -> XS;
        _ -> false
    end,
    Zlib = case lists:keyfind(zlib, 1, Opts) of
               {_, ZlibLimit} -> {true, ZlibLimit};
               _ -> {false, 0}
           end,
    StartTLS = lists:member(starttls, Opts),
    StartTLSRequired = lists:member(starttls_required, Opts),
    TLSEnabled = lists:member(tls, Opts),
    TLS = StartTLS orelse StartTLSRequired orelse TLSEnabled,
    TLSOpts1 =
    lists:filter(fun({certfile, _}) -> true;
                    ({ciphers, _}) -> true;
                    ({protocol_options, _}) -> true;
                    ({dhfile, _}) -> true;
                    (_) -> false
                 end, Opts),
    TLSOpts = [verify_none | TLSOpts1],
    IP = peerip(SockMod, Socket),
    %% Check if IP is blacklisted:
    case is_ip_blacklisted(IP) of
        true ->
            ?INFO_MSG("Connection attempt from blacklisted IP: ~s (~w)",
                      [jlib:ip_to_list(IP), IP]),
            {stop, normal};
        false ->
            Socket1 =
            case TLSEnabled of
                true -> mongoose_transport:starttls(SockMod, Socket, TLSOpts);
                false -> Socket
            end,
            SocketMonitor = mongoose_transport:monitor(SockMod, Socket1),
            {ok, wait_for_stream, #state{server         = ?MYNAME,
                                         socket         = Socket1,
                                         sockmod        = SockMod,
                                         socket_monitor = SocketMonitor,
                                         xml_socket     = XMLSocket,
                                         zlib           = Zlib,
                                         tls            = TLS,
                                         tls_required   = StartTLSRequired,
                                         tls_enabled    = TLSEnabled,
                                         tls_options    = TLSOpts,
                                         streamid       = new_id(),
                                         access         = Access,
                                         shaper         = Shaper,
                                         ip             = IP,
                                         lang           = default_language()},
             ?C2S_OPEN_TIMEOUT}
    end.

%% @doc Return list of all available resources of contacts,
get_subscribed(FsmRef) ->
    p1_fsm_old:sync_send_all_state_event(FsmRef, get_subscribed, 1000).

%%----------------------------------------------------------------------
%% Func: StateName/2
%%----------------------------------------------------------------------

-spec wait_for_stream(Item :: ejabberd:xml_stream_item(),
                      StateData :: state()) -> fsm_return().
wait_for_stream({xmlstreamstart, _Name, _} = StreamStart, StateData) ->
    handle_stream_start(StreamStart, StateData);
wait_for_stream(timeout, StateData) ->
    {stop, normal, StateData};
%% TODO: this clause is most likely dead code - can't be triggered
%%       with XMPP level tests;
%%       see github.com/esl/ejabberd_tests/tree/element-before-stream-start
wait_for_stream({xmlstreamelement, _}, StateData) ->
    c2s_stream_error(?INVALID_XML_ERR, StateData);
wait_for_stream({xmlstreamend, _}, StateData) ->
    c2s_stream_error(?INVALID_XML_ERR, StateData);
wait_for_stream({xmlstreamerror, _}, StateData) ->
    send_header(StateData, ?MYNAME, << "1.0">>, <<"">>),
    c2s_stream_error(?INVALID_XML_ERR, StateData);
wait_for_stream(closed, StateData) ->
    {stop, normal, StateData}.

handle_stream_start({xmlstreamstart, _Name, Attrs}, #state{} = S0) ->
    Server = jid:nameprep(xml:get_attr_s(<<"to">>, Attrs)),
    Lang = get_xml_lang(Attrs),
    S = S0#state{server = Server, lang = Lang},
    case {xml:get_attr_s(<<"xmlns:stream">>, Attrs),
          lists:member(Server, ?MYHOSTS)} of
        {?NS_STREAM, true} ->
            change_shaper(S, jid:make(<<>>, Server, <<>>)),
            Version = xml:get_attr_s(<<"version">>, Attrs),
            stream_start_by_protocol_version(Version, S);
        {?NS_STREAM, false} ->
            stream_start_error(?HOST_UNKNOWN_ERR, S);
        {_InvalidNS, _} ->
            stream_start_error(?INVALID_NS_ERR, S)
    end.

stream_start_error(Error, StateData) ->
    send_header(StateData, ?MYNAME, <<"">>, default_language()),
    c2s_stream_error(Error, StateData).

-spec c2s_stream_error(Error, State) -> Result when
      Error :: jlib:xmlel(),
      State :: state(),
      Result :: {stop, normal, state()}.
c2s_stream_error(Error, StateData) ->
    send_element(StateData, Error),
    send_trailer(StateData),
    {stop, normal, StateData}.

%% See RFC 6120 4.3.2:
%%
%%   If the initiating entity includes in the initial stream header
%%   the 'version' attribute set to a value of at least <<"1.0">> [...]
%%   receiving entity MUST send a <features/> child element [...]
%%
%% (http://xmpp.org/rfcs/rfc6120.html#streams-negotiation-features)
stream_start_by_protocol_version(<<"1.0">>, #state{} = S) ->
    stream_start_negotiate_features(S);
stream_start_by_protocol_version(_Pre10, #state{lang = Lang, server = Server} = S) ->
    send_header(S, Server, <<"">>, default_language()),
    case is_tls_required_but_unavailable(S) of
        false ->
            wait_for_legacy_auth(S);
        true ->
            c2s_stream_error(?POLICY_VIOLATION_ERR(Lang, <<"Use of STARTTLS required">>), S)
    end.

stream_start_negotiate_features(#state{} = S) ->
    send_header(S, S#state.server, <<"1.0">>, default_language()),
    case {S#state.authenticated, S#state.resource} of
        {false, _} ->
            stream_start_features_before_auth(S);
        {_, <<>>} ->
            stream_start_features_after_auth(S);
        {_, _} ->
            send_element(S, #xmlel{name = <<"stream:features">>}),
            fsm_next_state(wait_for_session_or_sm, S)
    end.

is_tls_required_but_unavailable(#state{} = S) ->
    (not S#state.tls_enabled) and S#state.tls_required.

%% TODO: Consider making this a completely different path (different FSM states!)
%%       than SASL auth negotiation - once wait_for_auth is targeted by refactoring.
%% For legacy auth see XEP-0078: Non-SASL Authentication
%% (http://xmpp.org/extensions/xep-0078.html).
wait_for_legacy_auth(#state{} = S) ->
    fsm_next_state(wait_for_auth, S).

stream_start_features_before_auth(#state{server = Server} = S) ->
    Creds = mongoose_credentials:new(Server),
    SASLState = cyrsasl:server_new(<<"jabber">>, Server, <<>>, [], Creds),
    SockMod = (S#state.sockmod):get_sockmod(S#state.socket),
    send_element(S, stream_features(determine_features(SockMod, S))),
    fsm_next_state(wait_for_feature_before_auth,
                   S#state{sasl_state = SASLState}).

stream_start_features_after_auth(#state{server = Server} = S) ->
    SockMod = (S#state.sockmod):get_sockmod(S#state.socket),
    Features = (maybe_compress_feature(SockMod, S)
                ++ [#xmlel{name = <<"bind">>,
                           attrs = [{<<"xmlns">>, ?NS_BIND}]},
                    #xmlel{name = <<"session">>,
                           attrs = [{<<"xmlns">>, ?NS_SESSION}]}]
                ++ maybe_roster_versioning_feature(Server)
                ++ hook_enabled_features(Server) ),
    send_element(S, stream_features(Features)),
    fsm_next_state(wait_for_feature_after_auth, S).

maybe_roster_versioning_feature(Server) ->
    ejabberd_hooks:run_fold(roster_get_versioning_feature,
                            Server, [], [Server]).

stream_features(FeatureElements) ->
    #xmlel{name = <<"stream:features">>,
           children = FeatureElements}.

%% From RFC 6120, section 5.3.1:
%%
%% If TLS is mandatory-to-negotiate, the receiving entity SHOULD NOT
%% advertise support for any stream feature except STARTTLS during the
%% initial stage of the stream negotiation process, because further stream
%% features might depend on prior negotiation of TLS given the order of
%% layers in XMPP (e.g., the particular SASL mechanisms offered by the
%% receiving entity will likely depend on whether TLS has been negotiated).
%%
%% http://xmpp.org/rfcs/rfc6120.html#tls-rules-mtn
determine_features(SockMod, #state{tls = TLS, tls_enabled = TLSEnabled,
                                   tls_required = TLSRequired,
                                   server = Server} = S) ->
    OtherFeatures = maybe_compress_feature(SockMod, S)
                 ++ maybe_sasl_mechanisms(Server)
                 ++ hook_enabled_features(Server),
    case can_use_tls(SockMod, TLS, TLSEnabled) of
        true ->
            case TLSRequired of
                true -> [starttls(required)];
                _    -> [starttls(optional)] ++ OtherFeatures
            end;
        false ->
            OtherFeatures
    end.

maybe_compress_feature(SockMod, #state{zlib = {ZLib, _}}) ->
    case can_use_zlib_compression(ZLib, SockMod) of
        true -> [compression_zlib()];
        _ -> []
    end.

maybe_sasl_mechanisms(Server) ->
    case cyrsasl:listmech(Server) of
        [] -> [];
        Mechanisms ->
            [#xmlel{name = <<"mechanisms">>,
                    attrs = [{<<"xmlns">>, ?NS_SASL}],
                    children = [ mechanism(S) || S <- Mechanisms ]}]
    end.

hook_enabled_features(Server) ->
    ejabberd_hooks:run_fold(c2s_stream_features, Server, [], [Server]).

starttls(TLSRequired)
  when TLSRequired =:= required;
       TLSRequired =:= optional ->
    #xmlel{name = <<"starttls">>,
           attrs = [{<<"xmlns">>, ?NS_TLS}],
           children = [ #xmlel{name = <<"required">>} || TLSRequired =:= required ]}.

can_use_tls(SockMod, TLS, TLSEnabled) ->
    TLS == true andalso (TLSEnabled == false) andalso SockMod == gen_tcp.

can_use_zlib_compression(Zlib, SockMod) ->
    Zlib andalso ( (SockMod == gen_tcp) orelse
                   (SockMod == fast_tls) ).

compression_zlib() ->
    #xmlel{name = <<"compression">>,
           attrs = [{<<"xmlns">>, ?NS_FEATURE_COMPRESS}],
           children = [#xmlel{name = <<"method">>,
                              children = [#xmlcdata{content = <<"zlib">>}]}]}.

mechanism(S) ->
    #xmlel{name = <<"mechanism">>,
           children = [#xmlcdata{content = S}]}.

get_xml_lang(Attrs) ->
    case xml:get_attr_s(<<"xml:lang">>, Attrs) of
        Lang when size(Lang) =< 35 ->
            %% As stated in BCP47, 4.4.1:
            %% Protocols or specifications that
            %% specify limited buffer sizes for
            %% language tags MUST allow for
            %% language tags of at least 35 characters.
            Lang;
        _ ->
            %% Do not store long language tag to
            %% avoid possible DoS/flood attacks
           <<>>
    end.

default_language() ->
    case ?MYLANG of
        undefined -> <<"en">>;
        DL -> DL
    end.

-spec wait_for_auth(Item :: ejabberd:xml_stream_item(),
                    State :: state()) -> fsm_return().
wait_for_auth({xmlstreamelement,
               #xmlel{name = <<"enable">>} = El}, StateData) ->
    maybe_unexpected_sm_request(wait_for_auth, El, StateData);
wait_for_auth({xmlstreamelement, El}, StateData) ->
    case is_auth_packet(El) of
        {auth, _ID, get, {U, _, _, _}} ->
            XE = jlib:make_result_iq_reply(El),
            UCdata = case U of
                         <<>> ->
                             [];
                         _ ->
                             [#xmlcdata{content = U}]
                     end,
            Res = case ejabberd_auth:plain_password_required(StateData#state.server) of
                      false ->
                          XE#xmlel{children = [#xmlel{name = <<"query">>,
                                                      attrs = [{<<"xmlns">>,
                                                                ?NS_AUTH}],
                                                      children = [#xmlel{name = <<"username">>,
                                                                         children = UCdata},
                                                                  #xmlel{name = <<"password">>},
                                                                  #xmlel{name = <<"digest">>},
                                                                  #xmlel{name = <<"resource">>}]}]};
                      true ->
                          XE#xmlel{children = [#xmlel{name = <<"query">>,
                                                      attrs = [{<<"xmlns">>,
                                                                ?NS_AUTH}],
                                                      children = [#xmlel{name = <<"username">>,
                                                                         children = UCdata},
                                                                  #xmlel{name = <<"password">>},
                                                                  #xmlel{name = <<"resource">>}]}]}
                  end,
            send_element(StateData, Res),
            fsm_next_state(wait_for_auth, StateData);
        {auth, _ID, set, {_U, _P, _D, <<>>}} ->
            Err = jlib:make_error_reply(El, ?ERR_AUTH_NO_RESOURCE_PROVIDED(StateData#state.lang)),
            send_element(StateData, Err),
            fsm_next_state(wait_for_auth, StateData);
        {auth, _ID, set, {U, P, D, R}} ->
            JID = jid:make(U, StateData#state.server, R),
            maybe_legacy_auth(JID, El, StateData, U, P, D, R);
        _ ->
            process_unauthenticated_stanza(StateData, El),
            fsm_next_state(wait_for_auth, StateData)
    end;
wait_for_auth(timeout, StateData) ->
    {stop, normal, StateData};
wait_for_auth({xmlstreamend, _Name}, StateData) ->
    send_trailer(StateData),
    {stop, normal, StateData};
wait_for_auth({xmlstreamerror, _}, StateData) ->
    send_element(StateData, ?INVALID_XML_ERR),
    send_trailer(StateData),
    {stop, normal, StateData};
wait_for_auth(closed, StateData) ->
    {stop, normal, StateData}.

maybe_legacy_auth(error, El, StateData, U, _P, _D, R) ->
    ?INFO_MSG("(~w) Forbidden legacy authentication for "
              "username '~s' with resource '~s'",
              [StateData#state.socket, U, R]),
    Err = jlib:make_error_reply(El, ?ERR_JID_MALFORMED),
    send_element(StateData, Err),
    fsm_next_state(wait_for_auth, StateData);
maybe_legacy_auth(JID, El, StateData, U, P, D, R) ->
    case user_allowed(JID, StateData) of
        true ->
            do_legacy_auth(JID, El, StateData, U, P, D, R);
        _ ->
            ?INFO_MSG("(~w) Forbidden legacy authentication for ~s",
                      [StateData#state.socket, jid:to_binary(JID)]),
            Err = jlib:make_error_reply(El, ?ERR_NOT_ALLOWED),
            send_element(StateData, Err),
            fsm_next_state(wait_for_auth, StateData)
    end.

do_legacy_auth(JID, El, StateData, U, P, D, R) ->
    case check_password_with_auth_module(U, StateData, P, D) of
        {true, AuthModule} ->
            do_open_legacy_session(El, StateData, U, R, JID,
                                   AuthModule);
        _ ->
            IP = peerip(StateData#state.sockmod, StateData#state.socket),
            ?INFO_MSG("(~w) Failed legacy authentication for ~s from IP ~s (~w)",
                      [StateData#state.socket,
                       jid:to_binary(JID), jlib:ip_to_list(IP), IP]),
            Err = jlib:make_error_reply(El, ?ERR_NOT_AUTHORIZED),
            ejabberd_hooks:run(auth_failed, StateData#state.server,
                               [U, StateData#state.server]),
            send_element(StateData, Err),
            fsm_next_state(wait_for_auth, StateData)
    end.

check_password_with_auth_module(User, #state{server = Server}, Password, <<>>) ->
    ejabberd_auth:check_password_with_authmodule(User, Server, Password);
check_password_with_auth_module(User, StateData, _, Digest) ->
    DGen = fun(PW) ->
                   Sid = StateData#state.streamid,
                   sha:sha1_hex(<<Sid/binary,
                                  PW/binary>>)
           end,
    ejabberd_auth:check_password_with_authmodule(User, StateData#state.server,
                                                 <<>>, Digest, DGen).

do_open_legacy_session(El, StateData, U, R, JID, AuthModule) ->
    ?INFO_MSG("(~w) Accepted legacy authentication for ~s by ~p",
              [StateData#state.socket, jid:to_binary(JID), AuthModule]),
    Res1 = jlib:make_result_iq_reply(El),
    Res = Res1#xmlel{children = []},
    send_element(StateData, Res),
    NewStateData = StateData#state{ user = U,
                                    resource = R,
                                    jid = JID,
                                    auth_module = AuthModule },
    do_open_session_common(mongoose_acc:new(), JID, NewStateData).

-spec wait_for_feature_before_auth(Item :: ejabberd:xml_stream_item(),
                               State :: state()) -> fsm_return().
wait_for_feature_before_auth({xmlstreamelement,
                          #xmlel{name = <<"enable">>} = El}, StateData) ->
    maybe_unexpected_sm_request(wait_for_feature_before_auth, El, StateData);
wait_for_feature_before_auth({xmlstreamelement, El}, StateData) ->
    #xmlel{name = Name, attrs = Attrs, children = Els} = El,
    {Zlib, _} = StateData#state.zlib,
    TLS = StateData#state.tls,
    TLSEnabled = StateData#state.tls_enabled,
    TLSRequired = StateData#state.tls_required,
    SockMod = (StateData#state.sockmod):get_sockmod(StateData#state.socket),
    case {xml:get_attr_s(<<"xmlns">>, Attrs), Name} of
        {?NS_SASL, <<"auth">>} when TLSEnabled or not TLSRequired ->
            Mech = xml:get_attr_s(<<"mechanism">>, Attrs),
            ClientIn = jlib:decode_base64(xml:get_cdata(Els)),
            StepResult = cyrsasl:server_start(StateData#state.sasl_state, Mech, ClientIn),
            {NewFSMState, NewStateData} = handle_sasl_step(StateData, StepResult),
            fsm_next_state(NewFSMState, NewStateData);
        {?NS_TLS, <<"starttls">>} when TLS == true,
                                       TLSEnabled == false,
                                       SockMod == gen_tcp ->
            TLSOpts = case ejabberd_config:get_local_option(
                             {domain_certfile, StateData#state.server}) of
                          undefined ->
                              StateData#state.tls_options;
                          CertFile ->
                              [{certfile, CertFile} |
                               lists:keydelete(
                                 certfile, 1, StateData#state.tls_options)]
                      end,
            Socket = StateData#state.socket,
            TLSSocket = (StateData#state.sockmod):starttls(
                                                    Socket, TLSOpts,
                                                    exml:to_binary(tls_proceed())),
            fsm_next_state(wait_for_stream,
                           StateData#state{socket = TLSSocket,
                                           streamid = new_id(),
                                           tls_enabled = true
                                          });
        {?NS_COMPRESS, <<"compress">>} when Zlib == true,
                                            ((SockMod == gen_tcp) or
                                             (SockMod == fast_tls)) ->
          check_compression_auth(El, wait_for_feature_before_auth, StateData);
        _ ->
          terminate_when_tls_required_but_not_enabled(TLSRequired, TLSEnabled,
                                                      StateData, El)
    end;
wait_for_feature_before_auth(timeout, StateData) ->
    {stop, normal, StateData};
wait_for_feature_before_auth({xmlstreamend, _Name}, StateData) ->
    send_trailer(StateData),
    {stop, normal, StateData};
wait_for_feature_before_auth({xmlstreamerror, _}, StateData) ->
    send_element(StateData, ?INVALID_XML_ERR),
    send_trailer(StateData),
    {stop, normal, StateData};
wait_for_feature_before_auth(closed, StateData) ->
    {stop, normal, StateData}.

compressed() ->
    #xmlel{name = <<"compressed">>,
           attrs = [{<<"xmlns">>, ?NS_COMPRESS}]}.

compress_unsupported_method() ->
    #xmlel{name = <<"failure">>,
           attrs = [{<<"xmlns">>, ?NS_COMPRESS}],
           children = [#xmlel{name = <<"unsupported-method">>}]}.

tls_proceed() ->
    #xmlel{name = <<"proceed">>,
           attrs = [{<<"xmlns">>, ?NS_TLS}]}.

compress_setup_failed() ->
    #xmlel{name = <<"failure">>,
           attrs = [{<<"xmlns">>, ?NS_COMPRESS}],
           children = [#xmlel{name = <<"setup-failed">>}]}.

-spec wait_for_sasl_response(Item :: ejabberd:xml_stream_item(),
                             State :: state()) -> fsm_return().
wait_for_sasl_response({xmlstreamelement,
                        #xmlel{name = <<"enable">>} = El}, StateData) ->
    maybe_unexpected_sm_request(wait_for_sasl_response, El, StateData);
wait_for_sasl_response({xmlstreamelement, El}, StateData) ->
    #xmlel{name = Name, attrs = Attrs, children = Els} = El,
    case {xml:get_attr_s(<<"xmlns">>, Attrs), Name} of
        {?NS_SASL, <<"response">>} ->
            ClientIn = jlib:decode_base64(xml:get_cdata(Els)),
            StepResult = cyrsasl:server_step(StateData#state.sasl_state, ClientIn),
            {NewFSMState, NewStateData} = handle_sasl_step(StateData, StepResult),
            fsm_next_state(NewFSMState, NewStateData);
        _ ->
            process_unauthenticated_stanza(StateData, El),
            fsm_next_state(wait_for_feature_before_auth, StateData)
    end;
wait_for_sasl_response(timeout, StateData) ->
    {stop, normal, StateData};
wait_for_sasl_response({xmlstreamend, _Name}, StateData) ->
    send_trailer(StateData),
    {stop, normal, StateData};
wait_for_sasl_response({xmlstreamerror, _}, StateData) ->
    send_element(StateData, ?INVALID_XML_ERR),
    send_trailer(StateData),
    {stop, normal, StateData};
wait_for_sasl_response(closed, StateData) ->
    {stop, normal, StateData}.

-spec wait_for_feature_after_auth(Item :: ejabberd:xml_stream_item(),
                              State :: state()) -> fsm_return().
wait_for_feature_after_auth({xmlstreamelement,
                         #xmlel{name = <<"enable">>} = El}, StateData) ->
    maybe_unexpected_sm_request(wait_for_feature_after_auth, El, StateData);
wait_for_feature_after_auth({xmlstreamelement,
                         #xmlel{name = <<"resume">>} = El}, StateData) ->
    maybe_resume_session(wait_for_feature_after_auth, El, StateData);
wait_for_feature_after_auth({xmlstreamelement, El}, StateData) ->
    case jlib:iq_query_info(El) of
        #iq{type = set, xmlns = ?NS_BIND, sub_el = SubEl} = IQ ->
            U = StateData#state.user,
            R1 = xml:get_path_s(SubEl, [{elem, <<"resource">>}, cdata]),
            R = case jid:resourceprep(R1) of
                    error -> error;
                    <<>> -> generate_random_resource();
                    Resource -> Resource
                end,
            case R of
                error ->
                    Err = jlib:make_error_reply(El, ?ERR_BAD_REQUEST),
                    send_element(StateData, Err),
                    fsm_next_state(wait_for_feature_after_auth, StateData);
                _ ->
                    JID = jid:make(U, StateData#state.server, R),
                    JIDEl = #xmlel{name = <<"jid">>,
                                   children = [#xmlcdata{content = jid:to_binary(JID)}]},
                    Res = IQ#iq{type = result,
                                sub_el = [#xmlel{name = <<"bind">>,
                                                 attrs = [{<<"xmlns">>, ?NS_BIND}],
                                                 children = [JIDEl]}]},
                    XmlEl = jlib:iq_to_xml(Res),
                    send_element(StateData, XmlEl),
                    fsm_next_state(wait_for_session_or_sm,
                                   StateData#state{resource = R, jid = JID})
            end;
        _ ->
            maybe_do_compress(El, wait_for_feature_after_auth, StateData)
    end;

wait_for_feature_after_auth(timeout, StateData) ->
    {stop, normal, StateData};

wait_for_feature_after_auth({xmlstreamend, _Name}, StateData) ->
    send_trailer(StateData),
    {stop, normal, StateData};

wait_for_feature_after_auth({xmlstreamerror, _}, StateData) ->
    send_element(StateData, ?INVALID_XML_ERR),
    send_trailer(StateData),
    {stop, normal, StateData};

wait_for_feature_after_auth(closed, StateData) ->
    {stop, normal, StateData}.

-spec wait_for_session_or_sm(Item :: ejabberd:xml_stream_item(),
                             State :: state()) -> fsm_return().
wait_for_session_or_sm({xmlstreamelement,
                        #xmlel{name = <<"enable">>} = El}, StateData) ->
    maybe_enable_stream_mgmt(wait_for_session_or_sm, El, StateData);

wait_for_session_or_sm({xmlstreamelement,
                        #xmlel{name = <<"r">>} = El}, StateData) ->
    maybe_send_sm_ack(xml:get_tag_attr_s(<<"xmlns">>, El),
                      StateData#state.stream_mgmt,
                      StateData#state.stream_mgmt_in,
                      wait_for_session_or_sm, StateData);

wait_for_session_or_sm({xmlstreamelement, El}, StateData0) ->
    StateData = maybe_increment_sm_incoming(StateData0#state.stream_mgmt,
                                            StateData0),
    case jlib:iq_query_info(El) of
        #iq{type = set, xmlns = ?NS_SESSION} ->
            Acc = mongoose_acc:from_element(El),
            {Res, _Acc1, NStateData} = maybe_open_session(Acc, StateData),
            case Res of
                stop -> {stop, normal, NStateData};
                wait -> fsm_next_state(wait_for_session_or_sm, NStateData);
                established ->  fsm_next_state_pack(session_established, NStateData)
            end;
        _ ->
            maybe_do_compress(El, wait_for_session_or_sm, StateData)
    end;

wait_for_session_or_sm(timeout, StateData) ->
    {stop, normal, StateData};

wait_for_session_or_sm({xmlstreamend, _Name}, StateData) ->
    send_trailer(StateData),
    {stop, normal, StateData};

wait_for_session_or_sm({xmlstreamerror, _}, StateData) ->
    send_element(StateData, ?INVALID_XML_ERR),
    send_trailer(StateData),
    {stop, normal, StateData};

wait_for_session_or_sm(closed, StateData) ->
    {stop, normal, StateData}.

maybe_do_compress(El = #xmlel{name = Name, attrs = Attrs}, NextState, StateData) ->
    SockMod = (StateData#state.sockmod):get_sockmod(StateData#state.socket),
    {Zlib, _} = StateData#state.zlib,
    case {xml:get_attr_s(<<"xmlns">>, Attrs), Name} of
        {?NS_COMPRESS, <<"compress">>} when Zlib == true,
                                            ((SockMod == gen_tcp) or
                                             (SockMod == fast_tls)) ->
            check_compression_auth(El, NextState, StateData);
        _ ->
            process_unauthenticated_stanza(StateData, El),
            fsm_next_state(NextState, StateData)

    end.

check_compression_auth(El, NextState, StateData) ->
    Auth = StateData#state.authenticated,
    case Auth of
        false ->
            send_element(StateData, compress_setup_failed()),
            fsm_next_state(NextState, StateData);
        _ ->
            check_compression_method(El, NextState, StateData)
    end.

check_compression_method(El, NextState, StateData) ->
    case exml_query:path(El, [{element, <<"method">>}, cdata]) of
        undefined ->
            send_element(StateData, compress_setup_failed()),
            fsm_next_state(NextState, StateData);
        <<"zlib">> ->
            {_, ZlibLimit} = StateData#state.zlib,
            Socket = StateData#state.socket,
            ZlibSocket
            = (StateData#state.sockmod):compress(Socket, ZlibLimit, exml:to_binary(compressed())),
            fsm_next_state(wait_for_stream,
                           StateData#state{socket = ZlibSocket, streamid = new_id()});
        _ ->
            send_element(StateData, compress_unsupported_method()),
            fsm_next_state(NextState, StateData)
    end.

-spec maybe_open_session(mongoose_acc:t(), state()) ->
    {wait | stop | established, mongoose_acc:t(), state()}.
maybe_open_session(Acc, #state{jid = JID} = StateData) ->
    case user_allowed(JID, StateData) of
        true ->
            do_open_session(Acc, JID, StateData);
        _ ->
            Acc1 = ejabberd_hooks:run_fold(forbidden_session_hook,
                               StateData#state.server, Acc, [JID]),
            ?INFO_MSG("(~w) Forbidden session for ~s",
                      [StateData#state.socket,
                       jid:to_binary(JID)]),
            Err = jlib:make_error_reply(Acc1, ?ERR_NOT_ALLOWED),
            Acc2 = send_element(Acc1, Err, StateData),
            {wait, Acc2, StateData}
    end.

-spec do_open_session(mongoose_acc:t(), jid(), state()) ->
    {stop | established, mongoose_acc:t(), state()}.
do_open_session(Acc, JID, StateData) ->
    ?INFO_MSG("(~w) Opened session for ~s", [StateData#state.socket, jid:to_binary(JID)]),
    Resp = jlib:make_result_iq_reply(mongoose_acc:get(element, Acc)),
    Packet = {jid:to_bare(StateData#state.jid), StateData#state.jid, Resp},
    case send_and_maybe_buffer_stanza(Acc, Packet, StateData) of
        {ok, Acc1, NStateData} ->
            do_open_session_common(Acc1, JID, NStateData);
        {resume, Acc1, NStateData} ->
            case maybe_enter_resume_session(NStateData) of
                {stop, normal, NextStateData} -> % error, resume not possible
                    c2s_stream_error(?SERR_INTERNAL_SERVER_ERROR, NextStateData),
                    {stop, Acc1, NStateData};
                {_, _, NextStateData, _} ->
                    do_open_session_common(Acc1, JID, NextStateData)
            end
    end.

do_open_session_common(Acc, JID, #state{user = U, resource = R} = NewStateData0) ->
    change_shaper(NewStateData0, JID),
    Acc1 = ejabberd_hooks:run_fold(roster_get_subscription_lists,
                                  NewStateData0#state.server,
                                  Acc,
                                  [U, NewStateData0#state.server]),
    {Fs, Ts, Pending} = mongoose_acc:get(subscription_lists, Acc1, {[], [], []}),
    LJID = jid:to_lower(jid:to_bare(JID)),
    Fs1 = [LJID | Fs],
    Ts1 = [LJID | Ts],
    PrivList = ejabberd_hooks:run_fold(privacy_get_user_list,
                                       NewStateData0#state.server,
                                       #userlist{},
                                       [U, NewStateData0#state.server]),
    SID = {p1_time_compat:timestamp(), self()},
    Conn = get_conn_type(NewStateData0),
    Info = [{ip, NewStateData0#state.ip}, {conn, Conn},
            {auth_module, NewStateData0#state.auth_module}],
    ReplacedPids = ejabberd_sm:open_session(SID, U, NewStateData0#state.server, R, Info),

    MonitorRefs = ordsets:from_list([{monitor(process, PID), PID} || PID <- ReplacedPids]),
    lists:foreach(
        fun({MonitorRef, PID}) ->
            receive
                {'DOWN', MonitorRef, _, _, _} -> ok
            after 100 ->
                ?WARNING_MSG("C2S process ~p for ~s replaced by ~p has not stopped before timeout",
                             [PID, jid:to_binary(NewStateData0#state.jid), self()])
            end
        end,
        MonitorRefs),

    NewStateData =
    NewStateData0#state{sid = SID,
                        conn = Conn,
                        pres_f = gb_sets:from_list(Fs1),
                        pres_t = gb_sets:from_list(Ts1),
                        pending_invitations = Pending,
                        privacy_list = PrivList},
    {established, Acc1, NewStateData}.

-spec session_established(Item :: ejabberd:xml_stream_item(),
                          State :: state()) -> fsm_return().
session_established({xmlstreamelement,
                     #xmlel{name = <<"enable">>} = El}, StateData) ->
    maybe_enable_stream_mgmt(session_established, El, StateData);

session_established({xmlstreamelement,
                     #xmlel{name = <<"a">>} = El}, StateData) ->
    stream_mgmt_handle_ack(session_established, El, StateData);

session_established({xmlstreamelement,
                     #xmlel{name = <<"r">>} = El}, StateData) ->
    maybe_send_sm_ack(xml:get_tag_attr_s(<<"xmlns">>, El),
                      StateData#state.stream_mgmt,
                      StateData#state.stream_mgmt_in,
                      session_established, StateData);
session_established({xmlstreamelement,
                     #xmlel{name = <<"inactive">>} = El}, State) ->
    mongoose_metrics:update(State#state.server, modCSIInactive, 1),

    maybe_inactivate_session(xml:get_tag_attr_s(<<"xmlns">>, El), State);

session_established({xmlstreamelement,
                     #xmlel{name = <<"active">>} = El}, State) ->
    mongoose_metrics:update(State#state.server, modCSIActive, 1),

    maybe_activate_session(xml:get_tag_attr_s(<<"xmlns">>, El), State);

session_established({xmlstreamelement, El}, StateData) ->
    FromJID = StateData#state.jid,
    % Check 'from' attribute in stanza RFC 3920 Section 9.1.2
    case check_from(El, FromJID) of
        'invalid-from' ->
            send_element(StateData, ?INVALID_FROM),
            send_trailer(StateData),
            {stop, normal, StateData};
        _NewEl ->
            NewState = maybe_increment_sm_incoming(StateData#state.stream_mgmt,
                                                   StateData),
            % initialise accumulator, fill with data
            El1 = fix_message_from_user(El, StateData#state.lang),
            Acc0 = mongoose_acc:from_element(El1),
            User = NewState#state.user,
            Server = NewState#state.server,
            To = exml_query:attr(El, <<"to">>),
            ToJID = case To of
                        undefined ->
                            jid:make(User, Server, <<>>);
                        _ ->
                            jid:from_binary(To)
                    end,
            Acc = mongoose_acc:update(Acc0, #{user => User,
                                              server => Server,
                                              from_jid => FromJID,
                                              from => jid:to_binary(FromJID),
                                              to_jid => ToJID,
                                              to => To}),
            Acc1 = mod_amp:check_packet(Acc, FromJID, initial_check),
            case mongoose_acc:get(amp_check_result, Acc1, ok) of
                drop -> fsm_next_state(session_established, NewState);
                _ -> process_outgoing_stanza(Acc1, NewState)
            end
    end;

%% We hibernate the process to reduce memory consumption after a
%% configurable activity timeout
session_established(timeout, StateData) ->
    %% TODO: Options must be stored in state:
    Options = [],
    proc_lib:hibernate(p1_fsm_old, enter_loop,
                       [?MODULE, Options, session_established, StateData]),
    fsm_next_state(session_established, StateData);
session_established({xmlstreamend, _Name}, StateData) ->
    send_trailer(StateData),
    {stop, normal, StateData};

session_established({xmlstreamerror, <<"child element too big">> = E}, StateData) ->
    send_element(StateData, ?POLICY_VIOLATION_ERR(StateData#state.lang, E)),
    send_trailer(StateData),
    {stop, normal, StateData};
session_established({xmlstreamerror, _}, StateData) ->
    send_element(StateData, ?INVALID_XML_ERR),
    send_trailer(StateData),
    {stop, normal, StateData};
session_established(closed, StateData) ->
    ?DEBUG("Session established closed - trying to enter resume_session", []),
    maybe_enter_resume_session(StateData#state.stream_mgmt_id, StateData).

%% @doc Process packets sent by user (coming from user on c2s XMPP
%% connection)
%% eventually it should return {mongoose_acc:t(), fsm_return()} so that the accumulator
%% comes back whence it originated
-spec process_outgoing_stanza(mongoose_acc:t(), state()) -> fsm_return().
process_outgoing_stanza(Acc, StateData) ->
    ToJID = mongoose_acc:get(to_jid, Acc),
    Name = mongoose_acc:get(name, Acc),
    NState = process_outgoing_stanza(Acc, ToJID, Name, StateData),
    ejabberd_hooks:run(c2s_loop_debug, [{xmlstreamelement, mongoose_acc:get(element, Acc)}]),
    fsm_next_state(session_established, NState).

process_outgoing_stanza(Acc, error, _Name, StateData) ->
    case mongoose_acc:get(type, Acc) of
        <<"error">> -> StateData;
        <<"result">> -> StateData;
        _ ->
            Err = jlib:make_error_reply(Acc, ?ERR_JID_MALFORMED),
            send_element(Acc, Err, StateData),
            StateData
    end;
process_outgoing_stanza(Acc, ToJID, <<"presence">>, StateData) ->
    FromJID = mongoose_acc:get(from_jid, Acc),
    Server = mongoose_acc:get(server, Acc),
    User = mongoose_acc:get(user, Acc),
    Res = ejabberd_hooks:run_fold(c2s_update_presence, Server, Acc, []),
    El = mongoose_acc:get(element, Res),
    Res1 = ejabberd_hooks:run_fold(user_send_packet,
                                   Server,
                                   Res,
                                   [FromJID, ToJID, El]),
    {_Acc1, NState} = case ToJID of
                          #jid{user = User,
                               server = Server,
                               resource = <<>>} ->
                               presence_update(Res1, FromJID, StateData);
                          _ ->
                               presence_track(Res1, StateData)
                      end,
    NState;
process_outgoing_stanza(Acc0, ToJID, <<"iq">>, StateData) ->
    Acc = mongoose_acc:require(xmlns, Acc0),
    FromJID = mongoose_acc:get(from_jid, Acc),
    Server = mongoose_acc:get(server, Acc),
    El = mongoose_acc:get(element, Acc),
    {_Acc, NState} = case mongoose_acc:get(xmlns, Acc, undefined) of
                         ?NS_PRIVACY ->
                             process_privacy_iq(Acc, ToJID, StateData);
                         ?NS_BLOCKING ->
                             process_privacy_iq(Acc, ToJID, StateData);
                         _ ->
                             Acc2 = ejabberd_hooks:run_fold(user_send_packet,
                                                            Server,
                                                            Acc,
                                                            [FromJID, ToJID, El]),
                             Acc3 = check_privacy_and_route(Acc2, StateData),
                             {Acc3, StateData}
    end,
    NState;
process_outgoing_stanza(Acc, ToJID, <<"message">>, StateData) ->
    FromJID = mongoose_acc:get(from_jid, Acc),
    Server = mongoose_acc:get(server, Acc),
    El = mongoose_acc:get(element, Acc),
    Acc1 = ejabberd_hooks:run_fold(user_send_packet,
                                   Server,
                                   Acc,
                                   [FromJID, ToJID, El]),
    _Acc2 = check_privacy_and_route(Acc1, StateData),
    StateData;
process_outgoing_stanza(_Acc, _ToJID, _Name, StateData) ->
    StateData.

%%-------------------------------------------------------------------------
%% session may be terminated for exmaple by mod_ping there is still valid
%% connection and resource want to send stanza.
resume_session({xmlstreamelement, _}, StateData) ->
    Err = ?POLICY_VIOLATION_ERR(StateData#state.lang,
                                <<"session in resume state cannot accept incoming stanzas">>),
    maybe_send_element_safe(StateData, Err),
    maybe_send_trailer_safe(StateData),
    {next_state, resume_session, StateData, hibernate};

%%-------------------------------------------------------------------------
%% ignore mod_ping closed messages because we are already in resume session
%% state
resume_session(closed, StateData) ->
    {next_state, resume_session, StateData, hibernate};
resume_session(timeout, StateData) ->
    {next_state, resume_session, StateData, hibernate};
resume_session(Msg, StateData) ->
    ?WARNING_MSG("unexpected message ~p", [Msg]),
    {next_state, resume_session, StateData, hibernate}.


%%----------------------------------------------------------------------
%% Func: StateName/3
%% Returns: {next_state, NextStateName, NextStateData}            |
%%          {next_state, NextStateName, NextStateData, Timeout}   |
%%          {reply, Reply, NextStateName, NextStateData}          |
%%          {reply, Reply, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                          |
%%          {stop, Reason, Reply, NewStateData}
%%----------------------------------------------------------------------

session_established(resume, _From, SD) ->
    handover_session(SD).

resume_session(resume, _From, SD) ->
    handover_session(SD).

%%----------------------------------------------------------------------
%% Func: handle_event/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%%----------------------------------------------------------------------
handle_event(keep_alive_packet, session_established,
             #state{server = Server, jid = JID} = StateData) ->
    ejabberd_hooks:run(user_sent_keep_alive, Server, [JID]),
    fsm_next_state(session_established, StateData);
handle_event(_Event, StateName, StateData) ->
    fsm_next_state(StateName, StateData).

%%----------------------------------------------------------------------
%% Func: handle_sync_event/4
%% Returns: {next_state, NextStateName, NextStateData}            |
%%          {next_state, NextStateName, NextStateData, Timeout}   |
%%          {reply, Reply, NextStateName, NextStateData}          |
%%          {reply, Reply, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                          |
%%          {stop, Reason, Reply, NewStateData}
%%----------------------------------------------------------------------
-spec handle_sync_event(Evt :: atom(),
                        From :: any(),
                        StateName :: statename(),
                        State :: state())
-> {'reply', Reply :: [any()], statename(), state()}
   | {'reply', Reply :: 'ok' | {_, _, _, _}, statename(), state(), timeout()}.
handle_sync_event(get_presence, _From, StateName, StateData) ->
    User = StateData#state.user,
    PresLast = StateData#state.pres_last,

    Show = get_showtag(PresLast),
    Status = get_statustag(PresLast),
    Resource = StateData#state.resource,

    Reply = {User, Resource, Show, Status},
    fsm_reply(Reply, StateName, StateData);
handle_sync_event(get_subscribed, _From, StateName, StateData) ->
    Subscribed = gb_sets:to_list(StateData#state.pres_f),
    {reply, Subscribed, StateName, StateData};
handle_sync_event(_Event, _From, StateName, StateData) ->
    Reply = ok,
    fsm_reply(Reply, StateName, StateData).


code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.

%%----------------------------------------------------------------------
%% Func: handle_info/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%%----------------------------------------------------------------------


%%% system events
handle_info(replaced, _StateName, StateData) ->
    Lang = StateData#state.lang,
    maybe_send_element_safe(StateData,
                            ?SERRT_CONFLICT(Lang, <<"Replaced by new connection">>)),
    maybe_send_trailer_safe(StateData),
    {stop, normal, StateData#state{authenticated = replaced}};
handle_info(new_offline_messages, session_established,
            #state{pres_last = Presence, pres_invis = Invisible} = StateData)
  when Presence =/= undefined orelse Invisible ->
    resend_offline_messages(mongoose_acc:new(), StateData),
    {next_state, session_established, StateData};
handle_info({'DOWN', Monitor, _Type, _Object, _Info}, _StateName, StateData)
  when Monitor == StateData#state.socket_monitor ->
    maybe_enter_resume_session(StateData#state.stream_mgmt_id, StateData);
handle_info(system_shutdown, StateName, StateData) ->
    case StateName of
        wait_for_stream ->
            send_header(StateData, ?MYNAME, <<"1.0">>, <<"en">>),
            send_element(StateData, ?SERR_SYSTEM_SHUTDOWN),
            send_trailer(StateData),
            ok;
        _ ->
            send_element(StateData, ?SERR_SYSTEM_SHUTDOWN),
            send_trailer(StateData),
            ok
    end,
    {stop, normal, StateData};
handle_info({force_update_presence, LUser}, StateName,
            #state{user = LUser, server = LServer} = StateData) ->
    NewStateData =
    case StateData#state.pres_last of
        #xmlel{name = <<"presence">>} ->
            PresenceEl = ejabberd_hooks:run_fold(
                           c2s_update_presence,
                           LServer,
                           StateData#state.pres_last,
                           [LUser, LServer]),
            StateData2 = StateData#state{pres_last = PresenceEl},
            Acc = mongoose_acc:from_element(PresenceEl),
            presence_update(Acc, StateData2#state.jid, StateData2),
            StateData2;
        _ ->
            StateData
    end,
    {next_state, StateName, NewStateData};
handle_info(resume_timeout, resume_session, StateData) ->
    {stop, normal, StateData};
handle_info(check_buffer_full, StateName, StateData) ->
    case is_buffer_full(StateData#state.stream_mgmt_buffer_size,
                        StateData#state.stream_mgmt_buffer_max) of
        true ->
            Err = ?RESOURCE_CONSTRAINT_ERR((StateData#state.lang),
                                           <<"too many unacked stanzas">>),
            send_element(StateData, Err),
            send_trailer(StateData),
            {stop, normal, StateData};
        false ->
            fsm_next_state(StateName,
                           StateData#state{stream_mgmt_constraint_check_tref = undefined})
    end;
handle_info({store_session_info, User, Server, Resource, KV, _FromPid}, StateName, StateData) ->
    ejabberd_sm:store_info(User, Server, Resource, KV),
    fsm_next_state(StateName, StateData);
handle_info(Info, StateName, StateData) ->
    handle_incoming_message(Info, StateName, StateData).

%%% incoming messages
handle_incoming_message({send_text, Text}, StateName, StateData) ->
    ?ERROR_MSG("{c2s:send_text, Text}: ~p~n", [{send_text, Text}]), % is it ever called?
    % it seems to be sometimes, by event sent from s2s
    send_text(StateData, Text),
    ejabberd_hooks:run(c2s_loop_debug, [Text]),
    fsm_next_state(StateName, StateData);
handle_incoming_message({broadcast, Acc}, StateName, StateData) ->
    Broadcast = mongoose_acc:get(element, Acc),
    ejabberd_hooks:run(c2s_loop_debug, [{broadcast, Broadcast}]),
    ?DEBUG("broadcast=~p", [Broadcast]),
    Res = handle_routed_broadcast(Broadcast, StateData),
    handle_broadcast_result(Res, StateName, StateData);
handle_incoming_message({route, From, To, Acc}, StateName, StateData) ->
    Acc1 = ejabberd_hooks:run_fold(c2s_loop_debug, Acc, [{route, From, To}]),
    Name = mongoose_acc:get(name, Acc1),
    process_incoming_stanza(Name, From, To, Acc1, StateName, StateData);
handle_incoming_message({send_filtered, Feature, From, To, Packet}, StateName, StateData) ->
    % this is used by pubsub and should be rewritten when someone rewrites pubsub module
    Acc = mongoose_acc:new(),
    Drop = ejabberd_hooks:run_fold(c2s_filter_packet, StateData#state.server,
        true, [StateData#state.server, StateData,
            Feature, To, Packet]),
    case {Drop, StateData#state.jid} of
        {true, _} ->
            ?DEBUG("Dropping packet from ~p to ~p", [jid:to_binary(From), jid:to_binary(To)]),
            fsm_next_state(StateName, StateData);
        {_, To} ->
            FinalPacket = jlib:replace_from_to(From, To, Packet),
            case privacy_check_packet(StateData, From, To, FinalPacket, in) of
                allow ->
                    {Act, _, NStateData} = send_and_maybe_buffer_stanza(Acc,
                                                                        {From, To, FinalPacket},
                                                                        StateData),
                    finish_state(Act, StateName, NStateData);
                _ ->
                    fsm_next_state(StateName, StateData)
            end;
        _ ->
            FinalPacket = jlib:replace_from_to(From, To, Packet),
            ejabberd_router:route(From, To, FinalPacket),
            fsm_next_state(StateName, StateData)
    end;
handle_incoming_message({broadcast, Type, From, Packet}, StateName, StateData) ->
    %% this version is used only by mod_pubsub, which does things the old way
    Recipients = ejabberd_hooks:run_fold(
        c2s_broadcast_recipients, StateData#state.server,
        [], [StateData#state.server, StateData, Type, From, Packet]),
    lists:foreach(fun(USR) -> ejabberd_router:route(From, jid:make(USR), Packet) end,
        lists:usort(Recipients)),
    fsm_next_state(StateName, StateData);
handle_incoming_message(Info, StateName, StateData) ->
    ?ERROR_MSG("Unexpected info: ~p", [Info]),
    fsm_next_state(StateName, StateData).

process_incoming_stanza(Name, From, To, Acc, StateName, StateData) ->
    Packet = mongoose_acc:get(element, Acc),
    {Act, _NextAcc, NextState} = case handle_routed(Name, From, To, Acc, StateData) of
                                     {allow, NewAcc, NewPacket, NewState} ->
                                         preprocess_and_ship(NewAcc, From, To, NewPacket, NewState);
                                     {allow, NewAcc, NewState} ->
                                         preprocess_and_ship(NewAcc, From, To, Packet, NewState);
                                     {Reason, NewAcc, NewState} ->
                                         response_negative(Name, Reason, From, To, NewAcc),
                                         {ok, NewAcc, NewState}
                                 end,
    finish_state(Act, StateName, NextState).

-spec preprocess_and_ship(Acc :: mongoose_acc:t(),
                          From :: ejabberd:jid(),
                          To :: ejabberd:jid(),
                          El :: xmlel(),
                          StateData :: state()) -> {ok | resume, mongoose_acc:t(), state()}.
preprocess_and_ship(Acc, From, To, El, StateData) ->
    #xmlel{attrs = Attrs} = El,
    Attrs2 = jlib:replace_from_to_attrs(jid:to_binary(From),
        jid:to_binary(To),
        Attrs),
    FixedEl = El#xmlel{attrs = Attrs2},
    Acc2 = ejabberd_hooks:run_fold(user_receive_packet,
                                   StateData#state.server,
                                   Acc,
                                   [StateData#state.jid, From, To, FixedEl]),
    ship_to_local_user(Acc2, {From, To, FixedEl}, StateData).

response_negative(<<"iq">>, forbidden, From, To, Acc) ->
    send_back_error(?ERR_FORBIDDEN, From, To, Acc);
response_negative(<<"iq">>, deny, From, To, Acc) ->
    IqType = mongoose_acc:get(type, Acc),
    response_iq_deny(IqType, From, To, Acc);
response_negative(<<"message">>, deny, From, To, Acc) ->
    mod_amp:check_packet(Acc, From, delivery_failed),
    send_back_error(?ERR_SERVICE_UNAVAILABLE, From, To, Acc);
response_negative(_, _, _, _, Acc) ->
    Acc.

response_iq_deny(<<"get">>, From, To, Acc) ->
    send_back_error(?ERR_SERVICE_UNAVAILABLE, From, To, Acc);
response_iq_deny(<<"set">>, From, To, Acc) ->
    send_back_error(?ERR_SERVICE_UNAVAILABLE, From, To, Acc);
response_iq_deny(_, _, _, Acc) ->
    Acc.

send_back_error(Etype, From, To, Acc) ->
    Err = jlib:make_error_reply(Acc, Etype),
    ejabberd_router:route(To, From, Acc, Err).

handle_routed(<<"presence">>, From, To, Acc, StateData) ->
    handle_routed_presence(From, To, Acc, StateData);
handle_routed(<<"iq">>, From, To, Acc, StateData) ->
    handle_routed_iq(From, To, Acc, StateData);
handle_routed(<<"message">>, _From, To, Acc, StateData) ->
    {Acc1, Res} = privacy_check_packet(Acc, To, in, StateData),
    case Res of
        allow ->
            {allow, Acc1, StateData};
        deny ->
            {deny, Acc1, StateData};
        block ->
            {deny, Acc1, StateData}
    end;
handle_routed(_, _From, _To, Acc, StateData) ->
    {ignore, Acc, StateData}.

-spec handle_routed_iq(From :: ejabberd:jid(),
                       To :: ejabberd:jid(),
                       Acc :: mongoose_acc:t(),
                       StateData :: state()) -> routing_result().
handle_routed_iq(From, To, Acc, StateData) ->
    Acc1 = mongoose_acc:require(iq_query_info, Acc),
    Qi = mongoose_acc:get(iq_query_info, Acc1),
    handle_routed_iq(From, To, Acc1, Qi, StateData).

-spec handle_routed_iq(From :: ejabberd:jid(),
                       To :: ejabberd:jid(),
                       Acc :: mongoose_acc:t(),
                       IQ :: invalid | not_iq | reply | ejabberd:iq(),
                       StateData :: state()) -> routing_result().
handle_routed_iq(From, To, Acc0, #iq{ xmlns = ?NS_LAST }, StateData) ->
    %% TODO: Support for mod_last / XEP-0012. Can we move it to the respective module?
    %%   Thanks to add_iq_handler(ejabberd_sm, ...)?
    {Acc, HasFromSub} = case is_subscribed_to_my_presence(From, StateData) of
                             true ->
                                 {A, R} = privacy_check_packet(Acc0, To, out, StateData),
                                 {A, R == 'allow'};
                             false ->
                                 {Acc0, false}
                         end,
    case HasFromSub of
        true ->
            {Acc1, Res} = privacy_check_packet(Acc, To, in, StateData),
            case Res of
                allow ->
                    {allow, Acc1, StateData};
                _ ->
                    {deny, Acc1, StateData}
            end;
        _ ->
            {forbidden, Acc, StateData}
    end;
handle_routed_iq(_From, To, Acc, IQ, StateData)
  when (is_record(IQ, iq)) orelse (IQ == reply) ->
    {Acc1, Res} = privacy_check_packet(Acc, To, in, StateData),
    case Res of
        allow ->
            {allow, Acc1, StateData};
        deny when is_record(IQ, iq) ->
            {deny, Acc1, StateData};
        deny when IQ == reply ->
            %% ???
            {deny, Acc1, StateData}
    end;
handle_routed_iq(_From, _To, Acc, IQ, StateData)
  when (IQ == invalid) or (IQ == not_iq) ->
    {invalid, Acc, StateData}.

-spec handle_routed_broadcast(Broadcast :: broadcast_type(), StateData :: state()) ->
    broadcast_result().
handle_routed_broadcast({item, IJID, ISubscription}, StateData) ->
    {new_state, roster_change(IJID, ISubscription, StateData)};
handle_routed_broadcast({exit, Reason}, _StateData) ->
    {exit, Reason};
handle_routed_broadcast({privacy_list, PrivList, PrivListName}, StateData) ->
    case ejabberd_hooks:run_fold(privacy_updated_list, StateData#state.server,
                                 false, [StateData#state.privacy_list, PrivList]) of
        false ->
            {new_state, StateData};
        NewPL ->
            PrivPushIQ = privacy_list_push_iq(PrivListName),
            F = jid:to_bare(StateData#state.jid),
            T = StateData#state.jid,
            PrivPushEl = jlib:replace_from_to(F, T, jlib:iq_to_xml(PrivPushIQ)),
            maybe_update_presence(StateData, NewPL),
            {send_new, F, T, PrivPushEl, StateData#state{privacy_list = NewPL}}
    end;
handle_routed_broadcast({blocking, UserList, Action, JIDs}, StateData) ->
    blocking_push_to_resources(Action, JIDs, StateData),
    blocking_presence_to_contacts(Action, JIDs, StateData),
    {new_state, StateData#state{privacy_list = UserList}};
handle_routed_broadcast(_, StateData) ->
    {new_state, StateData}.

-spec handle_broadcast_result(broadcast_result(), StateName :: atom(), StateData :: state()) ->
    any().
handle_broadcast_result({exit, ErrorMessage}, _StateName, StateData) ->
    Lang = StateData#state.lang,
    send_element(StateData, ?SERRT_CONFLICT(Lang, ErrorMessage)),
    send_trailer(StateData),
    {stop, normal, StateData};
handle_broadcast_result({send_new, From, To, Stanza, NewState}, StateName, _StateData) ->
    Acc = mongoose_acc:new(),
    {Act, _, NewStateData} = ship_to_local_user(Acc, {From, To, Stanza}, NewState),
    finish_state(Act, StateName, NewStateData);
handle_broadcast_result({new_state, NewState}, StateName, _StateData) ->
    fsm_next_state(StateName, NewState).

privacy_list_push_iq(PrivListName) ->
    #iq{type = set, xmlns = ?NS_PRIVACY,
        id = list_to_binary("push" ++ randoms:get_string()),
        sub_el = [#xmlel{name = <<"query">>,
                         attrs = [{<<"xmlns">>, ?NS_PRIVACY}],
                         children = [#xmlel{name = <<"list">>,
                                            attrs = [{<<"name">>, PrivListName}]}]}]}.

-spec handle_routed_presence(From :: ejabberd:jid(), To :: ejabberd:jid(),
                             Acc0 :: mongoose_acc:t(), StateData :: state()) -> routing_result().
handle_routed_presence(From, To, Acc, StateData) ->
    Packet = mongoose_acc:get(element, Acc),
    State = ejabberd_hooks:run_fold(c2s_presence_in, StateData#state.server,
                                    StateData, [{From, To, Packet}]),
    case mongoose_acc:get(type, Acc) of
        <<"probe">> ->
            {LFrom, LBFrom} = lowcase_and_bare(From),
            NewState = case am_i_available_to(LFrom, LBFrom, State) of
                           true -> State;
                           false -> make_available_to(LFrom, LBFrom, State)
                       end,
            Acc1 = process_presence_probe(From, To, Acc, NewState),
            {probe, Acc1, NewState};
        <<"error">> ->
            NewA = gb_sets:del_element(jid:to_lower(From), State#state.pres_a),
            {allow, Acc, State#state{pres_a = NewA}};
        <<"invisible">> ->
            El = mongoose_acc:get(element, Acc),
            #xmlel{attrs = Attrs} = El,
            Attrs1 = lists:keydelete(<<"type">>, 1, Attrs),
            Attrs2 = [{<<"type">>, <<"unavailable">>} | Attrs1],
            NEl = El#xmlel{attrs = Attrs2},
            {allow, Acc, NEl, State};
        <<"subscribe">> ->
            {Acc1, SRes} = privacy_check_packet(Acc, To, in, State),
            {SRes, Acc1, State};
        <<"subscribed">> ->
            {Acc1, SRes} = privacy_check_packet(Acc, To, in, State),
            {SRes, Acc1, State};
        <<"unsubscribe">> ->
            {Acc1, SRes} = privacy_check_packet(Acc, To, in, State),
            {SRes, Acc1, State};
        <<"unsubscribed">> ->
            {Acc1, SRes} = privacy_check_packet(Acc, To, in, State),
            {SRes, Acc1, State};
        _ ->
            handle_routed_available_presence(State, From, To, Acc)
    end.

-spec handle_routed_available_presence(State :: state(),
                                       From :: ejabberd:jid(),
                                       To :: ejabberd:jid(),
                                       Acc :: mongoose_acc:t()) -> routing_result().
handle_routed_available_presence(State, From, To, Acc) ->
    {Acc1, Res} = privacy_check_packet(Acc, To, in, State),
    case Res of
        allow ->
            {LFrom, LBFrom} = lowcase_and_bare(From),
            case am_i_available_to(LFrom, LBFrom, State) of
                true -> {allow, Acc1, State};
                false -> {allow, Acc1, make_available_to(LFrom, LBFrom, State)}
            end;
        _ ->
            {deny, Acc1, State}
    end.

am_i_available_to(LFrom, LBFrom, State) ->
    gb_sets:is_element(LFrom, State#state.pres_a)
    orelse (LFrom /= LBFrom)
    andalso gb_sets:is_element(LBFrom, State#state.pres_a).

make_available_to(LFrom, LBFrom, State) ->
    case gb_sets:is_element(LFrom, State#state.pres_f) of
        true ->
            A = gb_sets:add_element(LFrom, State#state.pres_a),
            State#state{pres_a = A};
        false ->
            case gb_sets:is_element(LBFrom, State#state.pres_f) of
                true ->
                    A = gb_sets:add_element(LBFrom, State#state.pres_a),
                    State#state{pres_a = A};
                false ->
                    State
            end
    end.

%%----------------------------------------------------------------------
%% Func: print_state/1
%% Purpose: Prepare the state to be printed on error log
%% Returns: State to print
%%----------------------------------------------------------------------
-spec print_state(state()) -> state().
print_state(State = #state{pres_t = T, pres_f = F, pres_a = A, pres_i = I}) ->
    State#state{pres_t = {pres_t, gb_sets:size(T)},
                pres_f = {pres_f, gb_sets:size(F)},
                pres_a = {pres_a, gb_sets:size(A)},
                pres_i = {pres_i, gb_sets:size(I)}
               }.

%%----------------------------------------------------------------------
%% Func: terminate/3
%% Purpose: Shutdown the fsm
%% Returns: any
%%----------------------------------------------------------------------
-spec terminate(Reason :: any(), statename(), state()) -> ok.
terminate(_Reason, StateName, StateData) ->
    case {should_close_session(StateName), StateData#state.authenticated} of
        {false, _} ->
            ok;
        %% if we are in an state wich have a session established
        {_, replaced} ->
            ?INFO_MSG("(~w) Replaced session for ~s",
                      [StateData#state.socket,
                       jid:to_binary(StateData#state.jid)]),
            From = StateData#state.jid,
            StatusEl = #xmlel{name = <<"status">>,
                              children = [#xmlcdata{content = <<"Replaced by new connection">>}]},
            Packet = #xmlel{name = <<"presence">>,
                            attrs = [{<<"type">>, <<"unavailable">>}],
                            children = [StatusEl]},
            Acc0 = mongoose_acc:from_element(Packet),
            Acc = mongoose_acc:put(from_jid, From, Acc0),
            ejabberd_sm:close_session_unset_presence(
              StateData#state.sid,
              StateData#state.user,
              StateData#state.server,
              StateData#state.resource,
              <<"Replaced by new connection">>,
              replaced),
            Acc1 = presence_broadcast(Acc, StateData#state.pres_a, StateData),
            presence_broadcast(Acc1, StateData#state.pres_i, StateData),
            reroute_unacked_messages(StateData);
        {_, resumed} ->
            ?INFO_MSG("(~w) Stream ~p resumed for ~s",
                      [StateData#state.socket,
                       StateData#state.stream_mgmt_id,
                       jid:to_binary(StateData#state.jid)]);
        _ ->
            ?INFO_MSG("(~w) Close session for ~s",
                      [StateData#state.socket,
                       jid:to_binary(StateData#state.jid)]),

            EmptySet = gb_sets:new(),
            case StateData of
                #state{pres_last = undefined,
                       pres_a = EmptySet,
                       pres_i = EmptySet,
                       pres_invis = false} ->
                    ejabberd_sm:close_session(StateData#state.sid,
                                              StateData#state.user,
                                              StateData#state.server,
                                              StateData#state.resource,
                                              normal);
                _ ->
                    From = StateData#state.jid,
                    Packet = #xmlel{name = <<"presence">>,
                                    attrs = [{<<"type">>, <<"unavailable">>}]},
                    Acc0 = mongoose_acc:from_element(Packet),
                    Acc = mongoose_acc:put(from_jid, From, Acc0),
                    ejabberd_sm:close_session_unset_presence(
                      StateData#state.sid,
                      StateData#state.user,
                      StateData#state.server,
                      StateData#state.resource,
                      <<"">>,
                      normal),
                    Acc1 = presence_broadcast(Acc, StateData#state.pres_a, StateData),
                    presence_broadcast(Acc1, StateData#state.pres_i, StateData)
            end,
            reroute_unacked_messages(StateData)
    end,
    (StateData#state.sockmod):close(StateData#state.socket),
    ok.

-spec reroute_unacked_messages(StateData :: state()) -> any().
reroute_unacked_messages(StateData) ->
    ?DEBUG("rerouting unacked messages", []),
    flush_stream_mgmt_buffer(StateData),
    bounce_csi_buffer(StateData),
    bounce_messages().

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

fix_message_from_user(#xmlel{attrs = Attrs} = El0, Lang) ->
    % do some cryptic preparation on xmlel
    NewEl1 = jlib:remove_attr(<<"xmlns">>, jlib:remove_delay_tags(El0)),
    case xml:get_attr_s(<<"xml:lang">>, Attrs) of
        <<>> ->
            case Lang of
                <<>> -> NewEl1;
                Lang ->
                    xml:replace_tag_attr(<<"xml:lang">>, Lang, NewEl1)
            end;
        _ ->
            NewEl1
    end.

should_close_session(resume_session) -> true;
should_close_session(session_established) -> true;
should_close_session(_) -> false.

-spec generate_random_resource() -> ejabberd:lresource().
generate_random_resource() ->
    list_to_binary(
      lists:concat(
        [randoms:get_string() | tuple_to_list(p1_time_compat:timestamp())])).

-spec change_shaper(state(), ejabberd:jid()) -> any().
change_shaper(StateData, JID) ->
    Shaper = acl:match_rule(StateData#state.server,
                            StateData#state.shaper, JID),
    (StateData#state.sockmod):change_shaper(StateData#state.socket, Shaper).


-spec send_text(state(), Text :: binary()) -> any().
send_text(StateData, Text) ->
    ?DEBUG("Send XML on stream = ~p", [Text]),
    Size = size(Text),
    mongoose_metrics:update(global, [data, xmpp, sent, xml_stanza_size], Size),
    (StateData#state.sockmod):send(StateData#state.socket, Text).

-spec maybe_send_element_safe(state(), El :: jlib:xmlel()) -> any().
maybe_send_element_safe(#state{stream_mgmt = false} = State, El) ->
    send_element(State, El);
maybe_send_element_safe(State, El) ->
    case catch send_element(State, El) of
        ok -> ok;
        _ -> error
    end.

-spec send_element(state(), xmlel()) -> any().
send_element(#state{server = Server} = StateData, #xmlel{} = El) ->
    % used mostly in states other then session_established
    Acc = mongoose_acc:from_element(El),
    Acc1 = send_element(mongoose_acc:put(server, Server, Acc), El, StateData),
    mongoose_acc:get(send_result, Acc1).

%% @doc This is the termination point - from here stanza is sent to the user
-spec send_element(mongoose_acc:t(), xmlel(), state()) -> mongoose_acc:t().
send_element(Acc, El,  #state{server = Server} = StateData) ->
    Acc1 = ejabberd_hooks:run_fold(xmpp_send_element, Server, Acc, [El]),
    Res = do_send_element(El, StateData),
    mongoose_acc:record_sending(Acc1, El, c2s, Res).

do_send_element(El, #state{sockmod = SockMod} = StateData)
                when StateData#state.xml_socket ->
    mongoose_transport:send_xml(SockMod, StateData#state.socket,
        {xmlstreamelement, El});
do_send_element(El, StateData) ->
    send_text(StateData, exml:to_binary(El)).


-spec send_header(State :: state(),
                  Server :: ejabberd:server(),
                  Version :: binary(),
                  Lang :: ejabberd:lang()) -> any().
send_header(StateData, Server, Version, Lang)
  when StateData#state.xml_socket ->
    VersionAttr = case Version of
                      <<>> -> [];
                      _ -> [{<<"version">>, Version}]
                  end,
    LangAttr = case Lang of
                   <<>> -> [];
                   _ -> [{<<"xml:lang">>, Lang}]
               end,
    Header = {xmlstreamstart,
              <<"stream:stream">>,
              VersionAttr ++
              LangAttr ++
              [{<<"xmlns">>, ?NS_CLIENT},
               {<<"xmlns:stream">>, <<"http://etherx.jabber.org/streams">>},
               {<<"id">>, StateData#state.streamid},
               {<<"from">>, Server}]},
    (StateData#state.sockmod):send_xml(StateData#state.socket, Header);
send_header(StateData, Server, Version, Lang) ->
    VersionStr = case Version of
                    <<>> -> [];
                     _ -> [" version='", Version, "'"]
                 end,
    LangStr = case Lang of
                  <<>> -> [];
                  _ -> [" xml:lang='", Lang, "'"]
              end,
    Header = list_to_binary(io_lib:format(?STREAM_HEADER,
                                          [StateData#state.streamid,
                                           Server,
                                           VersionStr,
                                           LangStr])),
    send_text(StateData, Header).

-spec maybe_send_trailer_safe(State :: state()) -> any().
maybe_send_trailer_safe(#state{stream_mgmt = false} = State) ->
    send_trailer(State);
maybe_send_trailer_safe(StateData) ->
    catch send_trailer(StateData).

send_trailer(StateData) when StateData#state.xml_socket ->
    (StateData#state.sockmod):send_xml(StateData#state.socket,
                                       {xmlstreamend, <<"stream:stream">>});
send_trailer(StateData) ->
    send_text(StateData, ?STREAM_TRAILER).


-spec send_and_maybe_buffer_stanza(mongoose_acc:t(), packet(), state()) ->
    {ok | resume, mongoose_acc:t(), state()}.
send_and_maybe_buffer_stanza(Acc, {J1, J2, El}, State)->
    % to be removed
    {SendResult, BufferedStateData} =
        send_and_maybe_buffer_stanza({J1, J2, mod_amp:strip_amp_el_from_request(El)}, State),
    mod_amp:check_packet(El, result_to_amp_event(SendResult)),
    case SendResult of
        ok ->
            case catch maybe_send_ack_request(BufferedStateData) of
                R when is_boolean(R) ->
                    {ok, Acc, BufferedStateData};
                _ ->
                    ?DEBUG("Send ack request error: ~p, try enter resume session", [SendResult]),
                    {resume, Acc, BufferedStateData}
            end;
        _ ->
            ?DEBUG("Send element error: ~p, try enter resume session", [SendResult]),
            {resume, Acc, BufferedStateData}
    end.

result_to_amp_event(ok) -> delivered;
result_to_amp_event(_) -> delivery_failed.

-spec send_and_maybe_buffer_stanza(packet(), state()) ->
    {ok | any(), state()}.
send_and_maybe_buffer_stanza({_, _, Stanza} = Packet, State) ->
    SendResult = maybe_send_element_safe(State, Stanza),
    BufferedStateData = buffer_out_stanza(Packet, State),
    {SendResult, BufferedStateData}.

-spec new_id() -> binary().
new_id() ->
    iolist_to_binary(randoms:get_string()).


-spec is_auth_packet(El :: jlib:xmlel()) -> boolean().
is_auth_packet(El) ->
    case jlib:iq_query_info(El) of
        #iq{id = ID, type = Type, xmlns = ?NS_AUTH, sub_el = SubEl} ->
            #xmlel{children = Els} = SubEl,
            {auth, ID, Type,
             get_auth_tags(Els, <<>>, <<>>, <<>>, <<>>)};
        _ ->
            false
    end.


-spec get_auth_tags(Els :: [jlib:xmlel()], _, _, _, _) -> {_, _, _, _}.
get_auth_tags([#xmlel{name = Name, children = Els}| L], U, P, D, R) ->
    CData = xml:get_cdata(Els),
    case Name of
        <<"username">> ->
            get_auth_tags(L, CData, P, D, R);
        <<"password">> ->
            get_auth_tags(L, U, CData, D, R);
        <<"digest">> ->
            get_auth_tags(L, U, P, CData, R);
        <<"resource">> ->
            get_auth_tags(L, U, P, D, CData);
        _ ->
            get_auth_tags(L, U, P, D, R)
    end;
get_auth_tags([_ | L], U, P, D, R) ->
    get_auth_tags(L, U, P, D, R);
get_auth_tags([], U, P, D, R) ->
    {U, P, D, R}.


%% Copied from ejabberd_socket.erl
-record(socket_state, {sockmod, socket, receiver}).

-spec get_conn_type(state()) -> conntype().
get_conn_type(StateData) ->
    case (StateData#state.sockmod):get_sockmod(StateData#state.socket) of
        gen_tcp -> c2s;
        fast_tls -> c2s_tls;
        ejabberd_zlib ->
            case ejabberd_zlib:get_sockmod((StateData#state.socket)#socket_state.socket) of
                gen_tcp -> c2s_compressed;
                fast_tls -> c2s_compressed_tls
            end;
        ejabberd_http_poll -> http_poll;
        ejabberd_http_bind -> http_bind;
        _ -> unknown
    end.


-spec process_presence_probe(From :: ejabberd:simple_jid() | ejabberd:jid(),
                             To :: ejabberd:jid(),
                             Acc :: mongoose_acc:t(),
                             State :: state()) -> mongoose_acc:t().
process_presence_probe(From, To, Acc, StateData) ->
    LFrom = jid:to_lower(From),
    LBareFrom = setelement(3, LFrom, <<>>),
    case StateData#state.pres_last of
        undefined ->
            Acc;
        _ ->
            case {should_retransmit_last_presence(LFrom, LBareFrom, StateData),
                  specifically_visible_to(LFrom, StateData)} of
                {true, _} ->
                    Timestamp = StateData#state.pres_timestamp,
                    Packet = xml:append_subtags(
                               StateData#state.pres_last,
                               %% To is the one sending the presence (the target of the probe)
                               [jlib:timestamp_to_xml(Timestamp, utc, To, <<>>)]),
                    check_privacy_and_route_probe(StateData, From, To, Acc, Packet);
                {false, true} ->
                    ejabberd_router:route(To, From, Acc, #xmlel{name = <<"presence">>});
                _ ->
                    Acc
            end
    end.

-spec check_privacy_and_route_probe(StateData :: state(),
                                    From :: ejabberd:jid(),
                                    To :: ejabberd:jid(),
                                    Acc :: mongoose_acc:t(),
                                    Packet :: exml:element()) -> mongoose_acc:t().
check_privacy_and_route_probe(StateData, From, To, Acc, Packet) ->
    {Acc1, Res} = privacy_check_packet(Acc, Packet, To, From, out, StateData),
    case Res of
        allow ->
            Pid = element(2, StateData#state.sid),
            Acc2 = ejabberd_hooks:run_fold(presence_probe_hook,
                               StateData#state.server,
                                Acc1,
                               [From, To, Pid]),
            %% Don't route a presence probe to oneself
            case jid:are_equal(From, To) of
                false ->
                    ejabberd_router:route(To, From, Acc2, Packet);
                true ->
                    Acc2
            end;
        _ ->
            Acc1
    end.

should_retransmit_last_presence(LFrom, LBareFrom,
                                #state{pres_invis = Invisible} = S) ->
    not Invisible
    andalso is_subscribed_to_my_presence(LFrom, LBareFrom, S)
    andalso not invisible_to(LFrom, LBareFrom, S).

is_subscribed_to_my_presence(JID, S) ->
    {Lowcase, Bare} = lowcase_and_bare(JID),
    is_subscribed_to_my_presence(Lowcase, Bare, S).

is_subscribed_to_my_presence(LFrom, LBareFrom, S) ->
    gb_sets:is_element(LFrom, S#state.pres_f)
    orelse (LFrom /= LBareFrom)
    andalso gb_sets:is_element(LBareFrom, S#state.pres_f).

am_i_subscribed_to_presence(LJID, LBareJID, S) ->
    gb_sets:is_element(LJID, S#state.pres_t)
    orelse (LJID /= LBareJID)
    andalso gb_sets:is_element(LBareJID, S#state.pres_t).

lowcase_and_bare(JID) ->
    LJID = jid:to_lower(JID),
    { LJID, jid:to_bare(LJID)}.

invisible_to(LFrom, LBareFrom, S) ->
    gb_sets:is_element(LFrom, S#state.pres_i)
    orelse (LFrom /= LBareFrom)
    andalso gb_sets:is_element(LBareFrom, S#state.pres_i).

%% @doc Is generally invisible, but visible to a particular resource?
specifically_visible_to(LFrom, #state{pres_invis = Invisible} = S) ->
    Invisible
    andalso gb_sets:is_element(LFrom, S#state.pres_f)
    andalso gb_sets:is_element(LFrom, S#state.pres_a).

%% @doc User updates his presence (non-directed presence packet)
-spec presence_update(Acc :: mongoose_acc:t(),
                      From :: 'undefined' | ejabberd:jid(),
                      State :: state()) -> {mongoose_acc:t(), state()}.
presence_update(Acc, From, StateData) ->
    Packet = mongoose_acc:get(element, Acc),
    case mongoose_acc:get(type, Acc) of
        <<"unavailable">> ->
            Status = case xml:get_subtag(Packet, <<"status">>) of
                         false ->
                             <<>>;
                         StatusTag ->
                             xml:get_tag_cdata(StatusTag)
                     end,
            Info = [{ip, StateData#state.ip}, {conn, StateData#state.conn},
                    {auth_module, StateData#state.auth_module}],
            Acc1 = ejabberd_sm:unset_presence(Acc,
                                              StateData#state.sid,
                                              StateData#state.user,
                                              StateData#state.server,
                                              StateData#state.resource,
                                              Status,
                                              Info),
            Acc2 = presence_broadcast(Acc1, StateData#state.pres_a, StateData),
            Acc3 = presence_broadcast(Acc2, StateData#state.pres_i, StateData),
            % and here we reach the end
            {Acc3, StateData#state{pres_last = undefined,
                                   pres_timestamp = undefined,
                                   pres_a = gb_sets:new(),
                                   pres_i = gb_sets:new(),
                                   pres_invis = false}};
        <<"invisible">> ->
            NewPriority = get_priority_from_presence(Packet),
            Acc0 = update_priority(Acc, NewPriority, Packet, StateData),
            case StateData#state.pres_invis of
                false ->
                    Acc1 = presence_broadcast(Acc0,
                                              StateData#state.pres_a,
                                              StateData),
                    Acc2 = presence_broadcast(Acc1,
                                              StateData#state.pres_i,
                                              StateData),
                    S1 = StateData#state{pres_last = undefined,
                                         pres_timestamp = undefined,
                                         pres_a = gb_sets:new(),
                                         pres_i = gb_sets:new(),
                                         pres_invis = true},
                    presence_broadcast_first(Acc2, From, S1, Packet);
                true ->
                    {Acc0, StateData}
            end;
        <<"error">> ->
            {Acc, StateData};
        <<"probe">> ->
            {Acc, StateData};
        <<"subscribe">> ->
            {Acc, StateData};
        <<"subscribed">> ->
            {Acc, StateData};
        <<"unsubscribe">> ->
            {Acc, StateData};
        <<"unsubscribed">> ->
            {Acc, StateData};
        _ ->
            presence_update_to_available(Acc, From, Packet, StateData)
    end.

-spec presence_update_to_available(Acc :: mongoose_acc:t(),
                                   From :: ejabberd:jid(),
                                   Packet :: exml:element(),
                                   StateData :: state()) -> {mongoose_acc:t(), state()}.
presence_update_to_available(Acc, From, Packet, StateData) ->
    OldPriority = case StateData#state.pres_last of
                      undefined ->
                          0;
                      OldPresence ->
                          get_priority_from_presence(OldPresence)
                  end,
    NewPriority = get_priority_from_presence(Packet),
    Timestamp = calendar:now_to_universal_time(os:timestamp()),
    Acc1 = update_priority(Acc, NewPriority, Packet, StateData),

    NewStateData = StateData#state{pres_last = Packet,
                                   pres_invis = false,
                                   pres_timestamp = Timestamp},

    FromUnavail = (StateData#state.pres_last == undefined) or StateData#state.pres_invis,
    ?DEBUG("from unavail = ~p~n", [FromUnavail]),
    presence_update_to_available(FromUnavail, Acc1, OldPriority, NewPriority, From,
                                 Packet, NewStateData).

%% @doc the first one is run when presence changes from unavailable to anything else
-spec presence_update_to_available(FromUnavailable :: boolean(),
                                   Acc :: mongoose_acc:t(),
                                   OldPriority :: integer(),
                                   NewPriority :: integer(),
                                   From :: ejabberd:jid(),
                                   Packet :: exml:element(),
                                   StateData :: state()) -> {mongoose_acc:t(), state()}.
presence_update_to_available(true, Acc, _, NewPriority, From, Packet, StateData) ->
    Acc2 = ejabberd_hooks:run_fold(user_available_hook,
                                   StateData#state.server,
                                   Acc,
                                   [StateData#state.jid]),
    Res = case NewPriority >= 0 of
              true ->
                  Acc3 = ejabberd_hooks:run_fold(roster_get_subscription_lists,
                                                 StateData#state.server,
                                                 Acc2,
                                                 [StateData#state.user,
                                                 StateData#state.server]),
                  {_, _, Pending} = mongoose_acc:get(subscription_lists, Acc3, {[], [], []}),
                  Acc4 = resend_offline_messages(Acc3, StateData),
                  resend_subscription_requests(Acc4,
                                               StateData#state{pending_invitations = Pending});
              false ->
                  {Acc2, StateData}
              end,
    {Accum, NewStateData1} = Res,
    presence_broadcast_first(Accum, From, NewStateData1, Packet);
presence_update_to_available(false, Acc, OldPriority, NewPriority, From, Packet, StateData) ->
    Acc2 = presence_broadcast_to_trusted(Acc,
                                         StateData,
                                         From,
                                         StateData#state.pres_f,
                                         StateData#state.pres_a,
                                         Packet),
    Acc3 = case OldPriority < 0 andalso NewPriority >= 0 of
               true ->
                   resend_offline_messages(Acc2, StateData);
               false ->
                   Acc2
           end,
    {Acc3, StateData}.

%% @doc User sends a directed presence packet
-spec presence_track(Acc :: mongoose_acc:t(),
                     State :: state()) -> {mongoose_acc:t(), state()}.
presence_track(Acc, StateData) ->
    To = mongoose_acc:get(to_jid, Acc),
    From = mongoose_acc:get(from_jid, Acc),
    LTo = jid:to_lower(To),
    User = StateData#state.user,
    Server = StateData#state.server,
    case mongoose_acc:get(type, Acc) of
        <<"unavailable">> ->
            Acc1 = check_privacy_and_route(Acc, StateData),
            I = gb_sets:del_element(LTo, StateData#state.pres_i),
            A = gb_sets:del_element(LTo, StateData#state.pres_a),
            {Acc1, StateData#state{pres_i = I,
                                   pres_a = A}};
        <<"invisible">> ->
            Acc1 = check_privacy_and_route(Acc, StateData),
            I = gb_sets:add_element(LTo, StateData#state.pres_i),
            A = gb_sets:del_element(LTo, StateData#state.pres_a),
            {Acc1, StateData#state{pres_i = I,
                                   pres_a = A}};
        <<"subscribe">> ->
            Acc2 = ejabberd_hooks:run_fold(roster_out_subscription,
                                           Server,
                                           Acc,
                                           [User, Server, To, subscribe]),
            Acc3 = check_privacy_and_route(Acc2, jid:to_bare(From), StateData),
            {Acc3, StateData};
        <<"subscribed">> ->
            Acc2 = ejabberd_hooks:run_fold(roster_out_subscription,
                                           Server,
                                           Acc,
                                           [User, Server, To, subscribed]),
            Acc3 = check_privacy_and_route(Acc2, jid:to_bare(From), StateData),
            {Acc3, StateData};
        <<"unsubscribe">> ->
            Acc2 = ejabberd_hooks:run_fold(roster_out_subscription,
                                           Server,
                                           Acc,
                                           [User, Server, To, unsubscribe]),
            Acc3 = check_privacy_and_route(Acc2, jid:to_bare(From), StateData),
            {Acc3, StateData};
        <<"unsubscribed">> ->
            Acc2 = ejabberd_hooks:run_fold(roster_out_subscription,
                                           Server,
                                           Acc,
                                           [User, Server, To, unsubscribed]),
            Acc3 = check_privacy_and_route(Acc2, jid:to_bare(From), StateData),
            {Acc3, StateData};
        <<"error">> ->
            Acc1 = check_privacy_and_route(Acc, StateData),
            {Acc1, StateData};
        <<"probe">> ->
            Acc1 = check_privacy_and_route(Acc, StateData),
            {Acc1, StateData};
        _ ->
            Acc1 = check_privacy_and_route(Acc, StateData),
            I = gb_sets:del_element(LTo, StateData#state.pres_i),
            A = gb_sets:add_element(LTo, StateData#state.pres_a),
            {Acc1, StateData#state{pres_i = I,
                                   pres_a = A}}
    end.

-spec check_privacy_and_route(Acc :: mongoose_acc:t(),
                              StateData :: state()) -> mongoose_acc:t().
check_privacy_and_route(Acc, StateData) ->
    check_privacy_and_route(Acc, mongoose_acc:get(from_jid, Acc), StateData).

-spec check_privacy_and_route(Acc :: mongoose_acc:t(),
                              FromRoute :: ejabberd:jid(),
                              StateData :: state()) -> mongoose_acc:t().
check_privacy_and_route(Acc, FromRoute, StateData) ->
    From = mongoose_acc:get(from_jid, Acc),
    To = mongoose_acc:get(to_jid, Acc),
    {Acc1, Res} = privacy_check_packet(Acc, To, out, StateData),
    Packet = mongoose_acc:get(element, Acc1),
    case Res of
       deny ->
           Err = jlib:make_error_reply(Packet, ?ERR_NOT_ACCEPTABLE_CANCEL),
           ejabberd_router:route(To, From, Acc1, Err);
       block ->
           Err = jlib:make_error_reply(Packet, ?ERR_NOT_ACCEPTABLE_BLOCKED),
           ejabberd_router:route(To, From, Acc1, Err);
       allow ->
           ejabberd_router:route(FromRoute, To, Acc1, Packet)
   end.


-spec privacy_check_packet(StateData :: state(),
                           From :: ejabberd:jid(),
                           To :: ejabberd:jid(),
                           Packet :: jlib:xmlel(),
                           Dir :: 'in' | 'out') -> allow|deny|block.
privacy_check_packet(StateData, From, To, #xmlel{} = Packet, Dir) ->
    ?DEPRECATED, % but triggered by routed brodcast
    Acc = mongoose_acc:from_element(Packet),
    Acc1 = mongoose_acc:put(from_jid, From, Acc),
    {_, Res} = privacy_check_packet(Acc1, To, Dir, StateData),
    Res.

-spec privacy_check_packet(Acc :: mongoose_acc:t(),
                           To :: ejabberd:jid(),
                           Dir :: 'in' | 'out',
                           StateData :: state()) -> {mongoose_acc:t(), allow|deny|block}.
privacy_check_packet(Acc, To, Dir, StateData) ->
    mongoose_privacy:privacy_check_packet(Acc,
                                          StateData#state.server,
                                          StateData#state.user,
                                          StateData#state.privacy_list,
                                          To,
                                          Dir).

-spec privacy_check_packet(Acc :: mongoose_acc:t(),
                           Packet :: xmlel(),
                           From :: ejabberd:jid(),
                           To :: ejabberd:jid(),
                           Dir :: 'in' | 'out',
                           StateData :: state()) -> {mongoose_acc:t(), allow|deny|block}.
privacy_check_packet(Acc, Packet, From, To, Dir, StateData) ->
    mongoose_privacy:privacy_check_packet({Acc, Packet},
                                          StateData#state.server,
                                          StateData#state.user,
                                          StateData#state.privacy_list,
                                          From,
                                          To,
                                          Dir).

-spec presence_broadcast(Acc :: mongoose_acc:t(),
                         JIDSet :: jid_set(),
                         State :: state()) -> mongoose_acc:t().
presence_broadcast(Acc, JIDSet, StateData) ->
    From = mongoose_acc:get(from_jid, Acc),
    lists:foldl(fun(JID, A) ->
                          FJID = jid:make(JID),
                          {A1, Res} = privacy_check_packet(A, FJID, out, StateData),
                          case Res of
                              allow ->
                                  ejabberd_router:route(From, FJID, A1);
                              _ ->
                                  A1
                          end
                  end, Acc, gb_sets:to_list(JIDSet)).

-spec presence_broadcast_to_trusted(Acc :: mongoose_acc:t(),
                                    State :: state(),
                                    From :: 'undefined' | ejabberd:jid(),
                                    T :: jid_set(),
                                    A :: jid_set(),
                                    Packet :: jlib:xmlel()) -> mongoose_acc:t().
presence_broadcast_to_trusted(Acc, StateData, From, T, A, Packet) ->
    lists:foldl(
      fun(JID, Ac) ->
              case gb_sets:is_element(JID, T) of
                  true ->
                      FJID = jid:make(JID),
                      check_privacy_and_route_or_ignore(Ac, StateData, From, FJID, Packet, out);
                  _ ->
                      Ac
              end
      end, Acc, gb_sets:to_list(A)).

-spec presence_broadcast_first(mongoose_acc:t(),
                               From :: 'undefined' | ejabberd:jid(),
                               State :: state(),
                               Packet :: jlib:xmlel()) -> {mongoose_acc:t(), state()}.
presence_broadcast_first(Acc0, From, StateData, Packet) ->
    Stanza = #xmlel{name = <<"presence">>,
        attrs = [{<<"type">>, <<"probe">>}]},
    Acc = gb_sets:fold(fun(JID, A) ->
                           ejabberd_router:route(From, jid:make(JID), A, Stanza)
                       end,
               Acc0,
               StateData#state.pres_t),
    case StateData#state.pres_invis of
        true ->
            {Acc, StateData};
        false ->
            {As, AccFinal} = gb_sets:fold(
                   fun(JID, {A, Accum}) ->
                           FJID = jid:make(JID),
                           Accum1 = check_privacy_and_route_or_ignore(Accum, StateData, From, FJID,
                                                             Packet, out),
                           {gb_sets:add_element(JID, A), Accum1}
                   end,
                   {StateData#state.pres_a, Acc},
                   StateData#state.pres_f),
            {AccFinal, StateData#state{pres_a = As}}
    end.

-spec roster_change(IJID :: ejabberd:simple_jid() | ejabberd:jid(),
                    ISubscription :: from | to | both | none,
                    State :: state()) -> state().
roster_change(IJID, ISubscription, StateData) ->
    LIJID = jid:to_lower(IJID),
    IsSubscribedToMe = (ISubscription == both) or (ISubscription == from),
    AmISubscribedTo = (ISubscription == both) or (ISubscription == to),
    WasSubscribedToMe = gb_sets:is_element(LIJID, StateData#state.pres_f),
    FSet = case IsSubscribedToMe of
               true ->
                   gb_sets:add_element(LIJID, StateData#state.pres_f);
               false ->
                   gb_sets:del_element(LIJID, StateData#state.pres_f)
           end,
    TSet = case AmISubscribedTo of
               true ->
                   gb_sets:add_element(LIJID, StateData#state.pres_t);
               false ->
                   gb_sets:del_element(LIJID, StateData#state.pres_t)
           end,
    case StateData#state.pres_last of
        undefined ->
            StateData#state{pres_f = FSet, pres_t = TSet};
        P ->
            ?DEBUG("roster changed for ~p~n", [StateData#state.user]),
            From = StateData#state.jid,
            To = jid:make(IJID),
            IsntInvisible = not StateData#state.pres_invis,
            ImAvailableTo = gb_sets:is_element(LIJID, StateData#state.pres_a),
            ImInvisibleTo = gb_sets:is_element(LIJID, StateData#state.pres_i),
            BecomeAvailable = IsntInvisible and IsSubscribedToMe and not WasSubscribedToMe,
            BecomeUnavailable = not IsSubscribedToMe and WasSubscribedToMe
                                and (ImAvailableTo or ImInvisibleTo),
            case {BecomeAvailable, BecomeUnavailable} of
                {true, _} ->
                    ?DEBUG("become_available_to: ~p~n", [LIJID]),
                    check_privacy_and_route_or_ignore(StateData, From, To, P, out),
                    A = gb_sets:add_element(LIJID,
                                          StateData#state.pres_a),
                    StateData#state{pres_a = A,
                                    pres_f = FSet,
                                    pres_t = TSet};
                {_, true} ->
                    ?DEBUG("become_unavailable_to: ~p~n", [LIJID]),
                    PU = #xmlel{name = <<"presence">>,
                                attrs = [{<<"type">>, <<"unavailable">>}]},
                    check_privacy_and_route_or_ignore(StateData, From, To, PU, out),
                    I = gb_sets:del_element(LIJID,
                                       StateData#state.pres_i),
                    A = gb_sets:del_element(LIJID,
                                       StateData#state.pres_a),
                    StateData#state{pres_i = I,
                                    pres_a = A,
                                    pres_f = FSet,
                                    pres_t = TSet};
                _ ->
                    StateData#state{pres_f = FSet, pres_t = TSet}
            end
    end.

-spec update_priority(Acc :: mongoose_acc:t(),
                      Priority :: integer(),
                      Packet :: jlib:xmlel(),
                      State :: state()) -> mongoose_acc:t().
update_priority(Acc, Priority, Packet, StateData) ->
    Info = [{ip, StateData#state.ip}, {conn, StateData#state.conn},
            {auth_module, StateData#state.auth_module}],
    ejabberd_sm:set_presence(Acc,
                             StateData#state.sid,
                             StateData#state.user,
                             StateData#state.server,
                             StateData#state.resource,
                             Priority,
                             Packet,
                             Info).


-spec get_priority_from_presence(Packet :: jlib:xmlel()) -> integer().
get_priority_from_presence(undefined) ->
    0;
get_priority_from_presence(PresencePacket) ->
    case xml:get_subtag(PresencePacket, <<"priority">>) of
        false ->
            0;
        SubEl ->
            case catch list_to_integer(binary_to_list(xml:get_tag_cdata(SubEl))) of
                P when is_integer(P) ->
                    P;
                _ ->
                    0
            end
    end.

-spec process_privacy_iq(Acc :: mongoose_acc:t(),
                         To :: ejabberd:jid(),
                         StateData :: state()) -> {mongoose_acc:t(), state()}.
process_privacy_iq(Acc0, To, StateData) ->
    Acc = mongoose_acc:require(iq_query_info, Acc0),
    IQ = mongoose_acc:get(iq_query_info, Acc),
    Acc1 = mongoose_acc:put(iq, IQ, Acc),
    From = mongoose_acc:get(from_jid, Acc1),
    #iq{type = Type, sub_el = SubEl} = IQ,
    {Acc2, NewStateData} = process_privacy_iq(Acc1, Type, To, StateData),
    Res = mongoose_acc:get(iq_result, Acc2, {error, ?ERR_FEATURE_NOT_IMPLEMENTED}),
    IQRes = case Res of
                {result, Result} ->
                    IQ#iq{type = result, sub_el = Result};
                {result, Result, _} ->
                    IQ#iq{type = result, sub_el = Result};
                {error, Error} ->
                    IQ#iq{type = error, sub_el = [SubEl, Error]}
            end,
    Acc3 = ejabberd_router:route(To, From, Acc2, jlib:iq_to_xml(IQRes)),
    {Acc3, NewStateData}.

-spec process_privacy_iq(Acc :: mongoose_acc:t(),
                         Type :: get | set,
                         To :: ejabberd:jid(),
                         StateData :: state()) -> {mongoose_acc:t(), state()}.
process_privacy_iq(Acc, get, To, StateData) ->
    From = mongoose_acc:get(from_jid, Acc),
    IQ = mongoose_acc:get(iq, Acc),
    Acc1 = ejabberd_hooks:run_fold(privacy_iq_get,
                                   StateData#state.server,
                                   Acc,
                                   [From, To, IQ, StateData#state.privacy_list]),
    {Acc1, StateData};
process_privacy_iq(Acc, set, To, StateData) ->
    From = mongoose_acc:get(from_jid, Acc),
    IQ = mongoose_acc:get(iq, Acc),
    Acc1 = ejabberd_hooks:run_fold(privacy_iq_set,
                                   StateData#state.server,
                                   Acc,
                                   [From, To, IQ]),
    case mongoose_acc:get(iq_result, Acc1, undefined) of
        {result, _, NewPrivList} ->
            maybe_update_presence(StateData, NewPrivList),
            NState = StateData#state{privacy_list = NewPrivList},
            {Acc1, NState};
        _ -> {Acc1, StateData}
    end.


-spec resend_offline_messages(mongoose_acc:t(), state()) -> mongoose_acc:t().
resend_offline_messages(Acc, StateData) ->
    ?DEBUG("resend offline messages~n", []),
    Acc1 = ejabberd_hooks:run_fold(resend_offline_messages_hook,
                                   StateData#state.server,
                                   Acc,
                                   [StateData#state.user, StateData#state.server]),
    Rs = mongoose_acc:get(offline_messages, Acc1, []),
    Acc2 = lists:foldl(
                       fun({route, From, To, Packet}, A) ->
                           resend_offline_message(A, StateData, From, To, Packet, in)
                       end,
                       Acc1,
                       Rs),
    mongoose_acc:remove(offline_messages, Acc2). % they are gone from db backend and sent


resend_offline_message(A, StateData, From, To, Packet, in) ->
    % this is one of very few (maybe the only) place where we have to tweak
    % basic accumulator properties - we are sending various messages only because we
    % received a presence (plus, sometimes the acc is empty)
    % all we leave are system stuff like ref and timestamp
    #xmlel{name = Name} = Packet,
    Type = exml_query:attr(Packet, <<"type">>),
    M = #{name => Name, type => Type, element => Packet, from_jid => From,
        from => jid:to_binary(From), to_jid => To, to => jid:to_binary(To)},
    Acc = mongoose_acc:update(A, M),
    check_privacy_and_route_or_ignore(Acc, StateData, From, To, Packet, in).


-spec check_privacy_and_route_or_ignore(StateData :: state(),
                                        From :: ejabberd:jid(),
                                        To :: ejabberd:jid(),
                                        Packet :: exml:element(),
                                        Dir :: in | out) -> any().
check_privacy_and_route_or_ignore(StateData, From, To, Packet, Dir) ->
    ?DEPRECATED, % but triggered by incoming roster change broadcast
    case privacy_check_packet(StateData, From, To, Packet, Dir) of
        allow -> ejabberd_router:route(From, To, Packet);
        _ -> ok
    end.

-spec check_privacy_and_route_or_ignore(Acc :: mongoose_acc:t(),
                                        StateData :: state(),
                                        From :: ejabberd:jid(),
                                        To :: ejabberd:jid(),
                                        Packet :: exml:element(),
                                        Dir :: in | out) -> any().
check_privacy_and_route_or_ignore(Acc, StateData, From, To, Packet, Dir) ->
    {Acc2, Res} = privacy_check_packet(Acc, Packet, From, To, Dir, StateData),
    case Res of
        allow -> ejabberd_router:route(From, To, Acc2, Packet);
        _ -> Acc2
    end.

-spec resend_subscription_requests(mongoose_acc:t(), state()) -> {mongoose_acc:t(), state()}.
resend_subscription_requests(Acc, #state{pending_invitations = Pending} = StateData) ->
    {NewAcc, NewState} = lists:foldl(
                 fun(XMLPacket, {A, #state{} = State}) ->
                         A1 = send_element(A, XMLPacket, State),
                         {value, From} =  xml:get_tag_attr(<<"from">>, XMLPacket),
                         {value, To} = xml:get_tag_attr(<<"to">>, XMLPacket),
                         BufferedStateData = buffer_out_stanza({From, To, XMLPacket}, State),
                         % this one will be next to tackle
                         maybe_send_ack_request(BufferedStateData),
                         {A1, BufferedStateData}
                 end, {Acc, StateData}, Pending),
    {NewAcc, NewState#state{pending_invitations = []}}.


get_showtag(undefined) ->
    <<"unavailable">>;
get_showtag(Presence) ->
    case xml:get_path_s(Presence, [{elem, <<"show">>}, cdata]) of
        <<>> -> <<"available">>;
        ShowTag -> ShowTag
    end.


get_statustag(undefined) ->
    <<>>;
get_statustag(Presence) ->
    case xml:get_path_s(Presence, [{elem, <<"status">>}, cdata]) of
        ShowTag -> ShowTag
    end.


-spec process_unauthenticated_stanza(State :: state(),
                                     El :: jlib:xmlel()) -> any().
process_unauthenticated_stanza(StateData, El) ->
    NewEl = case xml:get_tag_attr_s(<<"xml:lang">>, El) of
                <<>> ->
                    case StateData#state.lang of
                        <<>> -> El;
                        Lang ->
                            xml:replace_tag_attr(<<"xml:lang">>, Lang, El)
                    end;
                _ ->
                    El
            end,
    case jlib:iq_query_info(NewEl) of
        #iq{} = IQ ->
            Res = ejabberd_hooks:run_fold(c2s_unauthenticated_iq,
                                          StateData#state.server,
                                          empty,
                                          [StateData#state.server, IQ,
                                           StateData#state.ip]),
            case Res of
                empty ->
                    % The only reasonable IQ's here are auth and register IQ's
                    % They contain secrets, so don't include subelements to response
                    ResIQ = IQ#iq{type = error,
                                  sub_el = [?ERR_SERVICE_UNAVAILABLE]},
                    Res1 = jlib:replace_from_to(
                             jid:make(<<>>, StateData#state.server, <<>>),
                             jid:make(<<>>, <<>>, <<>>),
                             jlib:iq_to_xml(ResIQ)),
                    send_element(StateData, jlib:remove_attr(<<"to">>, Res1));
                _ ->
                    send_element(StateData, Res)
            end;
        _ ->
            % Drop any stanza, which isn't IQ stanza
            ok
    end.


-spec peerip(SockMod :: ejabberd:sockmod(), inet:socket())
-> undefined | {inet:ip_address(), inet:port_number()}.
peerip(SockMod, Socket) ->
    IP = case SockMod of
             gen_tcp -> inet:peername(Socket);
             _ -> mongoose_transport:peername(SockMod, Socket)
         end,
    case IP of
        {ok, IPOK} -> IPOK;
        _ -> undefined
    end.


%% @doc fsm_next_state_pack: Pack the StateData structure to improve sharing.
fsm_next_state_pack(StateName, StateData) ->
    fsm_next_state_gc(StateName, pack(StateData)).


%% @doc fsm_next_state_gc: Garbage collect the process heap to make use of
%% the newly packed StateData structure.
fsm_next_state_gc(StateName, PackedStateData) ->
    erlang:garbage_collect(),
    fsm_next_state(StateName, PackedStateData).


%% @doc fsm_next_state: Generate the next_state FSM tuple with different
%% timeout, depending on the future state
fsm_next_state(session_established, StateData) ->
    {next_state, session_established, StateData, ?C2S_HIBERNATE_TIMEOUT};
fsm_next_state(StateName, StateData) ->
    {next_state, StateName, StateData, ?C2S_OPEN_TIMEOUT}.


%% @doc fsm_reply: Generate the reply FSM tuple with different timeout,
%% depending on the future state
fsm_reply(Reply, session_established, StateData) ->
    {reply, Reply, session_established, StateData, ?C2S_HIBERNATE_TIMEOUT};
fsm_reply(Reply, StateName, StateData) ->
    {reply, Reply, StateName, StateData, ?C2S_OPEN_TIMEOUT}.


%% @doc Used by c2s blacklist plugins
-spec is_ip_blacklisted('undefined' | {inet:ip_address(), inet:port_number()}
                       ) -> boolean().
is_ip_blacklisted(undefined) ->
    false;
is_ip_blacklisted({IP, _Port}) ->
    ejabberd_hooks:run_fold(check_bl_c2s, false, [IP]).


%% @doc Check from attributes.
-spec check_from(El, C2SJID) -> Result when
      El :: jlib:xmlel(), C2SJID :: ejabberd:jid(),
      Result :: 'invalid-from'  | jlib:xmlel().
check_from(El, #jid{ luser = C2SU, lserver = C2SS, lresource = C2SR }) ->
    case xml:get_tag_attr(<<"from">>, El) of
        false ->
            El;
        {value, SJID} ->
            case jid:from_binary(SJID) of
                #jid{ luser = U, lserver = S, lresource = R }
                  when U == C2SU andalso S == C2SS andalso (R == C2SR orelse R == <<>>) -> El;
                _ -> 'invalid-from'
            end
    end.


fsm_limit_opts(Opts) ->
    case lists:keyfind(max_fsm_queue, 1, Opts) of
        {_, N} when is_integer(N) ->
            [{max_queue, N}];
        _ ->
            case ejabberd_config:get_local_option(max_fsm_queue) of
                N when is_integer(N) ->
                    [{max_queue, N}];
                _ ->
                    []
            end
    end.


-spec bounce_messages() -> 'ok'.
bounce_messages() ->
    receive
        {route, From, To, El} ->
            ejabberd_router:route(From, To, El),
            bounce_messages();
        {store_session_info, User, Server, Resource, KV, _FromPid} ->
            ejabberd_sm:store_info(User, Server, Resource, KV)
    after 0 ->
              ok
    end.

%% Return the messages in reverse order than they were received in!
flush_messages() ->
    flush_messages(0, []).

flush_messages(N, Acc) ->
    receive
        {route, _, _, _} = Msg ->
            flush_messages(N+1, [Msg | Acc])
    after 0 ->
              {N, Acc}
    end.


%%%----------------------------------------------------------------------
%%% XEP-0016
%%%----------------------------------------------------------------------

maybe_update_presence(StateData = #state{jid = JID, pres_f = Froms}, NewList) ->
    % Our own jid is added to pres_f, even though we're not a "contact", so for
    % the purposes of this check we don't want it:
    SelfJID = jid:to_lower(jid:to_bare(JID)),
    FromsExceptSelf = gb_sets:del_element(SelfJID, Froms),

    gb_sets:fold(
      fun(T, _) ->
              send_unavail_if_newly_blocked(StateData, jid:make(T), NewList),
              ok
      end, ok, FromsExceptSelf).

send_unavail_if_newly_blocked(StateData = #state{jid = JID},
                              ContactJID, NewList) ->
    ?DEPRECATED, % but triggered by routed brodcast
    Packet = #xmlel{name = <<"presence">>,
                    attrs = [{<<"type">>, <<"unavailable">>}]},
    %% WARNING: we can not use accumulator to cache privacy check result - this is
    %% the only place where the list to check against changes
    OldResult = privacy_check_packet(StateData, % CHCK
                                     JID, ContactJID, Packet, out),
    NewResult = privacy_check_packet(StateData#state{privacy_list = NewList},
                                     JID, ContactJID, Packet, out),
    send_unavail_if_newly_blocked(OldResult, NewResult, JID,
                                  ContactJID, Packet).

send_unavail_if_newly_blocked(allow, deny, From, To, Packet) ->
    ejabberd_router:route(From, To, Packet);
send_unavail_if_newly_blocked(_, _, _, _, _) ->
    ok.

%%%----------------------------------------------------------------------
%%% XEP-0191
%%%----------------------------------------------------------------------

-spec blocking_push_to_resources(Action :: blocking_type(),
                                 JIDS :: [binary()],
                                 State :: state()) -> ok.
blocking_push_to_resources(Action, JIDs, StateData) ->
    SubEl =
    case Action of
        block ->
            #xmlel{name = <<"block">>,
                   attrs = [{<<"xmlns">>, ?NS_BLOCKING}],
                   children = lists:map(
                                fun(JID) ->
                                        #xmlel{name = <<"item">>,
                                               attrs = [{<<"jid">>, JID}]}
                                end, JIDs)};
        unblock ->
            #xmlel{name = <<"unblock">>,
                   attrs = [{<<"xmlns">>, ?NS_BLOCKING}],
                   children = lists:map(
                                fun(JID) ->
                                        #xmlel{name = <<"item">>,
                                               attrs = [{<<"jid">>, JID}]}
                                end, JIDs)}
    end,
    PrivPushIQ = #iq{type = set, xmlns = ?NS_BLOCKING,
                     id = <<"push">>,
                     sub_el = [SubEl]},
    F = jid:to_bare(StateData#state.jid),
    T = StateData#state.jid,
    PrivPushEl = jlib:replace_from_to(F, T, jlib:iq_to_xml(PrivPushIQ)),
    ejabberd_router:route(F, T, PrivPushEl),
    ok.

-spec blocking_presence_to_contacts(Action :: blocking_type(),
                                    JIDs :: [binary()],
                                    State :: state()) -> ok.
blocking_presence_to_contacts(_Action, [], _StateData) ->
    ok;
blocking_presence_to_contacts(Action, [Jid|JIDs], StateData) ->
    Pres = case Action of
               block ->
                   #xmlel{name = <<"presence">>,
                       attrs = [{<<"xml:lang">>, <<"en">>}, {<<"type">>, <<"unavailable">>}]
                   };
               unblock ->
                   StateData#state.pres_last
           end,
    T = jid:from_binary(Jid),
    case is_subscribed_to_my_presence(T, StateData) of
        true ->
            F = jid:to_bare(StateData#state.jid),
            ejabberd_router:route(F, T, Pres);
        false ->
            ok
    end,
    blocking_presence_to_contacts(Action, JIDs, StateData).

-type pack_tree() :: gb_trees:tree(binary() | ejabberd:simple_jid(),
                                   binary() | ejabberd:simple_jid()).

%% @doc Try to reduce the heap footprint of the four presence sets
%% by ensuring that we re-use strings and Jids wherever possible.
-spec pack(S :: state()) -> state().
pack(S = #state{pres_a=A,
                pres_i=I,
                pres_f=F,
                pres_t=T}) ->
    {NewA, Pack1} = pack_jid_set(A, gb_trees:empty()),
    {NewI, Pack2} = pack_jid_set(I, Pack1),
    {NewF, Pack3} = pack_jid_set(F, Pack2),
    {NewT, _Pack4} = pack_jid_set(T, Pack3),
    %% Throw away Pack4 so that if we delete references to
    %% Strings or Jids in any of the sets there will be
    %% no live references for the GC to find.
    S#state{pres_a=NewA,
            pres_i=NewI,
            pres_f=NewF,
            pres_t=NewT}.


-spec pack_jid_set(Set :: jid_set(),
                   Pack :: pack_tree()) -> {jid_set(), pack_tree()}.
pack_jid_set(Set, Pack) ->
    Jids = gb_sets:to_list(Set),
    {PackedJids, NewPack} = pack_jids(Jids, Pack, []),
    {gb_sets:from_list(PackedJids), NewPack}.


-spec pack_jids([{_, _, _}], Pack :: pack_tree(), Acc :: [ejabberd:simple_jid()]) ->
    {[ejabberd:simple_jid()], pack_tree()}.
pack_jids([], Pack, Acc) -> {Acc, Pack};
pack_jids([{U, S, R}=Jid | Jids], Pack, Acc) ->
    case gb_trees:lookup(Jid, Pack) of
        {value, PackedJid} ->
            pack_jids(Jids, Pack, [PackedJid | Acc]);
        none ->
            {NewU, Pack1} = pack_string(U, Pack),
            {NewS, Pack2} = pack_string(S, Pack1),
            {NewR, Pack3} = pack_string(R, Pack2),
            NewJid = {NewU, NewS, NewR},
            NewPack = gb_trees:insert(NewJid, NewJid, Pack3),
            pack_jids(Jids, NewPack, [NewJid | Acc])
    end.


-spec pack_string(String :: binary(), Pack :: pack_tree()) -> {binary(), pack_tree()}.
pack_string(String, Pack) ->
    case gb_trees:lookup(String, Pack) of
        {value, PackedString} ->
            {PackedString, Pack};
        none ->
            {String, gb_trees:insert(String, String, Pack)}
    end.

%%%----------------------------------------------------------------------
%%% XEP-0352: Client State Indication
%%%----------------------------------------------------------------------
maybe_inactivate_session(?NS_CSI, #state{csi_state = active} = State) ->
    fsm_next_state(session_established, State#state{csi_state = inactive});
maybe_inactivate_session(_, State) ->
    fsm_next_state(session_established, State).

maybe_activate_session(?NS_CSI, #state{csi_state = inactive} = State) ->
    resend_csi_buffer(State);
maybe_activate_session(_, State) ->
    fsm_next_state(session_established, State).

resend_csi_buffer(State) ->
    NewState = flush_csi_buffer(State),
    fsm_next_state(session_established, NewState#state{csi_state=active}).

-spec ship_to_local_user(mongoose_acc:t(), packet(), state()) ->
    {ok | resume, mongoose_acc:t(), state()}.
ship_to_local_user(Acc, Packet, State) ->
    % this is coming in, no rewrite yet
    maybe_csi_inactive_optimisation(Acc, Packet, State).

-spec maybe_csi_inactive_optimisation(mongoose_acc:t(), packet(), state()) ->
    {ok | resume, mongoose_acc:t(), state()}.
maybe_csi_inactive_optimisation(Acc, Packet, #state{csi_state = active} = State) ->
    send_and_maybe_buffer_stanza(Acc, Packet, State);
maybe_csi_inactive_optimisation(Acc, Packet, #state{csi_buffer = Buffer} = State) ->
    NewBuffer = [Packet | Buffer],
    NewState = flush_or_buffer_packets(State#state{csi_buffer = NewBuffer}),
    {ok, Acc, NewState}.

flush_or_buffer_packets(State) ->
    MaxBuffSize = gen_mod:get_module_opt(State#state.server, mod_csi,
                                         buffer_max, 20),
    case length(State#state.csi_buffer) > MaxBuffSize of
        true ->
            flush_csi_buffer(State);
        _ ->
            State
    end.

-spec flush_csi_buffer(state()) -> state().
flush_csi_buffer(#state{csi_buffer = BufferOut} = State) ->
    %%lists:foldr to preserve order
    F = fun(Packet, {_, OldState}) ->
                send_and_maybe_buffer_stanza(Packet, OldState)
        end,
    {_, NewState} = lists:foldr(F, {ok, State}, BufferOut),
    NewState#state{csi_buffer = []}.

bounce_csi_buffer(#state{csi_buffer = []}) ->
    ok;
bounce_csi_buffer(#state{csi_buffer = Buffer}) ->
    re_route_packets(Buffer).
%%%----------------------------------------------------------------------
%%% XEP-0198: Stream Management
%%%----------------------------------------------------------------------

maybe_enable_stream_mgmt(NextState, El, StateData) ->
    case {xml:get_tag_attr_s(<<"xmlns">>, El),
          StateData#state.stream_mgmt,
          xml:get_tag_attr_s(<<"resume">>, El)}
    of
        {?NS_STREAM_MGNT_3, false, Resume} ->
            %% turn on
            {NewSD, EnabledEl} = case lists:member(Resume, [<<"true">>, <<"1">>]) of
                                     false ->
                                         {StateData, stream_mgmt_enabled()};
                                     true ->
                                         enable_stream_resumption(StateData)
                                 end,
            send_element(NewSD, EnabledEl),
            BufferMax = get_buffer_max(),
            AckFreq = get_ack_freq(),
            ResumeTimeout = get_resume_timeout(),
            fsm_next_state(NextState,
                           NewSD#state{stream_mgmt = true,
                                       stream_mgmt_buffer_max = BufferMax,
                                       stream_mgmt_ack_freq = AckFreq,
                                       stream_mgmt_resume_timeout = ResumeTimeout});
        {?NS_STREAM_MGNT_3, _, _} ->
            %% already on, ignore
            fsm_next_state(NextState, StateData);
        {_, _, _} ->
            %% invalid namespace
            send_element(StateData, ?INVALID_NS_ERR),
            send_trailer(StateData),
            {stop, normal, StateData}
    end.

enable_stream_resumption(SD) ->
    SMID = make_smid(),
    SID = case SD#state.sid of
              undefined -> {p1_time_compat:timestamp(), self()};
              RSID -> RSID
          end,
    ok = mod_stream_management:register_smid(SMID, SID),
    {SD#state{stream_mgmt_id = SMID, sid = SID},
     stream_mgmt_enabled([{<<"id">>, SMID}, {<<"resume">>, <<"true">>}])}.

make_smid() ->
    base64:encode(crypto:strong_rand_bytes(21)).

maybe_unexpected_sm_request(NextState, El, StateData) ->
    case xml:get_tag_attr_s(<<"xmlns">>, El) of
        ?NS_STREAM_MGNT_3 ->
            send_element(StateData, stream_mgmt_failed(<<"unexpected-request">>)),
            fsm_next_state(NextState, StateData);
        _ ->
            send_element(StateData, ?INVALID_NS_ERR),
            send_trailer(StateData),
            {stop, normal, StateData}
    end.

stream_mgmt_handle_ack(NextState, El, #state{} = SD) ->
    try
        {ns, ?NS_STREAM_MGNT_3} = {ns, xml:get_tag_attr_s(<<"xmlns">>, El)},
        Handled = binary_to_integer(xml:get_tag_attr_s(<<"h">>, El)),
        NSD = #state{} = do_handle_ack(Handled,
                                       SD#state.stream_mgmt_out_acked,
                                       SD#state.stream_mgmt_buffer,
                                       SD#state.stream_mgmt_buffer_size,
                                       SD),
        fsm_next_state(NextState, NSD)
    catch
        error:{badmatch, {ns, _}} ->
            maybe_send_element_safe(SD, ?INVALID_NS_ERR),
            maybe_send_trailer_safe(SD),
            {stop, normal, SD};
        throw:{policy_violation, Reason} ->
            maybe_send_element_safe(SD, ?POLICY_VIOLATION_ERR(SD#state.lang,
                                                              Reason)),
            maybe_send_trailer_safe(SD),
            {stop, normal, SD}
    end.

do_handle_ack(Handled, OldAcked, Buffer, BufferSize, SD) ->
    ToDrop = calc_to_drop(Handled, OldAcked),
    ToDrop > BufferSize andalso throw({policy_violation,
                                       <<"h attribute too big">>}),
    {Dropped, NewBuffer} = drop_last(ToDrop, Buffer),
    NewSize = BufferSize - Dropped,
    SD#state{stream_mgmt_out_acked = Handled,
             stream_mgmt_buffer = NewBuffer,
             stream_mgmt_buffer_size = NewSize}.

calc_to_drop(Handled, OldAcked) when Handled >= OldAcked ->
    Handled - OldAcked;
calc_to_drop(Handled, OldAcked) ->
    Handled + ?STREAM_MGMT_H_MAX - OldAcked + 1.

maybe_send_sm_ack(?NS_STREAM_MGNT_3, false, _NIncoming,
                  NextState, StateData) ->
    ?WARNING_MSG("received <r/> but stream management is off!", []),
    fsm_next_state(NextState, StateData);
maybe_send_sm_ack(?NS_STREAM_MGNT_3, true, NIncoming,
                  NextState, StateData) ->
    send_element(StateData, stream_mgmt_ack(NIncoming)),
    fsm_next_state(NextState, StateData);
maybe_send_sm_ack(_, _, _, _NextState, StateData) ->
    send_element(StateData, ?INVALID_NS_ERR),
    send_trailer(StateData),
    {stop, normal, StateData}.

maybe_increment_sm_incoming(false, StateData) ->
    StateData;
maybe_increment_sm_incoming(true, StateData) ->
    Incoming = StateData#state.stream_mgmt_in,
    StateData#state{stream_mgmt_in = increment_sm_incoming(Incoming)}.

increment_sm_incoming(Incoming) ->
    increment_sm_counter(Incoming, 1).

increment_sm_counter(Incoming, Increment)
  when Incoming + Increment >= ?STREAM_MGMT_H_MAX ->
    Increment - 1;
increment_sm_counter(Incoming, Increment) ->
    Incoming + Increment.

stream_mgmt_enabled() ->
    stream_mgmt_enabled([]).

stream_mgmt_enabled(ExtraAttrs) ->
    #xmlel{name = <<"enabled">>,
           attrs = [{<<"xmlns">>, ?NS_STREAM_MGNT_3}] ++ ExtraAttrs}.

stream_mgmt_failed(Reason) ->
    ReasonEl = #xmlel{name = Reason,
                      attrs = [{<<"xmlns">>, ?NS_STANZAS}]},
    #xmlel{name = <<"failed">>,
           attrs = [{<<"xmlns">>, ?NS_STREAM_MGNT_3}],
           children = [ReasonEl]}.

stream_mgmt_ack(NIncoming) ->
    #xmlel{name = <<"a">>,
           attrs = [{<<"xmlns">>, ?NS_STREAM_MGNT_3},
                    {<<"h">>, integer_to_binary(NIncoming)}]}.

buffer_out_stanza(_Packet, #state{stream_mgmt = false} = S) ->
    S;
buffer_out_stanza(_Packet, #state{stream_mgmt_buffer_max = no_buffer} = S) ->
    S;
buffer_out_stanza(Packet, #state{stream_mgmt_buffer = Buffer,
                                 stream_mgmt_buffer_size = BufferSize,
                                 stream_mgmt_buffer_max = BufferMax} = S) ->
    NewSize = BufferSize + 1,
    Timestamp = os:timestamp(),
    NPacket = maybe_add_timestamp(Packet, Timestamp),

    NS = case is_buffer_full(NewSize, BufferMax) of
             true ->
                 defer_resource_constraint_check(S);
             _ ->
                 S
         end,
    NS#state{stream_mgmt_buffer_size = NewSize,
             stream_mgmt_buffer = [NPacket | Buffer]}.

is_buffer_full(_BufferSize, infinity) ->
    false;
is_buffer_full(BufferSize, BufferMax) when BufferSize =< BufferMax ->
    false;
is_buffer_full(_, _) ->
    true.

%% @doc Drop last N elements from List.
%% It's not an error if N > length(List).
%% The actual number of dropped elements and an empty list is returned.
%% @end
-spec drop_last(N, List1) -> {Dropped, List2} when
      N :: non_neg_integer(),
      List1 :: list(),
      Dropped :: non_neg_integer(),
      List2 :: list().
drop_last(N, List) ->
    {ToDrop, List2} = lists:foldr(fun(E, {0, Acc}) ->
                                          {0, [E | Acc]};
                                     (_, {ToDrop, Acc}) ->
                                          {ToDrop-1, Acc}
                                  end, {N, []}, List),
    {N - ToDrop, List2}.

-spec get_buffer_max() -> pos_integer() | infinity.
get_buffer_max() ->
    mod_stream_management:get_buffer_max(?STREAM_MGMT_CACHE_MAX).

-spec get_ack_freq() -> pos_integer().
get_ack_freq() ->
    mod_stream_management:get_ack_freq(?STREAM_MGMT_ACK_FREQ).

-spec get_resume_timeout() -> pos_integer().
get_resume_timeout() ->
    mod_stream_management:get_resume_timeout(?STREAM_MGMT_RESUME_TIMEOUT).

maybe_send_ack_request(#state{stream_mgmt = false}) ->
    false;
maybe_send_ack_request(#state{stream_mgmt_ack_freq = never}) ->
    false;
maybe_send_ack_request(#state{stream_mgmt_out_acked = Out,
                              stream_mgmt_buffer_size = BufferSize,
                              stream_mgmt_ack_freq = AckFreq} = State)
  when (Out + BufferSize) rem AckFreq == 0 ->
    send_element(State, stream_mgmt_request()),
    true;
maybe_send_ack_request(_) ->
    false.

stream_mgmt_request() ->
    #xmlel{name = <<"r">>,
           attrs = [{<<"xmlns">>, ?NS_STREAM_MGNT_3}]}.

flush_stream_mgmt_buffer(#state{stream_mgmt = false}) ->
    false;
flush_stream_mgmt_buffer(#state{stream_mgmt_buffer = Buffer}) ->
    re_route_packets(Buffer).

re_route_packets(Buffer) ->
    %% TODO add delayed on it?
    [ejabberd_router:route(From, To, Packet)
     || {From, To, Packet} <- lists:reverse(Buffer)],
    ok.

finish_state(ok, StateName, StateData) ->
    fsm_next_state(StateName, StateData);
finish_state(resume, _, StateData) ->
    maybe_enter_resume_session(StateData).

maybe_enter_resume_session(StateData) ->
    maybe_enter_resume_session(StateData#state.stream_mgmt_id, StateData).

maybe_enter_resume_session(undefined, StateData) ->
    {stop, normal, StateData};
maybe_enter_resume_session(_SMID, #state{} = SD) ->
    NSD = case SD#state.stream_mgmt_resume_tref of
              undefined ->
                  Seconds = timer:seconds(SD#state.stream_mgmt_resume_timeout),
                  TRef = erlang:send_after(Seconds, self(), resume_timeout),
                  SD#state{stream_mgmt_resume_tref = TRef};
              _TRef ->
                  SD
          end,
    {next_state, resume_session, NSD, hibernate}.

maybe_resume_session(NextState, El, StateData) ->
    case {xml:get_tag_attr_s(<<"xmlns">>, El),
          xml:get_tag_attr_s(<<"previd">>, El)} of
        {?NS_STREAM_MGNT_3, SMID} ->
            MaybeSID = mod_stream_management:get_sid(SMID),
            do_resume_session(SMID, El, MaybeSID, StateData);
        {InvalidNS, _} ->
            ?INFO_MSG("ignoring <resume/> element "
                      "with invalid namespace ~s~n", [InvalidNS]),
            fsm_next_state(NextState, StateData)
    end.

do_resume_session(SMID, El, [{_, Pid}], StateData) ->
    try
        {ok, OldState} = p1_fsm_old:sync_send_event(Pid, resume),
        SID = {p1_time_compat:timestamp(), self()},
        Conn = get_conn_type(StateData),
        MergedState = merge_state(OldState,
                                  StateData#state{sid = SID, conn = Conn}),
        case stream_mgmt_handle_ack(session_established, El, MergedState) of
            {stop, _, _} = Stop ->
                Stop;
            {next_state, session_established, NSD, _} ->
                Priority = get_priority_from_presence(NSD#state.pres_last),
                Info = [{ip, NSD#state.ip},
                        {conn, NSD#state.conn},
                        {auth_module, NSD#state.auth_module}],
                ejabberd_sm:open_session(SID,
                                         NSD#state.user,
                                         NSD#state.server,
                                         NSD#state.resource,
                                         Priority, Info),
                ok = mod_stream_management:register_smid(SMID, SID),
                try
                    Resumed = stream_mgmt_resumed(NSD#state.stream_mgmt_id,
                                                  NSD#state.stream_mgmt_in),
                    send_element(NSD, Resumed),
                    [send_element(NSD, Packet)
                     || {_, _, Packet} <- lists:reverse(NSD#state.stream_mgmt_buffer)],

                    NSD2 = flush_csi_buffer(NSD),

                    fsm_next_state(session_established, NSD2)
                catch
                    %% errors from send_element
                    _:_ ->
                        ?INFO_MSG("resumption error while resending old stanzas"
                                  " entering resume state again smid: ~p~n", [SMID]),
                        maybe_enter_resume_session(SMID, NSD)
                end
        end
    catch
        _:_ ->
            ?WARNING_MSG("resumption error (invalid response from ~p)~n",
                         [Pid]),
            send_element(StateData, stream_mgmt_failed(<<"item-not-found">>)),
            fsm_next_state(wait_for_feature_after_auth, StateData)
    end;

do_resume_session(SMID, _El, [], StateData) ->
    ?WARNING_MSG("no previous session with stream id ~p~n", [SMID]),
    send_element(StateData, stream_mgmt_failed(<<"item-not-found">>)),
    fsm_next_state(wait_for_feature_after_auth, StateData).

merge_state(OldSD, SD) ->
    Preserve = [#state.jid,
                #state.user,
                #state.server,
                #state.resource,
                #state.pres_t,
                #state.pres_f,
                #state.pres_a,
                #state.pres_i,
                #state.pres_last,
                #state.pres_pri,
                #state.pres_timestamp,
                #state.pres_invis,
                #state.privacy_list,
                #state.aux_fields,
                #state.csi_buffer,
                #state.stream_mgmt,
                #state.stream_mgmt_in,
                #state.stream_mgmt_id,
                #state.stream_mgmt_out_acked,
                #state.stream_mgmt_buffer,
                #state.stream_mgmt_buffer_size,
                #state.stream_mgmt_buffer_max,
                #state.stream_mgmt_resume_timeout,
                #state.stream_mgmt_ack_freq],
    Copy = fun(Index, {Stale, Acc}) ->
                   {Stale, setelement(Index, Acc, element(Index, Stale))}
           end,
    element(2, lists:foldl(Copy, {OldSD, SD}, Preserve)).

stream_mgmt_resumed(SMID, Handled) ->
    #xmlel{name = <<"resumed">>,
           attrs = [{<<"xmlns">>, ?NS_STREAM_MGNT_3},
                    {<<"previd">>, SMID},
                    {<<"h">>, integer_to_binary(Handled)}]}.

handover_session(SD) ->
    %% Assert Stream Management is on; otherwise this should not be called.
    true = SD#state.stream_mgmt,
    ejabberd_sm:close_session(SD#state.sid,
                              SD#state.user,
                              SD#state.server,
                              SD#state.resource,
                              resumed),
    {N, Messages} = flush_messages(),
    NewSize = N + SD#state.stream_mgmt_buffer_size,
    NewBuffer = Messages ++ SD#state.stream_mgmt_buffer,
    NSD = SD#state{authenticated = resumed,
                   stream_mgmt_buffer_size = NewSize,
                   stream_mgmt_buffer = NewBuffer},
    {stop, normal, {ok, NSD}, NSD}.

maybe_add_timestamp({F, T, #xmlel{name= <<"message">>}=Packet}=PacketTuple, Timestamp) ->
    Type = xml:get_tag_attr_s(<<"type">>, Packet),
    case Type of
        <<"error">> ->
            PacketTuple;
        <<"headline">> ->
            PacketTuple;
        _ ->
            {F, T, add_timestamp(Timestamp, <<"localhost">>, Packet)}
    end;
maybe_add_timestamp(Packet, _Timestamp) ->
    Packet.

add_timestamp({_, _, Micro} = TimeStamp, Server, Packet) ->
    {D, {H, M, S}} = calendar:now_to_universal_time(TimeStamp),
    Time = {D, {H, M, S, Micro}},
    case xml:get_subtag(Packet, <<"delay">>) of
        false ->
            TimeStampXML = timestamp_xml(Server, Time),
            xml:append_subtags(Packet, [TimeStampXML]);
        _ ->
            Packet
    end.

timestamp_xml(Server, Time) ->
    FromJID = jid:make(<<>>, Server, <<>>),
    jlib:timestamp_to_xml(Time, utc, FromJID, <<"SM Storage">>).

defer_resource_constraint_check(#state{stream_mgmt_constraint_check_tref = undefined} = State)->
    Seconds = timer:seconds(?CONSTRAINT_CHECK_TIMEOUT),
    TRef = erlang:send_after(Seconds, self(), check_buffer_full),
    State#state{stream_mgmt_constraint_check_tref = TRef};
defer_resource_constraint_check(State)->
    State.

-spec sasl_success_stanza(any()) -> xmlel().
sasl_success_stanza(ServerOut) ->
    C = case ServerOut of
            undefined -> [];
            _ -> [#xmlcdata{content = jlib:encode_base64(ServerOut)}]
        end,
    #xmlel{name = <<"success">>,
           attrs = [{<<"xmlns">>, ?NS_SASL}],
           children = C}.

-spec sasl_failure_stanza(binary() | {binary(), iodata() | undefined}) -> xmlel().
sasl_failure_stanza(Error) when is_binary(Error) ->
    sasl_failure_stanza({Error, undefined});
sasl_failure_stanza({Error, Text}) ->
    #xmlel{name = <<"failure">>,
           attrs = [{<<"xmlns">>, ?NS_SASL}],
           children = [#xmlel{name = Error} | maybe_text_tag(Text)]}.

maybe_text_tag(undefined) -> [];
maybe_text_tag(Text) ->
    [#xmlel{name = <<"text">>,
            children = [#xmlcdata{content = Text}]}].

-spec sasl_challenge_stanza(any()) -> xmlel().
sasl_challenge_stanza(Challenge) ->
    #xmlel{name = <<"challenge">>,
           attrs = [{<<"xmlns">>, ?NS_SASL}],
           children = Challenge}.

handle_sasl_success(State, Creds) ->
    ServerOut = mongoose_credentials:get(Creds, sasl_success_response, undefined),
    send_element(State, sasl_success_stanza(ServerOut)),
    User = mongoose_credentials:get(Creds, username),
    AuthModule = mongoose_credentials:get(Creds, auth_module),
    ?INFO_MSG("(~w) Accepted authentication for ~s by ~p",
              [State#state.socket, User, AuthModule]),
    NewState = State#state{ streamid = new_id(),
                            authenticated = true,
                            auth_module = AuthModule,
                            user = User },
    {wait_for_stream, NewState}.

handle_sasl_step(#state{server = Server, socket = Sock} = State, StepRes) ->
    case StepRes of
        {ok, Creds} ->
            handle_sasl_success(State, Creds);
        {continue, ServerOut, NewSASLState} ->
            Challenge  = [#xmlcdata{content = jlib:encode_base64(ServerOut)}],
            send_element(State, sasl_challenge_stanza(Challenge)),
            {wait_for_sasl_response, State#state{sasl_state = NewSASLState}};
        {error, Error, Username} ->
            IP = peerip(State#state.sockmod, Sock),
            ?INFO_MSG("(~w) Failed authentication for ~s@~s from IP ~s (~w)",
                      [Sock, Username, Server, jlib:ip_to_list(IP), IP]),
            ejabberd_hooks:run(auth_failed, Server, [Username, Server]),
            send_element(State, sasl_failure_stanza(Error)),
            {wait_for_feature_before_auth, State};
        {error, Error} ->
            ejabberd_hooks:run(auth_failed, Server, [unknown, Server]),
            send_element(State, sasl_failure_stanza(Error)),
            {wait_for_feature_before_auth, State}
    end.

user_allowed(JID, #state{server = Server, access = Access}) ->
    case acl:match_rule(Server, Access, JID)  of
        allow ->
            open_session_allowed_hook(Server, JID);
        deny ->
            false
    end.

open_session_allowed_hook(Server, JID) ->
    allow == ejabberd_hooks:run_fold(session_opening_allowed_for_user,
                                     Server,
                                     allow, [JID]).

terminate_when_tls_required_but_not_enabled(true, false, StateData, _El) ->
    Lang = StateData#state.lang,
    send_element(StateData, ?POLICY_VIOLATION_ERR(
                               Lang, <<"Use of STARTTLS required">>)),
    send_trailer(StateData),
    {stop, normal, StateData};
terminate_when_tls_required_but_not_enabled(_, _, StateData, El) ->
    process_unauthenticated_stanza(StateData, El),
    fsm_next_state(wait_for_feature_before_auth, StateData).
