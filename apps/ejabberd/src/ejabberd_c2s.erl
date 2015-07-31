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
         send_element/2,
         socket_type/0,
         get_presence/1,
         get_aux_field/2,
         set_aux_field/3,
         del_aux_field/2,
         get_subscription/2,
         broadcast/4,
         get_subscribed/1]).

%% gen_fsm callbacks
-export([init/1,
         wait_for_stream/2,
         wait_for_auth/2,
         wait_for_feature_request/2,
         wait_for_bind_or_resume/2,
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

-behaviour(?GEN_FSM).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

-spec start(_, list())
-> {'error',_} | {'ok','undefined' | pid()} | {'ok','undefined' | pid(),_}.
start(SockData, Opts) ->
    ?SUPERVISOR_START.


start_link(SockData, Opts) ->
    ?GEN_FSM:start_link(ejabberd_c2s, [SockData, Opts],
                        fsm_limit_opts(Opts) ++ ?FSMOPTS).

socket_type() ->
    xml_stream.


%% @doc Return Username, Resource and presence information
get_presence(FsmRef) ->
    ?GEN_FSM:sync_send_all_state_event(FsmRef, get_presence, 1000).


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


-spec get_subscription(From :: ejabberd:jid(),
                       State :: state()) -> 'both' | 'from' | 'none' | 'to'.
get_subscription(From = #jid{}, StateData) ->
    get_subscription(jlib:jid_tolower(From), StateData);
get_subscription(LFrom, StateData) ->
    LBFrom = setelement(3, LFrom, <<>>),
    F = ?SETS:is_element(LFrom, StateData#state.pres_f) orelse
    ?SETS:is_element(LBFrom, StateData#state.pres_f),
    T = ?SETS:is_element(LFrom, StateData#state.pres_t) orelse
    ?SETS:is_element(LBFrom, StateData#state.pres_t),
    if F and T -> both;
       F -> from;
       T -> to;
       true -> none
    end.


-spec broadcast(FSM :: atom() | pid(), % | port() | {atom(),atom()} % guess
                Type :: atom(), % guess
                From :: ejabberd:jid(), % guess
                Packet :: jlib:xmlel() % guess
               ) -> {'broadcast', atom(), ejabberd:jid(), jlib:xmlel()}.
broadcast(FsmRef, Type, From, Packet) ->
    FsmRef ! {broadcast, Type, From, Packet}.


stop(FsmRef) ->
    ?GEN_FSM:send_event(FsmRef, closed).

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
            if
                TLSEnabled ->
                    SockMod:starttls(Socket, TLSOpts);
                true ->
                    Socket
            end,
            SocketMonitor = SockMod:monitor(Socket1),
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
                                         ip             = IP},
             ?C2S_OPEN_TIMEOUT}
    end.

%% @doc Return list of all available resources of contacts,
get_subscribed(FsmRef) ->
    ?GEN_FSM:sync_send_all_state_event(FsmRef, get_subscribed, 1000).

%%----------------------------------------------------------------------
%% Func: StateName/2
%%----------------------------------------------------------------------

-spec wait_for_stream(Item :: ejabberd:xml_stream_item(),
                      State :: state()) -> fsm_return().
wait_for_stream({xmlstreamstart, _Name, Attrs}, StateData) ->
    DefaultLang = case ?MYLANG of
                      undefined ->
                          "en";
                      DL ->
                          DL
                  end,
    case xml:get_attr_s(<<"xmlns:stream">>, Attrs) of
        ?NS_STREAM ->
            Server = jlib:nameprep(xml:get_attr_s(<<"to">>, Attrs)),
            case lists:member(Server, ?MYHOSTS) of
                true ->
                    Lang = case xml:get_attr_s(<<"xml:lang">>, Attrs) of
                               Lang1 when size(Lang1) =< 35 ->
                                   %% As stated in BCP47, 4.4.1:
                                   %% Protocols or specifications that
                                   %% specify limited buffer sizes for
                                   %% language tags MUST allow for
                                   %% language tags of at least 35 characters.
                                   binary_to_list(Lang1);
                               _ ->
                                   %% Do not store long language tag to
                                   %% avoid possible DoS/flood attacks
                                   ""
                           end,
                    change_shaper(StateData, jlib:make_jid(<<>>, Server, <<>>)),
                    case xml:get_attr_s(<<"version">>, Attrs) of
                        <<"1.0">> ->
                            send_header(StateData, Server, "1.0", DefaultLang),
                            case StateData#state.authenticated of
                                false ->
                                    SASLState =
                                    cyrsasl:server_new(
                                      <<"jabber">>, Server, <<>>, [],
                                      fun(U) ->
                                              ejabberd_auth:get_password_with_authmodule(
                                                U, Server)
                                      end,
                                      fun(U, P) ->
                                              ejabberd_auth:check_password_with_authmodule(
                                                U, Server, P)
                                      end,
                                      fun(U, P, D, DG) ->
                                              ejabberd_auth:check_password_with_authmodule(
                                                U, Server, P, D, DG)
                                      end),
                                    Mechs = lists:map(
                                              fun(S) ->
                                                      #xmlel{name = <<"mechanism">>,
                                                             children = [#xmlcdata{content = S}]}
                                              end, cyrsasl:listmech(Server)),
                                    SockMod =
                                    (StateData#state.sockmod):get_sockmod(
                                                                StateData#state.socket),
                                    {Zlib, _} = StateData#state.zlib,
                                    CompressFeature =
                                    case Zlib andalso
                                         ((SockMod == gen_tcp) orelse
                                          (SockMod == tls)) of
                                        true ->
                                            [#xmlel{name = <<"compression">>,
                                                    attrs = [{<<"xmlns">>, ?NS_FEATURE_COMPRESS}],
                                                    children = [#xmlel{name = <<"method">>,
                                                                       children = [#xmlcdata{content = <<"zlib">>}]}]}];
                                        _ ->
                                            []
                                    end,
                                    TLS = StateData#state.tls,
                                    TLSEnabled = StateData#state.tls_enabled,
                                    TLSRequired = StateData#state.tls_required,
                                    TLSFeature =
                                    case  (TLS == true) andalso
                                          (TLSEnabled == false) andalso
                                          (SockMod == gen_tcp) of
                                        true ->
                                            case TLSRequired of
                                                true ->
                                                    [#xmlel{name = <<"starttls">>,
                                                            attrs = [{"xmlns", ?NS_TLS}],
                                                            children = [#xmlel{name = <<"required">>}]}];
                                                _ ->
                                                    [#xmlel{name = <<"starttls">>,
                                                            attrs = [{<<"xmlns">>, ?NS_TLS}]}]
                                            end;
                                        false ->
                                            []
                                    end,
                                    send_element(StateData,
                                                 #xmlel{name = <<"stream:features">>,
                                                        children = TLSFeature ++ CompressFeature ++
                                                        [#xmlel{name = <<"mechanisms">>,
                                                                attrs = [{<<"xmlns">>, ?NS_SASL}],
                                                                children = Mechs}] ++
                                                        ejabberd_hooks:run_fold(
                                                          c2s_stream_features,
                                                          Server,
                                                          [], [Server])}),
                                    fsm_next_state(wait_for_feature_request,
                                                   StateData#state{
                                                     server = Server,
                                                     sasl_state = SASLState,
                                                     lang = Lang});
                                _ ->
                                    case StateData#state.resource of
                                        <<>> ->
                                            RosterVersioningFeature =
                                            ejabberd_hooks:run_fold(
                                              roster_get_versioning_feature,
                                              Server, [], [Server]),
                                            StreamFeatures =
                                            [#xmlel{name = <<"bind">>,
                                                    attrs = [{<<"xmlns">>, ?NS_BIND}]},
                                             #xmlel{name = <<"session">>,
                                                    attrs = [{<<"xmlns">>, ?NS_SESSION}]}]
                                            ++ RosterVersioningFeature
                                            ++ ejabberd_hooks:run_fold(
                                                 c2s_stream_features,
                                                 Server,
                                                 [], [Server]),
                                            send_element(
                                              StateData,
                                              #xmlel{name = <<"stream:features">>,
                                                     children = StreamFeatures}),
                                            fsm_next_state(wait_for_bind_or_resume,
                                                           StateData#state{
                                                             server = Server,
                                                             lang = Lang});
                                        _ ->
                                            send_element(
                                              StateData,
                                              #xmlel{name = <<"stream:features">>}),
                                            fsm_next_state(wait_for_session_or_sm,
                                                           StateData#state{
                                                             server = Server,
                                                             lang = Lang})
                                    end
                            end;
                        _ ->
                            send_header(StateData, Server, "", DefaultLang),
                            if
                                (not StateData#state.tls_enabled) and
                                StateData#state.tls_required ->
                                    send_element(
                                      StateData,
                                      ?POLICY_VIOLATION_ERR(
                                         Lang,
                                         "Use of STARTTLS required")),
                                    send_trailer(StateData),
                                    {stop, normal, StateData};
                                true ->
                                    fsm_next_state(wait_for_auth,
                                                   StateData#state{
                                                     server = Server,
                                                     lang = Lang})
                            end
                    end;
                _ ->
                    send_header(StateData, ?MYNAME, "", DefaultLang),
                    send_element(StateData, ?HOST_UNKNOWN_ERR),
                    send_trailer(StateData),
                    {stop, normal, StateData}
            end;
        _ ->
            send_header(StateData, ?MYNAME, "", DefaultLang),
            send_element(StateData, ?INVALID_NS_ERR),
            send_trailer(StateData),
            {stop, normal, StateData}
    end;
wait_for_stream(timeout, StateData) ->
    {stop, normal, StateData};
wait_for_stream({xmlstreamelement, _}, StateData) ->
    send_element(StateData, ?INVALID_XML_ERR),
    send_trailer(StateData),
    {stop, normal, StateData};
wait_for_stream({xmlstreamend, _}, StateData) ->
    send_element(StateData, ?INVALID_XML_ERR),
    send_trailer(StateData),
    {stop, normal, StateData};
wait_for_stream({xmlstreamerror, _}, StateData) ->
    send_header(StateData, ?MYNAME, "1.0", ""),
    send_element(StateData, ?INVALID_XML_ERR),
    send_trailer(StateData),
    {stop, normal, StateData};
wait_for_stream(closed, StateData) ->
    {stop, normal, StateData}.

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
            Res = case ejabberd_auth:plain_password_required(
                         StateData#state.server) of
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
            Err = jlib:make_error_reply(
                    El,
                    ?ERR_AUTH_NO_RESOURCE_PROVIDED(StateData#state.lang)),
            send_element(StateData, Err),
            fsm_next_state(wait_for_auth, StateData);
        {auth, _ID, set, {U, P, D, R}} ->
            JID = jlib:make_jid(U, StateData#state.server, R),
            case (JID /= error) andalso
                 (acl:match_rule(StateData#state.server,
                                 StateData#state.access, JID) == allow) of
                true ->
                    DGen = fun(PW) ->
                                   sha:sha1_hex(StateData#state.streamid
                                                ++ binary_to_list(PW)) end,
                    case ejabberd_auth:check_password_with_authmodule(
                           U, StateData#state.server, P, D, DGen) of
                        {true, AuthModule} ->
                            ?INFO_MSG(
                               "(~w) Accepted legacy authentication for ~s by ~p",
                               [StateData#state.socket,
                                jlib:jid_to_binary(JID), AuthModule]),
                            SID = {now(), self()},
                            Conn = get_conn_type(StateData),
                            Info = [{ip, StateData#state.ip}, {conn, Conn},
                                    {auth_module, AuthModule}],
                            Res1 = jlib:make_result_iq_reply(El),
                            Res = setelement(4, Res1, []),
                            send_element(StateData, Res),
                            ejabberd_sm:open_session(
                              SID, U, StateData#state.server, R, Info),
                            change_shaper(StateData, JID),
                            {Fs, Ts, Pending} = ejabberd_hooks:run_fold(
                                                  roster_get_subscription_lists,
                                                  StateData#state.server,
                                                  {[], [], []},
                                                  [U, StateData#state.server]),
                            LJID = jlib:jid_tolower(
                                     jlib:jid_remove_resource(JID)),
                            Fs1 = [LJID | Fs],
                            Ts1 = [LJID | Ts],
                            PrivList =
                            ejabberd_hooks:run_fold(
                              privacy_get_user_list, StateData#state.server,
                              #userlist{},
                              [U, StateData#state.server]),
                            NewStateData =
                            StateData#state{
                              user = U,
                              resource = R,
                              jid = JID,
                              sid = SID,
                              conn = Conn,
                              auth_module = AuthModule,
                              pres_f = ?SETS:from_list(Fs1),
                              pres_t = ?SETS:from_list(Ts1),
                              pending_invitations = Pending,
                              privacy_list = PrivList},
                            fsm_next_state_pack(session_established,
                                                NewStateData);
                        _ ->
                            IP = peerip(StateData#state.sockmod, StateData#state.socket),
                            ?INFO_MSG(
                               "(~w) Failed legacy authentication for ~s from IP ~s (~w)",
                               [StateData#state.socket,
                                jlib:jid_to_binary(JID), jlib:ip_to_list(IP), IP]),
                            Err = jlib:make_error_reply(
                                    El, ?ERR_NOT_AUTHORIZED),
                            ejabberd_hooks:run(auth_failed, StateData#state.server,
                                               [U, StateData#state.server]),
                            send_element(StateData, Err),
                            fsm_next_state(wait_for_auth, StateData)
                    end;
                _ ->
                    if
                        JID == error ->
                            ?INFO_MSG(
                               "(~w) Forbidden legacy authentication for "
                               "username '~s' with resource '~s'",
                               [StateData#state.socket, U, R]),
                            Err = jlib:make_error_reply(El, ?ERR_JID_MALFORMED),
                            send_element(StateData, Err),
                            fsm_next_state(wait_for_auth, StateData);
                        true ->
                            ?INFO_MSG(
                               "(~w) Forbidden legacy authentication for ~s",
                               [StateData#state.socket,
                                jlib:jid_to_binary(JID)]),
                            Err = jlib:make_error_reply(El, ?ERR_NOT_ALLOWED),
                            send_element(StateData, Err),
                            fsm_next_state(wait_for_auth, StateData)
                    end
            end;
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

-spec wait_for_feature_request(Item :: ejabberd:xml_stream_item(),
                               State :: state()) -> fsm_return().
wait_for_feature_request({xmlstreamelement,
                          #xmlel{name = <<"enable">>} = El}, StateData) ->
    maybe_unexpected_sm_request(wait_for_feature_request, El, StateData);
wait_for_feature_request({xmlstreamelement, El}, StateData) ->
    #xmlel{name = Name, attrs = Attrs, children = Els} = El,
    {Zlib, ZlibLimit} = StateData#state.zlib,
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
        {?NS_TLS_BIN, <<"starttls">>} when TLS == true,
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
                                                    xml:element_to_binary(
                                                      #xmlel{name = <<"proceed">>,
                                                             attrs = [{<<"xmlns">>, ?NS_TLS}]})),
            fsm_next_state(wait_for_stream,
                           StateData#state{socket = TLSSocket,
                                           streamid = new_id(),
                                           tls_enabled = true
                                          });
        {?NS_COMPRESS_BIN, <<"compress">>} when Zlib == true,
                                                ((SockMod == gen_tcp) or
                                                 (SockMod == tls)) ->
            case xml:get_subtag(El, <<"method">>) of
                false ->
                    send_element(StateData,
                                 #xmlel{name = <<"failure">>,
                                        attrs = [{<<"xmlns">>, ?NS_COMPRESS}],
                                        children = [#xmlel{name = <<"setup-failed">>}]}),
                    fsm_next_state(wait_for_feature_request, StateData);
                Method ->
                    case xml:get_tag_cdata(Method) of
                        <<"zlib">> ->
                            Socket = StateData#state.socket,
                            ZlibSocket = (StateData#state.sockmod):compress(
                                                                     Socket, ZlibLimit,
                                                                     xml:element_to_binary(
                                                                       #xmlel{name = <<"compressed">>,
                                                                              attrs = [{<<"xmlns">>, ?NS_COMPRESS}]})),
                            fsm_next_state(wait_for_stream,
                                           StateData#state{socket = ZlibSocket,
                                                           streamid = new_id()
                                                          });
                        _ ->
                            send_element(StateData,
                                         #xmlel{name = <<"failure">>,
                                                attrs = [{<<"xmlns">>, ?NS_COMPRESS}],
                                                children = [#xmlel{name = <<"unsupported-method">>}]}),
                            fsm_next_state(wait_for_feature_request,
                                           StateData)
                    end
            end;
        _ ->
            if
                TLSRequired and not TLSEnabled ->
                    Lang = StateData#state.lang,
                    send_element(StateData, ?POLICY_VIOLATION_ERR(
                                               Lang,
                                               "Use of STARTTLS required")),
                    send_trailer(StateData),
                    {stop, normal, StateData};
                true ->
                    process_unauthenticated_stanza(StateData, El),
                    fsm_next_state(wait_for_feature_request, StateData)
            end
    end;
wait_for_feature_request(timeout, StateData) ->
    {stop, normal, StateData};
wait_for_feature_request({xmlstreamend, _Name}, StateData) ->
    send_trailer(StateData),
    {stop, normal, StateData};
wait_for_feature_request({xmlstreamerror, _}, StateData) ->
    send_element(StateData, ?INVALID_XML_ERR),
    send_trailer(StateData),
    {stop, normal, StateData};
wait_for_feature_request(closed, StateData) ->
    {stop, normal, StateData}.

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
            StepResult = cyrsasl:server_step(StateData#state.sasl_state,ClientIn),
            {NewFSMState, NewStateData} = handle_sasl_step(StateData, StepResult),
            fsm_next_state(NewFSMState, NewStateData);
        _ ->
            process_unauthenticated_stanza(StateData, El),
            fsm_next_state(wait_for_feature_request, StateData)
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

-spec wait_for_bind_or_resume(Item :: ejabberd:xml_stream_item(),
                              State :: state()) -> fsm_return().
wait_for_bind_or_resume({xmlstreamelement,
                         #xmlel{name = <<"enable">>} = El}, StateData) ->
    maybe_unexpected_sm_request(wait_for_bind_or_resume, El, StateData);
wait_for_bind_or_resume({xmlstreamelement,
                         #xmlel{name = <<"resume">>} = El}, StateData) ->
    maybe_resume_session(wait_for_bind_or_resume, El, StateData);
wait_for_bind_or_resume({xmlstreamelement, El}, StateData) ->
    case jlib:iq_query_info(El) of
        #iq{type = set, xmlns = ?NS_BIND, sub_el = SubEl} = IQ ->
            U = StateData#state.user,
            R1 = xml:get_path_s(SubEl, [{elem, <<"resource">>}, cdata]),
            R = case jlib:resourceprep(R1) of
                    error -> error;
                    <<>> ->
                        list_to_binary(lists:concat(
                                         [randoms:get_string() | tuple_to_list(now())]));
                    Resource -> Resource
                end,
            case R of
                error ->
                    Err = jlib:make_error_reply(El, ?ERR_BAD_REQUEST),
                    send_element(StateData, Err),
                    fsm_next_state(wait_for_bind_or_resume, StateData);
                _ ->
                    JID = jlib:make_jid(U, StateData#state.server, R),
                    %%Server = StateData#state.server,
                    %%RosterVersioningFeature =
                    %%  ejabberd_hooks:run_fold(
                    %%  roster_get_versioning_feature, Server, [], [Server]),
                    %%StreamFeatures = [{xmlel, "session",
                    %%             [{"xmlns", ?NS_SESSION}], []} |
                    %%            RosterVersioningFeature],
                    %%send_element(StateData, {xmlel, "stream:features",
                    %%               [], StreamFeatures}),
                    Res = IQ#iq{type = result,
                                sub_el = [#xmlel{name = <<"bind">>,
                                                 attrs = [{<<"xmlns">>, ?NS_BIND}],
                                                 children = [#xmlel{name = <<"jid">>,
                                                                    children = [#xmlcdata{content = jlib:jid_to_binary(JID)}]}]}]},
                    XmlEl = jlib:iq_to_xml(Res),
                    send_element(StateData, XmlEl),
                    fsm_next_state(wait_for_session_or_sm,
                                   StateData#state{resource = R, jid = JID})
            end;
        _ ->
            fsm_next_state(wait_for_bind_or_resume, StateData)
    end;

wait_for_bind_or_resume(timeout, StateData) ->
    {stop, normal, StateData};

wait_for_bind_or_resume({xmlstreamend, _Name}, StateData) ->
    send_trailer(StateData),
    {stop, normal, StateData};

wait_for_bind_or_resume({xmlstreamerror, _}, StateData) ->
    send_element(StateData, ?INVALID_XML_ERR),
    send_trailer(StateData),
    {stop, normal, StateData};

wait_for_bind_or_resume(closed, StateData) ->
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
            U = StateData#state.user,
            R = StateData#state.resource,
            JID = StateData#state.jid,
            case acl:match_rule(StateData#state.server,
                                StateData#state.access, JID) of
                allow ->
                    ?INFO_MSG("(~w) Opened session for ~s",
                              [StateData#state.socket,
                               jlib:jid_to_binary(JID)]),
                    Res = jlib:make_result_iq_reply(El),
                    Packet = {jlib:jid_remove_resource(StateData#state.jid), StateData#state.jid, Res},
                    {_, _, NewStateData0, _} = send_and_maybe_buffer_stanza(Packet, StateData, wait_for_session_or_sm),
                    change_shaper(NewStateData0, JID),
                    {Fs, Ts, Pending} = ejabberd_hooks:run_fold(
                                          roster_get_subscription_lists,
                                          NewStateData0#state.server,
                                          {[], [], []},
                                          [U, NewStateData0#state.server]),
                    LJID = jlib:jid_tolower(jlib:jid_remove_resource(JID)),
                    Fs1 = [LJID | Fs],
                    Ts1 = [LJID | Ts],
                    PrivList =
                    ejabberd_hooks:run_fold(
                      privacy_get_user_list, NewStateData0#state.server,
                      #userlist{},
                      [U, NewStateData0#state.server]),
                    SID = {now(), self()},
                    Conn = get_conn_type(NewStateData0),
                    Info = [{ip, NewStateData0#state.ip}, {conn, Conn},
                            {auth_module, NewStateData0#state.auth_module}],
                    ejabberd_sm:open_session(
                      SID, U, NewStateData0#state.server, R, Info),
                    NewStateData =
                    NewStateData0#state{
                      sid = SID,
                      conn = Conn,
                      pres_f = ?SETS:from_list(Fs1),
                      pres_t = ?SETS:from_list(Ts1),
                      pending_invitations = Pending,
                      privacy_list = PrivList},
                    fsm_next_state_pack(session_established,
                                        NewStateData);
                _ ->
                    ejabberd_hooks:run(forbidden_session_hook,
                                       StateData#state.server, [JID]),
                    ?INFO_MSG("(~w) Forbidden session for ~s",
                              [StateData#state.socket,
                               jlib:jid_to_binary(JID)]),
                    Err = jlib:make_error_reply(El, ?ERR_NOT_ALLOWED),
                    send_element(StateData, Err),
                    fsm_next_state(wait_for_session_or_sm, StateData)
            end;
        _ ->
            fsm_next_state(wait_for_session_or_sm, StateData)
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
            check_amp_maybe_send(FromJID#jid.lserver, NewState, {FromJID, El})
    end;

%% We hibernate the process to reduce memory consumption after a
%% configurable activity timeout
session_established(timeout, StateData) ->
    %% TODO: Options must be stored in state:
    Options = [],
    proc_lib:hibernate(?GEN_FSM, enter_loop,
                       [?MODULE, Options, session_established, StateData]),
    fsm_next_state(session_established, StateData);
session_established({xmlstreamend, _Name}, StateData) ->
    send_trailer(StateData),
    {stop, normal, StateData};

session_established({xmlstreamerror, <<"XML stanza is too big">> = E}, StateData) ->
    send_element(StateData, ?POLICY_VIOLATION_ERR(StateData#state.lang, E)),
    send_trailer(StateData),
    {stop, normal, StateData};
session_established({xmlstreamerror, _}, StateData) ->
    send_element(StateData, ?INVALID_XML_ERR),
    send_trailer(StateData),
    {stop, normal, StateData};
session_established(closed, StateData) ->
    ?DEBUG("Session established closed - trying to enter resume_session",[]),
    maybe_enter_resume_session(StateData#state.stream_mgmt_id, StateData).


%%% XEP-0079 (AMP) related
check_amp_maybe_send(Host, State, {_FromJID, _El} = HookData) ->
    case ejabberd_hooks:run_fold(amp_check_packet, Host, HookData, []) of
        drop      -> fsm_next_state(session_established, State);
        {_,NewEl} -> session_established2(NewEl, State)
    end.

%% @doc Process packets sent by user (coming from user on c2s XMPP
%% connection)
-spec session_established2(El :: jlib:xmlel(), state()) -> fsm_return().
session_established2(El, StateData) ->
    #xmlel{name = Name, attrs = Attrs} = El,
    User = StateData#state.user,
    Server = StateData#state.server,
    FromJID = StateData#state.jid,
    To = xml:get_attr_s(<<"to">>, Attrs),
    ToJID = case To of
                <<>> ->
                    jlib:make_jid(User, Server, <<>>);
                _ ->
                    jlib:binary_to_jid(To)
            end,
    NewEl1 = jlib:remove_attr(<<"xmlns">>, jlib:remove_delay_tags(El)),
    NewEl = case xml:get_attr_s(<<"xml:lang">>, Attrs) of
                <<>> ->
                    case StateData#state.lang of
                        <<>> -> NewEl1;
                        Lang ->
                            xml:replace_tag_attr(<<"xml:lang">>, list_to_binary(Lang), NewEl1)
                    end;
                _ ->
                    NewEl1
            end,
    NewState =
    case ToJID of
        error ->
            case xml:get_attr_s(<<"type">>, Attrs) of
                <<"error">> -> StateData;
                <<"result">> -> StateData;
                _ ->
                    Err = jlib:make_error_reply(NewEl, ?ERR_JID_MALFORMED),
                    send_element(StateData, Err),
                    StateData
            end;
        _ ->
            case Name of
                <<"presence">> ->
                    PresenceEl = ejabberd_hooks:run_fold(
                                   c2s_update_presence,
                                   Server,
                                   NewEl,
                                   [User, Server]),
                    ejabberd_hooks:run(
                      user_send_packet,
                      Server,
                      [FromJID, ToJID, PresenceEl]),
                    case ToJID of
                        #jid{user = User,
                             server = Server,
                             resource = <<>>} ->
                            ?DEBUG("presence_update(~p,~n\t~p,~n\t~p)",
                                   [FromJID, PresenceEl, StateData]),
                            presence_update(FromJID, PresenceEl,
                                            StateData);
                        _ ->
                            presence_track(FromJID, ToJID, PresenceEl,
                                           StateData)
                    end;
                <<"iq">> ->
                    case jlib:iq_query_info(NewEl) of
                        #iq{xmlns = Xmlns} = IQ
                          when Xmlns == ?NS_PRIVACY;
                               Xmlns == ?NS_BLOCKING ->
                            process_privacy_iq(
                              FromJID, ToJID, IQ, StateData);
                        _ ->
                            ejabberd_hooks:run(
                              user_send_packet,
                              Server,
                              [FromJID, ToJID, NewEl]),
                            check_privacy_route(FromJID, StateData, FromJID, ToJID, NewEl),
                            StateData
                    end;
                <<"message">> ->
                    ejabberd_hooks:run(user_send_packet,
                                       Server,
                                       [FromJID, ToJID, NewEl]),
                    check_privacy_route(FromJID, StateData, FromJID,
                                        ToJID, NewEl),
                    StateData;
                _ ->
                    StateData
            end
    end,
    ejabberd_hooks:run(c2s_loop_debug, [{xmlstreamelement, El}]),
    fsm_next_state(session_established, NewState).

%%-------------------------------------------------------------------------
%% session may be terminated for exmaple by mod_ping there is still valid
%% connection and resource want to send stanza.
resume_session({xmlstreamelement, _}, StateData) ->
    Err = ?POLICY_VIOLATION_ERR(StateData#state.lang,
                                "session in resume state cannot accept incoming stanzas"),
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
    ?WARNING_MSG("unexpected message ~p",[Msg]),
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
   | {'reply', Reply :: 'ok' | {_,_,_,_}, statename(), state(), integer()}.
handle_sync_event(get_presence, _From, StateName, StateData) ->
    User = StateData#state.user,
    PresLast = StateData#state.pres_last,

    Show = get_showtag(PresLast),
    Status = get_statustag(PresLast),
    Resource = StateData#state.resource,

    Reply = {User, Resource, Show, Status},
    fsm_reply(Reply, StateName, StateData);
handle_sync_event(get_subscribed, _From, StateName, StateData) ->
    Subscribed = ?SETS:to_list(StateData#state.pres_f),
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
handle_info({send_text, Text}, StateName, StateData) ->
    send_text(StateData, Text),
    ejabberd_hooks:run(c2s_loop_debug, [Text]),
    fsm_next_state(StateName, StateData);
handle_info(replaced, _StateName, StateData) ->
    Lang = StateData#state.lang,
    maybe_send_element_safe(StateData,
                            ?SERRT_CONFLICT(Lang, "Replaced by new connection")),
    maybe_send_trailer_safe(StateData),
    {stop, normal, StateData#state{authenticated = replaced}};
%% Process Packets that are to be send to the user
handle_info({route, From, To, Packet}, StateName, StateData) ->
    {Pass, NewAttrs, NewState} = handle_routed(Packet#xmlel.name,
                                               From, To, Packet, StateData),
    if
        Pass == exit ->
            %% When Pass==exit, NewState contains a string instead of a #state{}
            Lang = StateData#state.lang,
            send_element(StateData, ?SERRT_CONFLICT(Lang, NewState)),
            send_trailer(StateData),
            {stop, normal, StateData};
        Pass == send_new ->
            %% When Pass==send_new, NewAttrs contains a {F,T Stanza} instead of a Attrs
            send_and_maybe_buffer_stanza(NewAttrs, NewState, StateName);
        Pass ->
            Attrs2 = jlib:replace_from_to_attrs(jlib:jid_to_binary(From),
                                                jlib:jid_to_binary(To),
                                                NewAttrs),
            FixedPacket = Packet#xmlel{attrs = Attrs2},
            ejabberd_hooks:run(user_receive_packet,
                               StateData#state.server,
                               [StateData#state.jid, From, To, FixedPacket]),
            ejabberd_hooks:run(c2s_loop_debug, [{route, From, To, Packet}]),

            send_and_maybe_buffer_stanza({From, To, FixedPacket}, NewState, StateName);
        true ->
            ejabberd_hooks:run(c2s_loop_debug, [{route, From, To, Packet}]),
            fsm_next_state(StateName, NewState)
    end;
handle_info({'DOWN', Monitor, _Type, _Object, _Info}, _StateName, StateData)
  when Monitor == StateData#state.socket_monitor ->
    maybe_enter_resume_session(StateData#state.stream_mgmt_id, StateData);
handle_info(system_shutdown, StateName, StateData) ->
    case StateName of
        wait_for_stream ->
            send_header(StateData, ?MYNAME, "1.0", "en"),
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
            presence_update(StateData2#state.jid,
                            PresenceEl,
                            StateData2),
            StateData2;
        _ ->
            StateData
    end,
    {next_state, StateName, NewStateData};
handle_info({broadcast, Type, From, Packet}, StateName, StateData) ->
    Recipients = ejabberd_hooks:run_fold(
                   c2s_broadcast_recipients, StateData#state.server,
                   [],
                   [StateData, Type, From, Packet]),
    lists:foreach(
      fun(USR) ->
              ejabberd_router:route(
                From, jlib:make_jid(USR), Packet)
      end, lists:usort(Recipients)),
    fsm_next_state(StateName, StateData);
handle_info(resume_timeout, resume_session, StateData) ->
    {stop, normal, StateData};
handle_info(check_buffer_full, StateName, StateData) ->
    case is_buffer_full(StateData#state.stream_mgmt_buffer_size,
                        StateData#state.stream_mgmt_buffer_max) of
        true ->
            Err = ?RESOURCE_CONSTRAINT_ERR((StateData#state.lang),
                                           "too many unacked stanzas"),
            send_element(StateData, Err),
            send_trailer(StateData),
            {stop, normal, StateData};
        false ->
            fsm_next_state(StateName,
                           StateData#state{stream_mgmt_constraint_check_tref = undefined})
    end;
handle_info(Info, StateName, StateData) ->
    ?ERROR_MSG("Unexpected info: ~p", [Info]),
    fsm_next_state(StateName, StateData).

handle_routed(<<"presence">>, From, To, Packet, StateData) ->
    handle_routed_presence(From, To, Packet, StateData);
handle_routed(<<"broadcast">>, _From, _To, Packet, StateData) ->
    handle_routed_broadcast(Packet, StateData);
handle_routed(<<"iq">>, From, To, Packet, StateData) ->
    handle_routed_iq(From, To, Packet, StateData);
handle_routed(<<"message">>, From, To, Packet, StateData) ->
    case privacy_check_packet(StateData, From, To, Packet, in) of
        allow ->
            {true, Packet#xmlel.attrs, StateData};
        deny ->
            {false, Packet#xmlel.attrs, StateData}
    end;
handle_routed(_, _From, _To, Packet, StateData) ->
    {true, Packet#xmlel.attrs, StateData}.

handle_routed_iq(From, To, Packet = #xmlel{attrs = Attrs}, StateData) ->
    case jlib:iq_query_info(Packet) of
        %% TODO: Support for mod_last / XEP-0012. Can we move it to the respective module?
        %%   Thanks to add_iq_handler(ejabberd_sm, ...)?
        #iq{xmlns = ?NS_LAST} ->
            HasFromSub = ( is_subscribed_to_my_presence(From, StateData)
                           andalso is_privacy_allow(StateData, To, From,
                                                    #xmlel{name = <<"presence">>}, out) ),
            case HasFromSub of
                true ->
                    case privacy_check_packet(StateData, From, To, Packet, in) of
                        allow ->
                            {true, Attrs, StateData};
                        deny ->
                            {false, Attrs, StateData}
                    end;
                _ ->
                    Err = jlib:make_error_reply(Packet, ?ERR_FORBIDDEN),
                    ejabberd_router:route(To, From, Err),
                    {false, Attrs, StateData}
            end;
        IQ when (is_record(IQ, iq)) or (IQ == reply) ->
            case privacy_check_packet(StateData, From, To, Packet, in) of
                allow ->
                    {true, Attrs, StateData};
                deny when is_record(IQ, iq) ->
                    Err = jlib:make_error_reply(Packet, ?ERR_SERVICE_UNAVAILABLE),
                    ejabberd_router:route(To, From, Err),
                    {false, Attrs, StateData};
                deny when IQ == reply ->
                    {false, Attrs, StateData}
            end;
        IQ when (IQ == invalid) or (IQ == not_iq) ->
            {false, Attrs, StateData}
    end.

handle_routed_broadcast(Packet, StateData) ->
    #xmlel{attrs = Attrs, children = Els} = Packet,
    ?DEBUG("broadcast~n~p~n", [Els]),
    case Els of
        [{item, IJID, ISubscription}] ->
            {false, Attrs,
             roster_change(IJID, ISubscription, StateData)};
        [{exit, Reason}] ->
            {exit, Attrs, Reason};
        [{privacy_list, PrivList, PrivListName}] ->
            case ejabberd_hooks:run_fold(privacy_updated_list, StateData#state.server,
                                         false, [StateData#state.privacy_list, PrivList])
            of
                false ->
                    {false, Attrs, StateData};
                NewPL ->
                    PrivPushIQ = privacy_list_push_iq(PrivListName),
                    F = jlib:jid_remove_resource(StateData#state.jid),
                    T = StateData#state.jid,
                    PrivPushEl = jlib:replace_from_to(F, T, jlib:iq_to_xml(PrivPushIQ)),
                    {send_new, {F, T, PrivPushEl}, StateData#state{privacy_list = NewPL}}
            end;
        [{blocking, What}] ->
            route_blocking(What, StateData),
            {false, Attrs, StateData};
        _ ->
            {false, Attrs, StateData}
    end.

privacy_list_push_iq(PrivListName) ->
    #iq{type = set, xmlns = ?NS_PRIVACY,
        id = list_to_binary("push" ++ randoms:get_string()),
        sub_el = [#xmlel{name = <<"query">>,
                         attrs = [{<<"xmlns">>, ?NS_PRIVACY}],
                         children = [#xmlel{name = <<"list">>,
                                            attrs = [{<<"name">>, PrivListName}]}]}]}.

handle_routed_presence(From, To, Packet = #xmlel{attrs = Attrs}, StateData) ->
    State = ejabberd_hooks:run_fold(c2s_presence_in, StateData#state.server,
                                    StateData, [{From, To, Packet}]),
    case xml:get_attr_s(<<"type">>, Attrs) of
        <<"probe">> ->
            {LFrom, LBFrom} = lowcase_and_bare(From),
            NewState = case am_i_available_to(LFrom, LBFrom, State) of
                           true -> State;
                           false -> make_available_to(LFrom, LBFrom, State)
                       end,
            process_presence_probe(From, To, NewState),
            {false, Attrs, NewState};
        <<"error">> ->
            NewA = remove_element(jlib:jid_tolower(From), State#state.pres_a),
            {true, Attrs, State#state{pres_a = NewA}};
        <<"invisible">> ->
            Attrs1 = lists:keydelete(<<"type">>, 1, Attrs),
            {true, [{<<"type">>, <<"unavailable">>} | Attrs1], State};
        <<"subscribe">> ->
            SRes = is_privacy_allow(State, From, To, Packet, in),
            {SRes, Attrs, State};
        <<"subscribed">> ->
            SRes = is_privacy_allow(State, From, To, Packet, in),
            {SRes, Attrs, State};
        <<"unsubscribe">> ->
            SRes = is_privacy_allow(State, From, To, Packet, in),
            {SRes, Attrs, State};
        <<"unsubscribed">> ->
            SRes = is_privacy_allow(State, From, To, Packet, in),
            {SRes, Attrs, State};
        _ ->
            case privacy_check_packet(State, From, To, Packet, in) of
                allow ->
                    {LFrom, LBFrom} = lowcase_and_bare(From),
                    case am_i_available_to(LFrom, LBFrom, State) of
                        true -> {true, Attrs, State};
                        false -> {true, Attrs, make_available_to(LFrom, LBFrom, State)}
                    end;
                deny ->
                    {false, Attrs, State}
            end
    end.

am_i_available_to(LFrom, LBFrom, State) ->
    ?SETS:is_element(LFrom, State#state.pres_a)
    orelse (LFrom /= LBFrom)
    andalso ?SETS:is_element(LBFrom, State#state.pres_a).

make_available_to(LFrom, LBFrom, State) ->
    case ?SETS:is_element(LFrom, State#state.pres_f) of
        true ->
            A = ?SETS:add_element(LFrom, State#state.pres_a),
            State#state{pres_a = A};
        false ->
            case ?SETS:is_element(LBFrom, State#state.pres_f) of
                true ->
                    A = ?SETS:add_element(LBFrom, State#state.pres_a),
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
    State#state{pres_t = {pres_t, ?SETS:size(T)},
                pres_f = {pres_f, ?SETS:size(F)},
                pres_a = {pres_a, ?SETS:size(A)},
                pres_i = {pres_i, ?SETS:size(I)}
               }.

%%----------------------------------------------------------------------
%% Func: terminate/3
%% Purpose: Shutdown the fsm
%% Returns: any
%%----------------------------------------------------------------------
-spec terminate(Reason :: any(), statename(), state()) -> ok.
terminate(_Reason, StateName, StateData) ->
    case  should_close_session(StateName) of
        %% if we are in an state wich have a session established
        true->
            case StateData#state.authenticated of
                replaced ->
                    ?INFO_MSG("(~w) Replaced session for ~s",
                              [StateData#state.socket,
                               jlib:jid_to_binary(StateData#state.jid)]),
                    From = StateData#state.jid,
                    Packet = #xmlel{name = <<"presence">>,
                                    attrs = [{<<"type">>, <<"unavailable">>}],
                                    children = [#xmlel{name = <<"status">>,
                                                       children = [#xmlcdata{content = "Replaced by new connection"}]}]},
                    ejabberd_sm:close_session_unset_presence(
                      StateData#state.sid,
                      StateData#state.user,
                      StateData#state.server,
                      StateData#state.resource,
                      "Replaced by new connection",
                      replaced),
                    presence_broadcast(
                      StateData, From, StateData#state.pres_a, Packet),
                    presence_broadcast(
                      StateData, From, StateData#state.pres_i, Packet);
                resumed ->
                    ?INFO_MSG("(~w) Stream ~p resumed for ~s",
                              [StateData#state.socket,
                               StateData#state.stream_mgmt_id,
                               jlib:jid_to_binary(StateData#state.jid)]);
                _ ->
                    ?INFO_MSG("(~w) Close session for ~s",
                              [StateData#state.socket,
                               jlib:jid_to_binary(StateData#state.jid)]),

                    EmptySet = ?SETS:new(),
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
                            ejabberd_sm:close_session_unset_presence(
                              StateData#state.sid,
                              StateData#state.user,
                              StateData#state.server,
                              StateData#state.resource,
                              "",
                              normal),
                            presence_broadcast(
                              StateData, From, StateData#state.pres_a, Packet),
                            presence_broadcast(
                              StateData, From, StateData#state.pres_i, Packet)
                    end
            end,
            if
                StateData#state.authenticated =/= resumed ->
                    ?DEBUG("rerouting unacked messages", []),
                    flush_stream_mgmt_buffer(StateData),
                    bounce_messages();
                true ->
                    ok
            end;
        false ->
            ok
    end,
    (StateData#state.sockmod):close(StateData#state.socket),
    ok.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------
should_close_session(resume_session) -> true;
should_close_session(session_established) -> true;
should_close_session(_) -> false.

-spec change_shaper(state(), ejabberd:jid()) -> any().
change_shaper(StateData, JID) ->
    Shaper = acl:match_rule(StateData#state.server,
                            StateData#state.shaper, JID),
    (StateData#state.sockmod):change_shaper(StateData#state.socket, Shaper).


-spec send_text(state(), Text :: binary()) -> any().
send_text(StateData, Text) ->
    ?DEBUG("Send XML on stream = ~p", [Text]),
    Size = size(Text),
    mongoose_metrics:update([data, xmpp, sent, xml_stanza_size], Size),
    (StateData#state.sockmod):send(StateData#state.socket, Text).

-spec maybe_send_element_safe(state(), El :: jlib:xmlel()) -> any().
maybe_send_element_safe(#state{stream_mgmt = false} = State, El) ->
    send_element(State, El);
maybe_send_element_safe(State, El) ->
    case catch send_element(State, El) of
        ok -> ok;
        _ -> error
    end.

send_element(#state{server = Server, sockmod = SockMod} = StateData, El)
  when StateData#state.xml_socket ->
    ejabberd_hooks:run(xmpp_send_element,
                       Server, [Server, El]),
    SockMod:send_xml(StateData#state.socket,
                     {xmlstreamelement, El});
send_element(#state{server = Server} = StateData, El) ->
    ejabberd_hooks:run(xmpp_send_element,
                       Server, [Server, El]),
    send_text(StateData, xml:element_to_binary(El)).


-spec send_header(State :: state(),
                  Server :: ejabberd:server(),
                  Version :: [46 | 48 | 49],
                  Lang :: ejabberd:lang()) -> any().
send_header(StateData, Server, Version, Lang)
  when StateData#state.xml_socket ->
    VersionAttr =
    case Version of
        "" -> [];
        _ -> [{<<"version">>, Version}]
    end,
    LangAttr =
    case Lang of
        "" -> [];
        _ -> [{<<"xml:lang">>, Lang}]
    end,
    Header =
    {xmlstreamstart,
     <<"stream:stream">>,
     VersionAttr ++
     LangAttr ++
     [{<<"xmlns">>, ?NS_CLIENT},
      {<<"xmlns:stream">>, <<"http://etherx.jabber.org/streams">>},
      {<<"id">>, StateData#state.streamid},
      {<<"from">>, Server}]},
    (StateData#state.sockmod):send_xml(
                                StateData#state.socket, Header);
send_header(StateData, Server, Version, Lang) ->
    VersionStr =
    case Version of
        "" -> "";
        _ -> [" version='", Version, "'"]
    end,
    LangStr =
    case Lang of
        "" -> "";
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


send_and_maybe_buffer_stanza({_, _, Stanza} = Packet, State, StateName)->
    SendResult = maybe_send_element_safe(State, Stanza),
    BufferedStateData = buffer_out_stanza(Packet, State),
    case SendResult of
        ok ->
            case catch maybe_send_ack_request(BufferedStateData) of
                R when is_boolean(R) ->
                    fsm_next_state(StateName, BufferedStateData);
                _ ->
                    ?DEBUG("Send ack request error: ~p, try enter resume session", [SendResult]),
                    maybe_enter_resume_session(BufferedStateData#state.stream_mgmt_id, BufferedStateData)
            end;
        _ ->
            ?DEBUG("Send element error: ~p, try enter resume session", [SendResult]),
            maybe_enter_resume_session(BufferedStateData#state.stream_mgmt_id, BufferedStateData)
    end.

-spec new_id() -> string().
new_id() ->
    randoms:get_string().


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
        ejabberd_tls -> c2s_tls;
        ejabberd_zlib ->
            case ejabberd_zlib:get_sockmod((StateData#state.socket)#socket_state.socket) of
                gen_tcp -> c2s_compressed;
                ejabberd_tls -> c2s_compressed_tls
            end;
        ejabberd_http_poll -> http_poll;
        ejabberd_http_bind -> http_bind;
        _ -> unknown
    end.


-spec process_presence_probe(From :: ejabberd:simple_jid() | ejabberd:jid(),
                             To :: ejabberd:jid(),
                             State :: state()) -> 'ok'.
process_presence_probe(From, To, StateData) ->
    LFrom = jlib:jid_tolower(From),
    LBareFrom = setelement(3, LFrom, <<>>),
    case StateData#state.pres_last of
        undefined ->
            ok;
        _ ->
            case {should_retransmit_last_presence(LFrom, LBareFrom, StateData),
                  specifically_visible_to(LFrom, StateData)} of
                {true, _} ->
                    Timestamp = StateData#state.pres_timestamp,
                    Packet = xml:append_subtags(
                               StateData#state.pres_last,
                               %% To is the one sending the presence (the target of the probe)
                               [jlib:timestamp_to_xml(Timestamp, utc, To, <<>>)]),
                    case privacy_check_packet(StateData, To, From, Packet, out) of
                        deny ->
                            ok;
                        allow ->
                            Pid = element(2, StateData#state.sid),
                            ejabberd_hooks:run(presence_probe_hook,
                                               StateData#state.server,
                                               [From, To, Pid]),
                            %% Don't route a presence probe to oneself
                            case jlib:are_equal_jids(From, To) of
                                false ->
                                    ejabberd_router:route(To, From, Packet);
                                true ->
                                    ok
                            end
                    end;
                {false, true} ->
                    ejabberd_router:route(To, From, #xmlel{name = <<"presence">>});
                _ ->
                    ok
            end
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
    ?SETS:is_element(LFrom, S#state.pres_f)
    orelse (LFrom /= LBareFrom)
    andalso ?SETS:is_element(LBareFrom, S#state.pres_f).

lowcase_and_bare(JID) ->
    LJID = jlib:jid_tolower(JID),
    { LJID, jlib:jid_remove_resource(LJID) }.

invisible_to(LFrom, LBareFrom, S) ->
    ?SETS:is_element(LFrom, S#state.pres_i)
    orelse (LFrom /= LBareFrom)
    andalso ?SETS:is_element(LBareFrom, S#state.pres_i).

%% @doc Is generally invisible, but visible to a particular resource?
specifically_visible_to(LFrom, #state{pres_invis = Invisible} = S) ->
    Invisible
    andalso ?SETS:is_element(LFrom, S#state.pres_f)
    andalso ?SETS:is_element(LFrom, S#state.pres_a).

%% @doc User updates his presence (non-directed presence packet)
-spec presence_update(From :: 'undefined' | ejabberd:jid(),
                      Pkt :: jlib:xmlel(),
                      State :: state()) -> state().
presence_update(From, Packet, StateData) ->
    #xmlel{attrs = Attrs} = Packet,
    case xml:get_attr_s(<<"type">>, Attrs) of
        <<"unavailable">> ->
            Status = case xml:get_subtag(Packet, <<"status">>) of
                         false ->
                             <<>>;
                         StatusTag ->
                             xml:get_tag_cdata(StatusTag)
                     end,
            Info = [{ip, StateData#state.ip}, {conn, StateData#state.conn},
                    {auth_module, StateData#state.auth_module}],
            ejabberd_sm:unset_presence(StateData#state.sid,
                                       StateData#state.user,
                                       StateData#state.server,
                                       StateData#state.resource,
                                       Status,
                                       Info),
            presence_broadcast(StateData, From, StateData#state.pres_a, Packet),
            presence_broadcast(StateData, From, StateData#state.pres_i, Packet),
            StateData#state{pres_last = undefined,
                            pres_timestamp = undefined,
                            pres_a = ?SETS:new(),
                            pres_i = ?SETS:new(),
                            pres_invis = false};
        <<"invisible">> ->
            NewPriority = get_priority_from_presence(Packet),
            update_priority(NewPriority, Packet, StateData),
            NewState =
            if
                not StateData#state.pres_invis ->
                    presence_broadcast(StateData, From,
                                       StateData#state.pres_a,
                                       Packet),
                    presence_broadcast(StateData, From,
                                       StateData#state.pres_i,
                                       Packet),
                    S1 = StateData#state{pres_last = undefined,
                                         pres_timestamp = undefined,
                                         pres_a = ?SETS:new(),
                                         pres_i = ?SETS:new(),
                                         pres_invis = true},
                    presence_broadcast_first(From, S1, Packet);
                true ->
                    StateData
            end,
            NewState;
        <<"error">> ->
            StateData;
        <<"probe">> ->
            StateData;
        <<"subscribe">> ->
            StateData;
        <<"subscribed">> ->
            StateData;
        <<"unsubscribe">> ->
            StateData;
        <<"unsubscribed">> ->
            StateData;
        _ ->
            OldPriority = case StateData#state.pres_last of
                              undefined ->
                                  0;
                              OldPresence ->
                                  get_priority_from_presence(OldPresence)
                          end,
            NewPriority = get_priority_from_presence(Packet),
            Timestamp = calendar:now_to_universal_time(os:timestamp()),
            update_priority(NewPriority, Packet, StateData),
            FromUnavail = (StateData#state.pres_last == undefined) or
            StateData#state.pres_invis,
            ?DEBUG("from unavail = ~p~n", [FromUnavail]),

            NewStateData = StateData#state{pres_last = Packet,
                                           pres_invis = false,
                                           pres_timestamp = Timestamp},
            if
                FromUnavail ->
                    ejabberd_hooks:run(user_available_hook,
                                       NewStateData#state.server,
                                       [NewStateData#state.jid]),
                    NewStateData1 = if NewPriority >= 0 ->
                                           {_, _, Pending} = ejabberd_hooks:run_fold(
                                                               roster_get_subscription_lists,
                                                               NewStateData#state.server,
                                                               {[], [], []},
                                                               [StateData#state.user, NewStateData#state.server]),
                                           resend_offline_messages(NewStateData),
                                           resend_subscription_requests(NewStateData#state{pending_invitations = Pending});
                                       true ->
                                           NewStateData
                                    end,
                    presence_broadcast_first(From, NewStateData1, Packet);
                true ->
                    presence_broadcast_to_trusted(NewStateData,
                                                  From,
                                                  NewStateData#state.pres_f,
                                                  NewStateData#state.pres_a,
                                                  Packet),
                    if OldPriority < 0, NewPriority >= 0 ->
                           resend_offline_messages(NewStateData);
                       true ->
                           ok
                    end,
                    NewStateData
            end
    end.


%% @doc User sends a directed presence packet
-spec presence_track(From :: ejabberd:jid(),
                     To :: ejabberd:jid(),
                     Pkt :: jlib:xmlel(),
                     State :: state()) -> state().
presence_track(From, To, Packet, StateData) ->
    #xmlel{attrs = Attrs} = Packet,
    LTo = jlib:jid_tolower(To),
    User = StateData#state.user,
    Server = StateData#state.server,
    case xml:get_attr_s(<<"type">>, Attrs) of
        <<"unavailable">> ->
            check_privacy_route(From, StateData, From, To, Packet),
            I = remove_element(LTo, StateData#state.pres_i),
            A = remove_element(LTo, StateData#state.pres_a),
            StateData#state{pres_i = I,
                            pres_a = A};
        <<"invisible">> ->
            check_privacy_route(From, StateData, From, To, Packet),
            I = ?SETS:add_element(LTo, StateData#state.pres_i),
            A = remove_element(LTo, StateData#state.pres_a),
            StateData#state{pres_i = I,
                            pres_a = A};
        <<"subscribe">> ->
            ejabberd_hooks:run(roster_out_subscription,
                               Server,
                               [User, Server, To, subscribe]),
            check_privacy_route(From, StateData, jlib:jid_remove_resource(From),
                                To, Packet),
            StateData;
        <<"subscribed">> ->
            ejabberd_hooks:run(roster_out_subscription,
                               Server,
                               [User, Server, To, subscribed]),
            check_privacy_route(From, StateData, jlib:jid_remove_resource(From),
                                To, Packet),
            StateData;
        <<"unsubscribe">> ->
            ejabberd_hooks:run(roster_out_subscription,
                               Server,
                               [User, Server, To, unsubscribe]),
            check_privacy_route(From, StateData, jlib:jid_remove_resource(From),
                                To, Packet),
            StateData;
        <<"unsubscribed">> ->
            ejabberd_hooks:run(roster_out_subscription,
                               Server,
                               [User, Server, To, unsubscribed]),
            check_privacy_route(From, StateData, jlib:jid_remove_resource(From),
                                To, Packet),
            StateData;
        <<"error">> ->
            check_privacy_route(From, StateData, From, To, Packet),
            StateData;
        <<"probe">> ->
            check_privacy_route(From, StateData, From, To, Packet),
            StateData;
        _ ->
            check_privacy_route(From, StateData, From, To, Packet),
            I = remove_element(LTo, StateData#state.pres_i),
            A = ?SETS:add_element(LTo, StateData#state.pres_a),
            StateData#state{pres_i = I,
                            pres_a = A}
    end.


-spec check_privacy_route(From :: 'undefined' | ejabberd:jid(),
                          StateData :: state(),
                          FromRoute :: ejabberd:jid(),
                          To :: ejabberd:jid(),
                          Packet :: jlib:xmlel()) -> 'ok'.
check_privacy_route(From, StateData, FromRoute, To, Packet) ->
    case privacy_check_packet(StateData, From, To, Packet, out) of
        deny ->
            Lang = StateData#state.lang,
            ErrText = "Your active privacy list has denied the routing of this stanza.",
            Err = jlib:make_error_reply(Packet, ?ERRT_NOT_ACCEPTABLE(Lang, ErrText)),
            ejabberd_router:route(To, From, Err),
            ok;
        allow ->
            ejabberd_router:route(FromRoute, To, Packet)
    end.


-spec privacy_check_packet(StateData :: state(),
                           From :: ejabberd:jid(),
                           To :: ejabberd:jid(),
                           Packet :: jlib:xmlel(),
                           Dir :: 'in' | 'out') -> any().
privacy_check_packet(StateData, From, To, Packet, Dir) ->
    ejabberd_hooks:run_fold(
      privacy_check_packet, StateData#state.server,
      allow,
      [StateData#state.user,
       StateData#state.server,
       StateData#state.privacy_list,
       {From, To, Packet},
       Dir]).


%% @doc Check if privacy rules allow this delivery
-spec is_privacy_allow(StateData :: state(),
                       From :: ejabberd:jid(),
                       To :: ejabberd:jid(),
                       Packet :: jlib:xmlel(),
                       Dir :: 'in' | 'out') -> boolean().
is_privacy_allow(StateData, From, To, Packet, Dir) ->
    allow == privacy_check_packet(StateData, From, To, Packet, Dir).


-spec presence_broadcast(State :: state(),
                         From :: 'undefined' | ejabberd:jid(),
                         JIDSet :: gb_set(),
                         Packet :: jlib:xmlel()) -> 'ok'.
presence_broadcast(StateData, From, JIDSet, Packet) ->
    lists:foreach(fun(JID) ->
                          FJID = jlib:make_jid(JID),
                          case privacy_check_packet(StateData, From, FJID, Packet, out) of
                              deny ->
                                  ok;
                              allow ->
                                  ejabberd_router:route(From, FJID, Packet)
                          end
                  end, ?SETS:to_list(JIDSet)).


-spec presence_broadcast_to_trusted(State :: state(),
                                    From :: 'undefined' | ejabberd:jid(),
                                    T :: gb_set(),
                                    A :: gb_set(),
                                    Packet :: jlib:xmlel()) -> 'ok'.
presence_broadcast_to_trusted(StateData, From, T, A, Packet) ->
    lists:foreach(
      fun(JID) ->
              case ?SETS:is_element(JID, T) of
                  true ->
                      FJID = jlib:make_jid(JID),
                      case privacy_check_packet(StateData, From, FJID, Packet, out) of
                          deny ->
                              ok;
                          allow ->
                              ejabberd_router:route(From, FJID, Packet)
                      end;
                  _ ->
                      ok
              end
      end, ?SETS:to_list(A)).


-spec presence_broadcast_first(From :: 'undefined' | ejabberd:jid(),
                               State :: state(),
                               Packet :: jlib:xmlel()) -> state().
presence_broadcast_first(From, StateData, Packet) ->
    ?SETS:fold(fun(JID, X) ->
                       ejabberd_router:route(
                         From,
                         jlib:make_jid(JID),
                         #xmlel{name = <<"presence">>,
                                attrs = [{<<"type">>, <<"probe">>}]}),
                       X
               end,
               [],
               StateData#state.pres_t),
    if
        StateData#state.pres_invis ->
            StateData;
        true ->
            As = ?SETS:fold(
                    fun(JID, A) ->
                            FJID = jlib:make_jid(JID),
                            case privacy_check_packet(StateData, From, FJID, Packet, out) of
                                deny ->
                                    ok;
                                allow ->
                                    ejabberd_router:route(From, FJID, Packet)
                            end,
                            ?SETS:add_element(JID, A)
                    end,
                    StateData#state.pres_a,
                    StateData#state.pres_f),
            StateData#state{pres_a = As}
    end.


-spec remove_element(E :: 'error' | tuple(),
                     Set :: gb_set()) -> gb_set().
remove_element(E, Set) ->
    case ?SETS:is_element(E, Set) of
        true ->
            ?SETS:del_element(E, Set);
        _ ->
            Set
    end.


-spec roster_change(IJID :: ejabberd:simple_jid() | ejabberd:jid(),
                    ISubscription :: from | to | both | none,
                    State :: state()) -> state().
roster_change(IJID, ISubscription, StateData) ->
    LIJID = jlib:jid_tolower(IJID),
    IsFrom = (ISubscription == both) or (ISubscription == from),
    IsTo   = (ISubscription == both) or (ISubscription == to),
    OldIsFrom = ?SETS:is_element(LIJID, StateData#state.pres_f),
    FSet = if
               IsFrom ->
                   ?SETS:add_element(LIJID, StateData#state.pres_f);
               true ->
                   remove_element(LIJID, StateData#state.pres_f)
           end,
    TSet = if
               IsTo ->
                   ?SETS:add_element(LIJID, StateData#state.pres_t);
               true ->
                   remove_element(LIJID, StateData#state.pres_t)
           end,
    case StateData#state.pres_last of
        undefined ->
            StateData#state{pres_f = FSet, pres_t = TSet};
        P ->
            ?DEBUG("roster changed for ~p~n", [StateData#state.user]),
            From = StateData#state.jid,
            To = jlib:make_jid(IJID),
            Cond1 = (not StateData#state.pres_invis) and IsFrom
            and (not OldIsFrom),
            Cond2 = (not IsFrom) and OldIsFrom
            and (?SETS:is_element(LIJID, StateData#state.pres_a) or
                 ?SETS:is_element(LIJID, StateData#state.pres_i)),
            if
                Cond1 ->
                    ?DEBUG("C1: ~p~n", [LIJID]),
                    case privacy_check_packet(StateData, From, To, P, out) of
                        deny ->
                            ok;
                        allow ->
                            ejabberd_router:route(From, To, P)
                    end,
                    A = ?SETS:add_element(LIJID,
                                          StateData#state.pres_a),
                    StateData#state{pres_a = A,
                                    pres_f = FSet,
                                    pres_t = TSet};
                Cond2 ->
                    ?DEBUG("C2: ~p~n", [LIJID]),
                    PU = #xmlel{name = <<"presence">>,
                                attrs = [{<<"type">>, <<"unavailable">>}]},
                    case privacy_check_packet(StateData, From, To, PU, out) of
                        deny ->
                            ok;
                        allow ->
                            ejabberd_router:route(From, To, PU)
                    end,
                    I = remove_element(LIJID,
                                       StateData#state.pres_i),
                    A = remove_element(LIJID,
                                       StateData#state.pres_a),
                    StateData#state{pres_i = I,
                                    pres_a = A,
                                    pres_f = FSet,
                                    pres_t = TSet};
                true ->
                    StateData#state{pres_f = FSet, pres_t = TSet}
            end
    end.


-spec update_priority(Priority :: integer(),
                      Packet :: jlib:xmlel(),
                      State :: state()) -> 'ok'.
update_priority(Priority, Packet, StateData) ->
    Info = [{ip, StateData#state.ip}, {conn, StateData#state.conn},
            {auth_module, StateData#state.auth_module}],
    ejabberd_sm:set_presence(StateData#state.sid,
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


-spec process_privacy_iq(From :: ejabberd:jid(),
                         To :: ejabberd:jid(),
                         IQ :: ejabberd:iq(),
                         State :: state()) -> state().
process_privacy_iq(From, To,
                   #iq{type = Type, sub_el = SubEl} = IQ,
                   StateData) ->
    {Res, NewStateData} =
    case Type of
        get ->
            R = ejabberd_hooks:run_fold(
                  privacy_iq_get, StateData#state.server,
                  {error, ?ERR_FEATURE_NOT_IMPLEMENTED},
                  [From, To, IQ, StateData#state.privacy_list]),
            {R, StateData};
        set ->
            case ejabberd_hooks:run_fold(
                   privacy_iq_set, StateData#state.server,
                   {error, ?ERR_FEATURE_NOT_IMPLEMENTED},
                   [From, To, IQ]) of
                {result, R, NewPrivList} ->
                    {{result, R},
                     StateData#state{privacy_list = NewPrivList}};
                R -> {R, StateData}
            end
    end,
    IQRes =
    case Res of
        {result, Result} ->
            IQ#iq{type = result, sub_el = Result};
        {error, Error} ->
            IQ#iq{type = error, sub_el = [SubEl, Error]}
    end,
    ejabberd_router:route(
      To, From, jlib:iq_to_xml(IQRes)),
    NewStateData.


-spec resend_offline_messages(state()) -> ok.
resend_offline_messages(StateData) ->
    ?DEBUG("resend offline messages~n",[]),
    case ejabberd_hooks:run_fold(
           resend_offline_messages_hook, StateData#state.server,
           [],
           [StateData#state.user, StateData#state.server]) of
        Rs when is_list(Rs) ->
            lists:foreach(
              fun({route,
                   From, To, #xmlel{} = Packet}) ->
                      Pass = case privacy_check_packet(StateData, From, To, Packet, in) of
                                 allow ->
                                     true;
                                 deny ->
                                     false
                             end,
                      if
                          Pass ->
                              %% Attrs2 = jlib:replace_from_to_attrs(
                              %%                 jlib:jid_to_binary(From),
                              %%                 jlib:jid_to_binary(To),
                              %%                 Attrs),
                              %% FixedPacket = {xmlel, Name, Attrs2, Els},
                              %% Use route instead of send_element to go through standard workflow
                              ejabberd_router:route(From, To, Packet);
                          %% send_element(StateData, FixedPacket),
                          %% ejabberd_hooks:run(user_receive_packet,
                          %%                         StateData#state.server,
                          %%                         [StateData#state.jid,
                          %%                          From, To, FixedPacket]);
                          true ->
                              ok
                      end
              end, Rs)
    end.


-spec resend_subscription_requests(state()) -> state().
resend_subscription_requests(#state{pending_invitations = Pending} = StateData) ->
    NewState = lists:foldl(fun(XMLPacket, #state{} = State) ->
                                   send_element(State, XMLPacket),
                                   {value, From} =  xml:get_tag_attr(<<"from">>, XMLPacket),
                                   {value, To} = xml:get_tag_attr(<<"to">>, XMLPacket),
                                   BufferedStateData = buffer_out_stanza({From, To, XMLPacket}, State),
                                   maybe_send_ack_request(BufferedStateData),
                                   BufferedStateData
                           end, StateData, Pending),
    NewState#state{pending_invitations = []}.


get_showtag(undefined) ->
    <<"unavailable">>;
get_showtag(Presence) ->
    case xml:get_path_s(Presence, [{elem, <<"show">>}, cdata]) of
        <<>>      -> <<"available">>;
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
                            xml:replace_tag_attr(<<"xml:lang">>, list_to_binary(Lang), El)
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
                             jlib:make_jid(<<>>, StateData#state.server, <<>>),
                             jlib:make_jid(<<>>, <<>>, <<>>),
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
             _ -> SockMod:peername(Socket)
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
is_ip_blacklisted({IP,_Port}) ->
    ejabberd_hooks:run_fold(check_bl_c2s, false, [IP]).


%% @doc Check from attributes.
-spec check_from(El :: jlib:xmlel(), FromJID :: ejabberd:jid()) -> 'invalid-from'  | jlib:xmlel().
check_from(El, FromJID) ->
    case xml:get_tag_attr(<<"from">>, El) of
        false ->
            El;
        {value, SJID} ->
            JID = jlib:binary_to_jid(SJID),
            case JID of
                error ->
                    'invalid-from';
                #jid{} ->
                    if
                        (JID#jid.luser == FromJID#jid.luser) and
                        (JID#jid.lserver == FromJID#jid.lserver) and
                        (JID#jid.lresource == FromJID#jid.lresource) ->
                            El;
                        (JID#jid.luser == FromJID#jid.luser) and
                        (JID#jid.lserver == FromJID#jid.lserver) and
                        (JID#jid.lresource == <<>>) ->
                            El;
                        true ->
                            'invalid-from'
                    end
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
            bounce_messages()
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
%%% XEP-0191
%%%----------------------------------------------------------------------

-spec route_blocking(What :: 'unblock_all' | {'block',[any()]} | {'unblock',[any()]},
                     State :: state()) -> 'ok'.
route_blocking(What, StateData) ->
    SubEl =
    case What of
        {block, JIDs} ->
            #xmlel{name = <<"block">>,
                   attrs = [{<<"xmlns">>, ?NS_BLOCKING}],
                   children = lists:map(
                                fun(JID) ->
                                        #xmlel{name = <<"item">>,
                                               attrs = [{<<"jid">>, jlib:jid_to_binary(JID)}]}
                                end, JIDs)};
        {unblock, JIDs} ->
            #xmlel{name = <<"unblock">>,
                   attrs = [{<<"xmlns">>, ?NS_BLOCKING}],
                   children = lists:map(
                                fun(JID) ->
                                        #xmlel{name = <<"item">>,
                                               attrs = [{<<"jid">>, jlib:jid_to_binary(JID)}]}
                                end, JIDs)};
        unblock_all ->
            #xmlel{name = <<"unblock">>,
                   attrs = [{<<"xmlns">>, ?NS_BLOCKING}]}
    end,
    PrivPushIQ =
    #iq{type = set, xmlns = ?NS_BLOCKING,
        id = <<"push">>,
        sub_el = [SubEl]},
    F = jlib:jid_remove_resource(StateData#state.jid),
    T = StateData#state.jid,
    PrivPushEl =
    jlib:replace_from_to(
      F,
      T,
      jlib:iq_to_xml(PrivPushIQ)),
    ejabberd_router:route(F, T, PrivPushEl),
    %% No need to replace active privacy list here,
    %% blocking pushes are always accompanied by
    %% Privacy List pushes
    ok.

%%%----------------------------------------------------------------------
%%% JID Set memory footprint reduction code
%%%----------------------------------------------------------------------

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


-spec pack_jid_set(Set :: gb_set(), Pack :: gb_tree()) -> {gb_set(),gb_tree()}.
pack_jid_set(Set, Pack) ->
    Jids = ?SETS:to_list(Set),
    {PackedJids, NewPack} = pack_jids(Jids, Pack, []),
    {?SETS:from_list(PackedJids), NewPack}.


-spec pack_jids([{_,_,_}], Pack :: gb_tree(), Acc :: [any()]) -> {[any()],gb_tree()}.
pack_jids([], Pack, Acc) -> {Acc, Pack};
pack_jids([{U,S,R}=Jid | Jids], Pack, Acc) ->
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


-spec pack_string(String :: binary(), Pack :: gb_tree()) -> {binary(), gb_tree()}.
pack_string(String, Pack) ->
    case gb_trees:lookup(String, Pack) of
        {value, PackedString} ->
            {PackedString, Pack};
        none ->
            {String, gb_trees:insert(String, String, Pack)}
    end.

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
              undefined -> {now(), self()};
              RSID -> RSID
          end,
    ok = mod_stream_management:register_smid(SMID, SID),
    {SD#state{stream_mgmt_id = SMID, sid = SID},
     stream_mgmt_enabled([{<<"id">>, SMID}, {<<"resume">>, <<"true">>}])}.

make_smid() ->
    base64:encode(crypto:rand_bytes(21)).

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
                                       "h attribute too big"}),
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
    %% TODO add delayed on it?
    [ejabberd_router:route(From, To, Packet)
     || {From, To, Packet} <- lists:reverse(Buffer)].

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
        {ok, OldState} = ?GEN_FSM:sync_send_event(Pid, resume),
        SID = {now(), self()},
        Conn = get_conn_type(StateData),
        MergedState = merge_state(OldState,
                                  StateData#state{sid = SID, conn = Conn}),
        Priority = get_priority_from_presence(MergedState#state.pres_last),
        Info = [{ip, MergedState#state.ip},
                {conn, MergedState#state.conn},
                {auth_module, MergedState#state.auth_module}],
        ejabberd_sm:open_session(SID,
                                 MergedState#state.user,
                                 MergedState#state.server,
                                 MergedState#state.resource,
                                 Priority, Info),
        ok = mod_stream_management:register_smid(SMID, SID),
        case stream_mgmt_handle_ack(session_established, El, MergedState) of
            {stop, _, _} = Stop ->
                Stop;
            {next_state, session_established, NSD, _} ->
                try
                    Resumed = stream_mgmt_resumed(NSD#state.stream_mgmt_id,
                                                  NSD#state.stream_mgmt_in),
                    send_element(NSD, Resumed),
                    [send_element(NSD, Packet)
                     || {_, _,Packet} <- lists:reverse(NSD#state.stream_mgmt_buffer)],
                    fsm_next_state(session_established, NSD)
                catch
                    %% errors from send_element
                    _:_ ->
                        ?INFO_MSG("resumption error while resending old stanzas entering resume state again smid: ~p~n",[SMID]),
                        maybe_enter_resume_session(SMID, NSD)
                end
        end
    catch
        _:_ ->
            ?WARNING_MSG("resumption error (invalid response from ~p)~n",
                         [Pid]),
            send_element(StateData, stream_mgmt_failed(<<"item-not-found">>)),
            fsm_next_state(wait_for_bind_or_resume, StateData)
    end;

do_resume_session(SMID, _El, [], StateData) ->
    ?WARNING_MSG("no previous session with stream id ~p~n", [SMID]),
    send_element(StateData, stream_mgmt_failed(<<"item-not-found">>)),
    fsm_next_state(wait_for_bind_or_resume, StateData).

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
                #state.stream_mgmt,
                #state.stream_mgmt_in,
                #state.stream_mgmt_id,
                #state.stream_mgmt_out_acked,
                #state.stream_mgmt_buffer,
                #state.stream_mgmt_buffer_size,
                #state.stream_mgmt_buffer_max,
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
            {F, T, add_timestamp(Timestamp,<<"localhost">>, Packet)}
    end;
maybe_add_timestamp(Packet, _Timestamp) ->
    Packet.

add_timestamp(undefined, _Server, Packet) ->
    Packet;
add_timestamp({_,_,Micro} = TimeStamp, Server, Packet) ->
    {D,{H,M,S}} = calendar:now_to_universal_time(TimeStamp),
    Time = {D,{H,M,S, Micro}},
    case xml:get_subtag(Packet, <<"delay">>) of
        false ->
            TimeStampXML = timestamp_xml(Server, Time),
            xml:append_subtags(Packet, [TimeStampXML]);
        _ ->
            Packet
    end.

timestamp_xml(Server, Time) ->
    FromJID = jlib:make_jid(<<>>, Server, <<>>),
    jlib:timestamp_to_xml(Time, utc, FromJID, <<"SM Storage">>).

defer_resource_constraint_check(#state{stream_mgmt_constraint_check_tref = undefined} = State)->
    Seconds = timer:seconds(?CONSTRAINT_CHECK_TIMEOUT),
    TRef = erlang:send_after(Seconds, self(), check_buffer_full),
    State#state{stream_mgmt_constraint_check_tref = TRef};
defer_resource_constraint_check(State)->
    State.

sasl_success_stanza(ServerOut) ->
    C = case ServerOut of
            undefined -> [];
            _ -> [#xmlcdata{content = jlib:encode_base64(ServerOut)}]
        end,
    #xmlel{name = <<"success">>,
           attrs = [{<<"xmlns">>, ?NS_SASL}],
           children = C}.

sasl_failure_stanza(Error) ->
    #xmlel{name = <<"failure">>,
           attrs = [{<<"xmlns">>, ?NS_SASL}],
           children = [#xmlel{name = Error}]}.

sasl_challenge_stanza(Challenge) ->
    #xmlel{name = <<"challenge">>,
           attrs = [{<<"xmlns">>, ?NS_SASL}],
           children = Challenge}.

handle_sasl_success(State, Props) ->
    handle_sasl_success(State, Props, undefined).
handle_sasl_success(State, Props, ServerOut) ->
    (State#state.sockmod):reset_stream(State#state.socket),
    send_element(State, sasl_success_stanza(ServerOut)),
    U = xml:get_attr_s(username, Props),
    AuthModule = xml:get_attr_s(auth_module, Props),
    ?INFO_MSG("(~w) Accepted authentication for ~s by ~p",
              [State#state.socket, U, AuthModule]),
    NewState = State#state{
                 streamid = new_id(),
                 authenticated = true,
                 auth_module = AuthModule,
                 user = U},
    {wait_for_stream, NewState}.

handle_sasl_step(#state{server = Server, socket= Sock} = State, StepRes) ->
    case StepRes of
        {ok, Props} ->
            handle_sasl_success(State, Props);
        {ok, Props, ServerOut} ->
            handle_sasl_success(State, Props, ServerOut);
        {continue, ServerOut, NewSASLState} ->
            Challenge  = [#xmlcdata{content = jlib:encode_base64(ServerOut)}],
            send_element(State, sasl_challenge_stanza(Challenge)),
            {wait_for_sasl_response, State#state{sasl_state = NewSASLState}};
        {error, Error, Username} ->
            IP = peerip(State#state.sockmod, Sock),
            ?INFO_MSG(
               "(~w) Failed authentication for ~s@~s from IP ~s (~w)",
               [Sock, Username, Server, jlib:ip_to_list(IP), IP]),
            ejabberd_hooks:run(auth_failed, Server, [Username, Server]),
            send_element(State, sasl_failure_stanza(Error)),
            {wait_for_feature_request, State};
        {error, Error} ->
            ejabberd_hooks:run(auth_failed, Server, [unknown, Server]),
            send_element(State, sasl_failure_stanza(Error)),
            {wait_for_feature_request, State}
    end.
