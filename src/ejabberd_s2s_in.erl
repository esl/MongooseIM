%%%----------------------------------------------------------------------
%%% File    : ejabberd_s2s_in.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Serve incoming s2s connection
%%% Created :  6 Dec 2002 by Alexey Shchepin <alexey@process-one.net>
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

-module(ejabberd_s2s_in).
-author('alexey@process-one.net').
-behaviour(gen_fsm_compat).

%% External exports
-export([start/2,
         start_link/2,
         match_domain/2,
         socket_type/0]).

%% gen_fsm callbacks
-export([init/1,
         wait_for_stream/2,
         wait_for_feature_request/2,
         stream_established/2,
         handle_event/3,
         handle_sync_event/4,
         code_change/4,
         handle_info/3,
         terminate/3]).

-include("mongoose.hrl").
-include("jlib.hrl").

-record(state, {socket,
                sockmod                 :: ejabberd:sockmod(),
                streamid                :: binary(),
                shaper,
                tls = false             :: boolean(),
                tls_enabled = false     :: boolean(),
                tls_required = false    :: boolean(),
                tls_cert_verify = false :: boolean(),
                tls_options = []        :: [{_, _}],
                server                  :: jid:server() | undefined,
                authenticated = false   :: boolean(),
                auth_domain             :: binary() | undefined,
                connections = dict:new(),
                timer                   :: reference()
              }).
-type state() :: #state{}.

-type statename() :: 'stream_established' | 'wait_for_feature_request'.
%% FSM handler return value
-type fsm_return() :: {'stop', Reason :: 'normal', state()}
                    | {'next_state', statename(), state()}
                    | {'next_state', statename(), state(), Timeout :: integer()}.
%-define(DBGFSM, true).

-ifdef(DBGFSM).
-define(FSMOPTS, [{debug, [trace]}]).
-else.
-define(FSMOPTS, []).
-endif.

%% Module start with or without supervisor:
-ifdef(NO_TRANSIENT_SUPERVISORS).
-define(SUPERVISOR_START, gen_fsm_compat:start(ejabberd_s2s_in, [SockData, Opts],
                                        ?FSMOPTS)).
-else.
-define(SUPERVISOR_START, supervisor:start_child(ejabberd_s2s_in_sup,
                                                 [SockData, Opts])).
-endif.

-define(STREAM_HEADER(Version),
        (<<"<?xml version='1.0'?>"
         "<stream:stream "
         "xmlns:stream='http://etherx.jabber.org/streams' "
         "xmlns='jabber:server' "
         "xmlns:db='jabber:server:dialback' "
         "id='", (StateData#state.streamid)/binary, "'", Version/binary, ">">>)
       ).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
-spec start(_, _) -> {'error', _}
                  | {'ok', 'undefined' | pid()}
                  | {'ok', 'undefined' | pid(), _}.
start(SockData, Opts) ->
    ?SUPERVISOR_START.


-spec start_link(_, _) -> 'ignore' | {'error', _} | {'ok', pid()}.
start_link(SockData, Opts) ->
    gen_fsm_compat:start_link(ejabberd_s2s_in, [SockData, Opts], ?FSMOPTS).


socket_type() ->
    xml_stream.

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
-spec init(_) -> {'ok', 'wait_for_stream', state()}.
init([{SockMod, Socket}, Opts]) ->
    ?DEBUG("started: ~p", [{SockMod, Socket}]),
    Shaper = case lists:keysearch(shaper, 1, Opts) of
                 {value, {_, S}} -> S;
                 _ -> none
             end,
    UseTLS = ejabberd_config:get_local_option(s2s_use_starttls),
    {StartTLS, TLSRequired, TLSCertVerify} = get_tls_params(UseTLS),
    TLSOpts1 = case ejabberd_config:get_local_option(s2s_certfile) of
                  undefined ->
                      [];
                  CertFile ->
                      [{certfile, CertFile}]
              end,
    TLSOpts2 = lists:filter(fun({protocol_options, _}) -> true;
                               ({dhfile, _}) -> true;
                               ({cafile, _}) -> true;
                               ({ciphers, _}) -> true;
                               (_) -> false
                            end, Opts),
    TLSOpts = lists:append(TLSOpts1, TLSOpts2),
    Timer = erlang:start_timer(ejabberd_s2s:timeout(), self(), []),
    {ok, wait_for_stream,
     #state{socket = Socket,
            sockmod = SockMod,
            streamid = new_id(),
            shaper = Shaper,
            tls = StartTLS,
            tls_enabled = false,
            tls_required = TLSRequired,
            tls_cert_verify = TLSCertVerify,
            tls_options = TLSOpts,
            timer = Timer}}.

%%----------------------------------------------------------------------
%% Func: StateName/2
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%%----------------------------------------------------------------------

-spec wait_for_stream(ejabberd:xml_stream_item(), state()) -> fsm_return().
wait_for_stream({xmlstreamstart, _Name, Attrs}, StateData) ->
    case {xml:get_attr_s(<<"xmlns">>, Attrs),
          xml:get_attr_s(<<"xmlns:db">>, Attrs),
          xml:get_attr_s(<<"to">>, Attrs),
          xml:get_attr_s(<<"version">>, Attrs) == <<"1.0">>} of
        {<<"jabber:server">>, _, Server, true} when
              StateData#state.tls and (not StateData#state.authenticated) ->
            send_text(StateData, ?STREAM_HEADER(<<" version='1.0'">>)),
            SASL =
                case StateData#state.tls_enabled of
                    true ->
                        verify_cert_and_get_sasl(StateData#state.sockmod,
                                                 StateData#state.socket,
                                                 StateData#state.tls_cert_verify);
                    _Else ->
                        []
                end,
            StartTLS = get_tls_xmlel(StateData),
            case SASL of
                {error_cert_verif, CertError} ->
                    RemoteServer = xml:get_attr_s(<<"from">>, Attrs),
                    ?INFO_MSG("Closing s2s connection: ~s <--> ~s (~s)",
                      [StateData#state.server, RemoteServer, CertError]),
                    send_text(StateData, exml:to_binary(
                                mongoose_xmpp_errors:policy_violation(<<"en">>, CertError))),
                    {atomic, Pid} = ejabberd_s2s:find_connection(
                                      jid:make(<<"">>, Server, <<"">>),
                                      jid:make(<<"">>, RemoteServer, <<"">>)),
                    ejabberd_s2s_out:stop_connection(Pid),

                    {stop, normal, StateData};
                _ ->
                    send_element(StateData,
                                 #xmlel{name = <<"stream:features">>,
                                        children = SASL ++ StartTLS ++
                                                   ejabberd_hooks:run_fold(
                                                     s2s_stream_features,
                                                     Server,
                                                     [], [Server])}),
                    {next_state, wait_for_feature_request, StateData#state{server = Server}}
            end;
        {<<"jabber:server">>, _, Server, true} when
              StateData#state.authenticated ->
            send_text(StateData, ?STREAM_HEADER(<<" version='1.0'">>)),
            send_element(StateData,
                         #xmlel{name = <<"stream:features">>,
                                children = ejabberd_hooks:run_fold(
                                             s2s_stream_features,
                                             Server,
                                             [], [Server])}),
            {next_state, stream_established, StateData};
        {<<"jabber:server">>, <<"jabber:server:dialback">>, _Server, _} ->
            send_text(StateData, ?STREAM_HEADER(<<"">>)),
            {next_state, stream_established, StateData};
        _ ->
            send_text(StateData, exml:to_binary(mongoose_xmpp_errors:invalid_namespace())),
            {stop, normal, StateData}
    end;
wait_for_stream({xmlstreamerror, _}, StateData) ->
    send_text(StateData,
              <<(?STREAM_HEADER(<<"">>))/binary, (mongoose_xmpp_errors:xml_not_well_formed_bin())/binary,
                (?STREAM_TRAILER)/binary>>),
    {stop, normal, StateData};
wait_for_stream(timeout, StateData) ->
    {stop, normal, StateData};
wait_for_stream(closed, StateData) ->
    {stop, normal, StateData}.


-spec wait_for_feature_request(ejabberd:xml_stream_item(), state()
                              ) -> fsm_return().
wait_for_feature_request({xmlstreamelement, El}, StateData) ->
    #xmlel{name = Name, attrs = Attrs, children = Els} = El,
    TLS = StateData#state.tls,
    TLSEnabled = StateData#state.tls_enabled,
    SockMod = (StateData#state.sockmod):get_sockmod(StateData#state.socket),
    case {xml:get_attr_s(<<"xmlns">>, Attrs), Name} of
        {?NS_TLS, <<"starttls">>} when TLS == true,
                                   TLSEnabled == false,
                                   SockMod == gen_tcp ->
            ?DEBUG(<<"starttls">>, []),
            Socket = StateData#state.socket,
            TLSOpts = case ejabberd_config:get_local_option(
                             {domain_certfile,
                              StateData#state.server}) of
                          undefined ->
                              StateData#state.tls_options;
                          CertFile ->
                              [{certfile, CertFile} |
                               lists:keydelete(
                                 certfile, 1,
                                 StateData#state.tls_options)]
                      end,
            TLSSocket = (StateData#state.sockmod):starttls(
                          Socket, TLSOpts,
                          exml:to_binary(
                            #xmlel{name = <<"proceed">>,
                                   attrs = [{<<"xmlns">>, ?NS_TLS}]})),
            {next_state, wait_for_stream,
             StateData#state{socket = TLSSocket,
                             streamid = new_id(),
                             tls_enabled = true,
                             tls_options = TLSOpts
                            }};
        {?NS_SASL, <<"auth">>} when TLSEnabled ->
            Mech = xml:get_attr_s(<<"mechanism">>, Attrs),
            case Mech of
                <<"EXTERNAL">> ->
                    Auth = jlib:decode_base64(xml:get_cdata(Els)),
                    AuthDomain = jid:nameprep(Auth),
                    CertData = (StateData#state.sockmod):get_peer_certificate(
                                 StateData#state.socket),
                    AuthRes = check_auth_domain(AuthDomain, CertData),
                    handle_auth_res(AuthRes, AuthDomain, StateData);
                _ ->
                    send_element(StateData,
                                 #xmlel{name = <<"failure">>,
                                        attrs = [{<<"xmlns">>, ?NS_SASL}],
                                        children = [#xmlel{name = <<"invalid-mechanism">>}]}),
                    {stop, normal, StateData}
            end;
        _ ->
            stream_established({xmlstreamelement, El}, StateData)
    end;
wait_for_feature_request({xmlstreamend, _Name}, StateData) ->
    send_text(StateData, ?STREAM_TRAILER),
    {stop, normal, StateData};
wait_for_feature_request({xmlstreamerror, _}, StateData) ->
    send_text(StateData, <<(mongoose_xmpp_errors:xml_not_well_formed_bin())/binary, (?STREAM_TRAILER)/binary>>),
    {stop, normal, StateData};
wait_for_feature_request(closed, StateData) ->
    {stop, normal, StateData}.


-spec stream_established(ejabberd:xml_stream_item(), state()) -> fsm_return().
stream_established({xmlstreamelement, El}, StateData) ->
    cancel_timer(StateData#state.timer),
    Timer = erlang:start_timer(ejabberd_s2s:timeout(), self(), []),
    case is_key_packet(El) of
        {key, To, From, Id, Key} ->
            ?DEBUG("GET KEY: ~p", [{To, From, Id, Key}]),
            LTo = jid:nameprep(To),
            LFrom = jid:nameprep(From),
            %% Checks if the from domain is allowed and if the to
            %% domain is handled by this server:
            case {ejabberd_s2s:allow_host(LTo, LFrom),
                  lists:member(LTo, ejabberd_router:dirty_get_all_domains())} of
                {true, true} ->
                    ejabberd_s2s_out:terminate_if_waiting_delay(LTo, LFrom),
                    ejabberd_s2s_out:start(LTo, LFrom,
                                           {verify, self(),
                                            Key, StateData#state.streamid}),
                    Conns = dict:store({LFrom, LTo}, wait_for_verification,
                                        StateData#state.connections),
                    change_shaper(StateData, LTo, jid:make(<<"">>, LFrom, <<"">>)),
                    {next_state,
                     stream_established,
                     StateData#state{connections = Conns,
                                     timer = Timer}};
                {_, false} ->
                    send_text(StateData, exml:to_binary(mongoose_xmpp_errors:host_unknown())),
                    {stop, normal, StateData};
                {false, _} ->
                    send_text(StateData, exml:to_binary(mongoose_xmpp_errors:invalid_from())),
                    {stop, normal, StateData}
            end;
        {verify, To, From, Id, Key} ->
            ?DEBUG("VERIFY KEY: ~p", [{To, From, Id, Key}]),
            LTo = jid:nameprep(To),
            LFrom = jid:nameprep(From),
            Type = case ejabberd_s2s:key({LTo, LFrom}, Id) of
                       Key -> <<"valid">>;
                       _ -> <<"invalid">>
                   end,
            send_element(StateData,
                         #xmlel{name = <<"db:verify">>,
                                attrs = [{<<"from">>, To},
                                         {<<"to">>, From},
                                         {<<"id">>, Id},
                                         {<<"type">>, Type}]}),
            {next_state, stream_established, StateData#state{timer = Timer}};
        _ ->
            NewEl = jlib:remove_attr(<<"xmlns">>, El),
            #xmlel{attrs = Attrs} = NewEl,
            FromS = xml:get_attr_s(<<"from">>, Attrs),
            From = jid:from_binary(FromS),
            ToS = xml:get_attr_s(<<"to">>, Attrs),
            To = jid:from_binary(ToS),
            case {From, To} of
                {error, _} -> ok;
                {_, error} -> ok;
                _ -> route_incoming_stanza(From, To, NewEl, StateData)
            end,
            ejabberd_hooks:run(s2s_loop_debug, [{xmlstreamelement, El}]),
            {next_state, stream_established, StateData#state{timer = Timer}}
    end;
stream_established({valid, From, To}, StateData) ->
    send_element(StateData,
                 #xmlel{name = <<"db:result">>,
                        attrs = [{<<"from">>, To},
                                 {<<"to">>, From},
                                 {<<"type">>, <<"valid">>}]}),
    LFrom = jid:nameprep(From),
    LTo = jid:nameprep(To),
    NSD = StateData#state{
            connections = dict:store({LFrom, LTo}, established,
                                      StateData#state.connections)},
    {next_state, stream_established, NSD};
stream_established({invalid, From, To}, StateData) ->
    send_element(StateData,
                 #xmlel{name = <<"db:result">>,
                        attrs = [{<<"from">>, To},
                                 {<<"to">>, From},
                                 {<<"type">>, <<"invalid">>}]}),
    LFrom = jid:nameprep(From),
    LTo = jid:nameprep(To),
    NSD = StateData#state{
            connections = dict:erase({LFrom, LTo},
                                      StateData#state.connections)},
    {next_state, stream_established, NSD};
stream_established({xmlstreamend, _Name}, StateData) ->
    send_text(StateData, ?STREAM_TRAILER),
    {stop, normal, StateData};
stream_established({xmlstreamerror, _}, StateData) ->
    send_text(StateData,
              <<(mongoose_xmpp_errors:xml_not_well_formed_bin())/binary, (?STREAM_TRAILER)/binary>>),
    {stop, normal, StateData};
stream_established(timeout, StateData) ->
    {stop, normal, StateData};
stream_established(closed, StateData) ->
    {stop, normal, StateData}.

-spec route_incoming_stanza(From :: jid:jid(),
                            To :: jid:jid(),
                            El :: exml:element(),
                            StateData :: state()) ->
    mongoose_acc:t() | error.
route_incoming_stanza(From, To, El, StateData) ->
    LFromS = From#jid.lserver,
    LToS = To#jid.lserver,
    #xmlel{name = Name} = El,
    Acc = mongoose_acc:new(#{ location => ?LOCATION,
                              lserver => LToS,
                              element => El,
                              from_jid => From,
                              to_jid => To }),
    case is_s2s_authenticated(LFromS, LToS, StateData) of
        true ->
            route_stanza(Name, Acc);
        false ->
            case is_s2s_connected(LFromS, LToS, StateData) of
                true ->
                    route_stanza(Name, Acc);
                false ->
                    error
            end
    end.

is_s2s_authenticated(_, _, #state{authenticated = false}) ->
    false;
is_s2s_authenticated(LFrom, LTo, #state{auth_domain = LFrom}) ->
    lists:member(LTo, ejabberd_router:dirty_get_all_domains());
is_s2s_authenticated(_, _, _) ->
    false.

is_s2s_connected(LFrom, LTo, StateData) ->
    case dict:find({LFrom, LTo}, StateData#state.connections) of
        {ok, established} ->
            true;
        _ ->
            false
    end.

-spec route_stanza(binary(), mongoose_acc:t()) -> mongoose_acc:t().
route_stanza(<<"iq">>, Acc) ->
    route_stanza(Acc);
route_stanza(<<"message">>, Acc) ->
    route_stanza(Acc);
route_stanza(<<"presence">>, Acc) ->
    route_stanza(Acc);
route_stanza(_, _Acc) ->
    error.

-spec route_stanza(mongoose_acc:t()) -> mongoose_acc:t().
route_stanza(Acc) ->
    From = mongoose_acc:from_jid(Acc),
    To = mongoose_acc:to_jid(Acc),
    LTo = To#jid.lserver,
    Acc1 = ejabberd_hooks:run_fold(s2s_receive_packet,
                                   LTo, Acc, []),
    ejabberd_router:route(From, To, Acc1).

%%----------------------------------------------------------------------
%% Func: StateName/3
%% Returns: {next_state, NextStateName, NextStateData}            |
%%          {next_state, NextStateName, NextStateData, Timeout}   |
%%          {reply, Reply, NextStateName, NextStateData}          |
%%          {reply, Reply, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                          |
%%          {stop, Reason, Reply, NewStateData}
%%----------------------------------------------------------------------
%state_name(Event, From, StateData) ->
%    Reply = ok,
%    {reply, Reply, state_name, StateData}.

%%----------------------------------------------------------------------
%% Func: handle_event/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%%----------------------------------------------------------------------
handle_event(_Event, StateName, StateData) ->
    {next_state, StateName, StateData}.

%%----------------------------------------------------------------------
%% Func: handle_sync_event/4
%% Returns: The associated StateData for this connection
%%   {reply, Reply, NextStateName, NextStateData}
%%   Reply = {state_infos, [{InfoName::atom(), InfoValue::any()]
%%----------------------------------------------------------------------
-spec handle_sync_event(any(), any(), statename(), state()
                       ) -> {'reply', 'ok' | {'state_infos', [any(), ...]}, atom(), state()}.
handle_sync_event(get_state_infos, _From, StateName, StateData) ->
    SockMod = StateData#state.sockmod,
    {Addr, Port} = try SockMod:peername(StateData#state.socket) of
                      {ok, {A, P}} ->  {A, P};
                      {error, _} -> {unknown, unknown}
                  catch
                      _:_ -> {unknown, unknown}
                  end,
    Domains =   case StateData#state.authenticated of
                    true ->
                        [StateData#state.auth_domain];
                    false ->
                        Connections = StateData#state.connections,
                        [D || {{D, _}, established} <-
                            dict:to_list(Connections)]
                end,
    Infos = [
             {direction, in},
             {statename, StateName},
             {addr, Addr},
             {port, Port},
             {streamid, StateData#state.streamid},
             {tls, StateData#state.tls},
             {tls_enabled, StateData#state.tls_enabled},
             {tls_options, StateData#state.tls_options},
             {authenticated, StateData#state.authenticated},
             {shaper, StateData#state.shaper},
             {sockmod, SockMod},
             {domains, Domains}
            ],
    Reply = {state_infos, Infos},
    {reply, Reply, StateName, StateData};

%%----------------------------------------------------------------------
%% Func: handle_sync_event/4
%% Returns: {next_state, NextStateName, NextStateData}            |
%%          {next_state, NextStateName, NextStateData, Timeout}   |
%%          {reply, Reply, NextStateName, NextStateData}          |
%%          {reply, Reply, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                          |
%%          {stop, Reason, Reply, NewStateData}
%%----------------------------------------------------------------------
handle_sync_event(_Event, _From, StateName, StateData) ->
    Reply = ok,
    {reply, Reply, StateName, StateData}.


code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.

%%----------------------------------------------------------------------
%% Func: handle_info/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%%----------------------------------------------------------------------
-spec handle_info(_, _, _) -> {next_state, atom(), state()} | {stop, normal, state()}.
handle_info({send_text, Text}, StateName, StateData) ->
    ?ERROR_MSG("{s2s_in:send_text, Text}: ~p~n", [{send_text, Text}]), % is it ever called?
    send_text(StateData, Text),
    {next_state, StateName, StateData};
handle_info({timeout, Timer, _}, _StateName,
            #state{timer = Timer} = StateData) ->
    {stop, normal, StateData};
handle_info(_, StateName, StateData) ->
    {next_state, StateName, StateData}.


%%----------------------------------------------------------------------
%% Func: terminate/3
%% Purpose: Shutdown the fsm
%% Returns: any
%%----------------------------------------------------------------------
-spec terminate(any(), statename(), state()) -> 'ok'.
terminate(Reason, _StateName, StateData) ->
    ?DEBUG("terminated: ~p", [Reason]),
    (StateData#state.sockmod):close(StateData#state.socket),
    ok.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

-spec send_text(state(), binary()) -> binary().
send_text(StateData, Text) ->
    (StateData#state.sockmod):send(StateData#state.socket, Text).


-spec send_element(state(), exml:element()) -> binary().
send_element(StateData, El) ->
    send_text(StateData, exml:to_binary(El)).


-spec change_shaper(state(), Host :: 'global' | binary(), jid:jid()) -> any().
change_shaper(StateData, Host, JID) ->
    Shaper = acl:match_rule(Host, StateData#state.shaper, JID),
    (StateData#state.sockmod):change_shaper(StateData#state.socket, Shaper).


-spec new_id() -> binary().
new_id() ->
    mongoose_bin:gen_from_crypto().


-spec cancel_timer(reference()) -> 'ok'.
cancel_timer(Timer) ->
    erlang:cancel_timer(Timer),
    receive
        {timeout, Timer, _} ->
            ok
    after 0 ->
            ok
    end.


-spec is_key_packet(exml:element()) -> 'false' | {'key', _, _, _, binary()}
                                  | {'verify', _, _, _, binary()}.
is_key_packet(#xmlel{name = Name, attrs = Attrs,
                     children = Els}) when Name == <<"db:result">> ->
    {key,
     xml:get_attr_s(<<"to">>, Attrs),
     xml:get_attr_s(<<"from">>, Attrs),
     xml:get_attr_s(<<"id">>, Attrs),
     xml:get_cdata(Els)};
is_key_packet(#xmlel{name = Name, attrs = Attrs,
                     children = Els}) when Name == <<"db:verify">> ->
    {verify,
     xml:get_attr_s(<<"to">>, Attrs),
     xml:get_attr_s(<<"from">>, Attrs),
     xml:get_attr_s(<<"id">>, Attrs),
     xml:get_cdata(Els)};
is_key_packet(_) ->
    false.


-spec match_domain(binary(), binary()) -> boolean().
match_domain(Domain, Domain) ->
    true;
match_domain(Domain, Pattern) ->
    DLabels = binary:split(Domain, <<".">>, [global]),
    PLabels = binary:split(Pattern, <<".">>, [global]),
    match_labels(DLabels, PLabels).


-spec match_labels([binary()], [binary()]) -> boolean().
match_labels([], []) ->
    true;
match_labels([], [_ | _]) ->
    false;
match_labels([_ | _], []) ->
    false;
match_labels([DL | DLabels], [PL | PLabels]) ->
    PLlist = binary_to_list(PL),
    case lists:all(fun(C) -> (($a =< C) andalso (C =< $z))
                                 orelse (($0 =< C) andalso (C =< $9))
                                 orelse (C == $-) orelse (C == $*)
                   end, PLlist) of
        true ->
            Regexp = xmerl_regexp:sh_to_awk(PLlist),
            case re:run(binary_to_list(DL), Regexp, [{capture, none}]) of
                match ->
                    match_labels(DLabels, PLabels);
                nomatch ->
                    false
            end;
        false ->
            false
    end.

verify_cert_and_get_sasl(SockMod, Socket, TLSCertVerify) ->
    case SockMod:get_peer_certificate(Socket) of
        {ok, _} ->
            [#xmlel{name = <<"mechanisms">>,
                    attrs = [{<<"xmlns">>, ?NS_SASL}],
                    children = [#xmlel{name = <<"mechanism">>,
                                       children = [#xmlcdata{content = <<"EXTERNAL">>}]}]}];
        {bad_cert, CertVerifyRes} ->
            check_sasl_tls_certveify(TLSCertVerify, CertVerifyRes);
        no_peer_cert -> []
    end.

check_sasl_tls_certveify(true, CertVerifyRes) ->
    {error_cert_verif, CertVerifyRes};
check_sasl_tls_certveify(false, _) ->
    [].

check_auth_domain(error, _) ->
    false;
check_auth_domain(AuthDomain, {ok, Cert}) ->
    case ejabberd_s2s:domain_utf8_to_ascii(AuthDomain) of
        false ->
            false;
        PCAuthDomain ->
            lists:any(
              fun(D) -> match_domain( PCAuthDomain, D) end,
              cert_utils:get_cert_domains(Cert))
    end;
check_auth_domain(_, _) ->
    false.

handle_auth_res(true, AuthDomain, StateData) ->
    send_element(StateData,
                 #xmlel{name = <<"success">>,
                        attrs = [{<<"xmlns">>, ?NS_SASL}]}),
    ?DEBUG("(~w) Accepted s2s authentication for ~s",
           [StateData#state.socket, AuthDomain]),
    {next_state, wait_for_stream,
     StateData#state{streamid = new_id(),
                     authenticated = true,
                     auth_domain = AuthDomain
                    }};
handle_auth_res(_, _, StateData) ->
    send_element(StateData,
                 #xmlel{name = <<"failure">>,
                        attrs = [{<<"xmlns">>, ?NS_SASL}]}),
    send_text(StateData, ?STREAM_TRAILER),
    {stop, normal, StateData}.


get_tls_params(undefined) ->
    {false, false, false};
get_tls_params(false) ->
    {false, false, false};
get_tls_params(true) ->
    {true, false, false};
get_tls_params(optional) ->
    {true, false, false};
get_tls_params(required) ->
    {true, true, false};
get_tls_params(required_trusted) ->
    {true, true, true}.

get_tls_xmlel(#state{tls_enabled = true}) ->
    [];
get_tls_xmlel(#state{tls_enabled = false, tls_required = false}) ->
    [#xmlel{name = <<"starttls">>,
            attrs = [{<<"xmlns">>, ?NS_TLS}]}];
get_tls_xmlel(#state{tls_enabled = false, tls_required = true}) ->
    [#xmlel{name = <<"starttls">>,
            attrs = [{<<"xmlns">>, ?NS_TLS}],
            children = [#xmlel{name = <<"required">>}]}].
