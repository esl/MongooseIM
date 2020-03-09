%%%----------------------------------------------------------------------
%%% File    : ejabberd_service.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : External component management (XEP-0114)
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
%%% Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
%%%
%%%----------------------------------------------------------------------

-module(ejabberd_service).
-author('alexey@process-one.net').
-xep([{xep, 114}, {version, "1.6"}]).

-behaviour(p1_fsm).
-behaviour(mongoose_packet_handler).

%% External exports
-export([start/2,
         start_link/2,
         send_text/2,
         send_element/2,
         socket_type/0]).

%% gen_fsm callbacks
-export([init/1,
         wait_for_stream/2,
         wait_for_handshake/2,
         stream_established/2,
         handle_event/3,
         handle_sync_event/4,
         code_change/4,
         handle_info/3,
         terminate/3,
         print_state/1]).

%% packet handler callback
-export([process_packet/5]).

-include("mongoose.hrl").
-include("jlib.hrl").
-include("external_component.hrl").

-type conflict_behaviour() :: disconnect | kick_old.

-record(state, {socket,
                sockmod      :: ejabberd:sockmod(),
                socket_monitor,
                streamid,
                password     :: binary(),
                host         :: binary() | undefined,
                is_subdomain :: boolean(),
                hidden_components = false :: boolean(),
                conflict_behaviour :: conflict_behaviour(),
                access,
                check_from
              }).
-type state() :: #state{}.

-type statename() :: wait_for_stream
                   | wait_for_handshake
                   | stream_established.
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

-define(STREAM_HEADER,
        <<"<?xml version='1.0'?>"
        "<stream:stream "
        "xmlns:stream='http://etherx.jabber.org/streams' "
        "xmlns='jabber:component:accept' "
        "id='~s' from='~s'>">>
       ).

-define(INVALID_HEADER_ERR,
        <<"<stream:stream "
        "xmlns:stream='http://etherx.jabber.org/streams'>"
        "<stream:error>Invalid Stream Header</stream:error>"
        "</stream:stream>">>
       ).

-define(INVALID_HANDSHAKE_ERR,
        <<"<stream:error>"
        "<not-authorized xmlns='urn:ietf:params:xml:ns:xmpp-streams'/>"
        "<text xmlns='urn:ietf:params:xml:ns:xmpp-streams' xml:lang='en'>"
        "Invalid Handshake</text>"
        "</stream:error>"
        "</stream:stream>">>
       ).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
-spec start(_, _) -> {'error', _} | {'ok', 'undefined' | pid()} | {'ok', 'undefined' | pid(), _}.
start(SockData, Opts) ->
    supervisor:start_child(ejabberd_service_sup, [SockData, Opts]).


-spec start_link(_, list()) -> 'ignore' | {'error', _} | {'ok', pid()}.
start_link(SockData, Opts) ->
    p1_fsm:start_link(ejabberd_service, [SockData, Opts],
                        fsm_limit_opts(Opts) ++ ?FSMOPTS).


socket_type() ->
    xml_stream.

%%%----------------------------------------------------------------------
%%% mongoose_packet_handler callback
%%%----------------------------------------------------------------------

-spec process_packet(Acc :: mongoose_acc:t(), From :: jid:jid(), To :: jid:jid(),
    El :: exml:element(), Pid :: pid()) -> any().
process_packet(Acc, From, To, El, Pid) ->
    Pid ! {route, From, To, Acc}.

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
-spec init([list() | {atom() | tuple(), _}, ...]) -> {'ok', 'wait_for_stream', state()}.
init([{SockMod, Socket}, Opts]) ->
    ?INFO_MSG("(~w) External service connected", [Socket]),
    Access = case lists:keysearch(access, 1, Opts) of
                 {value, {_, A}} -> A;
                 _ -> all
             end,
    {password, Password} = lists:keyfind(password, 1, Opts),
    Shaper = case lists:keysearch(shaper_rule, 1, Opts) of
                 {value, {_, S}} -> S;
                 _ -> none
             end,
    CheckFrom = case lists:keysearch(service_check_from, 1, Opts) of
                 {value, {_, CF}} -> CF;
                 _ -> true
             end,
    SockMod:change_shaper(Socket, Shaper),
    SocketMonitor = SockMod:monitor(Socket),
    HiddenComponents = proplists:get_value(hidden_components, Opts, false),
    ConflictBehaviour = proplists:get_value(conflict_behaviour, Opts, disconnect),
    {ok, wait_for_stream, #state{socket = Socket,
                                 sockmod = SockMod,
                                 socket_monitor = SocketMonitor,
                                 streamid = new_id(),
                                 password = Password,
                                 access = Access,
                                 check_from = CheckFrom,
                                 is_subdomain = false,
                                 hidden_components = HiddenComponents,
                                 conflict_behaviour = ConflictBehaviour
                                 }}.

%%----------------------------------------------------------------------
%% Func: StateName/2
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%%----------------------------------------------------------------------

-spec wait_for_stream(ejabberd:xml_stream_item(), state()) -> fsm_return().
wait_for_stream({xmlstreamstart, _Name, Attrs}, StateData) ->
    case xml:get_attr_s(<<"xmlns">>, Attrs) of
        <<"jabber:component:accept">> ->
            %% Note: XEP-0114 requires to check that destination is a Jabber
            %% component served by this Jabber server.
            %% However several transports don't respect that,
            %% so ejabberd doesn't check 'to' attribute (EJAB-717)
            To = xml:get_attr_s(<<"to">>, Attrs),
            Header = io_lib:format(?STREAM_HEADER, [StateData#state.streamid, To]),
            IsSubdomain = case xml:get_attr_s(<<"is_subdomain">>, Attrs) of
                <<"true">> -> true;
                _          -> false
            end,
            send_text(StateData, list_to_binary(Header)),
            StateData1 = StateData#state{host = To,
                                         is_subdomain = IsSubdomain},
            {next_state, wait_for_handshake, StateData1};
        _ ->
            send_text(StateData, ?INVALID_HEADER_ERR),
            {stop, normal, StateData}
    end;
wait_for_stream({xmlstreamerror, _}, StateData) ->
    Header = io_lib:format(?STREAM_HEADER,
                           [<<"none">>, ?MYNAME]),
    send_text(StateData, <<(iolist_to_binary(Header))/binary,
                           (mongoose_xmpp_errors:xml_not_well_formed_bin())/binary, (?STREAM_TRAILER)/binary>>),
    {stop, normal, StateData};
wait_for_stream(closed, StateData) ->
    {stop, normal, StateData};
wait_for_stream(replaced, StateData) ->
    {stop, normal, StateData}.


-spec wait_for_handshake(ejabberd:xml_stream_item(), state()) -> fsm_return().
wait_for_handshake({xmlstreamelement, El}, StateData) ->
    #xmlel{name = Name, children = Els} = El,
    case {Name, xml:get_cdata(Els)} of
        {<<"handshake">>, Digest} ->
            case sha:sha1_hex(StateData#state.streamid ++
                         StateData#state.password) of
                Digest ->
                    try_register_routes(StateData);
                _ ->
                    send_text(StateData, ?INVALID_HANDSHAKE_ERR),
                    {stop, normal, StateData}
            end;
        _ ->
            {next_state, wait_for_handshake, StateData}
    end;
wait_for_handshake({xmlstreamend, _Name}, StateData) ->
    {stop, normal, StateData};
wait_for_handshake({xmlstreamerror, _}, StateData) ->
    send_text(StateData, <<(mongoose_xmpp_errors:xml_not_well_formed_bin())/binary, (?STREAM_TRAILER)/binary>>),
    {stop, normal, StateData};
wait_for_handshake(replaced, StateData) ->
    %% We don't expect to receive replaced before handshake
    do_disconnect_on_conflict(StateData);
wait_for_handshake(closed, StateData) ->
    {stop, normal, StateData}.

-spec stream_established(ejabberd:xml_stream_item(), state()) -> fsm_return().
stream_established({xmlstreamelement, El}, StateData) ->
    NewEl = jlib:remove_attr(<<"xmlns">>, El),
    #xmlel{name = Name, attrs = Attrs} = NewEl,
    From = xml:get_attr_s(<<"from">>, Attrs),
    FromJID = case StateData#state.check_from of
                  %% If the admin does not want to check the from field
                  %% when accept packets from any address.
                  %% In this case, the component can send packet of
                  %% behalf of the server users.
                  false -> jid:from_binary(From);
                  %% The default is the standard behaviour in XEP-0114
                  _ ->
                      FromJID1 = jid:from_binary(From),
                      case FromJID1 of
                          #jid{lserver = Server} ->
                              case Server =:= StateData#state.host of
                                  true -> FromJID1;
                                  false -> error
                              end;
                          _ -> error
                      end
              end,
    To = xml:get_attr_s(<<"to">>, Attrs),
    ToJID = case To of
                <<>> -> error;
                _ -> jid:from_binary(To)
            end,
    if ((Name == <<"iq">>) or
        (Name == <<"message">>) or
        (Name == <<"presence">>)) and
       (ToJID /= error) and (FromJID /= error) ->
            ejabberd_router:route(FromJID, ToJID, NewEl);
       true ->
            ?INFO_MSG("Not valid Name (~p) or error in FromJID (~p) or ToJID (~p)~n", [Name, FromJID, ToJID]),
            Err = jlib:make_error_reply(NewEl, mongoose_xmpp_errors:bad_request()),
            send_element(StateData, Err),
            error
    end,
    {next_state, stream_established, StateData};
stream_established({xmlstreamend, _Name}, StateData) ->
    % TODO ??
    {stop, normal, StateData};
stream_established({xmlstreamerror, _}, StateData) ->
    send_text(StateData, <<(mongoose_xmpp_errors:xml_not_well_formed_bin())/binary, (?STREAM_TRAILER)/binary>>),
    {stop, normal, StateData};
stream_established(replaced, StateData) ->
    do_disconnect_on_conflict(StateData);
stream_established(closed, StateData) ->
    % TODO ??
    {stop, normal, StateData}.


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
handle_info({send_text, Text}, StateName, StateData) ->
    % is it ever called?
    ?ERROR_MSG("{service:send_text, Text}: ~p", [{send_text, Text}]),
    send_text(StateData, Text),
    {next_state, StateName, StateData};
handle_info({send_element, El}, StateName, StateData) ->
    % is it ever called?
    ?ERROR_MSG("{service:send_element, El}: ~p~n", [{send_text, El}]),
    send_element(StateData, El),
    {next_state, StateName, StateData};
handle_info({route, From, To, Acc}, StateName, StateData) ->
    Packet = mongoose_acc:element(Acc),
    ?DEBUG("event=packet_to_component,component=\"~s\",from=\"~s\",to=\"~s\"",
           [component_host(StateData), jid:to_binary(From), jid:to_binary(To)]),
    case acl:match_rule(global, StateData#state.access, From) of
        allow ->
            mongoose_hooks:packet_to_component(Acc, From, To),
            Attrs2 = jlib:replace_from_to_attrs(jid:to_binary(From),
                                                jid:to_binary(To),
                                                Packet#xmlel.attrs),
            Text = exml:to_binary(Packet#xmlel{ attrs = Attrs2 }),
            send_text(StateData, Text);
        deny ->
            ejabberd_router:route_error_reply(To, From, Acc, mongoose_xmpp_errors:not_allowed())
    end,
    {next_state, StateName, StateData};
handle_info({'DOWN', Monitor, _Type, _Object, _Info}, _StateName, StateData)
  when Monitor == StateData#state.socket_monitor ->
    {stop, normal, StateData};
handle_info(Info, StateName, StateData) ->
    ?ERROR_MSG("Unexpected info: ~p", [Info]),
    {next_state, StateName, StateData}.


%%----------------------------------------------------------------------
%% Func: terminate/3
%% Purpose: Shutdown the fsm
%% Returns: any
%%----------------------------------------------------------------------
terminate(Reason, StateName, StateData) ->
    ?INFO_MSG("terminated: ~p", [Reason]),
    case StateName of
        stream_established ->
            unregister_routes(StateData);
        _ ->
            ok
    end,
    (StateData#state.sockmod):close(StateData#state.socket),
    ok.

%%----------------------------------------------------------------------
%% Func: print_state/1
%% Purpose: Prepare the state to be printed on error log
%% Returns: State to print
%%----------------------------------------------------------------------
print_state(State) ->
    State.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

-spec send_text(state(), binary()) -> binary().
send_text(StateData, Text) ->
    ?DEBUG("event=send_text,component=\"~s\",text=\"~s\"", [component_host(StateData), Text]),
    (StateData#state.sockmod):send(StateData#state.socket, Text).


-spec send_element(state(), exml:element()) -> binary().
send_element(StateData, El) ->
    send_text(StateData, exml:to_binary(El)).


-spec new_id() -> string().
new_id() ->
    binary_to_list(mongoose_bin:gen_from_crypto()).

-spec component_host(state()) -> binary() | string().
component_host(#state{ host = undefined }) -> "undefined";
component_host(#state{ host = Host }) -> Host.

-spec fsm_limit_opts(maybe_improper_list()) -> [{'max_queue', integer()}].
fsm_limit_opts(Opts) ->
    case lists:keysearch(max_fsm_queue, 1, Opts) of
        {value, {_, N}} when is_integer(N) ->
            [{max_queue, N}];
        _ ->
            case ejabberd_config:get_local_option(max_fsm_queue) of
                N when is_integer(N) ->
                    [{max_queue, N}];
                _ ->
                    []
            end
    end.

try_register_routes(StateData) ->
    try_register_routes(StateData, 3).

try_register_routes(StateData, Retries) ->
    case register_routes(StateData) of
        ok ->
            send_text(StateData, <<"<handshake/>">>),
            {next_state, stream_established, StateData};
        {error, Reason} ->
            RoutesInfo = lookup_routes(StateData),
            ConflictBehaviour = StateData#state.conflict_behaviour,
            ?ERROR_MSG("event=component_registration_conflict "
                       "reason=~p conflict_behaviour=~p retries=~p routes_info=~p",
                       [Reason, ConflictBehaviour, Retries, RoutesInfo]),
            handle_registration_conflict(ConflictBehaviour, RoutesInfo, StateData, Retries)
    end.

routes_info_to_pids(RoutesInfo) ->
    {_Hosts, ExtComponentsPerHost} = lists:unzip(RoutesInfo),
    %% Flatten the list of lists
    ExtComponents = lists:append(ExtComponentsPerHost),
    %% Ignore handlers from other modules
    [mongoose_packet_handler:extra(H)
     || #external_component{handler = H} <- ExtComponents,
        mongoose_packet_handler:module(H) =:= ?MODULE].

handle_registration_conflict(kick_old, RoutesInfo, StateData, Retries) when Retries > 0 ->
    Pids = routes_info_to_pids(RoutesInfo),
    Results = lists:map(fun stop_process/1, Pids),
    AllOk = lists:all(fun(Result) -> Result =:= ok end, Results),
    case AllOk of
        true ->
            %% Do recursive call
            try_register_routes(StateData, Retries - 1);
        false ->
            ?ERROR_MSG("event=component_registration_kick_failed "
                       "pids=~p results=~p disconnecting_next",
                       [Pids, Results]),
            do_disconnect_on_conflict(StateData)
    end;
handle_registration_conflict(_Behaviour, _RoutesInfo, StateData, _Retries) ->
    do_disconnect_on_conflict(StateData).

do_disconnect_on_conflict(StateData) ->
    send_text(StateData, exml:to_binary(mongoose_xmpp_errors:stream_conflict())),
    {stop, normal, StateData}.

lookup_routes(StateData) ->
    Routes = get_routes(StateData),
    [{Route, ejabberd_router:lookup_component(Route)} || Route <- Routes].

-spec register_routes(state()) -> any().
register_routes(StateData = #state{hidden_components = AreHidden}) ->
    Routes = get_routes(StateData),
    Handler = mongoose_packet_handler:new(?MODULE, self()),
    ejabberd_router:register_components(Routes, node(), Handler, AreHidden).

-spec unregister_routes(state()) -> any().
unregister_routes(StateData) ->
    Routes = get_routes(StateData),
    ejabberd_router:unregister_components(Routes).

get_routes(#state{host=Subdomain, is_subdomain=true}) ->
    Hosts = ejabberd_config:get_global_option(hosts),
    component_routes(Subdomain, Hosts);
get_routes(#state{host=Host}) ->
    [Host].

-spec component_routes(binary(), [binary()]) -> [binary()].
component_routes(Subdomain, Hosts) ->
    [<<Subdomain/binary, ".", Host/binary>> || Host <- Hosts].

stop_process(Pid) ->
    p1_fsm:send_event(Pid, replaced),
    MonRef = erlang:monitor(process, Pid),
    receive
        {'DOWN', MonRef, process, Pid, Reason} ->
            ok
    after 5000 ->
            erlang:demonitor(MonRef, [flush]),
            {error, timeout}
    end.
