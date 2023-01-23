-module(mongoose_ldap_worker).

-behaviour(gen_server).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include("mongoose.hrl").

-type state() :: #{handle := none | eldap_utils:handle(),
                   servers := [string()],
                   tls_options := list(),
                   port := pos_integer(),
                   root_dn := binary(),
                   password := binary(),
                   connect_interval := pos_integer()}.
-type request() :: {function(), Args :: [any()]}.

%% gen_server callbacks

-spec init(gen_mod:module_opts()) -> {ok, state()}.
init(Options) ->
    State = initial_state(Options),
    self() ! connect,
    {ok, State}.

-spec handle_call(request(), {pid(), any()}, state()) -> {reply, any(), state()}.
handle_call(Request, _From, State) ->
    {Result, NewState} = call_eldap(Request, State),
    {reply, Result, NewState}.

-spec handle_cast(any(), state()) -> {noreply, state()}.
handle_cast(Cast, State) ->
    ?UNEXPECTED_CAST(Cast),
    {noreply, State}.

-spec handle_info(any(), state()) -> {noreply, state()}.
handle_info(connect, State) ->
    {noreply, connect(State)};
handle_info(Info, State) ->
    ?UNEXPECTED_INFO(Info),
    {noreply, State}.

-spec terminate(any(), state()) -> ok.
terminate(_Reason, #{handle := none}) -> ok;
terminate(_Reason, #{handle := Handle}) -> eldap:close(Handle).

-spec code_change(any(), state(), any()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% internal functions

initial_state(Opts) ->
    Opts#{handle => none}.

call_eldap(Request, State) ->
    case do_call_eldap(Request, State) of
        {error, Reason} when Reason =:= ldap_closed;
                             Reason =:= {gen_tcp_error, closed} ->
            ?LOG_INFO(#{what => ldap_request_failed, reason => Reason,
                        text => <<"LDAP request failed: connection closed, reconnecting...">>}),
            eldap:close(maps:get(handle, State)),
            NewState = connect(State#{handle := none}),
            retry_call_eldap(Request, NewState);
        Result ->
            {Result, State}
    end.

connect(State = #{handle := none,
                  servers := Servers,
                  port := Port,
                  root_dn := RootDN,
                  password := Password,
                  connect_interval := ConnectInterval}) ->
    AnonAuth = RootDN =:= <<>> andalso Password =:= <<>>,
    SSLConfig = case State of
                    #{tls := TLSOptions} -> [{sslopts, just_tls:make_ssl_opts(TLSOptions)}];
                    #{} -> []
                end,
    case eldap:open(Servers, [{port, Port}, {anon_auth, AnonAuth}] ++ SSLConfig) of
        {ok, Handle} ->
            case eldap:simple_bind(Handle, RootDN, Password) of
                ok ->
                    ?LOG_INFO(#{what => ldap_connected,
                                text => <<"Connected to LDAP server">>}),
                    State#{handle := Handle};
                Error ->
                    ?LOG_ERROR(#{what => ldap_auth_failed, reason => Error,
                                 text => <<"LDAP bind returns an error">>,
                                 ldap_servers => Servers, port => Port}),
                    eldap:close(Handle),
                    erlang:send_after(ConnectInterval, self(), connect),
                    State
            end;
        Error ->
            ?LOG_ERROR(#{what => ldap_connect_failed,
                         text => <<"LDAP open returns an error">>,
                         ldap_servers => Servers, port => Port, reason => Error}),
            erlang:send_after(ConnectInterval, self(), connect),
            State
    end.

retry_call_eldap(Request, State) ->
    Result = do_call_eldap(Request, State),
    {Result, State}.

do_call_eldap(_Request, #{handle := none}) -> {error, not_connected};
do_call_eldap({F, Args}, #{handle := Handle}) -> apply(eldap, F, [Handle | Args]).
