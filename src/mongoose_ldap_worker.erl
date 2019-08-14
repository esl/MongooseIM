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
-include("eldap.hrl").

-type state() :: #{handle := none | eldap:handle(),
                   servers := [string()],
                   encrypt := none | tls,
                   tls_options := list(),
                   port := pos_integer(),
                   root_dn := binary(),
                   password := binary(),
                   connect_interval := pos_integer()}.
-type request() :: {function(), Args :: [any()]}.

%% gen_server callbacks

-spec init(list()) -> {ok, state()}.
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
    ?ERROR_MSG("Invalid cast: ~p", [Cast]),
    {noreply, State}.

-spec handle_info(any(), state()) -> {noreply, state()}.
handle_info(connect, State) ->
    {noreply, connect(State)};
handle_info(Info, State) ->
    ?ERROR_MSG("Unexpected info: ~p", [Info]),
    {noreply, State}.

-spec terminate(any(), state()) -> ok.
terminate(_Reason, #{handle := none}) -> ok;
terminate(_Reason, #{handle := Handle}) -> eldap:close(Handle).

-spec code_change(any(), state(), any()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% internal functions

initial_state(Opts) ->
    Servers = eldap_utils:get_mod_opt(servers, Opts,
                      fun(L) ->
                              lists:map(fun(H) when is_list(H) -> H end, L)
                      end, ["localhost"]),
    Encrypt = eldap_utils:get_mod_opt(encrypt, Opts,
                      fun(tls) -> tls;
                         (none) -> none
                      end, none),
    TLSOptions = eldap_utils:get_mod_opt(tls_options, Opts,
                        fun(L) when is_list(L) -> L end, []),
    Port = eldap_utils:get_mod_opt(port, Opts,
                   fun(I) when is_integer(I), I>0 -> I end,
                   case Encrypt of
                       tls -> ?LDAPS_PORT;
                       starttls -> ?LDAP_PORT;
                       _ -> ?LDAP_PORT
                   end),
    RootDN = eldap_utils:get_mod_opt(rootdn, Opts,
                     fun iolist_to_binary/1,
                     <<"">>),
    Password = eldap_utils:get_mod_opt(password, Opts,
                 fun iolist_to_binary/1,
                 <<"">>),
    ConnectInterval = eldap_utils:get_mod_opt(connect_interval, Opts,
                                  fun(I) when is_integer(I), I>0 -> I end,
                                  default_connect_interval()),
    #{handle => none,
      servers => Servers,
      encrypt => Encrypt,
      tls_options => TLSOptions,
      port => Port,
      root_dn => RootDN,
      password => Password,
      connect_interval => ConnectInterval}.

call_eldap(Request, State) ->
    case do_call_eldap(Request, State) of
        {error, Reason} when Reason =:= ldap_closed;
                             Reason =:= {gen_tcp_error, closed} ->
            ?INFO_MSG("LDAP request failed: connection closed, reconnecting...", []),
            eldap:close(maps:get(handle, State)),
            NewState = connect(State#{handle := none}),
            retry_call_eldap(Request, NewState);
        Result ->
            {Result, State}
    end.

connect(State = #{handle := none,
                  servers := Servers,
                  encrypt := Encrypt,
                  tls_options := TLSOptions,
                  port := Port,
                  root_dn := RootDN,
                  password := Password,
                  connect_interval := ConnectInterval}) ->
    AnonAuth = RootDN =:= <<>> andalso Password =:= <<>>,
    SSLConfig = case Encrypt of
                    tls -> [{ssl, true}, {sslopts, TLSOptions}];
                    none -> [{ssl, false}]
                end,
    case eldap:open(Servers, [{port, Port}, {anon_auth, AnonAuth}] ++ SSLConfig) of
        {ok, Handle} ->
            case eldap:simple_bind(Handle, RootDN, Password) of
                ok ->
                    ?INFO_MSG("Connected to LDAP server", []),
                    State#{handle := Handle};
                Error ->
                    ?ERROR_MSG("LDAP authentication failed: ~p", [Error]),
                    eldap:close(Handle),
                    erlang:send_after(ConnectInterval, self(), connect),
                    State
            end;
        Error ->
            ?ERROR_MSG("LDAP connection failed: ~p", [Error]),
            erlang:send_after(ConnectInterval, self(), connect),
            State
    end.

retry_call_eldap(Request, State) ->
    Result = do_call_eldap(Request, State),
    {Result, State}.

do_call_eldap(_Request, #{handle := none}) -> {error, not_connected};
do_call_eldap({F, Args}, #{handle := Handle}) -> apply(eldap, F, [Handle | Args]).

default_connect_interval() ->
    10000.
