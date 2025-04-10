%% @doc SASL layer between C2S and MIM internal mongoose_acc.
%%
%% Implements RFC6120 Section 6 – but stanza agnostic, uses mongoose_acc instead
%%
%% This module is intended to have no side-effects, that is, it proccesses between XMPP contents
%% (c2s data, xml stanzas), and MongooseIM internal mongoose_acc,
%% which enables tracing by refs and timestamps.
%% It does not send anything on the socket nor changes the state of the client process.
%% This way, we can decouple SASL from the client process core code.
%% @end
-module(mongoose_c2s_sasl).

-include_lib("kernel/include/logger.hrl").

-export([new/1, start/4, continue/3]).

-type mechanism() :: binary().
-type maybe_username() :: undefined | jid:luser().
-type success() :: #{server_out := undefined | binary(),
                     jid := jid:jid(),
                     auth_module := cyrsasl:sasl_module(),
                     creds := mongoose_credentials:t()}.
-type continue() :: #{server_out := binary()}.
-type failure()  :: #{server_out := binary() | {binary(), undefined | iodata()},
                      maybe_username := maybe_username()}.
-type error() :: #{type := atom(), text := binary()}.
-type result() :: {success,  mongoose_acc:t(), success()}
                | {continue, mongoose_acc:t(), continue()}
                | {failure,  mongoose_acc:t(), failure()}
                | {error,  mongoose_acc:t(), error()}.
-export_type([result/0, success/0, continue/0, failure/0, error/0, mechanism/0]).

-spec new(mongoose_c2s:data()) -> mongoose_acc:t().
new(C2SData) ->
    HostType = mongoose_c2s:get_host_type(C2SData),
    LServer = mongoose_c2s:get_lserver(C2SData),
    LOpts = mongoose_c2s:get_listener_opts(C2SData),
    CredOpts = mongoose_credentials:make_opts(LOpts),
    Creds = mongoose_credentials:new(LServer, HostType, CredOpts),
    CyrSaslState = cyrsasl:server_new(<<"jabber">>, LServer, HostType, <<>>, [], Creds),
    sasl_acc(HostType, LServer, CyrSaslState, Creds).

-spec start(mongoose_c2s:data(), mongoose_acc:t(), mechanism(), binary()) -> result().
start(C2SData, SaslAcc, Mech, ClientIn) ->
    Socket = mongoose_c2s:get_socket(C2SData),
    LOpts = mongoose_c2s:get_listener_opts(C2SData),
    case {mongoose_xmpp_socket:is_ssl(Socket), LOpts} of
        {false, #{tls := #{mode := starttls_required}}} ->
            {error, SaslAcc, #{type => policy_violation, text => <<"Use of STARTTLS required">>}};
        _ ->
            AuthMech = mongoose_c2s:get_auth_mechs(C2SData),
            %% Provide SaslAcc for readonly access, so the cyrsasl mechanism
            %% has more visibility to initialize the mechanism state.
            SocketData = #{socket => Socket, auth_mech => AuthMech, listener_opts => LOpts,
                           sasl_state => SaslAcc},
            CyrSaslState = get_cyrsasl_state_from_acc(SaslAcc),
            CyrSaslResult = cyrsasl:server_start(CyrSaslState, Mech, ClientIn, SocketData),
            handle_sasl_step(C2SData, CyrSaslResult, SaslAcc)
    end.

-spec continue(mongoose_c2s:data(), mongoose_acc:t(), binary()) -> result().
continue(C2SData, SaslAcc, ClientIn) ->
    CyrSaslState = get_cyrsasl_state_from_acc(SaslAcc),
    CyrSaslResult = cyrsasl:server_step(CyrSaslState, ClientIn),
    handle_sasl_step(C2SData, CyrSaslResult, SaslAcc).

-spec handle_sasl_step(mongoose_c2s:data(), cyrsasl:sasl_result(), mongoose_acc:t()) -> result().
handle_sasl_step(C2SData, {ok, Creds}, SaslAcc) ->
    handle_sasl_success(C2SData, Creds, SaslAcc);
handle_sasl_step(C2SData, {continue, ServerOut, NewCyrSaslState}, SaslAcc) ->
    handle_sasl_continue(C2SData, ServerOut, NewCyrSaslState, SaslAcc);
handle_sasl_step(C2SData, {error, Error, Username}, SaslAcc) ->
    handle_sasl_failure(C2SData, Error, Username, undefined, SaslAcc);
handle_sasl_step(C2SData, {error, Error}, SaslAcc) ->
    MaybeUsername = maybe_get_username_from_data(C2SData),
    handle_sasl_failure(C2SData, Error, undefined, MaybeUsername, SaslAcc).

-spec handle_sasl_success(mongoose_c2s:data(), mongoose_credentials:t(), mongoose_acc:t()) -> result().
handle_sasl_success(C2SData, Creds, SaslAcc) ->
    ServerOut = mongoose_credentials:get(Creds, sasl_success_response, undefined),
    AuthModule = mongoose_credentials:get(Creds, auth_module),
    User = mongoose_credentials:get(Creds, username),
    LServer = mongoose_c2s:get_lserver(C2SData),
    Jid = jid:make_bare(User, LServer),
    Ret = #{server_out => ServerOut, jid => Jid, auth_module => AuthModule, creds => Creds},
    {success, SaslAcc, Ret}.

-spec handle_sasl_continue(
        mongoose_c2s:data(), binary(), cyrsasl:sasl_state(), mongoose_acc:t()) -> result().
handle_sasl_continue(_C2SData, ServerOut, NewCyrSaslState, SaslAcc) ->
    SaslAcc1 = set_cyrsasl_state_in_acc(SaslAcc, NewCyrSaslState),
    {continue, SaslAcc1, #{server_out => ServerOut}}.

-spec handle_sasl_failure(
        mongoose_c2s:data(), term(), maybe_username(), maybe_username(), mongoose_acc:t()) -> result().
handle_sasl_failure(_C2SData, Error, undefined, undefined, SaslAcc) ->
    {failure, SaslAcc, #{server_out => Error, maybe_username => undefined}};
handle_sasl_failure(_C2SData, Error, Username, _, SaslAcc) ->
    {failure, SaslAcc, #{server_out => Error, maybe_username => Username}}.

sasl_acc(HostType, LServer, CyrSaslState, Creds) ->
    Acc = mongoose_acc:new(#{host_type => HostType, lserver => LServer, location => ?LOCATION}),
    Fields = [{creds, Creds}, {cyrsasl, CyrSaslState}],
    mongoose_acc:set(?MODULE, Fields, Acc).

-spec set_cyrsasl_state_in_acc(mongoose_acc:t(), cyrsasl:sasl_state()) -> mongoose_acc:t().
set_cyrsasl_state_in_acc(SaslAcc, NewCyrSaslState) ->
    mongoose_acc:set(?MODULE, cyrsasl, NewCyrSaslState, SaslAcc).

-spec get_cyrsasl_state_from_acc(mongoose_acc:t()) -> cyrsasl:sasl_state().
get_cyrsasl_state_from_acc(SaslAcc) ->
    mongoose_acc:get(?MODULE, cyrsasl, SaslAcc).

-spec maybe_get_username_from_data(mongoose_c2s:data()) -> maybe_username().
maybe_get_username_from_data(C2SData) ->
    case mongoose_c2s:get_jid(C2SData) of
        undefined ->
            undefined;
        Jid ->
            {U, _S} = jid:to_lus(Jid),
               U
    end.
