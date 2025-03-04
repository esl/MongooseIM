%%%----------------------------------------------------------------------
%%% File    : cyrsasl_digest.erl
%%% Author  : Alexey Shchepin <alexey@sevcom.net>
%%% Purpose : DIGEST-MD5 SASL mechanism
%%% Created : 11 Mar 2003 by Alexey Shchepin <alexey@sevcom.net>
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

-module(cyrsasl_digest).
-author('alexey@sevcom.net').

-export([mechanism/0,
         mech_new/3,
         mech_step/2]).

-ignore_xref([mech_new/3]).

-deprecated({'_', '_', next_major_release}).

-include("mongoose.hrl").

-behaviour(cyrsasl).

-record(state, {step :: integer(),
                nonce :: binary(),
                username :: jid:user() | undefined,
                authzid,
                auth_module :: ejabberd_auth:authmodule(),
                host :: jid:server(),
                creds :: mongoose_credentials:t()
              }).

-type state() :: #state{}.

-spec mechanism() -> cyrsasl:mechanism().
mechanism() ->
    <<"DIGEST-MD5">>.

-spec mech_new(Host   :: jid:server(),
               Creds  :: mongoose_credentials:t(),
               Socket :: term()) -> {ok, state()}.
mech_new(Host, Creds, _Socket) ->
    Text = <<"The DIGEST-MD5 authentication mechanism is deprecated and "
        " will be removed in the next release, please consider using"
        " any of the SCRAM-SHA methods or equivalent instead.">>,
    mongoose_deprecations:log(
        {?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY},
        #{what => sasl_digest_md5_deprecated, text => Text},
        [{log_level, warning}]),
    {ok, #state{step = 1,
                nonce = mongoose_bin:gen_from_crypto(),
                host = Host,
                creds = Creds}}.

-spec mech_step(State :: tuple(), ClientIn :: any()) -> R when
      R :: {ok, mongoose_credentials:t()}
         | cyrsasl:error().
mech_step(#state{step = 1, nonce = Nonce} = State, _) ->
    {continue,
     <<"nonce=\"", Nonce/binary, "\",qop=\"auth\",charset=utf-8,algorithm=md5-sess">>,
     State#state{step = 3}};
mech_step(#state{step = 3, nonce = Nonce} = State, ClientIn) ->
    case parse(ClientIn) of
        bad ->
            {error, <<"bad-protocol">>};
        KeyVals ->
            authorize_if_uri_valid(State, KeyVals, Nonce)
    end;
mech_step(#state{step = 5,
                 auth_module = AuthModule,
                 username = UserName,
                 authzid = AuthzId,
                 creds = Creds}, <<>>) ->
    {ok, mongoose_credentials:extend(Creds, [{username, UserName},
                                             {authzid, AuthzId},
                                             {auth_module, AuthModule}])};
mech_step(State, Msg) ->
    ?LOG_DEBUG(#{what => sasl_digest_error_bad_protocol, sasl_state => State, message => Msg}),
    {error, <<"bad-protocol">>}.


authorize_if_uri_valid(State, KeyVals, Nonce) ->
    UserName = get_attr_s(KeyVals, <<"username">>),
    DigestURI = get_attr_s(KeyVals, <<"digest-uri">>),
    case is_digesturi_valid(DigestURI, State#state.host) of
        false ->
            ?LOG_DEBUG(#{what => unauthorized_login, reason => invalid_digest_uri,
                         message => DigestURI, user => UserName}),
            {error, <<"not-authorized">>, UserName};
        true ->
            maybe_authorize(UserName, KeyVals, Nonce, State)
    end.

maybe_authorize(UserName, KeyVals, Nonce, State) ->
    AuthzId = get_attr_s(KeyVals, <<"authzid">>),
    LServer = mongoose_credentials:lserver(State#state.creds),
    HostType = mongoose_credentials:host_type(State#state.creds),
    JID = jid:make_bare(UserName, LServer),
    case ejabberd_auth:get_passterm_with_authmodule(HostType, JID) of
        false ->
            {error, <<"not-authorized">>, UserName};
        {Passwd, AuthModule} ->
            DigestGen = fun(PW) -> response(KeyVals, UserName, PW, Nonce, AuthzId,
                                            <<"AUTHENTICATE">>)
                        end,
            ExtraCreds = [{username, UserName},
                          {password, <<>>},
                          {digest, get_attr_s(KeyVals, <<"response">>)},
                          {digest_gen, DigestGen}],
            Request = mongoose_credentials:extend(State#state.creds, ExtraCreds),
            do_authorize(UserName, KeyVals, Nonce, Passwd, Request, AuthzId, AuthModule, State)
    end.

do_authorize(UserName, KeyVals, Nonce, Passwd, Request, AuthzId, AuthModule, State) ->
    case ejabberd_auth:authorize(Request) of
        {ok, Result} ->
            RspAuth = response(KeyVals,
                               UserName, Passwd,
                               Nonce, AuthzId, <<>>),
            {continue,
             list_to_binary([<<"rspauth=">>, RspAuth]),
             State#state{step = 5,
                         auth_module = AuthModule,
                         username = UserName,
                         authzid = AuthzId,
                         creds = Result}};
      {error, not_authorized} ->
            {error, <<"not-authorized">>, UserName}
    end.

-spec parse(binary()) -> 'bad' | [{binary(), binary()}].
parse(S) ->
    parse1(S, <<>>, []).

parse1(<<$=, Cs/binary>>, S, Ts) ->
    parse2(Cs, binary_reverse(S), <<>>, Ts);
parse1(<<$,, Cs/binary>>, <<>>, Ts) ->
    parse1(Cs, <<>>, Ts);
parse1(<<$\s, Cs/binary>>, <<>>, Ts) ->
    parse1(Cs, <<>>, Ts);
parse1(<<C, Cs/binary>>, S, Ts) ->
    parse1(Cs, <<C, S/binary>>, Ts);
parse1(<<>>, <<>>, T) ->
    lists:reverse(T);
parse1(<<>>, _S, _T) ->
    bad.

parse2(<<$\", Cs/binary>>, Key, Val, Ts) ->
    parse3(Cs, Key, Val, Ts);
parse2(<<C, Cs/binary>>, Key, Val, Ts) ->
    parse4(Cs, Key, <<C, Val/binary>>, Ts);
parse2(<<>>, _, _, _) ->
    bad.

parse3(<<$\", Cs/binary>>, Key, Val, Ts) ->
    parse4(Cs, Key, Val, Ts);
parse3(<<$\\, C, Cs/binary>>, Key, Val, Ts) ->
    parse3(Cs, Key, <<C, Val/binary>>, Ts);
parse3(<<C, Cs/binary>>, Key, Val, Ts) ->
    parse3(Cs, Key, <<C, Val/binary>>, Ts);
parse3(<<>>, _, _, _) ->
    bad.

-spec parse4(binary(),
    Key :: binary(),
    Val :: binary(),
    Ts :: [{binary(), binary()}]) -> 'bad' | [{K :: binary(), V :: binary()}].
parse4(<<$,, Cs/binary>>, Key, Val, Ts) ->
    parse1(Cs, <<>>, [{Key, binary_reverse(Val)} | Ts]);
parse4(<<$\s, Cs/binary>>, Key, Val, Ts) ->
    parse4(Cs, Key, Val, Ts);
parse4(<<C, Cs/binary>>, Key, Val, Ts) ->
    parse4(Cs, Key, <<C, Val/binary>>, Ts);
parse4(<<>>, Key, Val, Ts) ->
    parse1(<<>>, <<>>, [{Key, binary_reverse(Val)} | Ts]).

binary_reverse(<<>>) ->
    <<>>;
binary_reverse(<<H, T/binary>>) ->
    <<(binary_reverse(T))/binary, H>>.

%% @doc Check if the digest-uri is valid.
%% RFC-2831 allows to provide the IP address in Host,
%% however ejabberd doesn't allow that.
%% If the service (for example jabber.example.org)
%% is provided by several hosts (being one of them server3.example.org),
%% then digest-uri can be like xmpp/server3.example.org/jabber.example.org
%% In that case, ejabberd only checks the service name, not the host.
-spec is_digesturi_valid(DigestURICase :: binary(),
                         JabberHost :: 'undefined' | jid:server()) -> boolean().
is_digesturi_valid(DigestURICase, JabberHost) ->
    DigestURI = jid:str_tolower(DigestURICase),
    case catch binary:split(DigestURI, <<"/">>) of
        [<<"xmpp">>, Host] when Host == JabberHost ->
            true;
        [<<"xmpp">>, _Host, ServName] when ServName == JabberHost ->
            true;
        _ ->
            false
    end.


-spec digit_to_xchar(byte()) -> char().
digit_to_xchar(D) when (D >= 0) and (D < 10) ->
    D + 48;
digit_to_xchar(D) ->
    D + 87.

-spec hex(binary()) -> binary().
hex(S) ->
    hex(S, <<>>).

-spec hex(binary(), binary()) -> binary().
hex(<<>>, Res) ->
    binary_reverse(Res);
hex(<<N, Ns/binary>>, Res) ->
    D1 = digit_to_xchar(N rem 16),
    D2 = digit_to_xchar(N div 16),
    hex(Ns, <<D1, D2, Res/binary>>).


-spec response(KeyVals :: [{binary(), binary()}],
               User :: jid:user(),
               Passwd :: binary(),
               Nonce :: binary(),
               AuthzId :: binary(),
               A2Prefix :: <<_:_*96>>) -> binary().
response(KeyVals, User, Passwd, Nonce, AuthzId, A2Prefix) ->
    Realm = get_attr_s(KeyVals, <<"realm">>),
    CNonce = get_attr_s(KeyVals, <<"cnonce">>),
    DigestURI = get_attr_s(KeyVals, <<"digest-uri">>),
    NC = get_attr_s(KeyVals, <<"nc">>),
    QOP = get_attr_s(KeyVals, <<"qop">>),
    A1 = case AuthzId of
             <<>> ->
                 list_to_binary(
                   [crypto:hash(md5, [User, <<":">>, Realm, <<":">>, Passwd]),
                     <<":">>, Nonce, <<":">>, CNonce]);
             _ ->
                 list_to_binary(
                   [crypto:hash(md5, [User, <<":">>, Realm, <<":">>, Passwd]),
                     <<":">>, Nonce, <<":">>, CNonce, <<":">>, AuthzId])
         end,
    A2 = case QOP of
             <<"auth">> ->
                 [A2Prefix, <<":">>, DigestURI];
             _ ->
                 [A2Prefix, <<":">>, DigestURI,
                     <<":00000000000000000000000000000000">>]
         end,
    T = [hex(crypto:hash(md5, A1)), <<":">>, Nonce, <<":">>,
        NC, <<":">>, CNonce, <<":">>, QOP, <<":">>,
        hex(crypto:hash(md5, A2))],
    hex(crypto:hash(md5, T)).

-spec get_attr_s([{binary(), binary()}], binary()) -> binary().
get_attr_s(Attrs, Name) ->
    case lists:keyfind(Name, 1, Attrs) of
        {Name, Value} ->
            Value;
        false ->
            <<>>
    end.
