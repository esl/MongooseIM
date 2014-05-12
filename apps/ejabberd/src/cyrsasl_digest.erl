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
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
%%% 02111-1307 USA
%%%
%%%----------------------------------------------------------------------

-module(cyrsasl_digest).
-author('alexey@sevcom.net').

-export([start/1,
         stop/0,
         mech_new/4,
         mech_step/2]).

-include("ejabberd.hrl").

-behaviour(cyrsasl).

-record(state, {step :: integer(),
                nonce,
                username :: ejabberd:user(),
                authzid,
                get_password :: cyrsasl:get_password_fun(),
                check_password :: cyrsasl:check_password_fun(),
                auth_module :: ejabberd_auth:authmodule(),
                host :: ejabberd:server()
              }).

start(_Opts) ->
    cyrsasl:register_mechanism(<<"DIGEST-MD5">>, ?MODULE, true).

stop() ->
    ok.

-spec mech_new(Host :: ejabberd:server(),
               GetPassword :: cyrsasl:get_password_fun(),
               CheckPassword :: cyrsasl:check_password_fun(),
               CheckPasswordDigest :: cyrsasl:check_pass_digest_fun()
               ) -> {ok, tuple()}.
mech_new(Host, GetPassword, _CheckPassword, CheckPasswordDigest) ->
    {ok, #state{step = 1,
                nonce = randoms:get_string(),
                host = Host,
                get_password = GetPassword,
                check_password = CheckPasswordDigest}}.

-spec mech_step(State :: tuple(),
                ClientIn :: any()
                ) -> {ok, proplists:proplist()} | {error, binary()}.
mech_step(#state{step = 1, nonce = Nonce} = State, _) ->
    {continue,
     list_to_binary("nonce=\"" ++ Nonce ++
     "\",qop=\"auth\",charset=utf-8,algorithm=md5-sess"),
     State#state{step = 3}};
mech_step(#state{step = 3, nonce = Nonce} = State, ClientIn) ->
    case parse(ClientIn) of
        bad ->
            {error, <<"bad-protocol">>};
        KeyVals ->
            DigestURI = xml:get_attr_s(<<"digest-uri">>, KeyVals),
            UserName = xml:get_attr_s(<<"username">>, KeyVals),
            case is_digesturi_valid(DigestURI, State#state.host) of
                false ->
                    ?DEBUG("User login not authorized because digest-uri "
                           "seems invalid: ~p", [DigestURI]),
                    {error, <<"not-authorized">>, UserName};
                true ->
                    AuthzId = xml:get_attr_s(<<"authzid">>, KeyVals),
                    case (State#state.get_password)(UserName) of
                        {false, _} ->
                            {error, <<"not-authorized">>, UserName};
                        {Passwd, AuthModule} ->
                                case (State#state.check_password)(UserName, <<>>,
                                        xml:get_attr_s(<<"response">>, KeyVals),
                                        fun(PW) -> response(KeyVals, UserName, PW, Nonce, AuthzId,
                                                <<"AUTHENTICATE">>) end) of
                                {true, _} ->
                                    RspAuth = response(KeyVals,
                                                       UserName, Passwd,
                                                       Nonce, AuthzId, <<>>),
                                    {continue,
                                     list_to_binary([<<"rspauth=">>, RspAuth]),
                                     State#state{step = 5,
                                                 auth_module = AuthModule,
                                                 username = UserName,
                                                 authzid = AuthzId}};
                                false ->
                                    {error, <<"not-authorized">>, UserName};
                                {false, _} ->
                                    {error, <<"not-authorized">>, UserName}
                            end
                    end
            end
    end;
mech_step(#state{step = 5,
                 auth_module = AuthModule,
                 username = UserName,
                 authzid = AuthzId}, <<>>) ->
    {ok, [{username, UserName}, {authzid, AuthzId},
          {auth_module, AuthModule}]};
mech_step(A, B) ->
    ?DEBUG("SASL DIGEST: A ~p B ~p", [A,B]),
    {error, <<"bad-protocol">>}.


-spec parse(binary()) -> 'bad' | [{binary(),binary()}].
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
    Ts :: [{binary(),binary()}]) -> 'bad' | [{K :: binary(), V :: binary()}].
parse4(<<$, , Cs/binary>>, Key, Val, Ts) ->
    parse1(Cs, <<>>, [{Key, binary_reverse(Val)} | Ts]);
parse4(<<$\s, Cs/binary>>, Key, Val, Ts) ->
    parse4(Cs, Key, Val, Ts);
parse4(<<C, Cs/binary>>, Key, Val, Ts) ->
    parse4(Cs, Key, <<C, Val/binary>>, Ts);
parse4(<<>>, Key, Val, Ts) ->
    parse1(<<>>, <<>>, [{Key, binary_reverse(Val)} | Ts]).

binary_reverse(<<>>) ->
    <<>>;
binary_reverse(<<H,T/binary>>) ->
    <<(binary_reverse(T))/binary,H>>.

%% @doc Check if the digest-uri is valid.
%% RFC-2831 allows to provide the IP address in Host,
%% however ejabberd doesn't allow that.
%% If the service (for example jabber.example.org)
%% is provided by several hosts (being one of them server3.example.org),
%% then digest-uri can be like xmpp/server3.example.org/jabber.example.org
%% In that case, ejabberd only checks the service name, not the host.
-spec is_digesturi_valid(DigestURICase :: binary(),
                         JabberHost :: 'undefined' | ejabberd:server()) -> boolean().
is_digesturi_valid(DigestURICase, JabberHost) ->
    DigestURI = stringprep:tolower(DigestURICase),
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

-spec hex(binary(),binary()) -> binary().
hex(<<>>, Res) ->
    binary_reverse(Res);
hex(<<N, Ns/binary>>, Res) ->
    D1 = digit_to_xchar(N rem 16),
    D2 = digit_to_xchar(N div 16),
    hex(Ns, <<D1, D2, Res/binary>>).


-spec response(KeyVals :: [{binary(),binary()}],
               User :: ejabberd:user(),
               Passwd :: binary(),
               Nonce :: binary(),
               AuthzId :: binary(),
               A2Prefix :: <<_:_*96>>) -> binary().
response(KeyVals, User, Passwd, Nonce, AuthzId, A2Prefix) ->
    Realm = xml:get_attr_s(<<"realm">>, KeyVals),
    CNonce = xml:get_attr_s(<<"cnonce">>, KeyVals),
    DigestURI = xml:get_attr_s(<<"digest-uri">>, KeyVals),
    NC = xml:get_attr_s(<<"nc">>, KeyVals),
    QOP = xml:get_attr_s(<<"qop">>, KeyVals),
    A1 = case AuthzId of
             <<>> ->
                 list_to_binary(
                   [crypto:md5([User, <<":">>, Realm, <<":">>, Passwd]),
                     <<":">>, Nonce, <<":">>, CNonce]);
             _ ->
                 list_to_binary(
                   [crypto:md5([User, <<":">>, Realm, <<":">>, Passwd]),
                     <<":">>, Nonce, <<":">>, CNonce, <<":">>, AuthzId])
         end,
    A2 = case QOP of
             <<"auth">> ->
                 [A2Prefix, <<":">>, DigestURI];
             _ ->
                 [A2Prefix, <<":">>, DigestURI,
                     <<":00000000000000000000000000000000">>]
         end,
    T = [hex(crypto:md5(A1)), <<":">>, Nonce, <<":">>,
        NC, <<":">>, CNonce, <<":">>, QOP, <<":">>,
        hex(crypto:md5(A2))],
    hex(crypto:md5(T)).



