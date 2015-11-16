-module(legacy_stream_helper).

-export([stream_start/1]).
-export([stream_start_pre_xmpp_1_0/1]).
-export([start_stream_pre_xmpp_1_0/3]).
-export([failed_legacy_auth/3]).
-export([legacy_auth_digest/3]).
-export([legacy_auth_plain/3]).

-include_lib("escalus/include/escalus_xmlns.hrl").
-include_lib("escalus/include/escalus.hrl").
-include_lib("exml/include/exml.hrl").
-include_lib("exml/include/exml_stream.hrl").

-define(NS_AUTH, <<"jabber:iq:auth">>).


stream_start_pre_xmpp_1_0(To) ->
    stream_start(lists:keystore(version, 1, default_context(To), {version, <<>>})).


stream_start(Context) ->
    %% Be careful! The closing slash here is a hack to enable implementation of from_template/2
    %% to parse the snippet properly. In standard XMPP <stream:stream> is just opening of an XML
    %% element, NOT A SELF CLOSING element.
    T = <<"<stream:stream {{version}} xml:lang='en' xmlns='jabber:client' "
          "               to='{{to}}' "
          "               xmlns:stream='{{stream_ns}}' />">>,
    %% So we rewrap the parsed contents from #xmlel{} to #xmlstreamstart{} here.
    #xmlel{name = Name, attrs = Attrs, children = []} = escalus_stanza:from_template(T, Context),
    #xmlstreamstart{name = Name, attrs = Attrs}.

default_context(To) ->
    [{version, <<"version='1.0'">>},
     {to, To},
     {stream_ns, ?NS_XMPP}].

start_stream_pre_xmpp_1_0(Conn, Props, UnusedFeatures) ->
    escalus:send(Conn, stream_start_pre_xmpp_1_0(escalus_users:get_server([], Props))),
    #xmlstreamstart{attrs = StreamAttrs} = StreamStart = escalus:wait_for_stanza(Conn),
    escalus:assert(is_stream_start, StreamStart),
    {<<"id">>, StreamID} = lists:keyfind(<<"id">>, 1, StreamAttrs),
    {Conn, [{stream_id, StreamID} | Props], UnusedFeatures}.

failed_legacy_auth(Conn, Props, UnusedFeatures) ->
    {stream_id, StreamID} = lists:keyfind(stream_id, 1, Props),
    [Username, _, Password] = escalus_users:get_usp([], Props),
    Digest = list_to_binary(generate_digest(StreamID, Password)),
    AuthReq = escalus_stanza:iq_set(?NS_AUTH, [username(Username), digest(Digest)]),
    escalus:send(Conn, AuthReq),
    Response = escalus:wait_for_stanza(Conn),
    %% This is the success case - we want to assert the error case.
    %% And the error case is achived by sending req without resource
    escalus:assert(is_error, [<<"modify">>, <<"not-acceptable">>], Response),
    {Conn, Props, UnusedFeatures}.

legacy_auth_digest(Conn, Props, UnusedFeatures) ->
    {stream_id, StreamID} = lists:keyfind(stream_id, 1, Props),
    [Username, _, Password] = escalus_users:get_usp([], Props),
    Digest = list_to_binary(generate_digest(StreamID, Password)),
    AuthReq = escalus_stanza:iq_set(?NS_AUTH,
                                    [username(Username), digest(Digest), res(<<"res">>)]),
    escalus:send(Conn, AuthReq),
    Response = escalus:wait_for_stanza(Conn),
    escalus:assert(is_iq_result, [AuthReq], Response),
    {Conn, Props, UnusedFeatures}.

legacy_auth_plain(Conn, Props, UnusedFeatures) ->
    [Username, _, Password] = escalus_users:get_usp([], Props),
    AuthReq = escalus_stanza:iq_set(?NS_AUTH,
                                    [username(Username), password(Password), res(<<"res">>)]),
    escalus:send(Conn, AuthReq),
    Response = escalus:wait_for_stanza(Conn),
    escalus:assert(is_iq_result, [AuthReq], Response),
    {Conn, Props, UnusedFeatures}.

res(Res) when is_binary(Res) ->
    #xmlel{name = <<"resource">>,
           children = [#xmlcdata{content = Res}]}.

username(Username) when is_binary(Username) ->
    #xmlel{name = <<"username">>,
           children = [#xmlcdata{content = Username}]}.

digest(Digest) when is_binary(Digest) ->
    #xmlel{name = <<"digest">>,
           children = [#xmlcdata{content = Digest}]}.

password(Password) when is_binary(Password) ->
    #xmlel{name = <<"password">>,
           children = [#xmlcdata{content = Password}]}.

generate_digest(SID, Password) ->
    %% compute digest
    D = binary_to_list(SID) ++ binary_to_list(Password),
    sha(D).

digit_to_xchar(D) when (D >= 0) and (D < 10) ->
    D + 48;
digit_to_xchar(D) ->
    D + 87.

sha(Text) ->
    Bin = crypto:hash(sha, Text),
    lists:reverse(ints_to_rxstr(binary_to_list(Bin), [])).

ints_to_rxstr([], Res) ->
    Res;
ints_to_rxstr([N | Ns], Res) ->
    ints_to_rxstr(Ns, [digit_to_xchar(N rem 16),
                       digit_to_xchar(N div 16) | Res]).


