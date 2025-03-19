%% Steps for S2S Dialback.
%% Diagram from https://xmpp.org/extensions/xep-0220.html#intro-howitworks
%%
%% Initiating                 Receiving
%%   Server                    Server
%% -----------               ---------
%%     |                          |
%%     |  [if necessary,          |
%%     |   perform DNS lookup     |
%%     |   on Target Domain,      |
%%     |   open TCP connection,   |
%%     |   and establish stream]  |
%%     | -----------------------> |
%%     |                          |                   Authoritative
%%     |   send dialback key      |                       Server
%%     | -------(STEP 1)--------> |                   -------------
%%     |                          |                          |
%%     |                          |  [if necessary,          |
%%     |                          |   perform DNS lookup,    |
%%     |                          |   on Sender Domain,      |
%%     |                          |   open TCP connection,   |
%%     |                          |   and establish stream]  |
%%     |                          | -----------------------> |
%%     |                          |                          |
%%     |                          |   send verify request    |
%%     |                          | -------(STEP 2)--------> |
%%     |                          |                          |
%%     |                          |   send verify response   |
%%     |                          | <------(STEP 3)--------- |
%%     |                          |
%%     |  report dialback result  |
%%     | <-------(STEP 4)-------- |
%%     |                          |

%% Because db:result and db:verify tags are confusing, use step numbers.
%% (db:result should've been named db:key).

-module(mongoose_s2s_dialback).
-export([step_1/2, step_2/3, step_3/3, step_4/2]).

-export([parse_key/1, parse_validity/1]).

-export([make_key/3]).

-xep([{xep, 185}, {version, "1.0"}]).   %% Dialback Key Generation and Validation
-xep([{xep, 220}, {version, "1.1.1"}]). %% Server Dialback

-include("jlib.hrl").

-compile({inline, [fromto_to_attrs/1, is_valid_to_type/1]}).

%% Initiating server sends dialback key
%% https://xmpp.org/extensions/xep-0220.html#example-1
-spec step_1(ejabberd_s2s:fromto(), ejabberd_s2s:s2s_dialback_key()) -> exml:element().
step_1(FromTo, Key) ->
    #xmlel{name = <<"db:result">>,
           attrs = fromto_to_attrs(FromTo),
           children = [#xmlcdata{content = Key}]}.

%% Receiving server sends verification request to authoritative server (step 2)
-spec step_2(ejabberd_s2s:fromto(), ejabberd_s2s:s2s_dialback_key(), ejabberd_s2s:stream_id()) -> exml:element().
step_2(FromTo, Key, StreamID) ->
    Attrs = fromto_to_attrs(FromTo),
    #xmlel{name = <<"db:verify">>,
           attrs = Attrs#{<<"id">> => StreamID},
           children = [#xmlcdata{content = Key}]}.

%% Receiving server is informed by authoritative server that key is valid or invalid (step 3)
-spec step_3(ejabberd_s2s:fromto(), ejabberd_s2s:stream_id(), boolean()) -> exml:element().
step_3(FromTo, StreamID, IsValid) ->
    Attrs = fromto_to_attrs(FromTo),
    #xmlel{name = <<"db:verify">>,
           attrs = Attrs#{<<"id">> => StreamID,
                          <<"type">> => is_valid_to_type(IsValid)}}.

%% Receiving server sends valid or invalid verification result to initiating server (step 4)
-spec step_4(ejabberd_s2s:fromto(), boolean()) -> exml:element().
step_4(FromTo, IsValid) ->
    Attrs = fromto_to_attrs(FromTo),
    #xmlel{name = <<"db:result">>,
           attrs = Attrs#{<<"type">> => is_valid_to_type(IsValid)}}.

-spec parse_key(exml:element()) -> false
    | {Step :: step_1 | step_2,
       FromTo :: ejabberd_s2s:fromto(),
       StreamID :: ejabberd_s2s:stream_id(),
       Key :: ejabberd_s2s:s2s_dialback_key()}.
parse_key(El = #xmlel{name = <<"db:result">>}) ->
    %% Initiating Server Sends Dialback Key (Step 1)
    parse_key(step_1, El);
parse_key(El = #xmlel{name = <<"db:verify">>}) ->
    %% Receiving Server Sends Verification Request to Authoritative Server (Step 2)
    parse_key(step_2, El);
parse_key(_) ->
    false.

%% Parse dialback verification result.
%% Verification result is stored in the `type' attribute and could be `valid' or `invalid'.
-spec parse_validity(exml:element()) -> false
    | {Step :: step_3 | step_4,
       FromTo :: ejabberd_s2s:fromto(),
       StreamID :: ejabberd_s2s:stream_id(),
       IsValid :: boolean()}.
parse_validity(#xmlel{name = <<"db:verify">>, attrs = Attrs}) ->
    %% Receiving Server is Informed by Authoritative Server that Key is Valid or Invalid (Step 3)
    parse_validity(step_3, Attrs);
parse_validity(#xmlel{name = <<"db:result">>, attrs = Attrs}) ->
    %% Receiving Server Sends Valid or Invalid Verification Result to Initiating Server (Step 4)
    parse_validity(step_4, Attrs);
parse_validity(_) ->
    false.

-spec make_key(ejabberd_s2s:fromto(), ejabberd_s2s:stream_id(), ejabberd_s2s:base16_secret()) ->
    ejabberd_s2s:s2s_dialback_key().
make_key({From, To}, StreamID, Secret) ->
    SecretHashed = binary:encode_hex(crypto:hash(sha256, Secret), lowercase),
    HMac = crypto:mac(hmac, sha256, SecretHashed, [From, " ", To, " ", StreamID]),
    binary:encode_hex(HMac, lowercase).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec parse_key(T, exml:element()) ->
    {T, ejabberd_s2s:fromto(), ejabberd_s2s:stream_id(), ejabberd_s2s:s2s_dialback_key()}.
parse_key(Step, #xmlel{attrs = Attrs} = El) ->
    FromTo = parse_from_to(Attrs),
    StreamID = maps:get(<<"id">>, Attrs, <<>>),
    Key = exml_query:cdata(El),
    {Step, FromTo, StreamID, Key}.

-spec parse_validity(T, exml:attrs()) -> {T, ejabberd_s2s:fromto(), ejabberd_s2s:stream_id(), boolean()}.
parse_validity(Step, Attrs) ->
    FromTo = parse_from_to(Attrs),
    StreamID = maps:get(<<"id">>, Attrs, <<>>),
    IsValid = maps:get(<<"type">>, Attrs, undefined) =:= <<"valid">>,
    {Step, FromTo, StreamID, IsValid}.

-spec parse_from_to(exml:attrs()) -> ejabberd_s2s:fromto().
parse_from_to(#{<<"from">> := Remote, <<"to">> := Local}) ->
    #jid{luser = <<>>, lresource = <<>>, lserver = LRemoteServer} = jid:from_binary(Remote),
    #jid{luser = <<>>, lresource = <<>>, lserver = LLocalServer} = jid:from_binary(Local),
    %% We use fromto() as seen by mongoose_s2s_out and ejabberd_s2s
    {LLocalServer, LRemoteServer}.

-spec fromto_to_attrs(ejabberd_s2s:fromto()) -> exml:attrs().
fromto_to_attrs({LocalServer, RemoteServer}) ->
    #{<<"from">> => LocalServer, <<"to">> => RemoteServer}.

-spec is_valid_to_type(boolean()) -> binary().
is_valid_to_type(true)  -> <<"valid">>;
is_valid_to_type(false) -> <<"invalid">>.
