%% @doc
%% This module defines callbacks for nksip application
%% Exported functions whill be called when there is new message to MongooseIM
%% or when there is an response to a SIP INVITE sent from MongooseIM to a SIP Proxy
%% @author Michal Piotrowski <michal.piotrowski@erlang-solutions.com>
%%
%%==============================================================================
%% Copyright 2018 Erlang Solutions Ltd.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%==============================================================================
-module(jingle_sip_callbacks).

-include("mongoose.hrl").
-include("jlib.hrl").
-include_lib("nksip/include/nksip.hrl").
-include_lib("nksip/include/nksip_call.hrl").

%% this is because nksip has wrong type specs
-dialyzer({nowarn_function, [sip_invite_unsafe/2,
                             sip_reinvite_unsafe/2,
                             sip_bye/2,
                             sip_cancel/3,
                             send_ringing_session_info/1,
                             invite_resp_callback/1
                             ]}).

%% SIP callbacks
-export([sip_invite/2]).
-export([sip_reinvite/2]).
-export([sip_info/2]).
-export([sip_bye/2]).
-export([sip_cancel/3]).
-export([sip_dialog_update/3]).
-export([invite_resp_callback/1]).

sip_invite(Req, Call) ->
    try
        sip_invite_unsafe(Req, Call)
    catch Class:Reason:StackTrace ->
            ?WARNING_MSG("Error parsing sip invite, class=~p, reason=~p, stacktrace=~p",
                         [Class, Reason, StackTrace]),
            {error, request_not_parsable}
    end.

sip_reinvite(Req, Call) ->
    try
        sip_reinvite_unsafe(Req, Call)
    catch Class:Reason:StackTrace ->
            ?WARNING_MSG("Error parsing sip invite, class=~p, reason=~p, stacktrace=~p",
                         [Class, Reason, StackTrace]),
            {error, request_not_parsable}
    end.


sip_invite_unsafe(Req, _Call) ->
    {FromJID, FromBinary} = get_user_from_sip_msg(from, Req),
    {ToJID, ToBinary} = get_user_from_sip_msg(to, Req),

    case ejabberd_sm:is_offline(ToJID) of
        false ->
            translate_and_deliver_invite(Req, FromJID, FromBinary, ToJID, ToBinary);
        _ ->
            {reply, temporarily_unavailable}
    end.

translate_and_deliver_invite(Req, FromJID, FromBinary, ToJID, ToBinary) ->
    CallID = nksip_sipmsg:header(<<"call-id">>, Req),
    Body = nksip_sipmsg:meta(body, Req),

    {ok, ReqID} = nksip_request:get_handle(Req),

    {CodecMap, SDP} = nksip_sdp_util:extract_codec_map(Body),

    OtherEls = sip_to_jingle:parse_sdp_attributes(SDP#sdp.attributes),

    ContentEls = [sip_to_jingle:sdp_media_to_content_el(Media, CodecMap) || Media <- SDP#sdp.medias],

    JingleEl = jingle_sip_helper:jingle_element(CallID, <<"session-initiate">>, ContentEls ++ OtherEls),

    ok = mod_jingle_sip_backend:set_incoming_request(CallID, ReqID, FromJID, ToJID, JingleEl),

    IQEl = jingle_sip_helper:jingle_iq(ToBinary, FromBinary, JingleEl),
    Acc = mongoose_acc:new(#{ location => ?LOCATION,
                              lserver => FromJID#jid.lserver,
                              element => IQEl,
                              from_jid => FromJID,
                              to_jid => ToJID }),
    maybe_route_to_all_sessions(FromJID, ToJID, Acc, IQEl),

    {reply, ringing}.

sip_reinvite_unsafe(Req, _Call) ->
    ?INFO_MSG("re-INVITE: ~p", [Req]),
    {FromJID, FromBinary} = get_user_from_sip_msg(from, Req),
    {ToJID, ToBinary} = get_user_from_sip_msg(to, Req),

    CallID = nksip_sipmsg:header(<<"call-id">>, Req),
    Body = nksip_sipmsg:meta(body, Req),

    {CodecMap, SDP} = nksip_sdp_util:extract_codec_map(Body),
    RemainingAttrs = SDP#sdp.attributes,
    OtherEls = sip_to_jingle:parse_sdp_attributes(RemainingAttrs),

    ContentEls = [sip_to_jingle:sdp_media_to_content_el(Media, CodecMap) || Media <- SDP#sdp.medias],
    Name = get_action_name_from_sdp(RemainingAttrs, <<"transport-info">>),
    JingleEl = jingle_sip_helper:jingle_element(CallID, Name, ContentEls ++ OtherEls),
    IQEl = jingle_sip_helper:jingle_iq(ToBinary, FromBinary, JingleEl),
    Acc = mongoose_acc:new(#{ location => ?LOCATION,
                              lserver => FromJID#jid.lserver,
                              element => IQEl,
                              from_jid => FromJID,
                              to_jid => ToJID }),
    maybe_route_to_all_sessions(FromJID, ToJID, Acc, IQEl),
    {reply, ok}.

get_action_name_from_sdp(Attrs, Default) ->
    case lists:keyfind(<<"jingle-action">>, 1, Attrs) of
        {_, [Name]} ->
            Name;
        _ ->
            Default
    end.


sip_info(Req, _Call) ->
    {FromJID, FromBinary} = get_user_from_sip_msg(from, Req),
    {ToJID, ToBinary} = get_user_from_sip_msg(to, Req),

    CallID = nksip_sipmsg:header(<<"call-id">>, Req),
    Body = nksip_sipmsg:meta(body, Req),

    ?INFO_MSG("event=sip_info to=~p call_id=~p body:~n~s", [ToBinary, CallID, Body]),

    noreply.

sip_bye(Req, _Call) ->
    {FromJID, FromBinary} = get_user_from_sip_msg(from, Req),
    {ToJID, ToBinary} = get_user_from_sip_msg(to, Req),

    CallID = nksip_sipmsg:header(<<"call-id">>, Req),
    ReasonEl = #xmlel{name = <<"reason">>,
                      children = [#xmlel{name = <<"success">>}]},
    JingleEl = jingle_sip_helper:jingle_element(CallID, <<"session-terminate">>, [ReasonEl]),
    IQEl = jingle_sip_helper:jingle_iq(ToBinary, FromBinary, JingleEl),
    Acc = mongoose_acc:new(#{ location => ?LOCATION,
                              lserver => FromJID#jid.lserver,
                              element => IQEl,
                              from_jid => FromJID,
                              to_jid => ToJID }),
    maybe_route_to_all_sessions(FromJID, ToJID, Acc, IQEl),

    {reply, ok}.

sip_cancel(_InviteReq, Req, _Call) ->
    {FromJID, FromBinary} = get_user_from_sip_msg(from, Req),
    {ToJID, ToBinary} = get_user_from_sip_msg(to, Req),

    CallID = nksip_sipmsg:header(<<"call-id">>, Req),
    ReasonEl = #xmlel{name = <<"reason">>,
                      children = [#xmlel{name = <<"decline">>}]},
    JingleEl = jingle_sip_helper:jingle_element(CallID, <<"session-terminate">>, [ReasonEl]),
    IQEl = jingle_sip_helper:jingle_iq(ToBinary, FromBinary, JingleEl),
    Acc = mongoose_acc:new(#{ location => ?LOCATION,
                              lserver => FromJID#jid.lserver,
                              element => IQEl,
                              from_jid => FromJID,
                              to_jid => ToJID }),
    maybe_route_to_all_sessions(FromJID, ToJID, Acc, IQEl),

    {reply, ok}.

sip_dialog_update(start, Dialog, Call) ->
    {ok, DialogHandle} = nksip_dialog:get_handle(Dialog),
    [Transaction | _] = Call#call.trans,
    case Transaction#trans.class of
        uas ->
            {ok, CallID} = nksip_dialog:call_id(Dialog),
            mod_jingle_sip_backend:set_incoming_handle(CallID, DialogHandle);

        _ ->
            ok
    end,
    noreply;
sip_dialog_update(stop, Dialog, _) ->
    {ok, CallID} = nksip_dialog:call_id(Dialog),

    noreply;
sip_dialog_update(_, _, _) ->
    noreply.

%% @doc
%% This function is called for every response to the SIP INVITE
%% SIP response contains the same headers as request
%% That's why we need to switch `from' and `to' when preparing and routing Jingle
%% to the request originator
%% interpreted status codes:
%% * 180 and 183 - provisional respons - we can send `ringing' session-info
%% * 200 - the invite was accepted we can sent `session-accepted' stanza
%% * 487 - this is to confirm INVITE cancelation from the other side (no action in this case)
%% * 603 - used to decline the INVITE by the reciving side
%% * all error responses between 400 and 700 result in genering session-terminate reason
invite_resp_callback({resp, 180, SIPMsg, _Call}) ->
    send_ringing_session_info(SIPMsg);
invite_resp_callback({resp, 183, SIPMsg, _Call}) ->
    send_ringing_session_info(SIPMsg);
invite_resp_callback({resp, 200, SIPMsg, _Call}) ->
    {ToJID, ToBinary} = get_user_from_sip_msg(from, SIPMsg),
    {FromJID, FromBinary} = get_user_from_sip_msg(to, SIPMsg),

    Body = nksip_sipmsg:meta(body, SIPMsg),
    CallID = nksip_sipmsg:header(<<"call-id">>, SIPMsg),
    {CodecMap, SDP} = nksip_sdp_util:extract_codec_map(Body),
    OtherEls = sip_to_jingle:parse_sdp_attributes(SDP#sdp.attributes),


    ContentEls = [sip_to_jingle:sdp_media_to_content_el(Media, CodecMap) || Media <- SDP#sdp.medias],

    JingleEl = jingle_sip_helper:jingle_element(CallID, <<"session-accept">>, ContentEls ++ OtherEls),
    IQEl = jingle_sip_helper:jingle_iq(ToBinary, FromBinary, JingleEl),
    Acc = mongoose_acc:new(#{ location => ?LOCATION,
                              lserver => FromJID#jid.lserver,
                              element => IQEl,
                              from_jid => FromJID,
                              to_jid => ToJID }),
    ok = mod_jingle_sip_backend:set_outgoing_accepted(CallID),
    maybe_route_to_all_sessions(FromJID, ToJID, Acc, IQEl),
    ok;
invite_resp_callback({resp, 487, _SIPMsg, _Call}) ->
    %% this error response only confirms that that the transaction was canceled
    %% the real `session-terminate` stanza is sent by `sip_cancel/3` callback
    ok;
invite_resp_callback({resp, 486, SIPMsg, _Call}) ->
    {ToJID, ToBinary} = get_user_from_sip_msg(from, SIPMsg),
    {FromJID, FromBinary} = get_user_from_sip_msg(to, SIPMsg),
    CallID = nksip_sipmsg:header(<<"call-id">>, SIPMsg),

    ReasonEl = #xmlel{name = <<"reason">>,
                      children = [#xmlel{name = <<"decline">>}]},
    JingleEl = jingle_sip_helper:jingle_element(CallID, <<"session-terminate">>, [ReasonEl]),
    IQEl = jingle_sip_helper:jingle_iq(ToBinary, FromBinary, JingleEl),
    Acc = mongoose_acc:new(#{ location => ?LOCATION,
                              lserver => FromJID#jid.lserver,
                              element => IQEl,
                              from_jid => FromJID,
                              to_jid => ToJID }),
    maybe_route_to_all_sessions(FromJID, ToJID, Acc, IQEl),
    ok;
invite_resp_callback({resp, ErrorCode, SIPMsg, _Call})
  when ErrorCode >= 400, ErrorCode =< 700 ->
    {ToJID, ToBinary} = get_user_from_sip_msg(from, SIPMsg),
    {FromJID, FromBinary} = get_user_from_sip_msg(to, SIPMsg),
    CallID = nksip_sipmsg:header(<<"call-id">>, SIPMsg),

    ReasonEl = make_session_terminate_reason_el(ErrorCode, SIPMsg),

    JingleEl = jingle_sip_helper:jingle_element(CallID, <<"session-terminate">>, [ReasonEl]),
    IQEl = jingle_sip_helper:jingle_iq(ToBinary, FromBinary, JingleEl),
    Acc = mongoose_acc:new(#{ location => ?LOCATION,
                              lserver => FromJID#jid.lserver,
                              element => IQEl,
                              from_jid => FromJID,
                              to_jid => ToJID }),
    maybe_route_to_all_sessions(FromJID, ToJID, Acc, IQEl),
    ok;


invite_resp_callback(Data) ->
    ?ERROR_MSG("Unknown SIP resp: ~p", [Data]).

send_ringing_session_info(SIPMsg) ->
    %% SIP response contains the same headers as request
    %% That's why we need to switch `from` and `to` when preparing Jingle packet
    {ToJID, ToBinary} = get_user_from_sip_msg(from, SIPMsg),
    {FromJID, FromBinary} = get_user_from_sip_msg(to, SIPMsg),

    DialogHandle = nksip_sipmsg:meta(dialog_handle, SIPMsg),
    CallID = nksip_sipmsg:header(<<"call-id">>, SIPMsg),

    mod_jingle_sip_backend:set_outgoing_handle(CallID, DialogHandle, FromJID, ToJID),

    RingingEl = #xmlel{name = <<"ringing">>,
                       attrs = [{<<"xmlns">>, <<"urn:xmpp:jingle:apps:rtp:info:1">>}]},
    JingleEl = jingle_sip_helper:jingle_element(CallID, <<"session-info">>, [RingingEl]),
    IQEl = jingle_sip_helper:jingle_iq(ToBinary, FromBinary, JingleEl),
    Acc = mongoose_acc:new(#{ location => ?LOCATION,
                              lserver => FromJID#jid.lserver,
                              element => IQEl,
                              from_jid => FromJID,
                              to_jid => ToJID }),
    maybe_route_to_all_sessions(FromJID, ToJID, Acc, IQEl),
    ok.

get_user_from_sip_msg(Field, SIPMsg) ->
    URI = nksip_sipmsg:meta(Field, SIPMsg),
    #uri{user = ToUserIn, domain = ToDomain, path = ToPath} = URI,

    Resource = path_to_res(ToPath),

    ToUser = jingle_sip_helper:maybe_rewrite_from_phone(ToDomain, ToUserIn),

    ToJID = jid:make(ToUser, ToDomain, Resource),
    {ToJID, jid:to_binary({ToUser, ToDomain, Resource})}.

path_to_res(<<"/", Rest/binary>>) ->
    Rest;
path_to_res(Other) ->
    Other.

make_session_terminate_reason_el(ErrorCode, #sipmsg{class = {resp, ErrorCode, Binary}}) ->
    Reason = #xmlel{name = <<"general-error">>},
    Details = #xmlel{name = <<"sip-error">>,
                     attrs = [{<<"code">>, integer_to_binary(ErrorCode)}],
                     children = [#xmlcdata{content = Binary}]},
    #xmlel{name = <<"reason">>,
           children = [Reason, Details]}.

maybe_route_to_all_sessions(From, To, Acc, Packet) ->
    PResources = ejabberd_sm:get_user_present_resources(To),
    lists:foreach(
      fun({_, R}) ->
              ejabberd_router:route(From, jid:replace_resource(To, R), Acc, Packet)
      end, PResources).

