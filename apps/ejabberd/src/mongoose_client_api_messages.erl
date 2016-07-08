-module(mongoose_client_api_messages).

-export([init/3]).
-export([content_types_provided/2]).
-export([content_types_accepted/2]).
-export([is_authorized/2]).
-export([allowed_methods/2]).

-export([to_json/2]).
-export([send_message/2]).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include_lib("exml/include/exml.hrl").

%% yes, there is no other option, this API has to run over encrypted connection
init({ssl, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_rest}.

is_authorized(Req, State) ->
    mongoose_client_api:is_authorized(Req, State).

content_types_provided(Req, State) ->
    {[
      {<<"application/json">>, to_json}
     ], Req, State}.

content_types_accepted(Req, State) ->
    {[
      {<<"application/json">>, send_message}
     ], Req, State}.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"POST">>], Req, State}.

to_json(Req, {_User, #jid{lserver = Server} = JID} = State) ->
    R = mod_mam:lookup_messages(Server,
                                _ArchiveID = mod_mam:archive_id_int(Server, JID),
                                _ArchiveJID = JID,
                                _Borders = undefined,
                                _RSM = undefined,
                                _Start = undefined,
                                _End = undefined,
                                _Now = undefined,
                                _WithJID = undefined,
                                _PageSize = 10,
                                _LimitPassed = true,
                                _MaxResultLimit = 50,
                                _IsSimple = true),
    {ok, {_, _, Msgs}} = R,
    Resp = [make_json_msg(Msg) || {_ID, _, Msg} <- Msgs],
    {jiffy:encode(Resp), Req, State}.

send_message(Req, {RawUser, FromJID} = State) ->
    {ok, Body, Req2} = cowboy_req:body(Req),
    #{<<"to">> := To, <<"body">> := MsgBody} = jiffy:decode(Body, [return_maps]),
    ToJID = jid:from_binary(To),
    UUID = uuid:uuid_to_string(uuid:get_v4(), binary_standard),
    XMLMsg = build_message(RawUser, To, UUID, MsgBody),
    ejabberd_hooks:run(rest_user_send_packet, FromJID#jid.lserver,
                       [FromJID, ToJID, XMLMsg]),
    ejabberd_router:route(FromJID, ToJID, XMLMsg),
    Resp = #{<<"id">> => UUID},
    Req3 = cowboy_req:set_resp_body(jiffy:encode(Resp), Req2),
    {true, Req3, State}.

build_message(From, To, Id, Body) ->
    Attrs = [{<<"from">>, From},
             {<<"to">>, To},
             {<<"id">>, Id},
             {<<"type">>, <<"chat">>}],
    #xmlel{name = <<"message">>,
           attrs = Attrs,
           children = [#xmlel{name = <<"body">>,
                              children = [#xmlcdata{content = Body}]}]}.

make_json_msg(Msg) ->
    BodyTag = exml_query:path(Msg, [{element, <<"body">>}]),
    #{from => exml_query:attr(Msg, <<"from">>),
      to => exml_query:attr(Msg, <<"to">>),
      id => exml_query:attr(Msg, <<"id">>),
      body => exml_query:cdata(BodyTag)}.

