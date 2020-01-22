-module(mongoose_client_api_messages).
-behaviour(cowboy_rest).

-export([trails/0]).

-export([init/2]).
-export([content_types_provided/2]).
-export([content_types_accepted/2]).
-export([is_authorized/2]).
-export([allowed_methods/2]).

-export([to_json/2]).
-export([send_message/2]).

-export([encode/2]).

-export([maybe_integer/1]).
-export([maybe_before_to_us/2]).

-include("mongoose.hrl").
-include("jlib.hrl").
-include("mongoose_rsm.hrl").
-include_lib("exml/include/exml.hrl").

trails() ->
    mongoose_client_api_messages_doc:trails().

init(Req, Opts) ->
    mongoose_client_api:init(Req, Opts).

is_authorized(Req, State) ->
    mongoose_client_api:is_authorized(Req, State).

content_types_provided(Req, State) ->
    {[
      {{<<"application">>, <<"json">>, '*'}, to_json}
     ], Req, State}.

content_types_accepted(Req, State) ->
    {[
      {{<<"application">>, <<"json">>, '*'}, send_message}
     ], Req, State}.

allowed_methods(Req, State) ->
    {[<<"OPTIONS">>, <<"GET">>, <<"POST">>], Req, State}.

to_json(Req, #{jid := JID} = State) ->
    With = cowboy_req:binding(with, Req),
    WithJID = maybe_jid(With),
    maybe_to_json_with_jid(WithJID, JID, Req, State).

maybe_to_json_with_jid(error, _, Req, State) ->
    Req2 = cowboy_req:reply(404, Req),
    {stop, Req2, State};
maybe_to_json_with_jid(WithJID, #jid{lserver = Server} = JID, Req, State) ->
    Now = os:system_time(microsecond),
    ArchiveID = mod_mam:archive_id_int(Server, JID),
    QS = cowboy_req:parse_qs(Req),
    PageSize = maybe_integer(proplists:get_value(<<"limit">>, QS, <<"50">>)),
    Before = maybe_integer(proplists:get_value(<<"before">>, QS)),
    End = maybe_before_to_us(Before, Now),
    RSM = #rsm_in{direction = before, id = undefined},
    R = mod_mam:lookup_messages(Server,
                                #{archive_id => ArchiveID,
                                  owner_jid => JID,
                                  rsm => RSM,
                                  borders => undefined,
                                  start_ts => undefined,
                                  end_ts => End,
                                  now => Now,
                                  with_jid => WithJID,
                                  search_text => undefined,
                                  page_size => PageSize,
                                  limit_passed => true,
                                  max_result_limit => 50,
                                  is_simple => true}),
    {ok, {_, _, Msgs}} = R,
    Resp = [make_json_msg(Msg, MAMId) || {MAMId, _, Msg} <- Msgs],
    {jiffy:encode(Resp), Req, State}.

send_message(Req, #{user := RawUser, jid := FromJID} = State) ->
    {ok, Body, Req2} = cowboy_req:read_body(Req),
    case mongoose_client_api:json_to_map(Body) of
        {ok, #{<<"to">> := To, <<"body">> := MsgBody}} when is_binary(To), is_binary(MsgBody) ->
            ToJID = jid:from_binary(To),
            UUID = uuid:uuid_to_string(uuid:get_v4(), binary_standard),
            XMLMsg0 = build_message(RawUser, To, UUID, MsgBody),
            Acc0 = mongoose_acc:new(#{ location => ?LOCATION,
                                       lserver => FromJID#jid.lserver,
                                       from_jid => FromJID,
                                       to_jid => ToJID,
                                       element => XMLMsg0 }),
            Acc1 = ejabberd_hooks:run_fold(rest_user_send_packet, FromJID#jid.lserver, Acc0,
                                           [FromJID, ToJID, XMLMsg0]),
            XMLMsg1 = mongoose_acc:element(Acc1),
            ejabberd_router:route(FromJID, ToJID, Acc1, XMLMsg1),
            Resp = #{<<"id">> => UUID},
            Req3 = cowboy_req:set_resp_body(jiffy:encode(Resp), Req2),
            {true, Req3, State};
        _ ->
            mongoose_client_api:bad_request(Req2, State)
    end.

build_message(From, To, Id, Body) ->
    Attrs = [{<<"from">>, From},
             {<<"to">>, To},
             {<<"id">>, Id},
             {<<"type">>, <<"chat">>}],
    #xmlel{name = <<"message">>,
           attrs = Attrs,
           children = [#xmlel{name = <<"body">>,
                              children = [#xmlcdata{content = Body}]}]}.

make_json_msg(Msg, MAMId) ->
    {Microsec, _} = mod_mam_utils:decode_compact_uuid(MAMId),
    encode(Msg, Microsec div 1000).

-spec encode(exml:item(), integer()) -> map().
encode(Msg, Timestamp) ->

    %Smack library specific query for properties.
    RawMsgProps = exml_query:subelement_with_name_and_ns(
                                        Msg,
                                        <<"properties">>,
                                        <<"http://www.jivesoftware.com/xmlns/xmpp/properties">>),

    BodyTag = exml_query:path(Msg, [{element, <<"body">>}]),

    ExtensionList =
      case RawMsgProps of
           #xmlel{children = Children} ->
                                        Props = [convert_prop_child(Child) || Child <- Children],
                                        [{<<"properties">>, maps:from_list(Props)}];
                                     _ ->
                                        []
      end,

    L = [{<<"from">>, exml_query:attr(Msg, <<"from">>)},
         {<<"to">>, exml_query:attr(Msg, <<"to">>)},
         {<<"id">>, exml_query:attr(Msg, <<"id">>)},
         {<<"body">>, exml_query:cdata(BodyTag)},
         {<<"timestamp">>, Timestamp} | ExtensionList],


    maps:from_list(L).

convert_prop_child(Child)->
    Name = exml_query:path(Child, [{element, <<"name">>}, cdata]),
    Value = exml_query:path(Child, [{element, <<"value">>}, cdata]),
    {Name, Value}.

maybe_jid(undefined) ->
    undefined;
maybe_jid(JID) ->
    jid:from_binary(JID).

maybe_integer(undefined) ->
    undefined;
maybe_integer(Val) ->
    binary_to_integer(Val).

maybe_before_to_us(undefined, Now) ->
    Now;
maybe_before_to_us(Timestamp, _) ->
   Timestamp * 1000.
