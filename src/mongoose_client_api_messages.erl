-module(mongoose_client_api_messages).

-export([init/3]).
-export([rest_init/2]).
-export([content_types_provided/2]).
-export([content_types_accepted/2]).
-export([is_authorized/2]).
-export([allowed_methods/2]).

-export([to_json/2]).
-export([send_message/2]).

-export([encode/2]).

-export([maybe_integer_qs_val/1]).
-export([maybe_before_to_us/2]).

-include("mongoose.hrl").
-include("jlib.hrl").
-include("mongoose_rsm.hrl").
-include_lib("exml/include/exml.hrl").

init(_Transport, _Req, _Opts) ->
    {upgrade, protocol, cowboy_rest}.

rest_init(Req, HandlerOpts) ->
    mongoose_client_api:rest_init(Req, HandlerOpts).

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
    {With, Req2} = cowboy_req:binding(with, Req),
    WithJID = maybe_jid(With),
    maybe_to_json_with_jid(WithJID, JID, Req2, State).

maybe_to_json_with_jid(error, _, Req, State) ->
    Req2 = cowboy_req:reply(404, Req),
    {halt, Req2, State};
maybe_to_json_with_jid(WithJID, #jid{lserver = Server} = JID, Req, State) ->
    Now = p1_time_compat:os_system_time(micro_seconds),
    ArchiveID = mod_mam:archive_id_int(Server, JID),
    {PageSize, Req2} = maybe_integer_qs_val(cowboy_req:qs_val(<<"limit">>, Req, <<"50">>)),
    {Before, Req3} = maybe_integer_qs_val(cowboy_req:qs_val(<<"before">>, Req2)),
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
    {jiffy:encode(Resp), Req3, State}.

send_message(Req, #{user := RawUser, jid := FromJID} = State) ->
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

make_json_msg(Msg, MAMId) ->
    {Microsec, _} = mod_mam_utils:decode_compact_uuid(MAMId),
    encode(Msg, Microsec div 1000).

-spec encode(exml:item(), integer()) -> map().
encode(Msg, Timestamp) ->
    BodyTag = exml_query:path(Msg, [{element, <<"body">>}]),
    #{from => exml_query:attr(Msg, <<"from">>),
      to => exml_query:attr(Msg, <<"to">>),
      id => exml_query:attr(Msg, <<"id">>),
      body => exml_query:cdata(BodyTag),
      timestamp => Timestamp}.

maybe_jid(undefined) ->
    undefined;
maybe_jid(JID) ->
    jid:from_binary(JID).

maybe_integer_qs_val({undefined, _Req} = R) ->
    R;
maybe_integer_qs_val({Val, Req}) ->
    {binary_to_integer(Val), Req}.

maybe_before_to_us(undefined, Now) ->
    Now;
maybe_before_to_us(Timestamp, _) ->
   Timestamp * 1000.

