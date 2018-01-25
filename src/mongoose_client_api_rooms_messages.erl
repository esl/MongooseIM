-module(mongoose_client_api_rooms_messages).

-export([init/3]).
-export([rest_init/2]).
-export([content_types_provided/2]).
-export([content_types_accepted/2]).
-export([is_authorized/2]).
-export([allowed_methods/2]).
-export([resource_exists/2]).
-export([allow_missing_post/2]).

-export([to_json/2]).
-export([from_json/2]).
-export([encode/2]).

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
    mongoose_client_api_rooms:content_types_provided(Req, State).

content_types_accepted(Req, State) ->
    mongoose_client_api_rooms:content_types_accepted(Req, State).

allowed_methods(Req, State) ->
    {[<<"OPTIONS">>, <<"GET">>, <<"POST">>], Req, State}.

resource_exists(Req, State) ->
    mongoose_client_api_rooms:resource_exists(Req, State).

allow_missing_post(Req, State) ->
    {false, Req, State}.

to_json(Req, #{role_in_room := none} = State) ->
    mongoose_client_api_rooms:forbidden_request(Req, State);
to_json(Req, #{jid := UserJID, room := Room} = State) ->
    RoomJID = maps:get(jid, Room),
    Server = UserJID#jid.server,
    Now = p1_time_compat:os_system_time(micro_seconds),
    ArchiveID = mod_mam_muc:archive_id_int(Server, RoomJID),
    LimitQSVal = cowboy_req:qs_val(<<"limit">>, Req, <<"50">>),
    {PageSize, Req2} = mongoose_client_api_messages:maybe_integer_qs_val(LimitQSVal),
    BeforeQSVal = cowboy_req:qs_val(<<"before">>, Req2),
    {Before, Req3} = mongoose_client_api_messages:maybe_integer_qs_val(BeforeQSVal),
    End = mongoose_client_api_messages:maybe_before_to_us(Before, Now),
    RSM = #rsm_in{direction = before, id = undefined},
    R = mod_mam_muc:lookup_messages(Server,
                                    #{archive_id => ArchiveID,
                                      owner_jid => RoomJID,
                                      rsm => RSM,
                                      borders => undefined,
                                      start_ts => undefined,
                                      end_ts => End,
                                      now => Now,
                                      with_jid => undefined,
                                      search_text => undefined,
                                      page_size => PageSize,
                                      limit_passed => true,
                                      max_result_limit => 50,
                                      is_simple => true}),
    {ok, {undefined, undefined, Msgs}} = R,
    JSONData = [make_json_item(Msg) || Msg <- Msgs],
    {jiffy:encode(JSONData), Req3, State}.

from_json(Req, #{user := User, jid := JID, room := Room} = State) ->
    {ok, Body, Req2} = cowboy_req:body(Req),
    JSONData = jiffy:decode(Body, [return_maps]),
    RoomJID = maps:get(jid, Room),
    UUID = uuid:uuid_to_string(uuid:get_v4(), binary_standard),
    Message = build_message(User, RoomJID, UUID, maps:get(<<"body">>, JSONData)),
    ejabberd_router:route(JID, RoomJID, Message),
    Resp = #{id => UUID},
    Req3 = cowboy_req:set_resp_body(jiffy:encode(Resp), Req2),
    {true, Req3, State}.

-spec build_message(From :: binary(), To :: jid:jid(), ID :: binary(), Body :: binary()) ->
    exml:element().
build_message(From, To, ID, Body) ->
    Attrs = [{<<"from">>, From},
             {<<"to">>, jid:to_binary(To)},
             {<<"id">>, ID},
             {<<"type">>, <<"groupchat">>}],
    BodyEl = #xmlel{name = <<"body">>,
                    children = [{xmlcdata, Body}]},
    #xmlel{name = <<"message">>,
           attrs = Attrs,
           children = [BodyEl]}.

-spec encode(Packet :: exml:element(), Timestamp :: integer()) -> map().
encode(Packet, Timestamp) ->
    From = exml_query:attr(Packet, <<"from">>),
    FromJID = jid:from_binary(From),
    Msg = make_json_item(Packet, FromJID, Timestamp),
    Msg#{room => FromJID#jid.luser}.

make_json_item({MAMID, JID, Msg}) ->
    {Microsec, _} = mod_mam_utils:decode_compact_uuid(MAMID),
    make_json_item(Msg, JID, Microsec div 1000).

make_json_item(Msg, JID, Timestamp) ->
    Item = #{id => exml_query:attr(Msg, <<"id">>),
             from => make_from(JID),
             timestamp => Timestamp},
    add_body_and_type(Item, Msg).

make_from(#jid{lresource = <<>>} = JID) ->
    jid:to_binary(JID);
make_from(#jid{lresource = Sender}) ->
    Sender.

add_body_and_type(Item, Msg) ->
    case exml_query:path(Msg, [{element, <<"x">>}, {element, <<"user">>}]) of
        undefined ->
            add_regular_message_body(Item, Msg);
        #xmlel{} = AffChange ->
            add_aff_change_body(Item, AffChange)
    end.

add_regular_message_body(Item, Msg) ->
    BodyTag = exml_query:path(Msg, [{element, <<"body">>}]),
    Body = exml_query:cdata(BodyTag),
    Item#{type => <<"message">>,
          body => Body}.

add_aff_change_body(Item, #xmlel{attrs = Attrs} = User) ->
    Item#{type => <<"affiliation">>,
          affiliation => proplists:get_value(<<"affiliation">>, Attrs),
          user => exml_query:cdata(User)}.

