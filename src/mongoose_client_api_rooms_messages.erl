-module(mongoose_client_api_rooms_messages).
-behaviour(cowboy_rest).

-export([init/2]).
-export([content_types_provided/2]).
-export([content_types_accepted/2]).
-export([is_authorized/2]).
-export([allowed_methods/2]).
-export([resource_exists/2]).
-export([allow_missing_post/2]).

-export([to_json/2]).
-export([from_json/2]).
-export([encode/2]).

-import(mongoose_client_api_messages, [maybe_integer/1, maybe_before_to_us/2]).

-include("mongoose.hrl").
-include("jlib.hrl").
-include("mongoose_rsm.hrl").
-include_lib("exml/include/exml.hrl").

init(Req, Opts) ->
    mongoose_client_api:init(Req, Opts).

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
    mongoose_client_api:forbidden_request(Req, State);
to_json(Req, #{jid := UserJID, room := Room} = State) ->
    RoomJID = maps:get(jid, Room),
    Server = UserJID#jid.server,
    Now = p1_time_compat:os_system_time(micro_seconds),
    ArchiveID = mod_mam_muc:archive_id_int(Server, RoomJID),
    QS = cowboy_req:parse_qs(Req),
    PageSize = maybe_integer(proplists:get_value(<<"limit">>, QS, <<"50">>)),
    Before = maybe_integer(proplists:get_value(<<"before">>, QS)),
    End = maybe_before_to_us(Before, Now),
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
    {jiffy:encode(JSONData), Req, State}.

from_json(Req, #{role_in_room := none} = State) ->
    mongoose_client_api:forbidden_request(Req, State);
from_json(Req, #{user := User, jid := JID, room := Room} = State) ->
    {ok, Body, Req2} = cowboy_req:read_body(Req),
    try
        JSONData = jiffy:decode(Body, [return_maps]),
        prepare_message_and_route_to_room(User, JID, Room, State, Req2, JSONData)
    catch
        throw:_R ->
            Req3 = cowboy_req:set_resp_body(<<"Request body is not a valid JSON">>, Req2),
            mongoose_client_api:bad_request(Req3, State)
    end.


prepare_message_and_route_to_room(User, JID, Room, State, Req, JSONData) ->
    RoomJID = #jid{lserver = MucHost} = maps:get(jid, Room),
    {ok, Host} = mongoose_subhosts:get_host(MucHost),
    UUID = uuid:uuid_to_string(uuid:get_v4(), binary_standard),
    case validate_body(JSONData) of
        {ok, Body} ->
            Message = build_message(User, RoomJID, UUID, Body),
            Acc = mongoose_acc:new(#{ location => ?LOCATION,
                                      lserver => JID#jid.lserver,
                                      from_jid => JID,
                                      to_jid => RoomJID,
                                      element => Message }),
            ejabberd_router:route(JID, RoomJID, Acc, Message),
            Resp = #{id => UUID},
            Req3 = cowboy_req:set_resp_body(jiffy:encode(Resp), Req),
            {true, Req3, State};
        {error, no_body} ->
            Req2 = cowboy_req:set_resp_body(<<"There is no body in the JSON">>, Req),
            mongoose_client_api:bad_request(Req2, State);
        _ ->
            Req2 = cowboy_req:set_resp_body(<<"Invalid body, it must be a string">>, Req),
            mongoose_client_api:bad_request(Req2, State)
    end.

validate_body(JSONData) ->
    case maps:get(<<"body">>, JSONData, undefined) of
        undefined ->
            {error, no_body};
        Body when is_binary(Body) ->
            {ok, Body};
        _ ->
            {error, not_acceptable_body}
    end.

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

