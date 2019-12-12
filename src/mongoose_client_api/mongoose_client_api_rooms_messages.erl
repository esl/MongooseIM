-module(mongoose_client_api_rooms_messages).
-behaviour(cowboy_rest).

-export([trails/0]).

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

trails() ->
    mongoose_client_api_rooms_messages_doc:trails().

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
    Now = os:system_time(microsecond),
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
    {ok, {_, _, Msgs}} = R,
    JSONData = [make_json_item(Msg) || Msg <- Msgs],
    {jiffy:encode(JSONData), Req, State}.

from_json(Req, #{role_in_room := none} = State) ->
    mongoose_client_api:forbidden_request(Req, State);
from_json(Req, #{user := User, jid := JID, room := Room} = State) ->
    {ok, Body, Req2} = cowboy_req:read_body(Req),
    case mongoose_client_api:json_to_map(Body) of
        {ok, JSONData} ->
            prepare_message_and_route_to_room(User, JID, Room, State, Req2, JSONData);
        _ ->
            mongoose_client_api:bad_request(Req2, <<"Request body is not a valid JSON">>, State)
    end.


prepare_message_and_route_to_room(User, JID, Room, State, Req, JSONData) ->
    RoomJID = #jid{lserver = _} = maps:get(jid, Room),
    UUID = uuid:uuid_to_string(uuid:get_v4(), binary_standard),
    case build_message_from_json(User, RoomJID, UUID, JSONData) of
        {ok, Message} ->
            Acc = mongoose_acc:new(#{ location => ?LOCATION,
                                      lserver => JID#jid.lserver,
                                      from_jid => JID,
                                      to_jid => RoomJID,
                                      element => Message }),
            ejabberd_router:route(JID, RoomJID, Acc, Message),
            Resp = #{id => UUID},
            Req3 = cowboy_req:set_resp_body(jiffy:encode(Resp), Req),
            {true, Req3, State};
        {error, ErrorMsg} ->
            Req2 = cowboy_req:set_resp_body(ErrorMsg, Req),
            mongoose_client_api:bad_request(Req2, State)
    end.

-spec build_message_from_json(From :: binary(), To :: jid:jid(), ID :: binary(), JSON :: map()) ->
    {ok, exml:element()} | {error, ErrorMsg :: binary()}.
build_message_from_json(From, To, ID, JSON) ->
    case build_children(JSON) of
        {error, _} = Err ->
            Err;
        [] ->
            {error, <<"No valid message elements">>};
        Children ->
            Attrs = [{<<"from">>, From},
                     {<<"to">>, jid:to_binary(To)},
                     {<<"id">>, ID},
                     {<<"type">>, <<"groupchat">>}],
            {ok, #xmlel{name = <<"message">>, attrs = Attrs, children = Children}}
    end.

build_children(JSON) ->
    lists:foldl(fun(_, {error, _} = Err) ->
                        Err;
                   (ChildBuilder, Children) ->
                        ChildBuilder(JSON, Children)
                end, [], [fun build_markable/2, fun build_marker/2, fun build_body/2]).

build_body(#{ <<"body">> := Body }, Children) when is_binary(Body) ->
    [#xmlel{ name = <<"body">>, children = [#xmlcdata{ content = Body }] } | Children];
build_body(#{ <<"body">> := _Body }, _Children) ->
    {error, <<"Invalid body, it must be a string">>};
build_body(_JSON, Children) ->
    Children.

build_marker(#{ <<"chat_marker">> := #{ <<"type">> := Type, <<"id">> := Id } }, Children)
  when Type == <<"received">>;
       Type == <<"displayed">>;
       Type == <<"acknowledged">> ->
    [#xmlel{ name = Type, attrs = [{<<"xmlns">>, ?NS_CHAT_MARKERS}, {<<"id">>, Id}] } | Children];
build_marker(#{ <<"chat_marker">> := _Marker }, _Children) ->
    {error, <<"Invalid marker, it must be 'received', 'displayed' or 'acknowledged'">>};
build_marker(_JSON, Children) ->
    Children.

build_markable(#{ <<"body">> := _Body, <<"markable">> := true }, Children) ->
    [#xmlel{ name = <<"markable">>, attrs = [{<<"xmlns">>, ?NS_CHAT_MARKERS}] } | Children];
build_markable(_JSON, Children) ->
    Children.

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

add_regular_message_body(Item0, Msg) ->
    Item1 = Item0#{type => <<"message">>},
    Item2 =
    case exml_query:path(Msg, [{element, <<"body">>}, cdata]) of
        undefined ->
            Item1;
        Body ->
            Item1#{body => Body}
    end,
    add_chat_marker(Item2, Msg).

add_chat_marker(Item0, Msg) ->
    case exml_query:subelement_with_ns(Msg, ?NS_CHAT_MARKERS) of
        undefined ->
            Item0;
        #xmlel{ name = <<"markable">> } ->
            Item0#{ markable => true };
        #xmlel{ name = Type } = Marker ->
            Item0#{ chat_marker => #{ type => Type, id => exml_query:attr(Marker, <<"id">>) } }
    end.

add_aff_change_body(Item, #xmlel{attrs = Attrs} = User) ->
    Item#{type => <<"affiliation">>,
          affiliation => proplists:get_value(<<"affiliation">>, Attrs),
          user => exml_query:cdata(User)}.
