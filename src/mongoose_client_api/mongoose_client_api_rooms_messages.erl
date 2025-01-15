-module(mongoose_client_api_rooms_messages).

-behaviour(mongoose_client_api).
-export([routes/0]).

-behaviour(cowboy_rest).
-export([trails/0,
         init/2,
         is_authorized/2,
         content_types_provided/2,
         content_types_accepted/2,
         allowed_methods/2,
         to_json/2,
         from_json/2]).

-export([encode/2]).

-ignore_xref([from_json/2, to_json/2, trails/0]).

-import(mongoose_client_api, [parse_body/1, parse_qs/1, try_handle_request/3, throw_error/2]).

-type req() :: cowboy_req:req().
-type state() :: map().

-include("jlib.hrl").
-include_lib("exml/include/exml.hrl").

-spec routes() -> mongoose_http_handler:routes().
routes() ->
    [{"/rooms/[:id]/messages", ?MODULE, #{}}].

trails() ->
    mongoose_client_api_rooms_messages_doc:trails().

-spec init(req(), state()) -> {cowboy_rest, req(), state()}.
init(Req, Opts) ->
    mongoose_client_api:init(Req, Opts).

-spec is_authorized(req(), state()) -> {true | {false, iodata()}, req(), state()}.
is_authorized(Req, State) ->
    mongoose_client_api:is_authorized(Req, State).

-spec content_types_provided(req(), state()) ->
          {[{{binary(), binary(), '*'}, atom()}], req(), state()}.
content_types_provided(Req, State) ->
    mongoose_client_api_rooms:content_types_provided(Req, State).

-spec content_types_accepted(req(), state()) ->
          {[{{binary(), binary(), '*'}, atom()}], req(), state()}.
content_types_accepted(Req, State) ->
    mongoose_client_api_rooms:content_types_accepted(Req, State).

-spec allowed_methods(req(), state()) -> {[binary()], req(), state()}.
allowed_methods(Req, State) ->
    {[<<"OPTIONS">>, <<"GET">>, <<"POST">>], Req, State}.

%% @doc Called for a method of type "GET"
-spec to_json(req(), state()) -> {iodata() | stop, req(), state()}.
to_json(Req, State) ->
    try_handle_request(Req, State, fun handle_get/2).

%% @doc Called for a method of type "POST"
-spec from_json(req(), state()) -> {true | stop, req(), state()}.
from_json(Req, State) ->
    try_handle_request(Req, State, fun handle_post/2).

%% Internal functions

handle_get(Req, State = #{jid := UserJid}) ->
    Bindings = cowboy_req:bindings(Req),
    RoomJid = mongoose_client_api_rooms:get_room_jid(Bindings, State, required),
    Args = parse_qs(Req),
    Limit = get_limit(Args),
    Before = get_before(Args),
    case mod_muc_light_api:get_room_messages(RoomJid, UserJid, Limit, Before) of
        {ok, Msgs} ->
            JSONData = [make_json_item(Msg) || Msg <- Msgs],
            {jiffy:encode(JSONData), Req, State};
        {room_not_found, Msg} ->
            throw_error(not_found, Msg);
        {not_room_member, Msg} ->
            throw_error(denied, Msg)
    end.

handle_post(Req, State = #{jid := UserJid}) ->
    Bindings = cowboy_req:bindings(Req),
    RoomJid = mongoose_client_api_rooms:get_room_jid(Bindings, State, required),
    Args = parse_body(Req),
    Children = verify_children(get_body(Args) ++ get_marker(Args) ++ get_markable(Args)),
    UUID = uuid:uuid_to_string(uuid:get_v4(), binary_standard),
    Attrs = #{<<"id">> => UUID},
    case mod_muc_light_api:send_message(RoomJid, UserJid, Children, Attrs) of
        {ok, _} ->
            Resp = #{id => UUID},
            Req3 = cowboy_req:set_resp_body(jiffy:encode(Resp), Req),
            {true, Req3, State};
        {room_not_found, Msg} ->
            throw_error(not_found, Msg);
        {not_room_member, Msg} ->
            throw_error(denied, Msg)
    end.

get_limit(#{limit := LimitBin}) ->
    try
        Limit = binary_to_integer(LimitBin),
        true = Limit >= 0 andalso Limit =< 500,
        Limit
    catch
        _:_ -> throw_error(bad_request, <<"Invalid limit">>)
    end;
get_limit(#{}) -> 50.

get_before(#{before := BeforeBin}) ->
    try
        1000 * binary_to_integer(BeforeBin)
    catch
        _:_ -> throw_error(bad_request, <<"Invalid value of 'before'">>)
    end;
get_before(#{}) -> undefined.

get_body(#{body := Body}) when is_binary(Body) ->
    [#xmlel{ name = <<"body">>, children = [#xmlcdata{ content = Body }] }];
get_body(#{body := _}) -> throw_error(bad_request, <<"Invalid message body">>);
get_body(#{}) -> [].

get_marker(#{chat_marker := #{type := Type, id := Id}})
  when Type == <<"received">>;
       Type == <<"displayed">>;
       Type == <<"acknowledged">> ->
    [#xmlel{ name = Type, attrs = #{<<"xmlns">> => ?NS_CHAT_MARKERS, <<"id">> => Id}}];
get_marker(#{chat_marker := _}) -> throw_error(bad_request, <<"Invalid chat marker">>);
get_marker(#{}) -> [].

get_markable(#{body := _, markable := true}) ->
    [#xmlel{ name = <<"markable">>, attrs = #{<<"xmlns">> => ?NS_CHAT_MARKERS} }];
get_markable(#{}) -> [].

verify_children([]) -> throw_error(bad_request, <<"No valid message elements">>);
verify_children(Children) -> Children.

-spec encode(Packet :: exml:element(), Timestamp :: integer()) -> map().
encode(Packet, Timestamp) ->
    From = exml_query:attr(Packet, <<"from">>),
    FromJID = jid:from_binary(From),
    Msg = make_json_item(Packet, FromJID, Timestamp),
    Msg#{room => FromJID#jid.luser}.

-spec make_json_item(mod_mam:message_row()) -> term().
make_json_item(#{id := MAMID, jid := JID, packet := Msg}) ->
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
          affiliation => maps:get(<<"affiliation">>, Attrs, undefined),
          user => exml_query:cdata(User)}.
