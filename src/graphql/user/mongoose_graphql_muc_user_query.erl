-module(mongoose_graphql_muc_user_query).
-behaviour(mongoose_graphql).

-export([execute/4]).

-ignore_xref([execute/4]).

-include("../mongoose_graphql_types.hrl").

-import(mongoose_graphql_helper, [make_error/2, format_result/2, null_to_undefined/1]).
-import(mongoose_graphql_muc_light_helper, [page_size_or_max_limit/2]).

execute(Ctx, _Obj, <<"listRooms">>, Args) ->
    list_rooms(Ctx, Args);
execute(Ctx, _Obj, <<"listRoomUsers">>, Args) ->
    list_room_users(Ctx, Args);
execute(Ctx, _Obj, <<"listRoomAffiliations">>, Args) ->
    list_room_affiliations(Ctx, Args);
execute(Ctx, _Obj, <<"getRoomMessages">>, Args) ->
    get_room_messages(Ctx, Args);
execute(Ctx, _Obj, <<"getRoomConfig">>, Args) ->
    get_room_config(Ctx, Args).

-spec list_rooms(map(), map()) -> {ok, map()}.
list_rooms(#{user := UserJID}, #{<<"mucDomain">> := MUCDomain, <<"limit">> := Limit,
                                 <<"index">> := Index}) ->
    Limit2 = null_to_undefined(Limit),
    Index2 = null_to_undefined(Index),
    {Rooms, RSM} = mod_muc_api:get_rooms(MUCDomain, UserJID, Limit2, Index2),
    {ok, mongoose_graphql_muc_helper:make_rooms_payload(Rooms, RSM)}.

-spec list_room_users(map(), map()) -> {ok, [{ok, map()}]} | {error, resolver_error()}.
list_room_users(#{user := UserJID}, #{<<"room">> := RoomJID}) ->
    case mod_muc_api:get_room_users(RoomJID, UserJID) of
        {ok, Users} ->
            {ok, mongoose_graphql_muc_helper:format_users(Users)};
        Error ->
            make_error(Error, #{room => jid:to_binary(RoomJID)})
    end.

-spec list_room_affiliations(map(), map()) -> {ok, [{ok, map()}]} | {error, resolver_error()}.
list_room_affiliations(#{user := UserJID}, #{<<"room">> := RoomJID, <<"affiliation">> := Aff}) ->
    Aff2 = null_to_undefined(Aff),
    case mod_muc_api:get_room_affiliation(RoomJID, UserJID, Aff2) of
        {ok, Affs} ->
            {ok, mongoose_graphql_muc_helper:format_affs(Affs)};
        Error ->
            make_error(Error, #{room => jid:to_binary(RoomJID)})
    end.

-spec get_room_config(map(), map()) -> {ok, map()} | {error, resolver_error()}.
get_room_config(#{user := UserJID}, #{<<"room">> := RoomJID}) ->
    case mod_muc_api:get_room_config(RoomJID, UserJID) of
        {ok, Config} ->
            {ok, mongoose_graphql_muc_helper:muc_room_config_to_map(Config)};
        Error ->
            make_error(Error, #{room => jid:to_binary(RoomJID)})
    end.

-spec get_room_messages(map(), map()) -> {ok, map()} | {error, resolver_error()}.
get_room_messages(#{user := UserJID}, #{<<"room">> := RoomJID, <<"pageSize">> := PageSize,
                                        <<"before">> := Before}) ->
    Before2 = null_to_undefined(Before),
    PageSize2 = page_size_or_max_limit(PageSize, 50),
    case mod_muc_api:get_room_messages(RoomJID, UserJID, PageSize2, Before2) of
        {ok, Rows} ->
            Maps = lists:map(fun mongoose_graphql_stanza_helper:row_to_map/1, Rows),
            {ok, #{<<"stanzas">> => Maps, <<"limit">> => PageSize2}};
        Err ->
            make_error(Err, #{room => RoomJID})
    end.
