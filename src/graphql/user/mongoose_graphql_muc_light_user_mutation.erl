-module(mongoose_graphql_muc_light_user_mutation).
-behaviour(mongoose_graphql).

-export([execute/4]).

-ignore_xref([execute/4]).

-include("../mongoose_graphql_types.hrl").

-import(mongoose_graphql_helper, [make_error/2, format_result/2]).
-import(mongoose_graphql_muc_light_helper, [make_room/1, make_ok_user/1, prepare_blocking_items/1,
                                            null_to_default/2, config_to_map/3]).

execute(Ctx, _Obj, <<"createRoom">>, Args) ->
    create_room(Ctx, Args);
execute(Ctx, _Obj, <<"changeRoomConfiguration">>, Args) ->
    change_room_config(Ctx, Args);
execute(Ctx, _Obj, <<"inviteUser">>, Args) ->
    invite_user(Ctx, Args);
execute(Ctx, _Obj, <<"deleteRoom">>, Args) ->
    delete_room(Ctx, Args);
execute(Ctx, _Obj, <<"kickUser">>, Args) ->
    kick_user(Ctx, Args);
execute(Ctx, _Obj, <<"sendMessageToRoom">>, Args) ->
    send_msg_to_room(Ctx, Args);
execute(Ctx, _Obj, <<"setBlockingList">>, Args) ->
    set_blocking_list(Ctx, Args).

-spec create_room(map(), map()) -> {ok, map()} | {error, resolver_error()}.
create_room(#{user := UserJID}, #{<<"id">> := RoomID, <<"mucDomain">> := MUCDomain,
                                  <<"name">> := RoomName, <<"subject">> := Subject,
                                  <<"options">> := Options}) ->
    case mod_muc_light_api:create_room(MUCDomain, null_to_default(RoomID, <<>>), UserJID,
                                       config_to_map(RoomName, Subject, Options)) of
        {ok, Room} ->
            {ok, make_room(Room)};
        Err ->
            make_error(Err, #{mucDomain => MUCDomain, id => RoomID})
    end.

-spec change_room_config(map(), map()) -> {ok, map()} | {error, resolver_error()}.
change_room_config(#{user := UserJID}, #{<<"room">> := RoomJID, <<"name">> := RoomName,
                                         <<"subject">> := Subject, <<"options">> := Options}) ->
    Config = config_to_map(RoomName, Subject, Options),
    case mod_muc_light_api:change_room_config(RoomJID, UserJID, Config) of
        {ok, Room} ->
            {ok, make_room(Room)};
        Err ->
            make_error(Err, #{room => jid:to_binary(RoomJID)})
    end.

-spec delete_room(map(), map()) -> {ok, binary()} | {error, resolver_error()}.
delete_room(#{user := UserJID}, #{<<"room">> := RoomJID}) ->
    Result = mod_muc_light_api:delete_room(RoomJID, UserJID),
    format_result(Result, #{room => jid:to_binary(RoomJID)}).

-spec invite_user(map(), map()) -> {ok, binary()} | {error, resolver_error()}.
invite_user(#{user := UserJID}, #{<<"room">> := RoomJID, <<"recipient">> := RecipientJID}) ->
    Result = mod_muc_light_api:invite_to_room(RoomJID, UserJID, RecipientJID),
    format_result(Result, #{room => jid:to_binary(RoomJID),
                            recipient => jid:to_binary(RecipientJID)}).

-spec kick_user(map(), map()) -> {ok, binary()} | {error, resolver_error()}.
kick_user(#{user := UserJID}, #{<<"room">> := RoomJID, <<"user">> := UserToKickJID}) ->
    AffectedJID = null_to_default(UserToKickJID, UserJID),
    Result = mod_muc_light_api:change_affiliation(RoomJID, UserJID, AffectedJID, remove),
    format_result(Result, #{user => jid:to_binary(AffectedJID)}).

-spec send_msg_to_room(map(), map()) -> {ok, binary()} | {error, resolver_error()}.
send_msg_to_room(#{user := UserJID}, #{<<"room">> := RoomJID, <<"body">> := Message}) ->
    Result = mod_muc_light_api:send_message(RoomJID, UserJID, Message),
    format_result(Result, #{room => jid:to_binary(RoomJID)}).

-spec set_blocking_list(map(), map()) -> {ok, binary()} | {error, resolver_error()}.
set_blocking_list(#{user := UserJID}, #{<<"items">> := Items}) ->
    Items2 = prepare_blocking_items(Items),
    Result = mod_muc_light_api:set_blocking(UserJID, Items2),
    format_result(Result, #{user => jid:to_binary(UserJID)}).
