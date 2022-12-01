-module(mongoose_graphql_muc_light_admin_mutation).
-behaviour(mongoose_graphql).

-export([execute/4]).

-ignore_xref([execute/4]).

-include("../mongoose_graphql_types.hrl").

-import(mongoose_graphql_helper, [make_error/2, format_result/2]).
-import(mongoose_graphql_muc_light_helper, [make_room/1, make_ok_user/1, prepare_blocking_items/1,
                                            null_to_default/2, config_to_map/3, get_not_loaded/1]).

execute(_Ctx, _Obj, <<"createRoom">>, Args) ->
    create_room(Args);
execute(_Ctx, _Obj, <<"changeRoomConfiguration">>, Args) ->
    change_room_config(Args);
execute(_Ctx, _Obj, <<"inviteUser">>, Args) ->
    invite_user(Args);
execute(_Ctx, _Obj, <<"deleteRoom">>, Args) ->
    delete_room(Args);
execute(_Ctx, _Obj, <<"kickUser">>, Args) ->
    kick_user(Args);
execute(_Ctx, _Obj, <<"sendMessageToRoom">>, Args) ->
    send_msg_to_room(Args);
execute(_Ctx, _Obj, <<"setBlockingList">>, Args) ->
    set_blocking_list(Args).

-spec create_room(map()) -> {ok, map()} | {error, resolver_error()}.
create_room(#{<<"id">> := RoomID, <<"mucDomain">> := MUCDomain, <<"name">> := RoomName,
              <<"owner">> := CreatorJID, <<"subject">> := Subject, <<"options">> := Options}) ->
    case mod_muc_light_api:create_room(MUCDomain, null_to_default(RoomID, <<>>), CreatorJID,
                                       config_to_map(RoomName, Subject, Options)) of
        {ok, Room} ->
            {ok, make_room(Room)};
        Err ->
            make_error(Err, #{mucDomain => MUCDomain, id => RoomID,
                              creator => jid:to_binary(CreatorJID)})
    end.

-spec change_room_config(map()) -> {ok, map()} | {error, resolver_error()}.
change_room_config(#{<<"room">> := RoomJID, <<"name">> := RoomName,
                     <<"owner">> := OwnerJID, <<"subject">> := Subject,
                     <<"options">> := Options}) ->
    Config = config_to_map(RoomName, Subject, Options),
    case mod_muc_light_api:change_room_config(RoomJID, OwnerJID, Config) of
        {ok, Room} ->
            {ok, make_room(Room)};
        Err ->
            make_error(Err, #{room => jid:to_binary(RoomJID), owner => jid:to_binary(OwnerJID)})
    end.

-spec delete_room(map()) -> {ok, binary()} | {error, resolver_error()}.
delete_room(#{<<"room">> := RoomJID}) ->
    Result = mod_muc_light_api:delete_room(RoomJID),
    format_result(Result, #{room => jid:to_binary(RoomJID)}).

-spec invite_user(map()) -> {ok, binary()} | {error, resolver_error()}.
invite_user(#{<<"room">> := RoomJID, <<"sender">> := SenderJID,
              <<"recipient">> := RecipientJID}) ->
    Result = mod_muc_light_api:invite_to_room(RoomJID, SenderJID, RecipientJID),
    format_result(Result, #{room => jid:to_binary(RoomJID), sender => jid:to_binary(SenderJID),
                            recipient => jid:to_binary(RecipientJID)}).

-spec kick_user(map()) -> {ok, binary()} | {error, resolver_error()}.
kick_user(#{<<"room">> := RoomJID, <<"user">> := UserJID}) ->
    Result = mod_muc_light_api:change_affiliation(RoomJID, UserJID, UserJID, remove),
    format_result(Result, #{user => jid:to_binary(UserJID)}).

-spec send_msg_to_room(map()) -> {ok, binary()} | {error, resolver_error()}.
send_msg_to_room(#{<<"room">> := RoomJID, <<"from">> := FromJID, <<"body">> := Message}) ->
    Result = mod_muc_light_api:send_message(RoomJID, FromJID, Message),
    format_result(Result, #{room => jid:to_binary(RoomJID), from => jid:to_binary(FromJID)}).

-spec set_blocking_list(map()) -> {ok, binary()} | {error, resolver_error()}.
set_blocking_list(#{<<"user">> := UserJID, <<"items">> := Items}) ->
    Items2 = prepare_blocking_items(Items),
    Result = mod_muc_light_api:set_blocking(UserJID, Items2),
    format_result(Result, #{user => jid:to_binary(UserJID)}).
