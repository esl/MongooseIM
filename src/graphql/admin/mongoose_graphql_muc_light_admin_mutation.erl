-module(mongoose_graphql_muc_light_admin_mutation).

-export([execute/4]).

-ignore_xref([execute/4]).

-include("../mongoose_graphql_types.hrl").

-import(mongoose_graphql_helper, [make_error/2, format_result/2]).
-import(mongoose_graphql_muc_light_helper, [make_room/1, make_ok_user/1]).

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
    send_msg_to_room(Args).

-spec create_room(map()) -> {ok, map()} | {error, resolver_error()}.
create_room(#{<<"id">> := null} = Args) ->
    create_room(Args#{<<"id">> => <<>>});
create_room(#{<<"id">> := RoomID, <<"domain">> := Domain, <<"name">> := RoomName,
              <<"owner">> := CreatorJID, <<"subject">> := Subject}) ->
    case mod_muc_light_api:create_room(Domain, RoomID, RoomName, CreatorJID, Subject) of
        {ok, Room} ->
            {ok, make_room(Room)};
        Err ->
            make_error(Err, #{domain => Domain, roomID => RoomID, creator => CreatorJID})
    end.

-spec change_room_config(map()) -> {ok, map()} | {error, resolver_error()}.
change_room_config(#{<<"id">> := RoomID, <<"domain">> := Domain, <<"name">> := RoomName,
                     <<"owner">> := OwnerJID, <<"subject">> := Subject}) ->
    case mod_muc_light_api:change_room_config(Domain, RoomID, RoomName, OwnerJID, Subject) of
        {ok, Room} ->
            {ok, make_room(Room)};
        Err ->
            make_error(Err, #{domain => Domain, id => RoomID, owner => OwnerJID})
    end.

-spec delete_room(map()) -> {ok, binary()} | {error, resolver_error()}.
delete_room(#{<<"domain">> := Domain, <<"id">> := RoomID}) ->
    Result = mod_muc_light_api:delete_room(Domain, RoomID),
    format_result(Result, #{domain => Domain, id => RoomID}).

-spec invite_user(map()) -> {ok, binary()} | {error, resolver_error()}.
invite_user(#{<<"domain">> := Domain, <<"name">> := Name, <<"sender">> := SenderJID,
              <<"recipient">> := RecipientJID}) ->
    Result = mod_muc_light_api:invite_to_room(Domain, Name, SenderJID, RecipientJID),
    format_result(Result, #{domain => Domain, name => Name,
                            sender => sender, recipient => RecipientJID}).

-spec kick_user(map()) -> {ok, binary()} | {error, resolver_error()}.
kick_user(#{<<"domain">> := Domain, <<"id">> := RoomID, <<"user">> := UserJID}) ->
    Result = mod_muc_light_api:remove_user_from_room(Domain, RoomID, UserJID, UserJID),
    format_result(Result, #{user => UserJID}).

-spec send_msg_to_room(map()) -> {ok, binary()} | {error, resolver_error()}.
send_msg_to_room(#{<<"domain">> := Domain, <<"name">> := RoomName, <<"from">> := FromJID,
              <<"body">> := Message}) ->
    Result = mod_muc_light_api:send_message(Domain, RoomName, FromJID, Message),
    format_result(Result, #{domain => Domain, name => RoomName, from => FromJID}).
