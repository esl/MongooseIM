-module(mongoose_graphql_muc_admin_mutation).
-behaviour(mongoose_graphql).

-export([execute/4]).

-ignore_xref([execute/4]).

-include("../mongoose_graphql_types.hrl").

-import(mongoose_graphql_helper, [make_error/2, format_result/2, null_to_default/2]).

execute(_Ctx, _Obj, <<"createInstantRoom">>, Args) ->
    create_instant_room(Args);
execute(_Ctx, _Obj, <<"inviteUser">>, Args) ->
    invite_user(Args);
execute(_Ctx, _Obj, <<"kickUser">>, Args) ->
    kick_user(Args);
execute(_Ctx, _Obj, <<"sendMessageToRoom">>, Args) ->
    send_message_to_room(Args);
execute(_Ctx, _Obj, <<"changeRoomConfiguration">>, Args) ->
    change_room_config(Args);
execute(_Ctx, _Obj, <<"deleteRoom">>, Args) ->
    delete_room(Args).

-spec create_instant_room(map()) -> {ok, map()} | {error, resolver_error()}.
create_instant_room(#{<<"mucDomain">> := MUCDomain, <<"name">> := Name,
                      <<"owner">> := OwnerJID, <<"nick">> := Nick}) ->
    case mod_muc_api:create_instant_room(MUCDomain, Name, OwnerJID, Nick) of
        {ok, Room} ->
            {ok, mongoose_graphql_muc_helper:make_room_desc(Room)};
        Error ->
            make_error(Error, #{mucDomain => MUCDomain,
                                owner => jid:to_binary(OwnerJID)})
    end.

-spec invite_user(map()) -> {ok, binary()} | {error, resolver_error()}.
invite_user(#{<<"room">> := RoomJID, <<"sender">> := SenderJID,
              <<"recipient">>  := RecipientJID, <<"reason">> := Reason}) ->
    Reason2 = null_to_default(Reason, mongoose_graphql_muc_helper:default_invite_reason("admin")),
    Res = mod_muc_api:invite_to_room(RoomJID, SenderJID, RecipientJID, Reason2),
    format_result(Res, #{room => jid:to_binary(RoomJID),
                         sender => jid:to_binary(SenderJID)}).

-spec kick_user(map()) -> {ok, binary()} | {error, resolver_error()}.
kick_user(#{<<"room">> := RoomJID, <<"nick">> := Nick,
              <<"reason">> := Reason}) ->
    Reason2 = null_to_default(Reason, mongoose_graphql_muc_helper:default_kick_reason("admin")),
    Res = mod_muc_api:kick_user_from_room(RoomJID, Nick, Reason2),
    format_result(Res, #{room => jid:to_binary(RoomJID)}).

-spec send_message_to_room(map()) -> {ok, binary()} | {error, resolver_error()}.
send_message_to_room(#{<<"room">> := RoomJID, <<"from">> := SenderJID,
              <<"body">> := Message}) ->
    Res = mod_muc_api:send_message_to_room(RoomJID, SenderJID, Message),
    format_result(Res, #{room => jid:to_binary(RoomJID),
                         from => jid:to_binary(SenderJID)}).

-spec change_room_config(map()) -> {ok, map()} | {error, resolver_error()}.
change_room_config(#{<<"room">> := RoomJID, <<"config">> := ConfigInput}) ->
    Fun = fun(Config) -> mongoose_graphql_muc_helper:make_muc_room_config(ConfigInput, Config) end,
    case mod_muc_api:modify_room_config(RoomJID, Fun) of
        {ok, Config} ->
            {ok, mongoose_graphql_muc_helper:muc_room_config_to_map(Config)};
        Error ->
            make_error(Error, #{room => jid:to_binary(RoomJID)})
    end.

-spec delete_room(map()) -> {ok, binary()} | {error, resolver_error()}.
delete_room(#{<<"room">> := RoomJID, <<"reason">> := Reason}) ->
    Reason2 = null_to_default(Reason,
                              mongoose_graphql_muc_helper:default_room_removal_reason("admin")),
    Res = mod_muc_api:delete_room(RoomJID, Reason2),
    format_result(Res, #{room => jid:to_binary(RoomJID)}).
