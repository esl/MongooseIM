-module(mongoose_graphql_muc_user_mutation).
-behaviour(mongoose_graphql).

-export([execute/4]).

-ignore_xref([execute/4]).

-include("../mongoose_graphql_types.hrl").

-import(mongoose_graphql_helper, [make_error/2, format_result/2, null_to_default/2,
                                  null_to_undefined/1]).

execute(Ctx, _Obj, <<"createInstantRoom">>, Args) ->
    create_instant_room(Ctx, Args);
execute(Ctx, _Obj, <<"inviteUser">>, Args) ->
    invite_user(Ctx, Args);
execute(Ctx, _Obj, <<"kickUser">>, Args) ->
    kick_user(Ctx, Args);
execute(Ctx, _Obj, <<"sendMessageToRoom">>, Args) ->
    send_message_to_room(Ctx, Args);
execute(Ctx, _Obj, <<"sendPrivateMessage">>, Args) ->
    send_private_message(Ctx, Args);
execute(Ctx, _Obj, <<"changeRoomConfiguration">>, Args) ->
    change_room_config(Ctx, Args);
execute(Ctx, _Obj, <<"deleteRoom">>, Args) ->
    delete_room(Ctx, Args);
execute(Ctx, _Obj, <<"setUserRole">>, Args) ->
    set_user_role(Ctx, Args);
execute(Ctx, _Obj, <<"setUserAffiliation">>, Args) ->
    set_user_affiliation(Ctx, Args);
execute(Ctx, _Obj, <<"enterRoom">>, Args) ->
    enter_room(Ctx, Args);
execute(Ctx, _Obj, <<"exitRoom">>, Args) ->
    exit_room(Ctx, Args).

-spec enter_room(map(), map()) -> {ok, binary()} | {error, resolver_error()}.
enter_room(#{user := UserJID}, #{<<"room">> := RoomJID, <<"nick">> := Nick,
                                 <<"resource">> := Resource, <<"password">> := Password}) ->
    UserJIDRes = jid:replace_resource(UserJID, Resource),
    RoomJIDRes = jid:replace_resource(RoomJID, Nick),
    Password2 = null_to_undefined(Password),
    Res = mod_muc_api:enter_room(RoomJIDRes, UserJIDRes, Password2),
    format_result(Res, #{room => jid:to_binary(RoomJID)}).

-spec exit_room(map(), map()) -> {ok, binary()} | {error, resolver_error()}.
exit_room(#{user := UserJID}, #{<<"room">> := RoomJID, <<"nick">> := Nick,
                                <<"resource">> := Resource}) ->
    UserJIDRes = jid:replace_resource(UserJID, Resource),
    RoomJIDRes = jid:replace_resource(RoomJID, Nick),
    Res = mod_muc_api:exit_room(RoomJIDRes, UserJIDRes),
    format_result(Res, #{room => jid:to_binary(RoomJID)}).

-spec create_instant_room(map(), map()) -> {ok, map()} | {error, resolver_error()}.
create_instant_room(#{user := UserJID}, #{<<"mucDomain">> := MUCDomain, <<"name">> := Name,
                                          <<"nick">> := Nick}) ->
    case mod_muc_api:create_instant_room(MUCDomain, Name, UserJID, Nick) of
        {ok, Room} ->
            {ok, mongoose_graphql_muc_helper:make_room_desc(Room)};
        Error ->
            make_error(Error, #{mucDomain => MUCDomain})
    end.

-spec invite_user(map(), map()) -> {ok, binary()} | {error, resolver_error()}.
invite_user(#{user := UserJID}, #{<<"room">> := RoomJID, <<"recipient">> := RecipientJID,
                                  <<"reason">> := Reason}) ->
    Reason2 = null_to_default(Reason, mongoose_graphql_muc_helper:default_invite_reason("user")),
    Res = mod_muc_api:invite_to_room(RoomJID, UserJID, RecipientJID, Reason2),
    format_result(Res, #{room => jid:to_binary(RoomJID)}).

-spec kick_user(map(), map()) -> {ok, binary()} | {error, resolver_error()}.
kick_user(#{user := _UserJID}, #{<<"room">> := RoomJID, <<"nick">> := Nick,
                                <<"reason">> := Reason}) ->
    Reason2 = null_to_default(Reason, mongoose_graphql_muc_helper:default_kick_reason("user")),
    Res = mod_muc_api:kick_user_from_room(RoomJID, Nick, Reason2),
    format_result(Res, #{room => jid:to_binary(RoomJID)}).

-spec send_message_to_room(map(), map()) -> {ok, binary()} | {error, resolver_error()}.
send_message_to_room(#{user := UserJID}, #{<<"room">> := RoomJID, <<"body">> := Message,
                                           <<"resource">> := Res}) ->
    Result = do_send_message_to_room(RoomJID, UserJID, Message, Res),
    format_result(Result, #{room => jid:to_binary(RoomJID)}).

-spec send_private_message(map(), map()) -> {ok, binary()} | {error, resolver_error()}.
send_private_message(#{user := UserJID}, #{<<"room">> := RoomJID, <<"body">> := Message,
                                           <<"toNick">> := ToNick, <<"resource">> := Res}) ->
    Result = case mongoose_graphql_muc_helper:add_user_resource(UserJID, Res) of
                 {ok, UserJIDRes} ->
                     mod_muc_api:send_private_message(RoomJID, UserJIDRes, ToNick, Message);
                 Error ->
                     Error
             end,
    format_result(Result, #{room => jid:to_binary(RoomJID)}).

-spec change_room_config(map(), map()) -> {ok, map()} | {error, resolver_error()}.
change_room_config(#{user := UserJID}, #{<<"room">> := RoomJID, <<"config">> := ConfigInput}) ->
    Fun = fun(Config) -> mongoose_graphql_muc_helper:make_muc_room_config(ConfigInput, Config) end,
    case mod_muc_api:modify_room_config(RoomJID, UserJID, Fun) of
        {ok, Config} ->
            {ok, mongoose_graphql_muc_helper:muc_room_config_to_map(Config)};
        Error ->
            make_error(Error, #{room => jid:to_binary(RoomJID)})
    end.

-spec delete_room(map(), map()) -> {ok, binary()} | {error, resolver_error()}.
delete_room(#{user := UserJID}, #{<<"room">> := RoomJID, <<"reason">> := Reason}) ->
    Reason2 = null_to_default(Reason,
                              mongoose_graphql_muc_helper:default_room_removal_reason("user")),
    Res = mod_muc_api:delete_room(RoomJID, UserJID, Reason2),
    format_result(Res, #{room => jid:to_binary(RoomJID)}).

-spec set_user_role(map(), map()) -> {ok, binary()} | {error, resolver_error()}.
set_user_role(#{user := UserJID}, #{<<"room">> := RoomJID, <<"nick">> := Nick,
                                    <<"role">> := Role}) ->
    Result = mod_muc_api:set_role(RoomJID, UserJID, Nick, Role),
    format_result(Result, #{room => jid:to_binary(RoomJID)}).

-spec set_user_affiliation(map(), map()) -> {ok, binary()} | {error, resolver_error()}.
set_user_affiliation(#{user := FromJID}, #{<<"room">> := RoomJID, <<"user">> := UserJID,
                                           <<"affiliation">> := Aff}) ->
    Result = mod_muc_api:set_affiliation(RoomJID, FromJID, UserJID, Aff),
    format_result(Result, #{room => jid:to_binary(RoomJID)}).

-spec do_send_message_to_room(jid:jid(), jid:jid(), binary(), binary() | null) ->
    {ok | no_session, iolist()}.
do_send_message_to_room(RoomJID, UserJID, Message, Res) ->
    case mongoose_graphql_muc_helper:add_user_resource(UserJID, Res) of
        {ok, UserJIDRes} ->
            mod_muc_api:send_message_to_room(RoomJID, UserJIDRes, Message);
        Error ->
            Error
    end.
