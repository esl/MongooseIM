-module(mongoose_graphql_muc_helper).

-export([make_rooms_payload/2, make_room_desc/1, muc_room_config_to_map/1, make_muc_room_config/2,
         format_user/1, format_users/1, add_user_resource/2, format_affs/1]).

-export([default_kick_reason/1, default_invite_reason/1, default_room_removal_reason/1]).

-ignore_xref(format_user/1).

-include("mongoose_rsm.hrl").
-include("mod_muc_room.hrl").

-spec add_user_resource(jid:jid(), binary() | null) ->
    {ok, jid:jid()} | {no_session, iolist()}.
add_user_resource(JID, null) ->
    case mongoose_session_api:get_user_resource(JID, 1) of
        {ok, Resource} ->
            {ok, jid:replace_resource(JID, Resource)};
        {wrong_res_number, _ErrMsg}->
            {no_session, "Given user does not have any session"}
    end;
add_user_resource(JID, Resource) ->
    {ok, jid:replace_resource(JID, Resource)}.

make_rooms_payload(Rooms, #rsm_out{count = Count, index = Index, first = First, last = Last}) ->
    Rooms2 = [{ok, make_room_desc(R)} || R <- Rooms],
    #{<<"rooms">> => Rooms2, <<"count">> => Count, <<"index">> => Index,
      <<"first">> => First, <<"last">> => Last}.

make_room_desc(#{jid := JID, title := Title} = Room) ->
    Private = maps:get(private, Room, null),
    UsersNumber = maps:get(users_number, Room, null),
    #{<<"jid">> => JID, <<"title">> => Title,
      <<"private">> => Private, <<"usersNumber">> => UsersNumber}.

default_invite_reason(EndpointName) ->
    iolist_to_binary(io_lib:format("User invited through the ~s GraphQL API", [EndpointName])).

default_kick_reason(EndpointName) ->
    iolist_to_binary(io_lib:format("User kicked through the ~s GraphQL API", [EndpointName])).

default_room_removal_reason(EndpointName) ->
    iolist_to_binary(io_lib:format("Room deleted through the ~s GraphQL API", [EndpointName])).

format_users(Users) ->
    [{ok, format_user(U)} || U <- Users].

format_user(#{jid := JID, role := Role, nick := Nick}) ->
    #{<<"jid">> => JID, <<"role">> => Role, <<"nick">> => Nick}.

format_affs(Affs) ->
    [{ok, format_aff(A)} || A <- Affs].

format_aff({JID, Aff}) ->
    #{<<"jid">> => JID, <<"affiliation">> => Aff}.

-spec muc_room_config_to_map(mod_muc_room:config()) -> map().
muc_room_config_to_map(Conf) ->
    #{<<"title">> => Conf#config.title,
      <<"description">> => Conf#config.description,
      <<"allowChangeSubject">> => Conf#config.allow_change_subj,
      <<"allowQueryUsers">> => Conf#config.allow_query_users,
      <<"allowPrivateMessages">> => Conf#config.allow_private_messages,
      <<"allowVisitorStatus">> => Conf#config.allow_visitor_status,
      <<"allowVisitorNickchange">> => Conf#config.allow_visitor_nickchange,
      <<"public">> => Conf#config.public,
      <<"publicList">> => Conf#config.public_list,
      <<"persistent">> => Conf#config.persistent,
      <<"moderated">> => Conf#config.moderated,
      <<"membersByDefault">> => Conf#config.members_by_default,
      <<"membersOnly">> => Conf#config.members_only,
      <<"allowUserInvites">> => Conf#config.allow_user_invites,
      <<"allowMultipleSession">> => Conf#config.allow_multiple_sessions,
      <<"passwordProtected">> => Conf#config.password_protected,
      <<"password">> => Conf#config.password,
      <<"anonymous">> => Conf#config.anonymous,
      <<"mayGetMemberList">> => format_maygetmemberlist(Conf#config.maygetmemberlist),
      <<"maxUsers">> => Conf#config.max_users,
      <<"logging">> => Conf#config.logging}.

-spec make_muc_room_config(map(), mod_muc_room:config()) -> mod_muc_room:config().
make_muc_room_config(Map, Conf) ->
    #config{
      title = maybe_value(<<"title">>, Conf#config.title, Map),
      description = maybe_value(<<"description">>, Conf#config.description, Map),
      allow_change_subj = maybe_value(<<"allowChangeSubject">>, Conf#config.allow_change_subj, Map),
      allow_query_users = maybe_value(<<"allowQueryUsers">>, Conf#config.allow_query_users, Map),
      allow_private_messages = maybe_value(<<"allowPrivateMessages">>,
                                           Conf#config.allow_private_messages, Map),
      allow_visitor_status = maybe_value(<<"allowVisitorStatus">>,
                                         Conf#config.allow_visitor_status, Map),
      allow_visitor_nickchange = maybe_value(<<"allowVisitorNickchange">>,
                                             Conf#config.allow_visitor_nickchange, Map),
      public = maybe_value(<<"public">>, Conf#config.public, Map),
      public_list = maybe_value(<<"publicList">>, Conf#config.public_list, Map),
      persistent = maybe_value(<<"persistent">>, Conf#config.persistent, Map),
      moderated = maybe_value(<<"moderated">>, Conf#config.moderated, Map),
      members_by_default = maybe_value(<<"membersByDefault">>, Conf#config.members_by_default, Map),
      members_only = maybe_value(<<"membersOnly">>, Conf#config.members_only, Map),
      allow_user_invites = maybe_value(<<"allowUserInvites">>, Conf#config.allow_user_invites, Map),
      allow_multiple_sessions = maybe_value(<<"allowMultipleSession">>,
                                            Conf#config.allow_multiple_sessions, Map),
      password_protected = maybe_value(<<"passwordProtected">>,
                                       Conf#config.password_protected, Map),
      password = maybe_value(<<"password">>, Conf#config.password, Map),
      anonymous = maybe_value(<<"anonymous">>, Conf#config.anonymous, Map),
      maygetmemberlist = maybe_value(<<"mayGetMemberList">>, Conf#config.maygetmemberlist, Map),
      max_users = maybe_value(<<"maxUsers">>, Conf#config.max_users, Map),
      logging = maybe_value(<<"logging">>, Conf#config.logging, Map)
     }.

maybe_value(Key, Default, Map) ->
    case maps:get(Key, Map) of
        null -> Default;
        Val -> Val
    end.

format_maygetmemberlist(Elements) -> [{ok, E} || E <- Elements].
