-module(mod_event_pusher_push_content).
-moduledoc "Builds push notification content".

-include("jlib.hrl").
-include("mod_muc_light.hrl").

-export([build/2, muc_light_kind/1]).

-type content() :: #{binary() => binary()}.
-type result() :: {ok, content()} | {error, term()}.
-type muc_light_kind() :: affiliations | configuration | destroy.
-type room() :: #{bin := binary(), lus := jid:simple_bare_jid()}.
-export_type([content/0, muc_light_kind/0]).

-spec build(message | jingle | muc_light, mongoose_acc:t()) -> result().
build(message, Acc) ->
    {From, To, Packet} = mongoose_acc:packet(Acc),
    case message_body(Packet) of
        {ok, Body} ->
            MessageCount = get_unread_count(Acc, To),
            SenderId = sender_id(From, Packet),
            {ok, content(SenderId, Body, MessageCount)};
        Error ->
            Error
    end;
build(jingle, Acc) ->
    {From, _To, Packet} = mongoose_acc:packet(Acc),
    case jingle_session_id(Packet) of
        {ok, SessionId} ->
            {ok, jingle_content(jid:to_binary(From), SessionId)};
        Error ->
            Error
    end;
build(muc_light, Acc) ->
    {From, To, Packet} = mongoose_acc:packet(Acc),
    case muc_light_kind(Packet) of
        undefined ->
            {error, missing_muc_light_element};
        Kind ->
            Room = #{bin => jid:to_bare_binary(jid:to_lower(From)), lus => jid:to_lus(From)},
            Body = muc_light_body(Kind, Room, Packet),
            Content = content(maps:get(bin, Room), Body, get_unread_count(Acc, To)),
            {ok, Content#{~"type" => ~"muc_light"}}
    end.

%% Kind of a MUC Light system message; a destroy message also carries an affiliations element.
%% Only messages from the bare room JID count: the room relays any element a member puts into
%% a groupchat message, but those messages come from room@muc/sender.
-spec muc_light_kind(exml:element()) -> muc_light_kind() | undefined.
muc_light_kind(Packet) ->
    case is_from_bare_room(Packet) of
        true -> muc_light_element_kind(Packet);
        false -> undefined
    end.

-spec is_from_bare_room(exml:element()) -> boolean().
is_from_bare_room(#xmlel{attrs = #{~"type" := ~"groupchat", ~"from" := From}}) ->
    case jid:from_binary(From) of
        #jid{luser = LUser, lserver = LServer, lresource = <<>>} ->
            LUser =/= <<>> andalso LServer =/= <<>>;
        _ ->
            false
    end;
is_from_bare_room(#xmlel{}) ->
    false.

-spec muc_light_element_kind(exml:element()) -> muc_light_kind() | undefined.
muc_light_element_kind(Packet) ->
    Kinds = [{?NS_MUC_LIGHT_DESTROY, destroy},
             {?NS_MUC_LIGHT_AFFILIATIONS, affiliations},
             {?NS_MUC_LIGHT_CONFIGURATION, configuration}],
    case [Kind || {NS, Kind} <- Kinds, exml_query:subelement_with_ns(Packet, NS) =/= undefined] of
        [Kind | _] -> Kind;
        [] -> undefined
    end.

-spec message_body(exml:element()) -> {ok, binary()} | {error, missing_message_body}.
message_body(Packet) ->
    case exml_query:subelement(Packet, ~"body") of
        undefined ->
            {error, missing_message_body};
        Body ->
            {ok, exml_query:cdata(Body)}
    end.

-spec jingle_session_id(exml:element()) ->
          {ok, binary()} | {error, invalid_jingle_element | missing_jingle_element}.
jingle_session_id(Packet) ->
    case exml_query:subelement_with_ns(Packet, ?JINGLE_MSG_NS) of
        #xmlel{attrs = #{~"id" := Id}} ->
            {ok, Id};
        #xmlel{} ->
            {error, invalid_jingle_element};
        _ ->
            {error, missing_jingle_element}
    end.

-spec get_unread_count(mongoose_acc:t(), jid:jid()) -> non_neg_integer().
get_unread_count(Acc, To) ->
    NewAcc = mongoose_hooks:inbox_unread_count(Acc, To),
    mongoose_acc:get(inbox, unread_count, 1, NewAcc).

-spec sender_id(jid:jid(), exml:element()) -> binary().
sender_id(From, Packet) ->
    case exml_query:attr(Packet, ~"type") of
        ~"chat" ->
            jid:to_bare_binary(jid:to_lower(From));
        ~"groupchat" ->
            jid:to_binary(jid:to_lower(From))
    end.

-spec content(binary(), binary(), non_neg_integer()) -> content().
content(SenderId, BodyCData, MessageCount) ->
    #{~"message-count" => integer_to_binary(MessageCount),
      ~"last-message-sender" => SenderId,
      ~"last-message-body" => BodyCData}.

-spec muc_light_body(muc_light_kind(), room(), exml:element()) -> binary().
muc_light_body(Kind, #{bin := RoomBin} = Room, Packet) ->
    Fields = [{~"type", ~"muc_light"},
              {~"event", atom_to_binary(Kind)},
              {~"room", RoomBin}
              | muc_light_fields(Kind, Room, Packet)],
    jiffy:encode({Fields}).

-spec muc_light_fields(muc_light_kind(), room(), exml:element()) -> [{binary(), jiffy:json_value()}].
muc_light_fields(destroy, _Room, _Packet) ->
    []; % the room is already gone, nothing to look up
muc_light_fields(affiliations, Room, Packet) ->
    AffEl = exml_query:subelement_with_ns(Packet, ?NS_MUC_LIGHT_AFFILIATIONS),
    Users = [{[{~"jid", exml_query:cdata(UserEl)},
               {~"affiliation", exml_query:attr(UserEl, ~"affiliation")}]}
             || UserEl <- exml_query:subelements(AffEl, ~"user")],
    room_name(Room) ++ versions(AffEl) ++ [{~"users", Users}];
muc_light_fields(configuration, Room, Packet) ->
    ConfigEl = exml_query:subelement_with_ns(Packet, ?NS_MUC_LIGHT_CONFIGURATION),
    Config = [{Name, exml_query:cdata(El)} || #xmlel{name = Name} = El <- ConfigEl#xmlel.children,
                                             Name =/= ~"version", Name =/= ~"prev-version"],
    RoomName = case lists:keyfind(~"roomname", 1, Config) of
                   {_, Name} -> [{~"roomname", Name}]; % renamed: the stanza has the new name
                   false -> room_name(Room)
               end,
    RoomName ++ versions(ConfigEl) ++ [{~"config", {Config}}].

%% The room may live on another host type than the recipient, or on another server.
-spec room_name(room()) -> [{binary(), binary()}].
room_name(#{lus := {_, RoomLServer} = RoomLUS}) ->
    case mongoose_domain_api:get_subdomain_host_type(RoomLServer) of
        {ok, HostType} ->
            room_name(HostType, RoomLUS);
        {error, not_found} ->
            []
    end.

-spec room_name(mongooseim:host_type(), jid:simple_bare_jid()) -> [{binary(), binary()}].
room_name(HostType, RoomLUS) ->
    case gen_mod:is_loaded(HostType, mod_muc_light) andalso
         mod_muc_light_db_backend:get_config(HostType, RoomLUS) of
        {ok, Config, _Version} ->
            case lists:keyfind(roomname, 1, Config) of
                {roomname, Name} when is_binary(Name) -> [{~"roomname", Name}];
                _ -> []
            end;
        _ ->
            []
    end.

-spec versions(exml:element()) -> [{binary(), binary()}].
versions(El) ->
    [{Key, Value} || {Key, ElName} <- [{~"version", ~"version"}, {~"prev_version", ~"prev-version"}],
                     Value <- [exml_query:path(El, [{element, ElName}, cdata])],
                     Value =/= undefined].

-spec jingle_content(binary(), binary()) -> content().
jingle_content(SenderId, SessionId) ->
    #{~"type" => ~"jmi",
      ~"jmi-sid" => SessionId,
      ~"jmi-from" => SenderId}.
