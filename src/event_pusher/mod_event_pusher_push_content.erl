-module(mod_event_pusher_push_content).
-moduledoc "Builds push notification content".

-include("jlib.hrl").
-include("mod_muc_light.hrl").

-export([build/2, muc_light_kind/1, recipient_affiliation/2]).

-type content() :: #{binary() => binary()}.
-type result() :: {ok, content()} | skip | {error, term()}.
-type muc_light_kind() :: affiliations | roomname | destroy.
-type muc_light_event() :: added | removed | affiliation_changed | members_changed | renamed | destroyed.
-type muc_light_fields() :: #{event := muc_light_event(),
                              room_name => binary(),
                              prev_room_name => binary(),
                              affiliation => binary(),
                              version => binary()}.
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
            Room = #{bin => jid:to_bare_binary(From), lus => jid:to_lus(From)},
            muc_light_content(muc_light_fields(Kind, Room, To, Packet, Acc), Room, To, Acc)
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
             {?NS_MUC_LIGHT_CONFIGURATION, roomname}],
    case [Kind || {NS, Kind} <- Kinds, exml_query:subelement_with_ns(Packet, NS) =/= undefined] of
        [roomname] -> configuration_kind(Packet);
        [Kind | _] -> Kind;
        [] -> undefined
    end.

%% Only renames are pushed; other configuration changes match no kind
-spec configuration_kind(exml:element()) -> roomname | undefined.
configuration_kind(Packet) ->
    case roomname_element(Packet) of
        undefined -> undefined;
        _ -> roomname
    end.

-spec roomname_element(exml:element()) -> exml:element() | undefined.
roomname_element(Packet) ->
    exml_query:path(Packet, [{element_with_ns, ?NS_MUC_LIGHT_CONFIGURATION}, {element, ~"roomname"}]).

%% Affiliation of the recipient as listed in the affiliations element, if listed
-spec recipient_affiliation(jid:jid(), exml:element()) -> binary() | undefined.
recipient_affiliation(Recipient, Packet) ->
    case exml_query:subelement_with_ns(Packet, ?NS_MUC_LIGHT_AFFILIATIONS) of
        undefined ->
            undefined;
        AffEl ->
            RecipientLUS = jid:to_lus(Recipient),
            case [UserEl || UserEl <- exml_query:subelements(AffEl, ~"user"),
                            user_lus(UserEl) =:= RecipientLUS] of
                [UserEl | _] -> exml_query:attr(UserEl, ~"affiliation");
                [] -> undefined
            end
    end.

-spec user_lus(exml:element()) -> jid:simple_bare_jid() | error.
user_lus(UserEl) ->
    case jid:from_binary(exml_query:cdata(UserEl)) of
        error -> error;
        Jid -> jid:to_lus(Jid)
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

-spec muc_light_fields(muc_light_kind(), room(), jid:jid(), exml:element(), mongoose_acc:t()) ->
          muc_light_fields().
muc_light_fields(destroy, _Room, _To, _Packet, _Acc) ->
    #{event => destroyed}; % the room is already gone, nothing to look up
muc_light_fields(affiliations, Room, To, Packet, _Acc) ->
    AffEl = exml_query:subelement_with_ns(Packet, ?NS_MUC_LIGHT_AFFILIATIONS),
    Fields = maybe_put(version, version(AffEl), maybe_put(room_name, room_name(Room), #{})),
    affiliation_fields(recipient_affiliation(To, Packet), AffEl, Fields);
muc_light_fields(roomname, _Room, _To, Packet, Acc) ->
    ConfigEl = exml_query:subelement_with_ns(Packet, ?NS_MUC_LIGHT_CONFIGURATION),
    PrevConfig = mongoose_acc:get(mod_muc_light, prev_config, [], Acc),
    Fields = maybe_put(version, version(ConfigEl), #{event => renamed}),
    maybe_put(prev_room_name, proplists:get_value(~"roomname", PrevConfig),
              maybe_put(room_name, exml_query:cdata(roomname_element(Packet)), Fields)).

%% A newcomer gets only its own entry with 'version' and no 'prev-version',
%% current members get the changed entries with both
-spec affiliation_fields(binary() | undefined, exml:element(), map()) -> muc_light_fields().
affiliation_fields(undefined, _AffEl, Fields) ->
    Fields#{event => members_changed};
affiliation_fields(~"none", _AffEl, Fields) ->
    Fields#{event => removed};
affiliation_fields(Affiliation, AffEl, Fields) ->
    Event = case exml_query:subelement(AffEl, ~"prev-version") of
                undefined -> added;
                _ -> affiliation_changed
            end,
    Fields#{event => Event, affiliation => Affiliation}.

-spec muc_light_content(muc_light_fields(), room(), jid:jid(), mongoose_acc:t()) -> {ok, content()} | skip.
muc_light_content(#{event := Event} = Fields, #{bin := RoomBin}, To, Acc) ->
    case renamed_to_same_name(Fields) of
        true ->
            skip;
        false ->
            Custom = #{~"type" => ~"muc-light",
                       ~"muc-light-event" => event_name(Event),
                       ~"muc-light-room" => RoomBin},
            Optional = [{~"muc-light-room-name", room_name},
                        {~"muc-light-room-prev-name", prev_room_name},
                        {~"muc-light-affiliation", affiliation},
                        {~"muc-light-room-version", version}],
            Extra = maps:from_list([{Key, maps:get(Field, Fields)}
                                    || {Key, Field} <- Optional, is_map_key(Field, Fields)]),
            Content = content(RoomBin, muc_light_body(Fields), get_unread_count(Acc, To)),
            {ok, maps:merge(Content, maps:merge(Custom, Extra))}
    end.

-spec renamed_to_same_name(muc_light_fields()) -> boolean().
renamed_to_same_name(#{event := renamed} = Fields) ->
    maps:get(room_name, Fields, undefined) =:= maps:get(prev_room_name, Fields, undefined);
renamed_to_same_name(#{}) ->
    false.

-spec event_name(muc_light_event()) -> binary().
event_name(affiliation_changed) -> ~"affiliation-changed";
event_name(members_changed) -> ~"members-changed";
event_name(Event) -> atom_to_binary(Event).

%% English fallback shown when the client does not rewrite the notification
-spec muc_light_body(muc_light_fields()) -> binary().
muc_light_body(#{event := added} = Fields) ->
    <<"You were added to ", (group(Fields))/binary>>;
muc_light_body(#{event := removed} = Fields) ->
    <<"You were removed from ", (group(Fields))/binary>>;
muc_light_body(#{event := affiliation_changed} = Fields) ->
    <<"Your role in ", (group(Fields))/binary, " changed">>;
muc_light_body(#{event := members_changed, room_name := Name}) ->
    <<Name/binary, ": members changed">>;
muc_light_body(#{event := members_changed}) ->
    ~"Group members changed";
muc_light_body(#{event := renamed, room_name := Name, prev_room_name := PrevName}) ->
    <<PrevName/binary, " was renamed to ", Name/binary>>;
muc_light_body(#{event := renamed, room_name := Name}) ->
    <<"Group renamed to ", Name/binary>>;
muc_light_body(#{event := renamed, prev_room_name := PrevName}) ->
    <<PrevName/binary, " was renamed">>;
muc_light_body(#{event := destroyed}) ->
    ~"A group was deleted".

-spec group(muc_light_fields()) -> binary().
group(#{room_name := Name}) -> Name;
group(#{}) -> ~"a group".

%% The room may live on another host type than the recipient, or on another server.
-spec room_name(room()) -> binary() | undefined.
room_name(#{lus := {_, RoomLServer} = RoomLUS}) ->
    case mongoose_domain_api:get_subdomain_host_type(RoomLServer) of
        {ok, HostType} ->
            room_name(HostType, RoomLUS);
        {error, not_found} ->
            undefined
    end.

-spec room_name(mongooseim:host_type(), jid:simple_bare_jid()) -> binary() | undefined.
room_name(HostType, RoomLUS) ->
    case gen_mod:is_loaded(HostType, mod_muc_light) andalso
         mod_muc_light_db_backend:get_config(HostType, RoomLUS) of
        {ok, Config, _Version} ->
            case lists:keyfind(roomname, 1, Config) of
                {roomname, Name} when is_binary(Name) -> Name;
                _ -> undefined
            end;
        _ ->
            undefined
    end.

-spec version(exml:element()) -> binary() | undefined.
version(El) ->
    exml_query:path(El, [{element, ~"version"}, cdata]).

-spec maybe_put(atom(), binary() | undefined, map()) -> map().
maybe_put(_Key, undefined, Fields) -> Fields;
maybe_put(_Key, <<>>, Fields) -> Fields;
maybe_put(Key, Value, Fields) -> Fields#{Key => Value}.

-spec jingle_content(binary(), binary()) -> content().
jingle_content(SenderId, SessionId) ->
    #{~"type" => ~"jmi",
      ~"jmi-sid" => SessionId,
      ~"jmi-from" => SenderId}.
