-module(mongoose_admin_api_muc_light).

-behaviour(mongoose_admin_api).
-export([routes/1]).

-behaviour(cowboy_rest).
-export([init/2,
         is_authorized/2,
         content_types_accepted/2,
         allowed_methods/2,
         from_json/2,
         delete_resource/2]).

-ignore_xref([to_json/2, from_json/2]).

-import(mongoose_admin_api, [try_handle_request/3, throw_error/2, parse_body/1, resource_created/4]).

-type req() :: cowboy_req:req().
-type state() :: mongoose_admin_api:state().

-spec routes(state()) -> mongoose_http_handler:routes().
routes(State) ->
    [{"/muc-lights/:domain", ?MODULE, State},
     {"/muc-lights/:domain/:id/participants", ?MODULE, State#{suffix => participants}},
     {"/muc-lights/:domain/:id/messages", ?MODULE, State#{suffix => messages}},
     {"/muc-lights/:domain/:id/management", ?MODULE, State#{suffix => management}}].

-spec init(req(), state()) -> {cowboy_rest, req(), state()}.
init(Req, State) ->
    mongoose_admin_api:init(Req, State).

-spec is_authorized(req(), state()) -> {true | {false, iodata()}, req(), state()}.
is_authorized(Req, State) ->
    mongoose_admin_api:is_authorized(Req, State).

-spec content_types_accepted(req(), state()) ->
          {[{{binary(), binary(), '*'}, atom()}], req(), state()}.
content_types_accepted(Req, State) ->
    {[
      {{<<"application">>, <<"json">>, '*'}, from_json}
     ], Req, State}.

-spec allowed_methods(req(), state()) -> {[binary()], req(), state()}.
allowed_methods(Req, State) ->
    {[<<"OPTIONS">>, <<"POST">>, <<"PUT">>, <<"DELETE">>], Req, State}.

%% @doc Called for a method of type "POST" and "PUT"
-spec from_json(req(), state()) -> {stop, req(), state()}.
from_json(Req, State) ->
    F = case cowboy_req:method(Req) of
            <<"POST">> -> fun handle_post/2;
            <<"PUT">> -> fun handle_put/2
        end,
    try_handle_request(Req, State, F).

%% @doc Called for a method of type "DELETE"
-spec delete_resource(req(), state()) -> {true | stop, req(), state()}.
delete_resource(Req, State) ->
    try_handle_request(Req, State, fun handle_delete/2).

%% Internal functions

handle_post(Req, #{suffix := participants} = State) ->
    Bindings = cowboy_req:bindings(Req),
    RoomJid = get_room_jid(Bindings),
    Args = parse_body(Req),
    SenderJid = get_sender_jid(Args),
    RecipientJid = get_recipient_jid(Args),
    case mod_muc_light_api:invite_to_room(RoomJid, SenderJid, RecipientJid) of
        {ok, _} ->
            {stop, Req, State};
        {muc_server_not_found, Msg} ->
            throw_error(not_found, Msg);
        {room_not_found, Msg} ->
            throw_error(not_found, Msg);
        {user_not_found, Msg} ->
            throw_error(bad_request, Msg);
        {not_room_member, Msg} ->
            throw_error(denied, Msg)
    end;
handle_post(Req, #{suffix := messages} = State) ->
    Bindings = cowboy_req:bindings(Req),
    RoomJid = get_room_jid(Bindings),
    Args = parse_body(Req),
    SenderJid = get_from_jid(Args),
    Body = get_message_body(Args),
    case mod_muc_light_api:send_message(RoomJid, SenderJid, Body) of
        {ok, _} ->
            {stop, Req, State};
        {muc_server_not_found, Msg} ->
            throw_error(not_found, Msg);
        {room_not_found, Msg} ->
            throw_error(not_found, Msg);
        {user_not_found, Msg} ->
            throw_error(bad_request, Msg);
        {not_room_member, Msg} ->
            throw_error(denied, Msg)
    end;
handle_post(Req, State) ->
    #{domain := MUCDomain} = cowboy_req:bindings(Req),
    Args = parse_body(Req),
    OwnerJid = get_owner_jid(Args),
    RoomName = get_room_name(Args),
    Subject = get_room_subject(Args),
    Config = #{<<"roomname">> => RoomName, <<"subject">> => Subject},
    case mod_muc_light_api:create_room(MUCDomain, OwnerJid, Config) of
        {ok, #{jid := RoomJid}} ->
            RoomJidBin = jid:to_binary(RoomJid),
            Path = [cowboy_req:uri(Req), "/", RoomJidBin],
            resource_created(Req, State, Path, RoomJidBin);
        {user_not_found, Msg} ->
            throw_error(bad_request, Msg);
        {muc_server_not_found, Msg} ->
            throw_error(not_found, Msg)
    end.

handle_put(Req, State) ->
    #{domain := MUCDomain} = cowboy_req:bindings(Req),
    Args = parse_body(Req),
    OwnerJid = get_owner_jid(Args),
    RoomName = get_room_name(Args),
    RoomId = get_room_id(Args),
    Subject = get_room_subject(Args),
    Config = #{<<"roomname">> => RoomName, <<"subject">> => Subject},
    case mod_muc_light_api:create_room(MUCDomain, RoomId, OwnerJid, Config) of
        {ok, #{jid := RoomJid}} ->
            RoomJidBin = jid:to_binary(RoomJid),
            Path = [cowboy_req:uri(Req), "/", RoomJidBin],
            resource_created(Req, State, Path, RoomJidBin);
        {muc_server_not_found, Msg} ->
            throw_error(not_found, Msg);
        {already_exists, Msg} ->
            throw_error(denied, Msg)
    end.

handle_delete(Req, #{suffix := management} = State) ->
    Bindings = cowboy_req:bindings(Req),
    RoomJid = get_room_jid(Bindings),
    case mod_muc_light_api:delete_room(RoomJid) of
        {ok, _} ->
            {true, Req, State};
        {muc_server_not_found, Msg} ->
            throw_error(not_found, Msg);
        {room_not_found, Msg} ->
            throw_error(not_found, Msg)
    end;
handle_delete(_Req, _State) ->
    throw_error(not_found, ""). % Cowboy returns the same for unknown paths

get_owner_jid(#{owner := Owner}) ->
    case jid:from_binary(Owner) of
        error -> throw_error(bad_request, <<"Invalid owner JID">>);
        OwnerJid -> OwnerJid
    end;
get_owner_jid(#{}) -> throw_error(bad_request, <<"Missing owner JID">>).

get_room_jid(#{domain := MUCDomain} = Bindings) ->
    RoomId = get_room_id(Bindings),
    case jid:make_bare(RoomId, MUCDomain) of
        error -> throw_error(bad_request, <<"Invalid room ID or domain name">>);
        RoomJid -> RoomJid
    end.

get_room_name(#{name := Name}) -> Name;
get_room_name(#{}) -> throw_error(bad_request, <<"Missing room name">>).

get_room_id(#{id := Id}) -> Id;
get_room_id(#{}) -> throw_error(bad_request, <<"Missing room ID">>).

get_room_subject(#{subject := Subject}) -> Subject;
get_room_subject(#{}) -> throw_error(bad_request, <<"Missing room subject">>).

get_message_body(#{body := Body}) -> Body;
get_message_body(#{}) -> throw_error(bad_request, <<"Missing message body">>).

get_from_jid(#{from := Sender}) ->
    case jid:from_binary(Sender) of
        error -> throw_error(bad_request, <<"Invalid sender JID">>);
        SenderJid -> SenderJid
    end;
get_from_jid(#{}) -> throw_error(bad_request, <<"Missing sender JID">>).

get_sender_jid(#{sender := Sender}) ->
    case jid:from_binary(Sender) of
        error -> throw_error(bad_request, <<"Invalid sender JID">>);
        SenderJid -> SenderJid
    end;
get_sender_jid(#{}) -> throw_error(bad_request, <<"Missing sender JID">>).

get_recipient_jid(#{recipient := Recipient}) ->
    case jid:from_binary(Recipient) of
        error -> throw_error(bad_request, <<"Invalid recipient JID">>);
        RecipientJid -> RecipientJid
    end;
get_recipient_jid(#{}) -> throw_error(bad_request, <<"Missing recipient JID">>).
