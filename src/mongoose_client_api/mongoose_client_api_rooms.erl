-module(mongoose_client_api_rooms).

-behaviour(mongoose_client_api).
-export([routes/0]).

-behaviour(cowboy_rest).
-export([trails/0]).
-export([init/2]).
-export([content_types_provided/2]).
-export([content_types_accepted/2]).
-export([is_authorized/2]).
-export([allowed_methods/2]).
-export([resource_exists/2]).
-export([assert_room_id_set/2]).

-export([to_json/2]).
-export([from_json/2]).

-ignore_xref([
    from_json/2, to_json/2, trails/0
]).

-include("mongoose.hrl").
-include("jlib.hrl").

-spec routes() -> mongoose_http_handler:routes().
routes() ->
    [{"/rooms/[:id]", ?MODULE, #{}}].

trails() ->
    mongoose_client_api_rooms_doc:trails().

init(Req, Opts) ->
    mongoose_client_api:init(Req, Opts).

is_authorized(Req, State) ->
    mongoose_client_api:is_authorized(Req, State).

content_types_provided(Req, State) ->
    {[
      {{<<"application">>, <<"json">>, '*'}, to_json}
     ], Req, State}.

content_types_accepted(Req, State) ->
    {[
      {{<<"application">>, <<"json">>, '*'}, from_json}
     ], Req, State}.

allowed_methods(Req, State) ->
    {[<<"OPTIONS">>, <<"GET">>, <<"POST">>, <<"PUT">>], Req, State}.

resource_exists(Req, #{jid := #jid{lserver = Server}} = State) ->
    RoomIDOrJID = cowboy_req:binding(id, Req),
    MUCLightDomain = muc_light_domain(Server),
    case RoomIDOrJID of
        undefined ->
            Method = cowboy_req:method(Req),
            case Method of
                <<"GET">> ->
                    {true, Req, State};
                _ ->
                    {false, Req, State}
            end;
        _ ->
            case validate_room_id(RoomIDOrJID, Server, Req) of
                {ok, RoomID} ->
                    State2 = set_room_id(RoomID, State),
                    does_room_exist(MUCLightDomain, Req, State2);
                _ ->
                    mongoose_client_api:bad_request(Req, <<"invalid_room_id">>, State)
            end
    end.

set_room_id(RoomID, State = #{}) ->
    State#{room_id => RoomID}.

does_room_exist(RoomS, Req, #{room_id := RoomU, jid := JID} = State) ->
    HostType = mod_muc_light_utils:muc_host_to_host_type(RoomS),
    case mod_muc_light_db_backend:get_info(HostType, {RoomU, RoomS}) of
        {ok, Config, Users, Version} ->
            Room = #{config => Config,
                     users => Users,
                     version => Version,
                     jid => jid:make_noprep(RoomU, RoomS, <<>>)},
            CallerRole = determine_role(jid:to_lus(JID), Users),
            {true, Req, State#{room => Room, role_in_room => CallerRole}};
        _ ->
            {false, Req, State}
    end.

to_json(Req, #{room := Room} = State) ->
    Config = maps:get(config, Room),
    Users = maps:get(users, Room),
    Resp = #{name => proplists:get_value(roomname, Config),
             subject => proplists:get_value(subject, Config),
             participants => [user_to_json(U) || U <- Users]
            },
    {jiffy:encode(Resp), Req, State};
to_json(Req, #{jid := #jid{luser = User, lserver = Server}} = State) ->
    HostType = mod_muc_light_utils:server_host_to_host_type(Server),
    Rooms = mod_muc_light_db_backend:get_user_rooms(HostType, {User, Server}, undefined),
    RoomsMap = [get_room_details(RoomUS) || RoomUS <- Rooms],
    {jiffy:encode(lists:flatten(RoomsMap)), Req, State}.

get_room_details({RoomID, RoomS} = RoomUS) ->
    HostType = mod_muc_light_utils:muc_host_to_host_type(RoomS),
    case mod_muc_light_db_backend:get_config(HostType, RoomUS) of
        {ok, Config, _} ->
            #{id => RoomID,
              name => proplists:get_value(roomname, Config),
              subject => proplists:get_value(subject, Config)};
        _ ->
            []
    end.


from_json(Req, State = #{was_replied := true}) ->
    {true, Req, State};
from_json(Req, State) ->
    Method = cowboy_req:method(Req),
    {ok, Body, Req2} = cowboy_req:read_body(Req),
    case mongoose_client_api:json_to_map(Body) of
        {ok, #{<<"name">> := N, <<"subject">> := S} = JSONData} when is_binary(N), is_binary(S) ->
            handle_request(Method, JSONData, Req2, State);
        _ ->
            mongoose_client_api:bad_request(Req, <<"Failed to parse parameters">>, State)
    end.

handle_request(Method, JSONData, Req, State) ->
    case handle_request_by_method(Method, JSONData, Req, State) of
        {ok, #{jid := RoomJID}} ->
            RespBody = #{<<"id">> => RoomJID#jid.luser},
            RespReq = cowboy_req:set_resp_body(jiffy:encode(RespBody), Req),
            {true, RespReq, State};
        {Short, Desc} ->
            mongoose_client_api:bad_request(Req, format_error(Short, Desc), State)
    end.

format_error(Short, Desc) ->
    jiffy:encode(#{module => <<"mongoose_client_api_rooms">>,
                   what => <<"Failed to create room">>,
                   reason => term_to_bin(Short),
                   description => term_to_bin(Desc)}).

term_to_bin(Term) ->
    iolist_to_binary(io_lib:format("~p", [Term])).

handle_request_by_method(<<"POST">>, JSONData, _Req,
                         #{jid := #jid{lserver = LServer} = UserJID}) ->
    #{<<"name">> := RoomName, <<"subject">> := Subject} = JSONData,
    MUCServer = muc_light_domain(LServer),
    mod_muc_light_api:create_room(MUCServer, UserJID, RoomName, Subject);
handle_request_by_method(<<"PUT">>, JSONData, Req, State) ->
    assert_room_id_set(Req, State),
    #{jid := #jid{lserver = LServer} = UserJID, room_id := RoomID} = State,
    #{<<"name">> := RoomName, <<"subject">> := Subject} = JSONData,
    MUCServer = muc_light_domain(LServer),
    mod_muc_light_api:create_room(MUCServer, RoomID, UserJID, RoomName, Subject).

assert_room_id_set(_Req, #{room_id := _} = _State) ->
    ok.

user_to_json({UserServer, Role}) ->
    #{user => jid:to_binary(UserServer),
      role => Role}.

muc_light_domain(Server) ->
    HostType = mod_muc_light_utils:server_host_to_host_type(Server),
    mod_muc_light_utils:server_host_to_muc_host(HostType, Server).

determine_role(US, Users) ->
    case lists:keyfind(US, 1, Users) of
        false -> none;
        {_, Role} ->
            Role
    end.

-spec validate_room_id(RoomIDOrJID :: binary(), Server :: binary(),
                       Req :: cowboy_req:req()) ->
    {ok, RoomID :: binary()} | error.
validate_room_id(RoomIDOrJID, Server, Req) ->
    MUCLightDomain = muc_light_domain(Server),
    case jid:from_binary(RoomIDOrJID) of
        #jid{luser = <<>>, lserver = RoomID, lresource = <<>>} ->
            {ok, RoomID};
        #jid{luser = RoomID, lserver = MUCLightDomain, lresource = <<>>} ->
            {ok, RoomID};
        Other ->
            ?LOG_WARNING(#{what => muc_invalid_room_id,
                           text => <<"REST received room_id field is invalid "
                                     "or of unknown format">>,
                           server => Server, room => RoomIDOrJID, reason => Other,
                           req => Req}),
            error
    end.
