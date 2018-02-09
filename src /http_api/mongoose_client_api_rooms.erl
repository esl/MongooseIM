-module(mongoose_client_api_rooms).

-export([init/3]).
-export([rest_init/2]).
-export([content_types_provided/2]).
-export([content_types_accepted/2]).
-export([is_authorized/2]).
-export([allowed_methods/2]).
-export([resource_exists/2]).

-export([forbidden_request/2]).

-export([to_json/2]).
-export([from_json/2]).

-include("mongoose.hrl").
-include("jlib.hrl").
-include_lib("exml/include/exml.hrl").

init(_Transport, _Req, _Opts) ->
    {upgrade, protocol, cowboy_rest}.

rest_init(Req, HandlerOpts) ->
    mongoose_client_api:rest_init(Req, HandlerOpts).

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
    {RoomIDOrJID, Req2} = cowboy_req:binding(id, Req),
    MUCLightDomain = muc_light_domain(Server),
    case RoomIDOrJID of
        undefined ->
            {Method, Req3} = cowboy_req:method(Req2),
            case Method of
                <<"GET">> ->
                    {true, Req3, State};
                _ ->
                    {false, Req3, State}
            end;
        _ ->
            case validate_room_id(RoomIDOrJID, Server) of
                {ok, RoomID} ->
                    does_room_exist(RoomID, MUCLightDomain, Req2, State);
                _ ->
                    bad_request(Req2, State)
            end
    end.

does_room_exist(RoomU, RoomS, Req, #{jid := JID} = State) ->
    NewState = State#{room_id => RoomU},
    case mod_muc_light_db_backend:get_info({RoomU, RoomS}) of
        {ok, Config, Users, Version} ->
            Room = #{config => Config,
                     users => Users,
                     version => Version,
                     jid => jid:make_noprep(RoomU, RoomS, <<>>)},
            CallerRole = determine_role(jid:to_lus(JID), Users),
            {true, Req, NewState#{room => Room, role_in_room => CallerRole}};
        _ ->
            {false, Req, NewState}
    end.

forbidden_request(Req, State) ->
    cowboy_req:reply(403, Req),
    {halt, Req, State}.

bad_request(Req, State) ->
    cowboy_req:reply(400, Req),
    {halt, Req, State}.

to_json(Req, #{room := Room} = State) ->
    Config = maps:get(config, Room),
    Users = maps:get(users, Room),
    Resp = #{name => proplists:get_value(roomname, Config),
             subject => proplists:get_value(subject, Config),
             participants => [user_to_json(U) || U <- Users]
            },
    {jiffy:encode(Resp), Req, State};
to_json(Req, #{jid := #jid{luser = User, lserver = Server}} = State) ->
    Rooms = mod_muc_light_db_backend:get_user_rooms({User, Server}, undefined),
    RoomsMap = [get_room_details(RoomUS) || RoomUS <- Rooms],
    {jiffy:encode(lists:flatten(RoomsMap)), Req, State}.

get_room_details({RoomID, _} = RoomUS) ->
    case mod_muc_light_db_backend:get_config(RoomUS) of
        {ok, Config, _} ->
            #{id => RoomID,
              name => proplists:get_value(roomname, Config),
              subject => proplists:get_value(subject, Config)};
        _ ->
            []
    end.

from_json(Req, State) ->
    {Method, Req2} = cowboy_req:method(Req),
    {ok, Body, Req3} = cowboy_req:body(Req2),
    JSONData = jiffy:decode(Body, [return_maps]),
    handle_request(Method, JSONData, Req3, State).

handle_request(Method, JSONData, Req, State) ->
    case handle_request_by_method(Method, JSONData, State) of
        {error, _} ->
            {false, Req, State};
        Room ->
            RoomJID = jid:from_binary(Room),
            RespBody = #{<<"id">> => RoomJID#jid.luser},
            RespReq = cowboy_req:set_resp_body(jiffy:encode(RespBody), Req),
            {true, RespReq, State}
    end.

handle_request_by_method(<<"POST">>, JSONData,
                         #{user := User, jid := #jid{lserver = Server}}) ->
    #{<<"name">>    := RoomName,
      <<"subject">> := Subject} = JSONData,
    mod_muc_light_commands:create_unique_room(Server, RoomName, User, Subject);
handle_request_by_method(<<"PUT">>, JSONData, State) ->
    #{user := User, jid := #jid{lserver = Server}, room_id := RoomID} = State,
    #{<<"name">>    := RoomName, <<"subject">> := Subject} = JSONData,
    mod_muc_light_commands:create_identifiable_room(Server,
                                                    RoomID,
                                                    RoomName,
                                                    User,
                                                    Subject).

user_to_json({UserServer, Role}) ->
    #{user => jid:to_binary(UserServer),
      role => Role}.

muc_light_domain(Server) ->
    gen_mod:get_module_opt_subhost(Server, mod_muc_light, mod_muc_light:default_host()).

determine_role(US, Users) ->
    case lists:keyfind(US, 1, Users) of
        false -> none;
        {_, Role} ->
            Role
    end.

-spec validate_room_id(RoomIDOrJID :: binary(), Server :: binary()) ->
    {ok, RoomID :: binary()} | error.
validate_room_id(RoomIDOrJID, Server) ->
    MUCLightDomain = muc_light_domain(Server),
    case jid:from_binary(RoomIDOrJID) of
        #jid{luser = <<>>, lserver = RoomID, lresource = <<>>} ->
            {ok, RoomID};
        #jid{luser = RoomID, lserver = MUCLightDomain, lresource = <<>>} ->
            {ok, RoomID};
        _ ->
            error
    end.
