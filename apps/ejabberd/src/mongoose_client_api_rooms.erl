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

-include("ejabberd.hrl").
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
    {[<<"GET">>, <<"POST">>], Req, State}.

resource_exists(Req, #{jid := #jid{lserver = Server}} = State) ->
    {RoomID, Req2} = cowboy_req:binding(id, Req),
    MUCLightDomain = muc_light_domain(Server),
    case RoomID of
        undefined ->
            {Method, Req3} = cowboy_req:method(Req2),
            case Method of
                <<"GET">> ->
                    {true, Req3, State};
                _ ->
                    {false, Req3, State}
            end;
        _ ->
            does_room_exist(RoomID, MUCLightDomain, Req2, State)
    end.

does_room_exist(RoomU, RoomS, Req, #{jid := JID} = State) ->
    case mod_muc_light_db_backend:get_info({RoomU, RoomS}) of
        {ok, Config, Users, Version} ->
            Room = #{config => Config,
                     users => Users,
                     version => Version,
                     jid => jid:make_noprep(RoomU, RoomS, <<>>)},
            CallerRole = determine_role(jid:to_lus(JID), Users),
            NewState = State#{room => Room, role_in_room => CallerRole},
            {true, Req, NewState};
        _ ->
            {Method, Req2} = cowboy_req:method(Req),
            case Method of
                <<"PUT">> ->
                    {ok, Req3} = cowboy_req:reply(404, Req2),
                    {halt, Req3, State};
                _ ->
                    {false, Req, State}
            end
    end.

forbidden_request(Req, State) ->
    cowboy_req:reply(403, Req),
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

handle_request(<<"POST">>, JSONData, Req,
               #{user := User, jid := #jid{lserver = Server}} = State) ->
    #{<<"name">> := RoomName, <<"subject">> := Subject} = JSONData,
    case mod_muc_light_commands:create_unique_room(Server, RoomName, User, Subject) of
        {error, _} ->
            {false, Req, State};
        Room ->
            RoomJID = jid:from_binary(Room),
            RespBody = #{<<"id">> => RoomJID#jid.luser},
            RespReq = cowboy_req:set_resp_body(jiffy:encode(RespBody), Req),
            {true, RespReq, State}
    end.

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
