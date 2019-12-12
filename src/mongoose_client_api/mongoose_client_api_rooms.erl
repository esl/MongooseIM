-module(mongoose_client_api_rooms).
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

-include("mongoose.hrl").
-include("jlib.hrl").
-include_lib("exml/include/exml.hrl").

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
            case validate_room_id(RoomIDOrJID, Server) of
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
    case mod_muc_light_db_backend:get_info({RoomU, RoomS}) of
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


from_json(Req, State = #{was_replied := true}) ->
    {true, Req, State};
from_json(Req, State) ->
    Method = cowboy_req:method(Req),
    {ok, Body, Req2} = cowboy_req:read_body(Req),
    case mongoose_client_api:json_to_map(Body) of
        {ok, #{<<"name">> := N, <<"subject">> := S} = JSONData} when is_binary(N), is_binary(S) ->
            handle_request(Method, JSONData, Req2, State);
        _ ->
            {false, Req, State}
    end.

handle_request(Method, JSONData, Req, State) ->
    case handle_request_by_method(Method, JSONData, Req, State) of
        {error, _} ->
            {false, Req, State};
        Room ->
            RoomJID = jid:from_binary(Room),
            RespBody = #{<<"id">> => RoomJID#jid.luser},
            RespReq = cowboy_req:set_resp_body(jiffy:encode(RespBody), Req),
            {true, RespReq, State}
    end.

handle_request_by_method(<<"POST">>, JSONData, _Req,
                         #{user := User, jid := #jid{lserver = Server}}) ->
    #{<<"name">> := RoomName, <<"subject">> := Subject} = JSONData,
    mod_muc_light_commands:create_unique_room(Server, RoomName, User, Subject);

handle_request_by_method(<<"PUT">>, JSONData, Req, State) ->
    assert_room_id_set(Req, State),
    #{user := User, jid := #jid{lserver = Server}, room_id := RoomID} = State,
    #{<<"name">> := RoomName, <<"subject">> := Subject} = JSONData,
    mod_muc_light_commands:create_identifiable_room(Server,
                                                    RoomID,
                                                    RoomName,
                                                    User,
                                                    Subject).

assert_room_id_set(_Req, #{room_id := _} = _State) ->
    ok.

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
            ?WARNING_MSG("issue=invalid_room_id id=~p muclight_domain=~p",
                         [RoomIDOrJID, MUCLightDomain]),
            error
    end.
