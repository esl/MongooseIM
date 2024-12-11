-module(mongoose_client_api_rooms).

-behaviour(mongoose_client_api).
-export([routes/0]).

-behaviour(cowboy_rest).
-export([trails/0,
         init/2,
         is_authorized/2,
         content_types_provided/2,
         content_types_accepted/2,
         allowed_methods/2,
         to_json/2,
         from_json/2]).

%% Used by mongoose_client_api_rooms_*
-export([get_room_jid/3,
         get_room_name/1,
         get_room_subject/1]).

-ignore_xref([from_json/2, to_json/2, trails/0]).

-import(mongoose_client_api, [parse_body/1, try_handle_request/3, throw_error/2]).

-type req() :: cowboy_req:req().
-type state() :: map().

-include("jlib.hrl").

-spec routes() -> mongoose_http_handler:routes().
routes() ->
    [{"/rooms/[:id]", ?MODULE, #{}}].

trails() ->
    mongoose_client_api_rooms_doc:trails().

-spec init(req(), state()) -> {cowboy_rest, req(), state()}.
init(Req, Opts) ->
    mongoose_client_api:init(Req, Opts).

-spec is_authorized(req(), state()) -> {true | {false, iodata()}, req(), state()}.
is_authorized(Req, State) ->
    mongoose_client_api:is_authorized(Req, State).

-spec content_types_provided(req(), state()) ->
          {[{{binary(), binary(), '*'}, atom()}], req(), state()}.
content_types_provided(Req, State) ->
    {[
      {{<<"application">>, <<"json">>, '*'}, to_json}
     ], Req, State}.

-spec content_types_accepted(req(), state()) ->
          {[{{binary(), binary(), '*'}, atom()}], req(), state()}.
content_types_accepted(Req, State) ->
    {[
      {{<<"application">>, <<"json">>, '*'}, from_json}
     ], Req, State}.

-spec allowed_methods(req(), state()) -> {[binary()], req(), state()}.
allowed_methods(Req, State) ->
    {[<<"OPTIONS">>, <<"GET">>, <<"POST">>, <<"PUT">>], Req, State}.

%% @doc Called for a method of type "GET"
-spec to_json(req(), state()) -> {iodata() | stop, req(), state()}.
to_json(Req, State) ->
    try_handle_request(Req, State, fun handle_get/2).

%% @doc Called for a method of type "POST" and "PUT"
-spec from_json(req(), state()) -> {stop, req(), state()}.
from_json(Req, State) ->
    F = case cowboy_req:method(Req) of
            <<"POST">> -> fun handle_post/2;
            <<"PUT">> -> fun handle_put/2
        end,
    try_handle_request(Req, State, F).

%% Internal functions

handle_get(Req, State = #{jid := UserJid}) ->
    Bindings = cowboy_req:bindings(Req),
    case get_room_jid(Bindings, State, optional) of
        undefined ->
            {ok, Rooms} = mod_muc_light_api:get_user_rooms(UserJid),
            {jiffy:encode(lists:flatmap(fun room_us_to_json/1, Rooms)), Req, State};
        RoomJid ->
            case mod_muc_light_api:get_room_info(RoomJid, UserJid) of
                {ok, Info} ->
                    {jiffy:encode(room_info_to_json(Info)), Req, State};
                {room_not_found, Msg} ->
                    throw_error(not_found, Msg);
                {not_room_member, Msg} ->
                    throw_error(denied, Msg)
            end
    end.

handle_post(Req, State = #{jid := UserJid}) ->
    MUCLightDomain = muc_light_domain(State),
    Args = parse_body(Req),
    Name = get_room_name(Args),
    Subject = get_room_subject(Args),
    Config = #{<<"roomname">> => Name, <<"subject">> => Subject},
    {ok, #{jid := RoomJid}} = mod_muc_light_api:create_room(MUCLightDomain, UserJid, Config),
    room_created(Req, State, RoomJid).

handle_put(Req, State = #{jid := UserJid}) ->
    Bindings = cowboy_req:bindings(Req),
    #jid{luser = RoomId, lserver = MUCLightDomain} = get_room_jid(Bindings, State, required),
    Args = parse_body(Req),
    Name = get_room_name(Args),
    Subject = get_room_subject(Args),
    Config = #{<<"roomname">> => Name, <<"subject">> => Subject},
    case mod_muc_light_api:create_room(MUCLightDomain, RoomId, UserJid, Config) of
        {ok, #{jid := RoomJid}} ->
            room_created(Req, State, RoomJid);
        {already_exists, Msg} ->
            throw_error(denied, Msg)
    end.

room_created(Req, State, RoomJid) ->
    RespBody = #{<<"id">> => RoomJid#jid.luser},
    Req2 = cowboy_req:set_resp_body(jiffy:encode(RespBody), Req),
    Req3 = cowboy_req:reply(201, Req2),
    {stop, Req3, State}.

-spec room_us_to_json(jid:simple_bare_jid()) -> [jiffy:json_value()].
room_us_to_json({RoomU, RoomS}) ->
    #jid{luser = RoomId} = RoomJid = jid:make_noprep(RoomU, RoomS, <<>>),
    case mod_muc_light_api:get_room_info(RoomJid) of
        {ok, Info} ->
            NS = room_name_and_subject(Info),
            [NS#{id => RoomId}];
        {room_not_found, _} ->
            [] % room was removed after listing rooms, but before this query
    end.

-spec room_info_to_json(mod_muc_light_api:room()) -> jiffy:json_value().
room_info_to_json(Info = #{aff_users := AffUsers}) ->
    NS = room_name_and_subject(Info),
    NS#{participants => lists:map(fun user_to_json/1, AffUsers)}.

room_name_and_subject(#{options := #{<<"roomname">> := Name, <<"subject">> := Subject}}) ->
    #{name => Name, subject => Subject}.

user_to_json({UserServer, Role}) ->
    #{user => jid:to_binary(UserServer),
      role => Role}.

get_room_jid(#{id := IdOrJid}, State, _) ->
    MUCLightDomain = muc_light_domain(State),
    case jid:nodeprep(IdOrJid) of
        error ->
            case jid:from_binary(IdOrJid) of
                error ->
                    throw_error(bad_request, <<"Invalid room ID">>);
                #jid{lserver = MUCLightDomain} = Jid ->
                    Jid;
                #jid{} ->
                    throw_error(bad_request, <<"Invalid MUC Light domain">>)
            end;
        RoomId when RoomId =/= <<>> ->
            jid:make_noprep(RoomId, MUCLightDomain, <<>>)
    end;
get_room_jid(#{}, _State, required) -> throw_error(bad_request, <<"Missing room ID">>);
get_room_jid(#{}, _State, optional) -> undefined.

muc_light_domain(#{creds := Creds}) ->
    HostType = mongoose_credentials:host_type(Creds),
    LServer = mongoose_credentials:lserver(Creds),
    try
        mod_muc_light_utils:server_host_to_muc_host(HostType, LServer)
    catch
        _:_ -> throw_error(not_found, <<"MUC Light server not found">>)
    end.

get_room_name(#{name := Name}) -> Name;
get_room_name(#{}) -> throw_error(bad_request, <<"Missing room name">>).

get_room_subject(#{subject := Subject}) -> Subject;
get_room_subject(#{}) -> throw_error(bad_request, <<"Missing room subject">>).
