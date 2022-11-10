-module(mongoose_client_api_rooms_config).

-behaviour(mongoose_client_api).
-export([routes/0]).

-behaviour(cowboy_rest).
-export([trails/0,
         init/2,
         is_authorized/2,
         content_types_accepted/2,
         allowed_methods/2,
         from_json/2]).

-ignore_xref([from_json/2, trails/0]).

-import(mongoose_client_api, [parse_body/1, try_handle_request/3, throw_error/2]).
-import(mongoose_client_api_rooms, [get_room_jid/3, get_room_name/1, get_room_subject/1]).

-type req() :: cowboy_req:req().
-type state() :: map().

-spec routes() -> mongoose_http_handler:routes().
routes() ->
    [{"/rooms/[:id]/config", ?MODULE, #{}}].

trails() ->
    mongoose_client_api_rooms_config_doc:trails().

-spec init(req(), state()) -> {cowboy_rest, req(), state()}.
init(Req, Opts) ->
    mongoose_client_api:init(Req, Opts).

-spec is_authorized(req(), state()) -> {true | {false, iodata()}, req(), state()}.
is_authorized(Req, State) ->
    mongoose_client_api:is_authorized(Req, State).

content_types_accepted(Req, State) ->
    mongoose_client_api_rooms:content_types_accepted(Req, State).

allowed_methods(Req, State) ->
    {[<<"OPTIONS">>, <<"PUT">>], Req, State}.

%% @doc Called for a method of type "PUT"
-spec from_json(req(), state()) -> {true | stop, req(), state()}.
from_json(Req, State) ->
    try_handle_request(Req, State, fun handle_put/2).

%% Internal functions

handle_put(Req, State = #{jid := UserJid}) ->
    Bindings = cowboy_req:bindings(Req),
    RoomJid = get_room_jid(Bindings, State, required),
    Args = parse_body(Req),
    Name = get_room_name(Args),
    Subject = get_room_subject(Args),
    Config = #{<<"roomname">> => Name, <<"subject">> => Subject},
    case mod_muc_light_api:change_room_config(RoomJid, UserJid, Config) of
        {ok, _} ->
            {true, Req, State};
        {not_room_member, Msg} ->
            throw_error(denied, Msg);
        {not_allowed, Msg} ->
            throw_error(denied, Msg);
        {room_not_found, Msg} ->
            throw_error(not_found, Msg);
        {validation_error, Msg} ->
            throw_error(bad_request, Msg)
    end.
