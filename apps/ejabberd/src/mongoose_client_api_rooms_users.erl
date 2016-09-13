-module(mongoose_client_api_rooms_users).

-export([init/3]).
-export([rest_init/2]).
-export([content_types_provided/2]).
-export([content_types_accepted/2]).
-export([is_authorized/2]).
-export([allowed_methods/2]).
-export([resource_exists/2]).
-export([allow_missing_post/2]).

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
    mongoose_client_api_rooms:content_types_provided(Req, State).

content_types_accepted(Req, State) ->
    mongoose_client_api_rooms:content_types_accepted(Req, State).

allowed_methods(Req, State) ->
    {[<<"POST">>], Req, State}.

resource_exists(Req, State) ->
    mongoose_client_api_rooms:resource_exists(Req, State).

allow_missing_post(Req, State) ->
    {false, Req, State}.

from_json(Req, #{role_in_room := none} = State) ->
    mongoose_client_api_rooms:forbidden_request(Req, State);
from_json(Req, #{user := User, jid := #jid{lserver = Server}} = State) ->
    {ok, Body, Req2} = cowboy_req:body(Req),
    JSONData = jiffy:decode(Body, [return_maps]),
    #{<<"user">> := UserToInvite} = JSONData,
    {RoomId, Req3} = cowboy_req:binding(id, Req2),
    mod_muc_light_admin:invite_to_room_id(Server, RoomId, User, UserToInvite),
    {true, Req3, State}.

