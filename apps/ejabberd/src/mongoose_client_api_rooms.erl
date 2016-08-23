-module(mongoose_client_api_rooms).

-export([init/3]).
-export([content_types_provided/2]).
-export([content_types_accepted/2]).
-export([is_authorized/2]).
-export([allowed_methods/2]).

-export([to_json/2]).
-export([from_json/2]).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include_lib("exml/include/exml.hrl").

%% yes, there is no other option, this API has to run over encrypted connection
init({ssl, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_rest}.

is_authorized(Req, State) ->
    mongoose_client_api:is_authorized(Req, State).

content_types_provided(Req, State) ->
    {[
      {<<"application/json">>, to_json}
     ], Req, State}.

content_types_accepted(Req, State) ->
    {[
      {<<"application/json">>, from_json}
     ], Req, State}.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"POST">>, <<"PUT">>], Req, State}.

to_json(Req, {_User, #jid{lserver = Server} = JID} = State) ->
    {ok, Req, State}.

from_json(Req, State) ->
    {Method, Req2} = cowboy_req:method(Req),
    {ok, Body, Req3} = cowboy_req:body(Req2),
    JSONData = jiffy:decode(Body, [return_maps]),
    handle_request(Method, JSONData, Req3, State).

handle_request(<<"POST">>, JSONData, Req,
               {User, #jid{lserver = Server}} = State) ->
    #{<<"id">> := RoomID, <<"subject">> := Subject} = JSONData,
    case mod_muc_light_admin:create_unique_room(Server, RoomID, User, Subject) of
        {error, _} ->
            {false, Req, State};
        _RoomJID ->
            {true, Req, State}
    end;
handle_request(<<"PUT">>, JSONData, Req,
               {User, #jid{lserver = Server}} = State) ->
    #{<<"user">> := UserToInvite} = JSONData,
    {RoomId, Req2} = cowboy_req:binding(id, Req),
    mod_muc_light_admin:invite_to_room(Server, RoomId, User, UserToInvite),
    {true, Req2, State}.


