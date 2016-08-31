-module(mongoose_client_api_rooms_messages).

-export([init/3]).
-export([rest_init/2]).
-export([content_types_provided/2]).
-export([content_types_accepted/2]).
-export([is_authorized/2]).
-export([allowed_methods/2]).
-export([resource_exists/2]).
-export([allow_missing_post/2]).

-export([to_json/2]).
-export([from_json/2]).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include_lib("exml/include/exml.hrl").

%% yes, there is no other option, this API has to run over encrypted connection
init({ssl, http}, _Req, _Opts) ->
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
    {[<<"GET">>, <<"POST">>], Req, State}.

resource_exists(Req, State) ->
    mongoose_client_api_rooms:resource_exists(Req, State).

allow_missing_post(Req, State) ->
    {false, Req, State}.

to_json(Req, #{room := Room} = State) ->
    Config = maps:get(config, Room),
    Resp = #{name => proplists:get_value(roomname, Config),
             subject => proplists:get_value(subject, Config)
            },
    {jiffy:encode(Resp), Req, State}.

from_json(Req, #{user := User, jid := JID, room := Room} = State) ->
    {ok, Body, Req2} = cowboy_req:body(Req),
    JSONData = jiffy:decode(Body, [return_maps]),
    RoomJID = maps:get(jid, Room),
    UUID = uuid:uuid_to_string(uuid:get_v4(), binary_standard),
    Message = build_message(User, RoomJID, UUID, maps:get(<<"body">>, JSONData)),
    ejabberd_router:route(JID, RoomJID, Message),
    Resp = #{id => UUID},
    Req3 = cowboy_req:set_resp_body(jiffy:encode(Resp), Req2),
    {true, Req3, State}.

-spec build_message(From :: binary(), To :: ejabberd:jid(), ID :: string(), Body :: binary()) ->
    jlib:xmlel().
build_message(From, To, ID, Body) ->
    Attrs = [{<<"from">>, From},
             {<<"to">>, jid:to_binary(To)},
             {<<"id">>, ID},
             {<<"type">>, <<"groupchat">>}],
    BodyEl = #xmlel{name = <<"body">>,
                    children = [{xmlcdata, Body}]},
    #xmlel{name = <<"message">>,
           attrs = Attrs,
           children = [BodyEl]}.

