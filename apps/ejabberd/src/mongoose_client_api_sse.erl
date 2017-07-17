-module(mongoose_client_api_sse).

-behaviour(lasse_handler).

-include("ejabberd.hrl").
-include("jlib.hrl").

-export([init/3]).
-export([handle_notify/2]).
-export([handle_info/2]).
-export([handle_error/3]).
-export([terminate/3]).

init(_InitArgs, _LastEvtId, Req) ->
    {ok, Req1, State0} = mongoose_client_api:rest_init(Req, []),
    {Authorization, Req2, State} = mongoose_client_api:is_authorized(Req1, State0),
    maybe_init(Authorization, Req2, State#{id => 1}).

maybe_init(true, Req, #{jid := JID} = State) ->
    SID = {os:timestamp(), self()},
    User = JID#jid.user,
    Server = JID#jid.server,
    UUID = uuid:uuid_to_string(uuid:get_v4(), binary_standard),
    Resource = <<"sse-", UUID/binary>>,

    ejabberd_sm:open_session(SID, User, Server, Resource, 1, []),

    {ok, Req, State#{sid => SID, jid => jid:replace_resource(JID, Resource)}};
maybe_init(true, Req, State) ->
    %% This is for OPTIONS method
    {shutdown, 200, [], <<>>, Req, State};
maybe_init({false, Value}, Req, State) ->
    Headers = [{<<"www-authenticate">>, Value}],
    {shutdown, 401, Headers, <<>>, Req, State}.

handle_notify(_Msg, State) ->
    {nosend, State}.

handle_info({route, _From, _To, Acc}, State) ->
    handle_info({route, _From, _To, Acc, mongoose_acc:get(element, Acc)}, State);
handle_info({route, _From, _To, Acc, El}, State) ->
    handle_msg(mongoose_acc:get(name, Acc), Acc, El, State);
handle_info(_Msg, State) ->
    {nosend, State}.

handle_msg(<<"message">>, Acc, El, State) ->
    Timestamp = usec:from_now(os:timestamp()),
    Type = mongoose_acc:get(type, Acc),
    maybe_send_message_event(Type, El, Timestamp, State).

handle_error(_Msg, _Reson, State) ->
    {nosend, State}.

terminate(_Reson, _Req, State) ->
    case maps:get(sid, State, undefined) of
        undefined ->
            ok;
        SID ->
            #jid{user = U, server = S, resource = R} = maps:get(jid, State),
            ejabberd_sm:close_session(SID, U, S, R, normal)
    end,
    State.

maybe_send_message_event(<<"chat">>, Packet, Timestamp, #{id := ID} = State) ->
    Data = jiffy:encode(mongoose_client_api_messages:encode(Packet, Timestamp)),
    Event = #{id => integer_to_binary(ID), event => <<"message">>, data => Data},
    {send, Event, State#{id := ID + 1}};
maybe_send_message_event(<<"groupchat">>, Packet, Timestamp, #{id := ID} = State) ->
    Data = jiffy:encode(mongoose_client_api_rooms_messages:encode(Packet, Timestamp)),
    Event = #{id => integer_to_binary(ID), event => <<"room.message">>, data => Data},
    {send, Event, State#{id := ID + 1}};
maybe_send_message_event(_, _, _, State) ->
    {nosend, State}.
