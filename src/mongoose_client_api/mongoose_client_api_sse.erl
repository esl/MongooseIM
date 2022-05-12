-module(mongoose_client_api_sse).
-behaviour(lasse_handler).

-include("mongoose.hrl").
-include("jlib.hrl").

-export([init/3]).
-export([handle_notify/2]).
-export([handle_info/2]).
-export([handle_error/3]).
-export([terminate/3]).

init(_InitArgs, _LastEvtId, Req) ->
    ?LOG_DEBUG(#{what => client_api_sse_init, req => Req}),
    {cowboy_rest, Req1, State0} = mongoose_client_api:init(Req, []),
    {Authorization, Req2, State} = mongoose_client_api:is_authorized(Req1, State0),
    maybe_init(Authorization, Req2, State#{id => 1}).

maybe_init(true, Req, #{jid := JID, creds := Creds} = State) ->
    SID = ejabberd_sm:make_new_sid(),
    UUID = uuid:uuid_to_string(uuid:get_v4(), binary_standard),
    Resource = <<"sse-", UUID/binary>>,
    NewJid = jid:replace_resource(JID, Resource),
    HostType = mongoose_credentials:host_type(Creds),
    ejabberd_sm:open_session(HostType, SID, NewJid, 1, #{}),
    {ok, Req, State#{sid => SID, jid => NewJid}};
maybe_init(true, Req, State) ->
    %% This is for OPTIONS method
    {shutdown, 200, #{}, <<>>, Req, State};
maybe_init({false, Value}, Req, State) ->
    Headers = #{<<"www-authenticate">> => Value},
    {shutdown, 401, Headers, <<>>, Req, State}.

handle_notify(Msg, State) ->
    ?UNEXPECTED_INFO(Msg),
    {nosend, State}.

handle_info({route, _From, _To, Acc}, State) ->
    #xmlel{ name = TagName } = El = mongoose_acc:element(Acc),
    handle_msg(TagName, Acc, El, State);
handle_info(Msg, State) ->
    ?UNEXPECTED_INFO(Msg),
    {nosend, State}.

handle_msg(<<"message">>, Acc, El, State) ->
    Timestamp = os:system_time(microsecond),
    Type = mongoose_acc:stanza_type(Acc),
    maybe_send_message_event(Type, El, Timestamp, State).

handle_error(Msg, Reason, State) ->
    ?LOG_WARNING(#{what => sse_handle_error, msg => Msg, reason => Reason}),
    {nosend, State}.

terminate(_Reason, _Req, #{creds := Creds} = State) ->
    case maps:get(sid, State, undefined) of
        undefined ->
            ok;
        SID ->
            JID = #jid{lserver = S} = maps:get(jid, State),
            Acc = mongoose_acc:new(
                    #{location => ?LOCATION,
                      lserver => S,
                      host_type => mongoose_credentials:host_type(Creds),
                      element => undefined}),
            ejabberd_sm:close_session(Acc, SID, JID, normal)
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
