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
    {Authorization, Req2, State} = mongoose_client_api:is_authorized(Req, #{}),
    maybe_init(Authorization, Req2, State).

maybe_init(true, Req, #{jid := JID} = State) ->
    SID = {os:timestamp(), self()},
    User = JID#jid.user,
    Server = JID#jid.server,
    UUID = uuid:uuid_to_string(uuid:get_v4(), binary_standard),
    Resource = <<"sse-", UUID/binary>>,

    ok = ejabberd_sm:open_session(SID, User, Server, Resource, 1, []),

    {ok, Req, State#{sid => SID, jid => jid:replace_resource(JID, Resource)}};
maybe_init({false, Value}, Req, State) ->
    Headers = [{<<"www-authenticate">>, Value}],
    {shutdown, 401, Headers, <<>>, Req, State}.

handle_notify(_Msg, State) ->
    {nosend, State}.

handle_info({route, _From, _To, #xmlel{name = <<"message">>} = Packet}, State) ->
    Timestamp = usec:from_now(os:timestamp()),
    Type = exml_query:attr(Packet, <<"type">>),
    maybe_send_message_event(Type, Packet, Timestamp, State);
handle_info(_Msg, State) ->
    {nosend, State}.

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

maybe_send_message_event(<<"chat">>, Packet, Timestamp, State) ->
    Data = jiffy:encode(mongoose_client_api_messages:encode(Packet, Timestamp)),
    Event = #{id => <<"1">>,
              event => <<"msg">>,
              data => Data
             },
    {send, Event,  State};
maybe_send_message_event(_, _, _, State) ->
    {nosend, State}.

