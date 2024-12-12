-module(mongoose_client_api_sse).

-behaviour(mongoose_client_api).
-export([routes/0]).

-behaviour(lasse_handler).
-export([init/3]).
-export([handle_notify/2]).
-export([handle_info/2]).
-export([handle_error/3]).
-export([terminate/3]).

-include("mongoose.hrl").
-include("jlib.hrl").

-spec routes() -> mongoose_http_handler:routes().
routes() ->
    [{"/sse", lasse_handler, #{module => mongoose_client_api_sse}}].

init(_InitArgs, _LastEvtId, Req) ->
    process_flag(trap_exit, true), % needed for 'terminate' to be called
    ?LOG_DEBUG(#{what => client_api_sse_init, req => Req}),
    {cowboy_rest, Req1, State0} = mongoose_client_api:init(Req, []),
    {Authorization, Req2, State} = mongoose_client_api:is_authorized(Req1, State0),
    % set 1h timeout to prevent client from frequent disconnections
    cowboy_req:cast({set_options, #{
        idle_timeout => 3600000
    }}, Req),
    maybe_init(Authorization, Req2, State#{id => 1}).

maybe_init(true, Req, #{jid := JID} = State) ->
    {ok, Session} = mongoose_stanza_api:open_session(JID, false),
    {ok, Req, State#{session => Session}};
maybe_init(true, Req, State) ->
    %% This is for OPTIONS method
    {shutdown, 200, #{}, <<>>, Req, State};
maybe_init({false, Value}, Req, State) ->
    Headers = #{<<"www-authenticate">> => Value},
    {shutdown, 401, Headers, <<>>, Req, State}.

handle_notify(Msg, State) ->
    ?UNEXPECTED_INFO(Msg),
    {nosend, State}.

handle_info({route, Acc}, State) ->
    #xmlel{ name = TagName } = El = mongoose_acc:element(Acc),
    handle_msg(TagName, Acc, El, State);
handle_info(Msg, State) ->
    ?UNEXPECTED_INFO(Msg),
    {nosend, State}.

handle_msg(<<"message">>, Acc, El, State) ->
    Timestamp = os:system_time(microsecond),
    Type = mongoose_acc:stanza_type(Acc),
    maybe_send_message_event(Type, El, Timestamp, State);
handle_msg(_, _, _, State) ->
    {nosend, State}.

handle_error(Msg, Reason, State) ->
    ?LOG_WARNING(#{what => sse_handle_error, msg => Msg, reason => Reason}),
    {nosend, State}.

terminate(_Reason, _Req, #{session := Session}) ->
    mongoose_stanza_api:close_session(Session),
    ok;
terminate(_Reason, _Req, _State) ->
    ok.

maybe_send_message_event(<<"chat">>, Packet, Timestamp, #{id := ID} = State) ->
    Data = iolist_to_binary(jiffy:encode(mongoose_client_api_messages:encode(Packet, Timestamp))),
    Event = #{id => integer_to_binary(ID), event => <<"message">>, data => Data},
    {send, Event, State#{id := ID + 1}};
maybe_send_message_event(<<"groupchat">>, Packet, Timestamp, #{id := ID} = State) ->
    Data = iolist_to_binary(jiffy:encode(mongoose_client_api_rooms_messages:encode(Packet, Timestamp))),
    Event = #{id => integer_to_binary(ID), event => <<"room.message">>, data => Data},
    {send, Event, State#{id := ID + 1}};
maybe_send_message_event(_, _, _, State) ->
    {nosend, State}.
