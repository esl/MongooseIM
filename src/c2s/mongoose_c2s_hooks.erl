%% @doc This module builds an interface to c2s event handling
-module(mongoose_c2s_hooks).

-type fn() :: fun((mongoose_acc:t(), params(), gen_hook:extra()) -> result()).
-type params() :: #{c2s_data := mongoose_c2s:data(),
                    c2s_state := mongoose_c2s:state(),
                    event_type := undefined | gen_statem:event_type(),
                    event_tag => atom(),
                    event_content := undefined | term(),
                    reason := undefined | term()}.
-type result() :: gen_hook:hook_fn_ret(mongoose_acc:t()).
-export_type([fn/0, params/0, result/0]).

%% XML handlers
-export([user_send_packet/3,
         user_receive_packet/3,
         user_send_message/3,
         user_send_iq/3,
         user_send_presence/3,
         user_send_xmlel/3,
         user_receive_message/3,
         user_receive_iq/3,
         user_receive_presence/3,
         user_receive_xmlel/3,
         xmpp_presend_element/3
        ]).

%% General event handlers
-export([foreign_event/3,
         user_open_session/3,
         user_terminate/3,
         reroute_unacked_messages/3,
         user_stop_request/3,
         user_socket_closed/3,
         user_socket_error/3]).

%%% @doc Event triggered after a user sends _any_ packet to the server.
%%% Examples of handlers can be metrics, archives, and any other subsystem
%%% that wants to see all stanzas the user delivers.
-spec user_send_packet(HostType, Acc, Params) -> Result when
    HostType :: mongooseim:host_type(),
    Acc :: mongoose_acc:t(),
    Params :: params(),
    Result :: result().
user_send_packet(HostType, Acc, Params) ->
    gen_hook:run_fold(user_send_packet, HostType, Acc, Params).

%% @doc Triggered when a user receives a packet through any routing mechanism.
%% Examples of handlers can be metrics or carbons.
-spec user_receive_packet(HostType, Acc, Params) -> Result when
    HostType :: mongooseim:host_type(),
    Acc :: mongoose_acc:t(),
    Params :: params(),
    Result :: result().
user_receive_packet(HostType, Acc, Params) ->
    gen_hook:run_fold(user_receive_packet, HostType, Acc, Params).

%% @doc Triggered when the user sends a stanza of type `message'
-spec user_send_message(HostType, Acc, Params) -> Result when
    HostType :: mongooseim:host_type(),
    Acc :: mongoose_acc:t(),
    Params :: params(),
    Result :: result().
user_send_message(HostType, Acc, Params) ->
    gen_hook:run_fold(user_send_message, HostType, Acc, Params).

%% @doc Triggered when the user sends a stanza of type `iq'
-spec user_send_iq(HostType, Acc, Params) -> Result when
    HostType :: mongooseim:host_type(),
    Acc :: mongoose_acc:t(),
    Params :: params(),
    Result :: result().
user_send_iq(HostType, Acc, Params) ->
    Acc1 = mongoose_iq:update_acc_info(Acc),
    gen_hook:run_fold(user_send_iq, HostType, Acc1, Params).

%% @doc Triggered when the user sends a stanza of type `presence'
-spec user_send_presence(HostType, Acc, Params) -> Result when
    HostType :: mongooseim:host_type(),
    Acc :: mongoose_acc:t(),
    Params :: params(),
    Result :: result().
user_send_presence(HostType, Acc, Params) ->
    gen_hook:run_fold(user_send_presence, HostType, Acc, Params).

%% @doc Triggered when the user sends a packet which is not a proper XMPP stanza, i.e.,
%% it is not of types `message', `iq', nor `presence'.
-spec user_send_xmlel(HostType, Acc, Params) -> Result when
    HostType :: mongooseim:host_type(),
    Acc :: mongoose_acc:t(),
    Params :: params(),
    Result :: result().
user_send_xmlel(HostType, Acc, Params) ->
    gen_hook:run_fold(user_send_xmlel, HostType, Acc, Params).


%% @doc Triggered when the user received a stanza of type `message'
-spec user_receive_message(HostType, Acc, Params) -> Result when
    HostType :: mongooseim:host_type(),
    Acc :: mongoose_acc:t(),
    Params :: params(),
    Result :: result().
user_receive_message(HostType, Acc, Params) ->
    gen_hook:run_fold(user_receive_message, HostType, Acc, Params).

%% @doc Triggered when the user received a stanza of type `iq'
-spec user_receive_iq(HostType, Acc, Params) -> Result when
    HostType :: mongooseim:host_type(),
    Acc :: mongoose_acc:t(),
    Params :: params(),
    Result :: result().
user_receive_iq(HostType, Acc, Params) ->
    Acc1 = mongoose_iq:update_acc_info(Acc),
    gen_hook:run_fold(user_receive_iq, HostType, Acc1, Params).

%% @doc Triggered when the user received a stanza of type `presence'
-spec user_receive_presence(HostType, Acc, Params) -> Result when
    HostType :: mongooseim:host_type(),
    Acc :: mongoose_acc:t(),
    Params :: params(),
    Result :: result().
user_receive_presence(HostType, Acc, Params) ->
    gen_hook:run_fold(user_receive_presence, HostType, Acc, Params).

%% @doc Triggered when the user received a packet which is not a proper XMPP stanza, i.e.,
%% it is not of types `message', `iq', nor `presence'.
-spec user_receive_xmlel(HostType, Acc, Params) -> Result when
    HostType :: mongooseim:host_type(),
    Acc :: mongoose_acc:t(),
    Params :: params(),
    Result :: result().
user_receive_xmlel(HostType, Acc, Params) ->
    gen_hook:run_fold(user_receive_xmlel, HostType, Acc, Params).

-spec xmpp_presend_element(HostType, Acc, Params) -> Result when
    HostType :: mongooseim:host_type(),
    Acc :: mongoose_acc:t(),
    Params :: params(),
    Result :: result().
xmpp_presend_element(HostType, Acc, Params) ->
    gen_hook:run_fold(xmpp_presend_element, HostType, Acc, Params).

%% @doc Triggered when the c2s statem process receives any event it is not defined to handle.
%% These events should not by default stop the process, and they are expected to
%% be handled by a single event handler, which should then stop the hook fold.
%% If no handler handles the event, it is logged.
-spec foreign_event(HostType, Acc, Params) -> Result when
    HostType :: mongooseim:host_type(),
    Acc :: mongoose_acc:t(),
    Params :: map(),
    Result :: result().
foreign_event(HostType, Acc, Params) ->
    gen_hook:run_fold(foreign_event, HostType, Acc, Params).

%% @doc Triggered when the user binds a resource and attempts to open a session
%% This is ran _before_ registering the user in the session table.
%% If any handler returns a `stop' tag, the session establishment is rejected
%% and the user may be allowed to retry
-spec user_open_session(HostType, Acc, Params) -> Result when
    HostType :: mongooseim:host_type(),
    Acc :: mongoose_acc:t(),
    Params :: params(),
    Result :: result().
user_open_session(HostType, Acc, Params) ->
    gen_hook:run_fold(user_open_session, HostType, Acc, Params).

%% @doc Triggered when the user session is irrevocably terminating.
%% This is ran _before_ removing the user from the session table and closing his socket.
-spec user_terminate(HostType, Acc, Params) -> Result when
    HostType :: mongooseim:host_type(),
    Acc :: mongoose_acc:t(),
    Params :: params(),
    Result :: mongoose_acc:t().
user_terminate(HostType, Acc, Params) ->
    {_, Res} = gen_hook:run_fold(user_terminate, HostType, Acc, Params),
    Res.

%% @doc Triggered when the user session is irrevocably terminating.
%% This is ran _after_ removing the user from the session table, but before closing the socket
-spec reroute_unacked_messages(HostType, Acc, Params) -> Result when
    HostType :: mongooseim:host_type(),
    Acc :: mongoose_acc:t(),
    Params :: params(),
    Result :: mongoose_acc:t().
reroute_unacked_messages(HostType, Acc, Params) ->
    {_, Res} = gen_hook:run_fold(reroute_unacked_messages, HostType, Acc, Params),
    Res.

%% These conditions required that one and only one handler declared full control over it,
%% by making the hook stop at that point. If so, the process remains alive,
%% in control of the handler, otherwise, the condition is treated as terminal.
%% See `mongoose_c2s:stop_if_unhandled/3'

%% @doc Triggered when an external event requests the connection to be closed.
-spec user_stop_request(HostType, Acc, Params) -> Result when
    HostType :: mongooseim:host_type(),
    Acc :: mongoose_acc:t(),
    Params :: params(),
    Result :: result().
user_stop_request(HostType, Acc, Params) ->
    gen_hook:run_fold(user_stop_request, HostType, Acc, Params).

%% @doc Triggered when the socket dies.
-spec user_socket_closed(HostType, Acc, Params) -> Result when
    HostType :: mongooseim:host_type(),
    Acc :: mongoose_acc:t(),
    Params :: params(),
    Result :: result().
user_socket_closed(HostType, Acc, Params) ->
    gen_hook:run_fold(user_socket_closed, HostType, Acc, Params).

%% @doc Triggered when the socket errors out.
-spec user_socket_error(HostType, Acc, Params) -> Result when
    HostType :: mongooseim:host_type(),
    Acc :: mongoose_acc:t(),
    Params :: params(),
    Result :: result().
user_socket_error(HostType, Acc, Params) ->
    gen_hook:run_fold(user_socket_error, HostType, Acc, Params).
