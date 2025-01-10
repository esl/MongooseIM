-module(mongoose_component_socket).

-export([new/1, handle_data/2, activate/1, close/1, send_xml/2]).

-callback new(mongoose_listener:transport_module(), ranch:ref(), mongoose_listener:options()) -> state().
-callback peername(state()) -> {inet:ip_address(), inet:port_number()}.
-callback handle_data(state(), {tcp, term(), iodata()}) ->
    iodata() | {raw, [exml:element()]} | {error, term()}.
-callback activate(state()) -> ok.
-callback close(state()) -> ok.
-callback send_xml(state(), iodata() | exml_stream:element() | [exml_stream:element()]) ->
    ok | {error, term()}.

-record(component_socket, {module :: module(),
                           state :: state()}).
-type socket() :: #component_socket{}.
-type state() :: term().
-type conn_type() :: component.
-export_type([socket/0, state/0, conn_type/0]).

-spec new(mongoose_listener:init_args()) -> socket().
new({Module, Ref, Transport, LOpts}) ->
    State = Module:new(Transport, Ref, LOpts),
    C2SSocket = #component_socket{
        module = Module,
        state = State},
    activate(C2SSocket),
    C2SSocket.

-spec handle_data(socket(), {tcp, term(), iodata()}) ->
    iodata() | {raw, [term()]} | {error, term()}.
handle_data(#component_socket{module = Module, state = State}, Payload) ->
    Module:handle_data(State, Payload);
handle_data(_, _) ->
    {error, bad_packet}.

-spec activate(socket()) -> ok | {error, term()}.
activate(#component_socket{module = Module, state = State}) ->
    Module:activate(State).

-spec close(socket()) -> ok.
close(#component_socket{module = Module, state = State}) ->
    Module:close(State).

-spec send_xml(socket(), exml_stream:element() | [exml_stream:element()]) -> ok | {error, term()}.
send_xml(#component_socket{module = Module, state = State}, XML) ->
    Module:send_xml(State, XML).
