-module(mongoose_log_filter).
-export([fill_metadata_filter/2]).
-export([format_c2s_state_filter/2]).
-export([format_acc_filter/2]).
-export([format_packet_filter/2]).
-export([format_stacktrace_filter/2]).
-export([preserve_acc_filter/2]).

-include("mongoose.hrl").
-include_lib("jid/include/jid.hrl").
-include("ejabberd_c2s.hrl").

%% The templater in flatlog works with meta fields.
%% So, we would need a filter, that takes the interesting fields
%% from msg to metadata.
-spec fill_metadata_filter(logger:log_event(), term()) -> logger:filter_return().
fill_metadata_filter(Event=#{msg := {report, Msg}, meta := Meta}, Fields) ->
    FieldMap = maps:with(Fields, Msg),
    %% Remove the fields to not print them twice
    Msg2 = maps:without(Fields, Msg),
    Event#{meta => maps:merge(FieldMap, Meta), msg => {report, Msg2}}.


format_c2s_state_filter(Event=#{msg := {report, Msg=#{c2s_state := State}}}, _) ->
    StateMap = c2s_state_to_map(State),
    %% C2S fields have lower priority, if the field is already present in msg.
    Msg2 = maps:merge(StateMap, maps:remove(c2s_state, Msg)),
    Event#{msg => {report, Msg2}};
format_c2s_state_filter(Event, _) ->
    Event.

format_acc_filter(Event=#{msg := {report, Msg=#{acc := Acc}}}, _) ->
    FormattedAcc = format_acc(Acc),
    Msg2 = maps:merge(FormattedAcc, maps:remove(acc, Msg)),
    Event#{msg => {report, Msg2}};
format_acc_filter(Event, _) ->
    Event.

%% Encodes exml_packet
format_packet_filter(Event=#{msg := {report, Msg=#{exml_packet := Packet}}}, _) ->
    BinPacket = exml_packet:to_binary(Packet),
    Msg2 = maps:put(packet, BinPacket, maps:remove(exml_packet, Msg)),
    Event#{msg => {report, Msg2}};
format_packet_filter(Event, _) ->
    Event.

format_stacktrace_filter(Event=#{msg := {report, Msg=#{stacktrace := S}}}, _) ->
    Event#{msg => {report, Msg#{stacktrace => format_stacktrace(S)} }};
format_stacktrace_filter(Event, _) ->
    Event.

format_acc(#{origin_pid := OriginPid, timestamp := TS, stanza := StanzaMap}) ->
    Map = format_stanza_map(StanzaMap),
    Map#{acc_timestamp => format_microseconds(TS),
         origin_pid => format_term(OriginPid)};
format_acc(_) ->
    #{}.

format_stanza_map(#{element := Elem, from_jid := From, to_jid := To}) ->
    #{packet => exml:to_binary(Elem),
      from_jid => jid:to_binary(From),
      to_jid => jid:to_binary(To)};
format_stanza_map(_) ->
    #{}.

preserve_acc_filter(Event=#{msg := {report, Msg=#{acc := Acc}}}, _) ->
    Event#{msg => {report, Msg#{acc_original => format_term(Acc)}}};
preserve_acc_filter(Event, _) ->
    Event.


c2s_state_to_map(#state{socket = Socket, streamid = StreamId,
                        jid = Jid, sid = Sid}) ->
    SocketMap = ejabberd_socket:format_socket(Socket),
    SocketMap#{
      streamid => StreamId,
      jid => maybe_jid_to_binary(Jid),
      user => maybe_jid_to_luser(Jid),
      server => maybe_jid_to_lserver(Jid),
      resource => maybe_jid_to_lresource(Jid),
      session_started => maybe_sid_to_timestamp(Sid)}.

format_term(X) -> iolist_to_binary(io_lib:format("~0p", [X])).

maybe_jid_to_binary(Jid = #jid{}) -> jid:to_binary(Jid);
maybe_jid_to_binary(_) -> undefined.

maybe_jid_to_luser(#jid{luser = LUser}) -> LUser;
maybe_jid_to_luser(_) -> undefined.

maybe_jid_to_lserver(#jid{lserver = LServer}) -> LServer;
maybe_jid_to_lserver(_) -> undefined.

maybe_jid_to_lresource(#jid{lresource = LResource}) -> LResource;
maybe_jid_to_lresource(_) -> undefined.

maybe_sid_to_timestamp({Timestamp, _Pid}) -> format_term(Timestamp); % TODO format as timestamp
maybe_sid_to_timestamp(_) -> undefined.

format_microseconds(N) ->
    calendar:system_time_to_rfc3339(N, [{unit, microsecond},
                                        {offset, 0},
                                        {time_designator, $T}]).


format_stacktrace(Stacktrace) ->
    iolist_to_binary(do_format_stacktrace(Stacktrace)).

do_format_stacktrace([{Mod,Fun,Arity,Info}|T]) ->
    Line = proplists:get_value(line, Info, 0),
    H = io_lib:format("~p:~p/~p:~p", [Mod, Fun, Arity, Line]),
    more_format_stacktrace(H, T);
do_format_stacktrace([Other|T]) ->
    H = io_lib:format("~p", [Other]),
    more_format_stacktrace(H, T);
do_format_stacktrace([]) ->
    [].

more_format_stacktrace(H, []) -> [H];
more_format_stacktrace(H, T) ->
    case T of
        [] -> [H];
        _ -> [H, " "|do_format_stacktrace(T)]
    end.
