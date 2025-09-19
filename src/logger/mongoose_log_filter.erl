-module(mongoose_log_filter).
-export([fill_metadata_filter/2]).
-export([format_c2s_state_filter/2]).
-export([format_acc_filter/2]).
-export([format_packet_filter/2]).
-export([format_stacktrace_filter/2]).
-export([format_term_filter/2]).
-export([preserve_acc_filter/2]).
-export([remove_fields_filter/2]).
-export([filter_module/2]).

-ignore_xref([filter_module/2, format_acc_filter/2, format_c2s_state_filter/2,
              format_packet_filter/2, format_stacktrace_filter/2, format_term_filter/2,
              preserve_acc_filter/2, remove_fields_filter/2]).

-include("mongoose.hrl").
-include_lib("jid/include/jid.hrl").

%% The templater in flatlog works with meta fields.
%% So, we would need a filter, that takes the interesting fields
%% from msg to metadata.
-spec fill_metadata_filter(logger:log_event(), term()) -> logger:filter_return().
fill_metadata_filter(Event=#{msg := {report, Msg}, meta := Meta}, Fields) ->
    FieldMap = maps:with(Fields, Msg),
    %% Remove the fields to not print them twice
    Msg2 = maps:without(Fields, Msg),
    Event#{meta => maps:merge(FieldMap, Meta), msg => {report, Msg2}};
fill_metadata_filter(Event, _) ->
    Event.

format_c2s_state_filter(Event=#{msg := {report, Msg=#{c2s_data := State}}}, _) ->
    do_format_c2s_state(Event, Msg, State, c2s_data);
%% Backward compatibility for logs that incorrectly use c2s_state key for StateData
format_c2s_state_filter(Event=#{msg := {report, Msg=#{c2s_state := State}}}, _) ->
    try
        do_format_c2s_state(Event, Msg, State, c2s_state)
    catch
        _:_ ->
            %% Not a c2s_data record, leave as is
            Event
    end;
format_c2s_state_filter(Event, _) ->
    Event.

%% Helper function to format c2s state data
do_format_c2s_state(Event, Msg, State, KeyToRemove) ->
    StateMap = filter_undefined(c2s_data_to_map(State)),
    %% C2S fields have lower priority, if the field is already present in msg.
    Msg2 = maps:merge(StateMap, maps:remove(KeyToRemove, Msg)),
    Event#{msg => {report, Msg2}}.

format_acc_filter(Event=#{msg := {report, Msg=#{acc := Acc}}}, _) ->
    FormattedAcc = format_acc(Acc),
    Msg2 = maps:merge(FormattedAcc, maps:remove(acc, Msg)),
    Event#{msg => {report, Msg2}};
format_acc_filter(Event, _) ->
    Event.

%% Encodes exml_packet
format_packet_filter(Event=#{msg := {report, Msg=#{exml_packet := Packet}}}, _) ->
    BinPacket = exml:to_binary(Packet),
    Msg2 = maps:put(packet, BinPacket, maps:remove(exml_packet, Msg)),
    Event#{msg => {report, Msg2}};
format_packet_filter(Event, _) ->
    Event.

format_stacktrace_filter(Event=#{msg := {report, Msg=#{stacktrace := S}}}, _) ->
    FmtArgs = format_stacktrace_args(S),
    Msg2 = case FmtArgs of
               <<>> -> Msg;
               _ -> Msg#{stacktrace_args => FmtArgs}
           end,
    Msg3 = case format_stacktrace(S) of
               <<>> -> Msg2;
               FmtStack -> Msg2#{stacktrace => FmtStack}
           end,
    Event#{msg => {report, Msg3 }};
format_stacktrace_filter(Event, _) ->
    Event.

format_term_filter(Event = #{msg := {report, Msg}}, Keys) ->
    FormattedMsg = lists:foldl(fun format_value/2, Msg, Keys),
    Event#{msg => {report, FormattedMsg}};
format_term_filter(Event, _) ->
    Event.

format_value(Key, Msg) ->
    case maps:find(Key, Msg) of
        {ok, Value} -> Msg#{Key := format_term(Value)};
        error -> Msg
    end.

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

remove_fields_filter(Event=#{msg := {report, Msg=#{}}}, FieldNames) ->
    Msg2 = maps:without(FieldNames, Msg),
    Event#{msg => {report, Msg2}};
remove_fields_filter(Event, _) ->
    Event.


c2s_data_to_map(State) ->
    SocketMap = format_socket(mongoose_c2s:get_socket(State)),
    Jid = mongoose_c2s:get_jid(State),
    SocketMap#{
      streamid => mongoose_c2s:get_stream_id(State),
      jid => maybe_jid_to_binary(Jid),
      user => maybe_jid_to_luser(Jid),
      server => maybe_jid_to_lserver(Jid),
      resource => maybe_jid_to_lresource(Jid),
      session_started => maybe_sid_to_timestamp(mongoose_c2s:get_sid(State))}.

format_term(X) -> iolist_to_binary(io_lib:format("~0p", [X])).

maybe_jid_to_binary(Jid = #jid{}) -> jid:to_binary(Jid);
maybe_jid_to_binary(_) -> undefined.

maybe_jid_to_luser(#jid{luser = LUser}) -> LUser;
maybe_jid_to_luser(_) -> undefined.

maybe_jid_to_lserver(#jid{lserver = LServer}) -> LServer;
maybe_jid_to_lserver(_) -> undefined.

maybe_jid_to_lresource(#jid{lresource = LResource}) -> LResource;
maybe_jid_to_lresource(_) -> undefined.

maybe_sid_to_timestamp({Timestamp, _Pid}) -> format_microseconds(Timestamp).

format_microseconds(N) ->
    calendar:system_time_to_rfc3339(N, [{unit, microsecond},
                                        {offset, 0},
                                        {time_designator, $T}]).

format_socket(undefined) ->
    #{};
format_socket(Socket) ->
    DestAddress = mongoose_xmpp_socket:get_ip(Socket),
    #{
        transport => mongoose_xmpp_socket:get_transport(Socket),
        conn_type => mongoose_xmpp_socket:get_conn_type(Socket),
        dest_address => format_address(DestAddress)
    }.

format_address({Address, Port}) ->
    #{
        address => inet:ntoa(Address),
        port => Port
    }.

format_stacktrace_args([{_Mod,_Fun,Args,_Info}|_]) when is_list(Args) ->
    iolist_to_binary(io_lib:format("~p", [Args]));
format_stacktrace_args(_) ->
    <<>>.

format_stacktrace(Stacktrace) ->
    iolist_to_binary(do_format_stacktrace(Stacktrace)).

do_format_stacktrace([{Mod,Fun,Args,Info}|T]) when is_list(Args) ->
    Arity = length(Args),
    do_format_stacktrace([{Mod,Fun,Arity,Info}|T]);
do_format_stacktrace([{Mod,Fun,Arity,Info}|T]) ->
    Line = proplists:get_value(line, Info, 0),
    H = io_lib:format("~p:~p/~p:~p", [Mod, Fun, Arity, Line]),
    more_format_stacktrace(H, T);
do_format_stacktrace([Other|T]) ->
    H = io_lib:format("~p", [Other]),
    more_format_stacktrace(H, T);
do_format_stacktrace([]) ->
    [].

more_format_stacktrace(H, []) ->
    [H];
more_format_stacktrace(H, T) ->
    [H, " "|do_format_stacktrace(T)].

filter_undefined(Map) ->
    maps:filter(fun(_, V) -> V =/= undefined end, Map).

filter_module(Event = #{meta := #{mfa := {M,_,_}}}, Modules) when is_list(Modules) ->
    case lists:member(M, Modules) of
        true ->
            Event;
        false ->
            stop
    end;
filter_module(_Event, _Modules) ->
    stop. %% module unknown, drop
