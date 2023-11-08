%%%===================================================================
%%% @copyright (C) 2016, Erlang Solutions Ltd.
%%% @doc Module providing support for websockets in MongooseIM
%%% @end
%%%===================================================================
-module(mongoose_traffic_channel).

-behaviour(cowboy_websocket).

%% cowboy_http_websocket_handler callbacks
-export([init/2,
         websocket_init/1,
         websocket_handle/2,
         websocket_info/2,
         terminate/3]).

-include("mongoose.hrl").
-include("jlib.hrl").

-define(LISTENER, ?MODULE).
-define(MAX_ITEMS, 500).
-define(MAX_TRACED, 100).

-type str_pid() :: binary(). % representation of traced user's session pid
-type str_stanza() :: binary().

-record(state, {traces = #{},
                tracing = false,
                current = <<>>,
                mappings = #{},
                start_times = #{}}).

-type state() :: #state{traces :: #{str_pid() => queue:queue()},
                        tracing :: boolean(),
                        current :: str_pid(),
                        mappings :: #{str_pid() => jid:jid()},
                        start_times :: #{str_pid() => float()}}.

%%--------------------------------------------------------------------
%% Common callbacks for all cowboy behaviours
%%--------------------------------------------------------------------

-spec init(cowboy_req:req(), proplists:proplist()) ->
    {cowboy_websocket, cowboy_req:req(), proplists:proplist(), map()}.
init(Req, Opts) ->
    Peer = cowboy_req:peer(Req),
    PeerCert = cowboy_req:cert(Req),
    ?DEBUG("cowboy init: ~p~n", [{Req, Opts}]),
    AllModOpts = [{peer, Peer}, {peercert, PeerCert} | Opts],
    %% upgrade protocol
    {cowboy_websocket, Req, AllModOpts, #{}}.

terminate(_Reason, _Req, _State) ->
    ok.

%%--------------------------------------------------------------------
%% cowboy_http_websocket_handler callbacks
%%--------------------------------------------------------------------

% Called for every new websocket connection.
websocket_init(Opts) ->
    ?DEBUG("websocket_init: ~p~n", [Opts]),
    gen_server:call(mongoose_traffic, {register, self()}),
    {ok, #state{}}.

% Called when a text message arrives.
websocket_handle({text, Msg}, State) ->
    case handle(jiffy:decode(Msg), State) of
        {Event, State1} ->
            {reply, reply(Event), State1};
        {Event, Payload, State1} ->
            {reply, reply(Event, Payload), State1}
    end;

websocket_handle({binary, Msg}, State) ->
    ?DEBUG("Received binary: ~p", [Msg]),
    {ok, State};

% With this callback we can handle other kind of
% messages, like binary.
websocket_handle(Any, State) ->
    ?DEBUG("Received non-text: ~p", [Any]),
    {ok, State}.

% Other messages from the system are handled here.
-spec websocket_info({message,
                      mongoose_debug:direction(),
                      pid(),
                      jid:jid(),
                      str_stanza()},
                     state()) ->
    {ok | stop, state()}.
websocket_info({message, _Dir, _P, _J, _Stanza}, #state{tracing = false} = State) ->
    {ok, State};
websocket_info({message, Dir, Pid, Jid, Stanza} = Message, State) ->
    Spid = pid_to_binary(Pid),
    Now = now_seconds(),
    {Traces1, Mappings, IsNewMapping} = record_item(Now,
                                                    Dir,
                                                    Spid, Jid,
                                                    Stanza,
                                                    State#state.traces,
                                                    State#state.mappings),
    State1 = State#state{traces = Traces1},
    State2 = State1#state{mappings = Mappings},
    State3 = maybe_store_start_time(Spid, Now, State2),
    case maps:size(Traces1) of
        N when N > ?MAX_TRACED ->
            force_stop_tracing(State1);
        _ ->
            maybe_send_to_user(Now, IsNewMapping, Message, State3)
    end;
websocket_info(stop, State) ->
    {stop, State};
websocket_info(Info, State) ->
    ?DEBUG("unknown info: ~p", [Info]),
    {ok, State}.

force_stop_tracing(State) ->
    State1 = State#state{tracing = false},
    M = reply(<<"error">>, #{<<"reason">> => <<"too_many_traced_procs">>}),
    {reply, M, State1}.

maybe_send_to_user(Now, IsNewMapping, {message, Dir, Pid, Jid, Stanza}, State) ->
    Spid = pid_to_binary(Pid),
    Announcement = maybe_announce_new(IsNewMapping, Now, Spid, Jid),
    Msg = maybe_send_current(Now, Dir, Spid, Stanza, State),
    {reply, Announcement ++ Msg, State}.

maybe_announce_new(true, StartTime, Spid, Jid) ->
    {BareJid, FullJid} = case classify_jid(Jid) of
                             empty -> {<<>>, <<>>};
                             bare -> {<<>>, jid:to_bare_binary(Jid)};
                             full -> {jid:to_binary(Jid), <<>>}
                         end,
    [reply(<<"new_trace">>,
           #{<<"pid">> => Spid,
             <<"start_time">> => StartTime,
             <<"bare_jid">> => BareJid,
             <<"full_jid">> => FullJid})];
maybe_announce_new(false, _, _, _) ->
    [].

maybe_send_current(Now, Dir, Spid, Stanza, State) ->
    case is_current(Spid, State) of
        true ->
            Tm = Now - maps:get(Spid, State#state.start_times),
            M = reply(<<"message">>, #{<<"dir">> => atom_to_binary(Dir, utf8),
                                       <<"time">> => Tm,
                                       <<"stanza">> => Stanza
                  }),
            [M];
        false ->
            []
    end.


handle({Json}, State) ->
    M = maps:from_list(Json),
    handle(maps:get(<<"event">>, M), maps:get(<<"payload">>, M), State).

handle(<<"get_status">>, _, State) ->
    return_status(State);
handle(<<"trace_flag">>, {Payload}, State) ->
     #{<<"value">> := Flag} = maps:from_list(Payload),
     return_status(State#state{tracing = Flag});
handle(<<"get_trace">>, {Payload}, State) ->
    #{<<"pid">> := Pid} = maps:from_list(Payload),
    {<<"get_trace">>,
     #{<<"pid">> => Pid, <<"trace">> => format_trace(maps:get(Pid, State#state.traces, []),
                                                     maps:get(Pid, State#state.start_times))},
     State#state{current = Pid}};
handle(<<"clear_all">>, _, State) ->
    {<<"cleared_all">>,
     State#state{traces = #{}, current = <<>>, mappings = #{}, start_times = #{}}};
handle(<<"heartbeat">>, _, State) ->
    {<<"heartbeat_ok">>,
     <<>>,
     State};
handle(Event, Payload, State) ->
    ?LOG_WARNING(#{what => unknown_event,
                text => <<"Traffic monitor sent something I don't understand">>,
                event => Event, payload => Payload}),
    {<<"error">>, <<"unknown event">>, State}.


return_status(State) ->
    {<<"status">>,
     #{<<"trace_flag">> => State#state.tracing},
     State}.

reply(Event) ->
    reply(Event, #{}).

reply(Event, Payload) ->
    {text, jiffy:encode(#{<<"event">> => Event, <<"payload">> => Payload})}.



%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec is_current(str_pid(), state()) -> boolean().
is_current(J, #state{current = J}) -> true;
is_current(_, _)                   -> false.

record_item(Time, Dir, Spid, Jid, Stanza, Traces, Mappings) ->
    Tr = case maps:get(Spid, Traces, undefined) of
             undefined ->
                 queue:new();
             Q -> Q
         end,
    IsNew = is_new_mapping(maps:get(Spid, Mappings, undefined), Jid),
    Mappings1 = case IsNew of
                    true ->
                        maps:put(Spid, Jid, Mappings);
                    false ->
                        Mappings
                end,
    Tr1 = queue:in({Time, Dir, Stanza}, Tr),
    Tr2 = case queue:len(Tr1) of
              ?MAX_ITEMS -> queue:out(Tr1);
              _ -> Tr1
          end,
    {maps:put(Spid, Tr2, Traces), Mappings1, IsNew}.

format_trace([], _StartTime) ->
    [];
format_trace(Trace, StartTime) ->
    lists:map(fun({Time, Dir, Stanza}) ->
                  #{<<"dir">> => atom_to_binary(Dir, utf8),
                    <<"time">> => Time - StartTime,
                    <<"stanza">> => Stanza}
              end,
              lists:reverse(queue:to_list(Trace))).

-spec pid_to_binary(pid()) -> str_pid().
pid_to_binary(Pid) when is_pid(Pid) ->
    [Spid] = io_lib:format("~p", [Pid]),
    list_to_binary(Spid).

-spec classify_jid(binary() | jid:jid()) -> empty | bare | full.
classify_jid(Bin) when is_binary(Bin) -> classify_jid(jid:from_binary(Bin));
classify_jid(undefined) -> empty;
classify_jid(#jid{lresource = <<>>}) -> bare;
classify_jid(#jid{}) -> full.

% we map pids to jids, initially we don't know the jid, then we
% know bare jid, and then full jid
% we need to know when it changes so that send an update to client
-spec is_new_mapping(jid:jid(), jid:jid()) -> boolean().
is_new_mapping(Old, New) ->
    case {classify_jid(Old), classify_jid(New)} of
        {S, S} -> false;
        _ -> true
    end.

now_seconds() ->
    os:system_time(microsecond) / 1000000.

maybe_store_start_time(Spid, Time, #state{start_times = StartTimes} = State) ->
    case maps:get(Spid, StartTimes, undefined) of
        undefined ->
            State#state{start_times = maps:put(Spid, Time, StartTimes)};
        _ ->
            State
    end.
