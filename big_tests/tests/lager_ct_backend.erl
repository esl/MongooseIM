-module(lager_ct_backend).

-behaviour(gen_event).

% API for tests
-export([start/0, stop/0]).
-export([capture/1, stop_capture/0]).
-export([recv/1]).

% gen_event callbacks
-export([init/1, handle_call/2, handle_event/2, handle_info/2, terminate/2, code_change/3]).

-import(mongoose_helper, [successful_rpc/3]).

-type state() :: #{
        receivers := [{pid(), lager:log_level()}]
       }.

-type filter_fun() :: fun((lager:log_level(), binary()) -> boolean()).

%% ------------------------------------------------------------
%% API for tests
%% ------------------------------------------------------------

start() ->
    mongoose_helper:inject_module(?MODULE, no_reload),
    successful_rpc(gen_event, add_handler, [lager_event, ?MODULE, []]).

stop() ->
    successful_rpc(gen_event, delete_handler, [lager_event, ?MODULE, []]).

-spec capture(Level :: lager:log_level()) -> ok.
capture(Level) ->
    successful_rpc(gen_event, call, [lager_event, ?MODULE, {capture, self(), Level}]).

stop_capture() ->
    successful_rpc(gen_event, call, [lager_event, ?MODULE, {stop_capture, self()}]).

-spec recv(filter_fun()) -> ReceivedLogs :: [binary()].
recv(FilterFun) ->
    recv(FilterFun, []).

%% ------------------------------------------------------------
%% gen_event callbacks
%% ------------------------------------------------------------

-spec init(any()) -> {ok, state()}.
init(_) ->
    {ok, #{ receivers => [] }}.

handle_call({capture, Pid, Level}, #{ receivers := Receivers } = State) ->
    NReceivers = lists:keystore(Pid, 1, Receivers, {Pid, Level}),
    {ok, ok, State#{ receivers := NReceivers }};
handle_call({stop_capture, Pid}, #{ receivers := Receivers } = State) ->
    NReceivers = lists:keydelete(Pid, 1, Receivers),
    {ok, ok, State#{ receivers := NReceivers }}.

handle_event({log, LagerMsg}, #{ receivers := Receivers } = State) ->
    Msg = iolist_to_binary(lager_msg:message(LagerMsg)),
    Severity = lager_msg:severity(LagerMsg),
    lists:foreach(
      fun({Pid, Level}) when Level == Severity ->
              Pid ! {captured_log, Severity, Msg};
         ({_Pid, _Level}) ->
              ok
      end, Receivers),
    {ok, State};
handle_event(_Event, State) ->
    {ok, State}.

handle_info(_Info, State) ->
    {ok, State}.

terminate(_Arg, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------
%% Internal functions
%% ------------------------------------------------------------

-spec recv(FilterFun :: filter_fun(), OtherMsgs :: [term()]) -> ReceivedLogs :: [binary()].
recv(FilterFun, OtherMsgs) ->
    receive
        {captured_log, Severity, Msg} ->
            case FilterFun(Severity, Msg) of
                true ->
                    [ {Severity, Msg} | recv(FilterFun, OtherMsgs) ];
                false ->
                    recv(FilterFun, OtherMsgs)
            end;
        OtherMsg ->
            recv(FilterFun, [OtherMsg | OtherMsgs])
    after
        100 ->
            lists:foreach(fun(Msg) ->
                                  self() ! Msg
                          end, lists:reverse(OtherMsgs)),
            []
    end.

