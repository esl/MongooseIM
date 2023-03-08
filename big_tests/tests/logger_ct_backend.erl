-module(logger_ct_backend).

% API for tests
-export([start/0,
         start/1,
         stop/0,
         stop/1,
         capture/1,
         capture/2,
         stop_capture/0,
         stop_capture/1,
         recv/1]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, terminate/2]).

% Logger handler callback
-export([adding_handler/1, removing_handler/1, log/2]).

-import(mongoose_helper, [successful_rpc/3,
                          successful_rpc/4]).

-import(distributed_helper, [mim/0]).

-type state() :: #{
        receivers := [{pid(), logger:level()}]
       }.

-type filter_fun() :: fun((logger:level(), binary()) -> boolean()).

%% ------------------------------------------------------------
%% API for tests
%% ------------------------------------------------------------
start() ->
    start(mim()).

-spec start(Node :: distributed_helper:rpc_spec()) -> term().
start(Node) ->
    mongoose_helper:inject_module(Node, ?MODULE, reload),
    successful_rpc(Node, logger, add_handler, [?MODULE, ?MODULE, #{}]).

stop() ->
    stop(mim()).

-spec stop(Node :: distributed_helper:rpc_spec()) -> term().
stop(Node) ->
    successful_rpc(Node, logger, remove_handler, [?MODULE]).

-spec capture(Level :: logger:level()) -> term().
capture(Level) ->
    capture(Level, mim()).

-spec capture(Level :: logger:level(), Node :: distributed_helper:rpc_spec()) -> term().
capture(Level, Node) ->
    successful_rpc(Node, gen_server, call, [?MODULE, {capture, self(), Level}]).

stop_capture() ->
    stop_capture(mim()).

-spec stop_capture(Node :: distributed_helper:rpc_spec()) -> term().
stop_capture(Node) ->
    successful_rpc(Node, gen_server, call, [?MODULE, {stop_capture, self()}]).

-spec recv(filter_fun()) -> ReceivedLogs :: [binary()].
recv(FilterFun) ->
    recv(FilterFun, []).

%% ------------------------------------------------------------
%% logger handler callback
%% ------------------------------------------------------------
adding_handler(Config) ->
    {ok, Pid} = gen_server:start({local, ?MODULE}, ?MODULE, Config, []),
    {ok, Config#{config => Config#{pid => Pid}}}.

removing_handler(#{config := #{pid := Pid}}) ->
    gen_server:stop(Pid).

log(LogEvent, #{config := #{pid := Pid}} = Config) ->
    gen_server:cast(Pid, {log, LogEvent, Config}).

%% ------------------------------------------------------------
%% gen_server callbacks
%% ------------------------------------------------------------
-spec init(any()) -> {ok, state()}.
init(_) ->
    {ok, #{receivers => []}}.

handle_call({capture, Pid, Level}, _, #{receivers := Receivers} = State) ->
    NReceivers = lists:keystore(Pid, 1, Receivers, {Pid, Level}),
    {reply, ok, State#{receivers := NReceivers}};
handle_call({stop_capture, Pid}, _, #{receivers := Receivers} = State) ->
    NReceivers = lists:keydelete(Pid, 1, Receivers),
    {reply, ok, State#{receivers := NReceivers}}.

handle_cast({log, #{level := Severity} = LogEvent, #{formatter := {FModule, FConfig}}},
            #{receivers := Receivers} = State) ->
    Msg = lists:flatten(FModule:format(LogEvent, FConfig)),
    lists:foreach(
      fun({Pid, Level}) when Level == Severity ->
              Pid ! {captured_log, Severity, Msg};
         ({_Pid, _Level}) ->
              ok
      end, Receivers),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

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
            end
    after 0 -> []
    end.
