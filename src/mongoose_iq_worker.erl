
-module(mongoose_iq_worker).
-behaviour(gen_server).

%% API
-export([start/0, start_link/0, process_iq/6, stop/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2]).

-ignore_xref([start_link/0]).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------
-spec start() -> {ok,pid()}.
start() ->
    supervisor:start_child(ejabberd_iq_sup, []).

-spec start_link() ->
    'ignore' | {'error', _} | {'ok', pid()}.
start_link() ->
    gen_server:start_link(?MODULE, ok, []).

-spec process_iq(Pid :: pid(),
                 IQHandler :: mongoose_iq_handler:t(),
                 Acc :: mongoose_acc:t(),
                 From :: jid:jid(),
                 To :: jid:jid(),
                 IQ :: jlib:iq()) -> ok.
process_iq(Pid, IQHandler, Acc, From, To, IQ) ->
    gen_server:cast(Pid, {process_iq, IQHandler, Acc, From, To, IQ}).

-spec stop(Pid :: pid()) -> ok.
stop(Pid) ->
    gen_server:cast(Pid, stop).
%%--------------------------------------------------------------------
%% gen_server callbacks
%%--------------------------------------------------------------------

%% @doc Initiates the server
-spec init(_) -> {ok, ok}.
init(_) ->
    %% no need for a state
    {ok, ok}.

handle_call(stop, _From, State) ->
    Reply = ok,
    {stop, normal, Reply, State};
handle_call(_Msg, _From, State) ->
    {reply, not_implemented, State}.

handle_cast({process_iq, IQHandler, Acc, From, To, IQ}, State) ->
    mongoose_iq_handler:execute_handler(IQHandler, Acc, From, To, IQ),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.
