%% @doc Heir for ETS tables created at run time.
-module(mongoose_ets_heir).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-ignore_xref([start_link/0]).

-include("mongoose_logger.hrl").

-spec start_link() -> gen_server:start_ret().
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, noargs, []).

-spec init(noargs) -> {ok, #{}}.
init(noargs) ->
    {ok, #{}}.

-spec handle_call(term(), gen_server:from(), #{}) -> {reply, {error, unexpected_call}, #{}}.
handle_call(Request, From, State) ->
    ?UNEXPECTED_CALL(Request, From),
    {reply, {error, unexpected_call}, State}.

-spec handle_cast(term(), #{}) -> {noreply, #{}}.
handle_cast(Msg, State) ->
    ?UNEXPECTED_CAST(Msg),
    {noreply, State}.

-spec handle_info(term(), #{}) -> {noreply, #{}}.
handle_info({'ETS-TRANSFER', _Table, _FromPid, _HeirData}, State) ->
    {noreply, State};
handle_info(Info, State) ->
    ?UNEXPECTED_INFO(Info),
    {noreply, State}.
