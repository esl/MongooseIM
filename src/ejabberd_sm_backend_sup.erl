%%%-------------------------------------------------------------------
%%% @author Konrad Kaplita
%%% @copyright (C) 2011, Konrad Kaplita
%%% @doc
%%%
%%% @end
%%% Created : 18 Nov 2011 by Konrad Kaplita
%%%-------------------------------------------------------------------
-module(ejabberd_sm_backend_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-ignore_xref([start_link/0]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> {ok, Pid :: pid()} | ignore | {error, Reason :: term()}.
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2, 3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%% @end
%%--------------------------------------------------------------------
-spec init([]) -> {ok, {{one_for_one, 1000, 3600}, []}}.
init([]) ->
    {ok, {{one_for_one, 1000, 3600}, []}}.
