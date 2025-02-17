%%%-------------------------------------------------------------------
%%% @author Konrad Kaplita
%%% @copyright (C) 2011, Konrad Kaplita
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

-spec start_link() -> supervisor:startlink_ret().
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, noargs).

%% @private
-spec init(noargs) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init(noargs) ->
    {ok, {{one_for_one, 1000, 3600}, []}}.
