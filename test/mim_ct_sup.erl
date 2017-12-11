-module(mim_ct_sup).

-behaviour(supervisor).

-export([start_link/1, init/1]).

start_link(Name) ->
    supervisor:start_link({local, Name}, ?MODULE, []).

init([]) ->
    {ok, {{one_for_one, 10, 1}, []}}.
