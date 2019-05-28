%%%-------------------------------------------------------------------
%%% @author denys.gonchar@erlang-solutions.com
%%% @copyright (C) 2019, Erlang Solutions
%%% @doc
%%%
%%% @end
%%% Created : 28. May 2019 17:23
%%%-------------------------------------------------------------------
-module(service_cache).

-behaviour(mongoose_service).

%% API
-export([start/1, stop/0]).
-export([lookup/2]).

-type lookup_result() :: {ok, Value :: term()} | error.
-type lookup_fn() :: fun(()-> lookup_result()).

-export_type([lookup_fn/0, lookup_result/0]).

start(Opts) -> cache_tab:new(?MODULE, Opts).
stop() -> cache_tab:delete(?MODULE).


-spec lookup(Key :: term(), lookup_fn()) -> lookup_result().
lookup(Key, Fun) ->
    case mongoose_service:is_loaded(?MODULE) of
        true ->
            cache_tab:lookup(?MODULE, Key, Fun);
        false ->
            Fun()
    end.