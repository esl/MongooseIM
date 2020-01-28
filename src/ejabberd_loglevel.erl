%%%----------------------------------------------------------------------
%%% File    : ejabberd_loglevel.erl
%%% Author  : Piotr Nosek <piotr.nosek@erlang-solutions.com>
%%% Purpose : Loglevel switcher.
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2012   ProcessOne, Erlang Solutions Ltd.
%%%
%%% This program is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation; either version 2 of the
%%% License, or (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with this program; if not, write to the Free Software
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
%%% 02111-1307 USA
%%%
%%%----------------------------------------------------------------------

-module(ejabberd_loglevel).
-author('mongoose-im@erlang-solutions.com').

-export([init/0,
         set/1,
         get/0,
         set_custom/2,
         clear_custom/0, clear_custom/1]).

-export([get_log_files/0]).
-export([dir/0]).

-include("mongoose.hrl").

-export_type([loglevel/0]).
-type loglevel() :: none | critical | error | warning | info | debug.

-define(LOG_LEVELS,
    [{0, none},
     {1, critical},
     {2, error},
     {3, warning},
     {4, info},
     {5, debug}]).

-define(ETS_TRACE_TAB, ejabberd_lager_traces).

-spec init() -> atom() | ets:tid().
init() ->
    lager:start(),
    ets:new(?ETS_TRACE_TAB, [bag, named_table, public]).

-spec get() -> [{{atom(), term()} | atom(), {non_neg_integer(), loglevel()}}].
get() ->
    Backends = [Backend || Sink <- lager:list_all_sinks(), Backend <- gen_event:which_handlers(Sink)],
    [{Backend, lists:keyfind(lager:get_loglevel(Backend), 2, ?LOG_LEVELS)}
     || Backend <- Backends, Backend /= lager_backend_throttle].

-spec set(loglevel() | integer()) -> [Result] when
    Result :: {LagerBackend, ok | {error, Reason}},
    %% Yes, these are two different errors!
    Reason :: bad_log_level | bad_loglevel,
    LagerBackend :: lager_console_backend | {lager_file_backend, Path},
    Path :: string().
set(Level) when is_integer(Level) ->
    {_, Name} = lists:keyfind(Level, 1, ?LOG_LEVELS),
    set(Name);
set(Level) ->
    Backends = [Backend || Sink <- lager:list_all_sinks(), Backend <- gen_event:which_handlers(Sink)],
    Files = [{B, lager:set_loglevel(lager_file_backend, File, Level)}
             || B = {lager_file_backend, File} <- Backends],
    Consoles = [{B, lager:set_loglevel(lager_console_backend, Level)}
                || B = lager_console_backend <- Backends],
    Files ++ Consoles.

-spec set_custom(Module :: atom(), loglevel() | integer()) -> [Result] when
    Result :: {lager_console_backend | {lager_file_backend, string()},
               ok | {error, any()}}.
set_custom(Module, Level) when is_integer(Level) ->
    {_, Name} = lists:keyfind(Level, 1, ?LOG_LEVELS),
    set_custom(Module, Name);
set_custom(Module, Level) when is_atom(Level) ->
    clear_custom(Module),
    Backends = [Backend || Sink <- lager:list_all_sinks(), Backend <- gen_event:which_handlers(Sink)],
    [{Backend, set_trace(Backend, Module, Level)}
     || Backend <- Backends,
     Backend /= lager_backend_throttle].

set_trace(Backend, Module, Level) ->
    case lager:trace(Backend, [{module, Module}], Level) of
        {error, _} = E -> E;
        {ok, Trace} ->
            true = ets:insert(?ETS_TRACE_TAB, {Module, Trace}),
            ok
    end.

-spec clear_custom() -> ok.
clear_custom() ->
    ets:safe_fixtable(?ETS_TRACE_TAB, true),
    ets:foldl(fun clear_trace/2, ok, ?ETS_TRACE_TAB),
    ets:delete_all_objects(?ETS_TRACE_TAB),
    ets:safe_fixtable(?ETS_TRACE_TAB, false),
    ok.

clear_trace({_Module, Trace}, ok) ->
    lager:stop_trace(Trace).

-spec clear_custom(Module :: atom()) -> ok.
clear_custom(Module) when is_atom(Module) ->
    case ets:lookup(?ETS_TRACE_TAB, Module) of
        [] -> ok;
        [_ | _] = Traces ->
            ets:delete(?ETS_TRACE_TAB, Module),
            [lager:stop_trace(Trace) || {_, Trace} <- Traces],
            ok
    end.

get_log_files() ->
    [lager_util:expand_path(File) || {{lager_file_backend, File}, _, _} <- lager_config:global_get(handlers)].

-spec dir() -> string().
dir() ->
    case application:get_env(lager, log_root) of
        {ok, LogDir} -> LogDir;
        _ -> ""
    end.

