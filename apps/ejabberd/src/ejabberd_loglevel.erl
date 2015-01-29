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
-author('piotr.nosek@erlang-solutions.com').

-export([init/0,
         set/1,
         get/0,
         set_custom/2,
         clear_custom/0,
         clear_custom/1
         ]).

-include("ejabberd.hrl").

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

%% @private
-spec log_path() -> string().
log_path() ->
    ejabberd_app:get_log_path().

-spec init() -> atom() | ets:tid().
init() ->
    lager:start(),
    ets:new(?ETS_TRACE_TAB, [set, named_table, public]).

-spec get() -> {integer(), loglevel()}.
get() ->
    Backends = gen_event:which_handlers(lager_event),
    LevelName = (catch [ throw(lager:get_loglevel(Backend))
                         || Backend <- Backends,
                            Backend /= lager_backend_throttle ]),
    lists:keyfind(LevelName, 2, ?LOG_LEVELS).

-spec set(loglevel() | integer()) -> any().
set(Level) when is_integer(Level) ->
    {_, Name} = lists:keyfind(Level, 1, ?LOG_LEVELS),
    set(Name);
set(Level) ->
    Backends = gen_event:which_handlers(lager_event),
    Files = [ lager:set_loglevel(lager_file_backend, File, Level)
              || {lager_file_backend, File} <- Backends ],
    Consoles = [ lager:set_loglevel(lager_console_backend, Level)
                 || lager_console_backend <- Backends ],
    %% Should just flatten to a single 'ok'.
    lists:usort(Files ++ Consoles).

-spec set_custom(Module :: atom(), loglevel() | integer()) -> 'true'.
set_custom(Module, Level) when is_integer(Level) ->
    {_, Name} = lists:keyfind(Level, 1, ?LOG_LEVELS),
    set_custom(Module, Name);
set_custom(Module, Level) when is_atom(Level) ->
    clear_custom(Module),
    Path = log_path(),
    {ok, ConsoleTrace} = lager:trace_console([{module, Module}], Level),
    {ok, FileTrace}  = lager:trace_file(Path, [{module, Module}], Level),
    ets:insert(?ETS_TRACE_TAB, {Module, ConsoleTrace, FileTrace}).

-spec clear_custom() -> 'ok' | 'true'.
clear_custom() ->
    clear_custom('_').

-spec clear_custom(Module :: atom()) -> 'ok' | 'true'.
clear_custom(Module) when is_atom(Module) ->
    case ets:lookup(?ETS_TRACE_TAB, Module) of
        [{_, ConsoleTrace, FileTrace}] ->
            lager:stop_trace(ConsoleTrace),
            lager:stop_trace(FileTrace),
            ets:delete(?ETS_TRACE_TAB, Module);
        [] ->
            ok
    end.
