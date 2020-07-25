-module(mongoose_logs).

-export([set_global_loglevel/1]).
-export([get_global_loglevel/0]).
-export([set_module_loglevel/2]).
-export([clear_module_loglevel/1]).
-export([get_log_files/0]).
-export([dir/0]).
-export([loglevel_number_keyword/1]).

-type level() :: none | logger:level() | all | -1..8.

-spec get_global_loglevel() -> logger:level().
get_global_loglevel() ->
    maps:get(level, logger:get_primary_config()).

-spec set_global_loglevel(level()) ->
    ok | {error, {invalid_level, term()}}.
set_global_loglevel(Level) when is_integer(Level) ->
    set_global_loglevel(loglevel_number_keyword(Level));
set_global_loglevel(Level) ->
    logger:update_primary_config(#{level => Level}).

-spec set_module_loglevel(module(), level()) ->
    ok | {error, term()}.
set_module_loglevel(Module, Level) when is_integer(Level) ->
    set_module_loglevel(Module, loglevel_number_keyword(Level));
set_module_loglevel(Module, Level) ->
    logger:set_module_level(Module, Level).

-spec clear_module_loglevel(module()) -> ok | {error, term()}.
clear_module_loglevel(Module) ->
    set_module_loglevel(Module, get_global_loglevel()).

-spec get_log_files() -> [filename:name()].
get_log_files() ->
    [ File || #{config := #{file := File}} <- logger:get_handler_config() ].

-spec dir() -> string().
dir() ->
    case logger:get_handler_config(disk_log) of
        {ok, #{config := #{file := Path}}} ->
            filename:dirname(Path);
        _ ->
            ""
    end.

-spec loglevel_number_keyword(-1..8) -> none | logger:level();
                             (none | logger:level()) -> -1..8.
loglevel_number_keyword(-1) -> none;
loglevel_number_keyword(0) -> emergency;
loglevel_number_keyword(1) -> alert;
loglevel_number_keyword(2) -> critical;
loglevel_number_keyword(3) -> error;
loglevel_number_keyword(4) -> warning;
loglevel_number_keyword(5) -> notice;
loglevel_number_keyword(6) -> info;
loglevel_number_keyword(7) -> debug;
loglevel_number_keyword(8) -> all;
loglevel_number_keyword(none)      -> -1;
loglevel_number_keyword(emergency) -> 0;
loglevel_number_keyword(alert)     -> 1;
loglevel_number_keyword(critical)  -> 2;
loglevel_number_keyword(error)     -> 3;
loglevel_number_keyword(warning)   -> 4;
loglevel_number_keyword(notice)    -> 5;
loglevel_number_keyword(info)      -> 6;
loglevel_number_keyword(debug)     -> 7;
loglevel_number_keyword(all)       -> 8.
