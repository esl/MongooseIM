-module(mongoose_logs).

-export([set_global_loglevel/1]).
-export([get_global_loglevel/0]).
-export([set_module_loglevel/2]).
-export([clear_module_loglevel/1]).
-export([get_log_files/0]).
-export([dir/0]).
-export([loglevel_keyword_to_number/1]).

-export_type([atom_log_level/0]).

-ignore_xref([clear_module_loglevel/1, set_module_loglevel/2]).

-type atom_log_level() :: none | logger:level() | all.
-type int_log_level() :: -1..8.
-type level() :: atom_log_level() | int_log_level().

%% Sets primary log level
-spec get_global_loglevel() -> atom_log_level().
get_global_loglevel() ->
    maps:get(level, logger:get_primary_config()).

-spec set_global_loglevel(level()) ->
    ok | {error, {invalid_level, term()}}.
set_global_loglevel(Level) when is_integer(Level) ->
    set_global_loglevel(loglevel_number_to_keyword(Level));
set_global_loglevel(Level) ->
    logger:update_primary_config(#{level => Level}).

-spec set_module_loglevel(module(), level()) ->
    ok | {error, term()}.
set_module_loglevel(Module, Level) when is_integer(Level) ->
    set_module_loglevel(Module, loglevel_number_to_keyword(Level));
set_module_loglevel(Module, Level) ->
    logger:set_module_level(Module, Level).

-spec clear_module_loglevel(module()) -> ok | {error, term()}.
clear_module_loglevel(Module) ->
    set_module_loglevel(Module, get_global_loglevel()).

-spec get_log_files() -> [file:filename()].
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

-spec loglevel_number_to_keyword(int_log_level())  -> atom_log_level().
loglevel_number_to_keyword(-1) -> none;
loglevel_number_to_keyword(0) -> emergency;
loglevel_number_to_keyword(1) -> alert;
loglevel_number_to_keyword(2) -> critical;
loglevel_number_to_keyword(3) -> error;
loglevel_number_to_keyword(4) -> warning;
loglevel_number_to_keyword(5) -> notice;
loglevel_number_to_keyword(6) -> info;
loglevel_number_to_keyword(7) -> debug;
loglevel_number_to_keyword(8) -> all.

-spec loglevel_keyword_to_number(atom_log_level()) -> int_log_level().
loglevel_keyword_to_number(none)      -> -1;
loglevel_keyword_to_number(emergency) -> 0;
loglevel_keyword_to_number(alert)     -> 1;
loglevel_keyword_to_number(critical)  -> 2;
loglevel_keyword_to_number(error)     -> 3;
loglevel_keyword_to_number(warning)   -> 4;
loglevel_keyword_to_number(notice)    -> 5;
loglevel_keyword_to_number(info)      -> 6;
loglevel_keyword_to_number(debug)     -> 7;
loglevel_keyword_to_number(all)       -> 8.
