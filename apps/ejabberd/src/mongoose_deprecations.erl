%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% File    : mongoose_deprecations.erl
%% Author  : Dominik Stanaszek <dominik.stanaszek@erlang-solutions.com>
%% Purpose : More generic deprecation handling
%% Created : 10 Oct 2017
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc This module is responsible for initialising
%% and stopping stuff needed to handle different deprecation
%% warinings as well as for exposing API for loging these
%% deprecations.
%% It checks whether a specific deprecation warning is not exceeding
%% given frequency of logging.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(mongoose_deprecations).
-author("dominik.stanaszek@erlang-solutions.com").

-export([start/0, stop/0, log/2, log/3]).

-include("ejabberd.hrl").

-define(DEPRECATIONS, [mam02]).                 % List of deprecation tags
-define(DEPRECATION_TAB, deprecations).         % ETS table name
-define(DEFAULT_COOLDOWN_HOURS, 6).             % default cooldown time

-type deprecation_tag() :: atom().
-type log_level() :: warning | error.
-type unix_timestamp() :: mod_mam:unix_timestamp().


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Public API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec start() -> ok.
start() ->
    prepare_ets(),
    ok.

-spec stop() -> ok.
stop() ->
    destroy_ets(),
    ok.

-spec log(deprecation_tag(), string()) -> ok.
log(Tag, Msg) ->
    maybe_log(Tag, Msg, error).

-spec log(deprecation_tag(), string(), log_level()) -> ok.
log(Tag, Msg, Lvl) ->
    maybe_log(Tag, Msg, Lvl).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Private functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% @doc In `deprecation` table there will be tags
%% as keys (indicating certain deprecation) and as
%% values:
%%      * 'not_logged' - this deprecation has not been
%%                       logged yet
%%      * unix_timestamp - timestamp of last logged
%%                         deprecation message
-spec prepare_ets() -> ok.
prepare_ets() ->
    ets:new(?DEPRECATION_TAB, [{read_concurrency, true}, named_table, public]),
    lists:foreach(fun(Tag) ->
                        ets:insert(?DEPRECATION_TAB, {Tag, not_logged})
                  end,
                  ?DEPRECATIONS),
    ok.

-spec destroy_ets() -> ok.
destroy_ets() ->
    ets:delete(?DEPRECATION_TAB),
    ok.

-spec maybe_log(deprecation_tag(), string(), log_level()) -> ok.
maybe_log(Tag, Msg, Lvl) ->
    [{Tag, Timestamp}] = ets:lookup(?DEPRECATION_TAB, Tag),
    case did_cooldown_elapse(Timestamp) of
        true ->
            log_lvl(Msg, Lvl),
            ets:insert(?DEPRECATION_TAB, {Tag, os:timestamp()}),
            ok;
        false ->
            ok
    end,
    ok.

-spec did_cooldown_elapse(unix_timestamp() | 'not_logged') -> boolean().
did_cooldown_elapse(not_logged) -> true;
did_cooldown_elapse(LastLogged) ->
    Now = os:timestamp(),
    timer:now_diff(Now, LastLogged) > default_cooldown().

-spec default_cooldown() -> unix_timestamp().
default_cooldown() -> ?DEFAULT_COOLDOWN_HOURS * 21600000000.


-spec log_lvl(string(), log_level()) -> ok.
log_lvl(Msg, error) ->
    ?ERROR_MSG(Msg, []);
log_lvl(Msg, warning) ->
    ?WARNING_MSG(Msg, []).


