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

-define(DEPRECATION_TAB, deprecations).         % ETS table name
-define(DEFAULT_COOLDOWN_HOURS, 6).             % default cooldown time

-type deprecation_tag() :: atom().              % Specifies the deprecation
-type log_level() :: warning | error.
-type unix_timestamp() :: mod_mam:unix_timestamp().


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Public API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Should be called before using the module. Sets everything
%% needed up
-spec start() -> ok.
start() ->
    prepare_ets(),
    ok.

%% @doc Used after using the module, when we won't log deprecation
%% messages again.
-spec stop() -> ok.
stop() ->
    destroy_ets(),
    ok.

%% @doc Should be used to log deprecation messages. It logs
%% keeping proper frequency.
-spec log(deprecation_tag(), string()) -> ok.
log(Tag, Msg) ->
    maybe_log(Tag, Msg, error).

-spec log(deprecation_tag(), string(), log_level()) -> ok.
log(Tag, Msg, Lvl) ->
    maybe_log(Tag, Msg, Lvl).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Private functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% @doc Deprecation table will hold pairs in form:
%%      {deprecation_tag(), unix_timestamp()}
%% and will indicate at what (unix) time last deprecation
%% warining was logged concerning a deprecation connected to
%% the deprecation tag specified in a key
-spec prepare_ets() -> ok.
prepare_ets() ->
    ets:new(?DEPRECATION_TAB, [{read_concurrency, true}, named_table, public]),
    ok.

-spec destroy_ets() -> ok.
destroy_ets() ->
    ets:delete(?DEPRECATION_TAB),
    ok.

-spec maybe_log(deprecation_tag(), string(), log_level()) -> ok.
maybe_log(Tag, Msg, Lvl) ->
    Timestamp = case ets:lookup(?DEPRECATION_TAB, Tag) of
                             [] ->
                               not_logged;
                             [{Tag, LastLogged}] ->
                                 LastLogged
                         end,
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


