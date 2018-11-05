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

%% Test API
-export([log_with_lvl/2]).

-include("mongoose.hrl").

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
%% keeping proper frequency. Opts can be:
%%      * cooldown - the minimal interval (in milliseconds)
%%                   to be held between logs. Default: 6 hours
%%                   It is internally represented in microseconds
%%                   but API requires milliseconds.
%%      * log_level - 'warning' or 'error'
-spec log(deprecation_tag(), string(), proplists:proplist()) -> ok.
log(Tag, Msg, Opts) ->
    Cooldown = milliseconds_to_microseconds(
                 proplists:get_value(cooldown, Opts, default_cooldown())
                ),
    LogLvl = proplists:get_value(log_level, Opts, default_log_lvl()),
    maybe_log(Tag, Msg, LogLvl, Cooldown).

-spec log(deprecation_tag(), string()) -> ok.
log(Tag, Msg) ->
    log(Tag, Msg, []).

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

-spec maybe_log(deprecation_tag(), string(), log_level(), unix_timestamp()) -> ok.
maybe_log(Tag, Msg, Lvl, Cooldown) ->
    Timestamp = case ets:lookup(?DEPRECATION_TAB, Tag) of
                             [] ->
                                not_logged;
                             [{Tag, LastLogged}] ->
                                 LastLogged
                         end,
    case did_cooldown_elapse(Timestamp, Cooldown) of
        true ->
            ?MODULE:log_with_lvl(Msg, Lvl),     % ?MODULE lets meck mock it
            ets:insert(?DEPRECATION_TAB, {Tag, os:timestamp()}),
            ok;
        false ->
            ok
    end.

-spec did_cooldown_elapse(unix_timestamp() | 'not_logged', unix_timestamp())
        -> boolean().
did_cooldown_elapse(not_logged, _) -> true;
did_cooldown_elapse(LastLogged, Cooldown) ->
    Now = os:timestamp(),
    timer:now_diff(Now, LastLogged) > Cooldown.

-spec default_cooldown() -> unix_timestamp().
default_cooldown() -> ?DEFAULT_COOLDOWN_HOURS * 3600000000.

-spec default_log_lvl() -> log_level().
default_log_lvl() -> error.

-spec log_with_lvl(string(), log_level()) -> ok.
log_with_lvl(Msg, error) ->
    ?ERROR_MSG(Msg, []);
log_with_lvl(Msg, warning) ->
    ?WARNING_MSG(Msg, []).

-spec milliseconds_to_microseconds(Milliseconds :: integer()) 
        -> unix_timestamp().
milliseconds_to_microseconds(N) -> N * 1000.
