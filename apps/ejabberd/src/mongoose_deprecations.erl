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
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(mongoose_deprecations).
-author("dominik.stanaszek@erlang-solutions.com").

-export([start/0, stop/0, log/1, log/2]).

-include("ejabberd.hrl").

-type deprecation() :: atom().
-type log_level() :: warning | error.

-spec start() -> ok.
start() ->
    start_mam02_reminder(),
    ok.

-spec stop() -> ok.
stop() ->
    stop_mam02_reminder(),
    ok.

%% @doc Performs a log specific for a deprecation
%% which is passed as an atom.
%% It can also log a custom string.
%% Logs as ERROR message.
-spec log(deprecation() | string()) -> ok.
log(mam02_archived) ->
    maybe_log_mamv02_deprecation_error();
log(DeprMsg) ->
    log(DeprMsg, error).

%% @doc The same as `log/1` but you can specify
%% a log level on the second argument.
%% It's either `error` or `warining`.
-spec log(string(), log_level()) -> ok.
log(DeprMsg, error) ->
    ?ERROR_MSG(DeprMsg, []);
log(DeprMsg, warning) ->
    ?WARNING_MSG(DeprMsg, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%% MAM v0.2 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc These functions handle a system of reminding that mam v0.2
%% element `</archived>` is going to be deprecated. Logs each time
%% this element is added to a stanza but not more frequently than once
%% per some period of time.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-type unix_timestamp() :: mod_mam:unix_timestamp().

-define(DEPRECATION_TAB, mam_v02_deprecation).
-define(COOLDOWN_HOURS, 6).

-spec start_mam02_reminder() -> ok.
start_mam02_reminder() ->
    ets:new(?DEPRECATION_TAB, [{read_concurrency, true}, named_table, public]),
    ets:insert(?DEPRECATION_TAB, {last_logged, not_logged}),
    ok.

-spec stop_mam02_reminder() -> ok.
stop_mam02_reminder() ->
    ets:delete(?DEPRECATION_TAB),
    ok.

-spec maybe_log_mamv02_deprecation_error() -> ok.
maybe_log_mamv02_deprecation_error() ->
    [{last_logged, Timestamp}] = ets:lookup(?DEPRECATION_TAB, last_logged),
    case did_cooldown_elapse(Timestamp) of
        true ->
            deprecate_archived_element_message(),
            ets:insert(?DEPRECATION_TAB, {last_logged, os:timestamp()}),
            ok;
        false ->
            ok
    end,
    ok.

-spec did_cooldown_elapse(unix_timestamp()) -> boolean().
did_cooldown_elapse(not_logged) -> true;
did_cooldown_elapse(LastLogged) ->
    Now = os:timestamp(),
    timer:now_diff(Now, LastLogged) > deprecate_error_cooldown_time().

-spec deprecate_archived_element_message() -> ok.
deprecate_archived_element_message() ->
    ?ERROR_MSG("Archived element is going to be deprecated in release 3.0.0."
               " It is not recommended to use it."
               " Consider using a <stanza-id/> element instead", []).

 -spec deprecate_error_cooldown_time() -> unix_timestamp().
 deprecate_error_cooldown_time() -> ?COOLDOWN_HOURS * 21600000000.
