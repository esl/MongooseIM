%% Helper module for checking expected log messages
%% For assertions, see log_helper.hrl

-module(log_helper).
-compile([export_all, nowarn_export_all]).

%% Call in init_per_suite/group
set_up() ->
    ets_helper:new(?MODULE),
    ok = logger:add_handler(?MODULE, ?MODULE, #{}).

%% Call in init_per_testcase
subscribe() ->
    subscribe(self()).

subscribe(Pid) ->
    ets:insert(?MODULE, {Pid}),
    ok.

%% Call in end_per_testcase
unsubscribe() ->
    unsubscribe(self()).

unsubscribe(Pid) ->
    ets:delete(?MODULE, Pid),
    ok.

%% Call in end_per_suite/group
tear_down() ->
    logger:remove_handler(?MODULE),
    ets_helper:delete(?MODULE).

%% Logger callback
log(Event, #{}) ->
    [Receiver ! {log, Event} || {Receiver} <- ets:tab2list(?MODULE)].
