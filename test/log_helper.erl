%% Helper module for checking expected log messages
%% For assertions, see log_helper.hrl

-module(log_helper).
-compile([export_all, nowarn_export_all]).

%% Call in init_per_testcase
set_up() ->
    logger:add_handler(handler_id(), ?MODULE, #{receiver => self()}).

%% Call in end_per_testcase
tear_down() ->
    logger:remove_handler(handler_id()).

%% Logger callback
log(Event, #{receiver := Receiver}) ->
    Receiver ! {log, Event}.

handler_id() ->
    list_to_atom(pid_to_list(self())).
