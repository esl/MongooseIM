-module(tcp_listener_helper).
-compile([export_all, nowarn_export_all]).

setup_meck() ->
    meck:expect(mongoose_tcp_listener, read_connection_details, fun read_connection_details/2).

read_connection_details(Socket, Opts) ->
    persistent_term:put(tcp_listener_helper_info, {Socket, self()}),
    gen_tcp:close(Socket),
    Result = meck:passthrough([Socket, Opts]),
    persistent_term:put(tcp_listener_helper_result, Result),
    Result.
