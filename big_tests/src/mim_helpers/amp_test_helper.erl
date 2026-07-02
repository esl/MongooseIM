-module(amp_test_helper).
-compile([export_all, nowarn_export_all]).

setup_meck() ->
    meck:expect(ranch_tcp, send, fun ranch_tcp_send/2).

ranch_tcp_send(Socket, Data) ->
    try binary:match(Data, <<"Recipient connection breaks">>) of
        nomatch -> meck:passthrough([Socket, Data]);
        _ -> {error, simulated}
    catch
        _:_ -> meck:passthrough([Socket, Data])
    end.
