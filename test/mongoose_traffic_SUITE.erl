-module(mongoose_traffic_SUITE).
-compile([export_all, nowarn_export_all]).

-include_lib("kernel/include/logger.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("jlib.hrl").

all() ->
    [mongoose_debug, mongoose_traffic].

init_per_suite(Config) ->
    application:ensure_all_started(exometer_core),
    mongoose_config:set_opts(#{all_metrics_are_global => false}),
    {ok, _} = application:ensure_all_started(jid),
    Config.

end_per_suite(Config) ->
    mongoose_config:erase_opts(),
    application:stop(exometer_core),
    Config.

init_per_testcase(_, Config) ->
    mongooseim_helper:start_link_loaded_hooks(),
    Config.

end_per_testcase(_, Config) ->
    meck:unload(),
    Config.


%%----------------------------------------------------------------
%% test cases
%%----------------------------------------------------------------

mongoose_debug(_) ->
    Me = self(),
    Fmtr = fun(I) -> Me ! I, <<"ok">> end,
    code:ensure_loaded(mongoose_debug),
    % this is how mongoose_debug is meant to be used
    1 = recon_trace:calls({mongoose_debug, traffic, '_'}, 10, [{scope, local}, {formatter, Fmtr}]),
    mongoose_debug:start(localhost, []),
    call_hooks_in(),
    ?assertMatch({trace, _ ,call, {mongoose_debug,
                                   traffic,
                                   ["a@localhost/c"," C >>>> MiM "," ", _]}},
                 receive_msg()),
    ok.

mongoose_traffic(_) ->
    mongoose_traffic:start(localhost, #{standalone => true}),
    gen_server:call(mongoose_traffic, {register, self()}),
    call_hooks_in(),
    ?assertMatch({message,client_to_server, _, #jid{}, _},
                 receive_msg()),
    gen_server:call(mongoose_traffic, {unregister, self()}),
    call_hooks_in(),
    call_hooks_out(),
    no_new_msg(),
    ok.

call_hooks_in() ->
    Acc = mongoose_acc:new(#{location => ?LOCATION, lserver => localhost}),
    From = jid:from_binary(<<"a@localhost/c">>),
    El = #xmlel{name = <<"testelement">>},
    mongoose_hooks:c2s_debug(Acc, {client_to_server, From, El}),
    ok.

call_hooks_out() ->
    Acc = mongoose_acc:new(#{location => ?LOCATION, lserver => localhost}),
    From = jid:from_binary(<<"a@localhost/c">>),
    El = #xmlel{name = <<"testelement">>},
    mongoose_hooks:c2s_debug(Acc, {server_to_client, From, El}),
    ok.

receive_msg() ->
    receive
        M -> M
    after 100 ->
        ct:fail("message not received", [])
    end.

no_new_msg() ->
    receive
        M -> ct:fail("unexpected message received: ~p", [M])
    after 100 ->
        ok
    end.

get_handlers_for_all_hooks() ->
    maps:to_list(persistent_term:get(gen_hook, #{})).

flush() ->
    receive
        M ->
            ct:pal("received: ~p", [M]),
            flush()
    after 100 ->
        ct:pal("asdf over: ~p", [over]),
        ok
    end.
