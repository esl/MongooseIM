#!/usr/bin/env escript
%% -*- erlang -*-
%%! -noinput

-mode(compile).

main(_) ->
    io:format("Template smoke script started~n", []),
    MIM_DIR = os:getenv("MIM_DIR"),
    DemoPath = filename:join(MIM_DIR, "etc/demo.config"),
    {ok, Cfg} = file:consult(DemoPath),
    [[{ssl,SSL}]] = Cfg,
    ['tlsv1.2','tlsv1.3'] = proplists:get_value(protocol_version, SSL),
    700 = proplists:get_value(session_lifetime, SSL),
    ok.
