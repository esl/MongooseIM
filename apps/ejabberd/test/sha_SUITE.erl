-module(sha_SUITE).
-compile([export_all]).

all() ->
    [ sanity_check ].

sanity_check(_) ->
    %% @doc vis: echo -n "Foo" | sha1sum
    <<"201a6b3053cc1422d2c3670b62616221d2290929">> =  sha:sha1_hex(<<"Foo">>).
