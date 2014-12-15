-module(sha_SUITE).
-compile([export_all]).

-include_lib("ejabberd/include/amp.hrl").
-include_lib("exml/include/exml.hrl").

-include_lib("common_test/include/ct.hrl").
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-import(prop_helper, [prop/2, readable_bitstring/0]).

-define(ae(Expected, Actual), ?assertEqual(Expected, Actual)).


all() ->
    [ sanity_check,
      always_produces_well_formed_output
    ].

sanity_check(_) ->
    %% @doc vis: echo -n "Foo" | sha1sum
    <<"201a6b3053cc1422d2c3670b62616221d2290929">> =  sha:sha1_hex(<<"Foo">>).

always_produces_well_formed_output(_) ->
    prop(always_produces_well_formed_output,
         ?FORALL(BinaryBlob, binary(),
                 true == is_well_formed(sha:sha1_hex(BinaryBlob)))).

is_well_formed(Binary) ->
    40 == size(Binary) andalso
    nomatch == re:run(Binary, "[^0-9a-f]").
