-module(mongoose_bin_SUITE).
-compile([export_all, nowarn_export_all]).

-include_lib("proper/include/proper.hrl").
-import(prop_helper, [prop/2]).

all() ->
    [ sanity_check,
      always_produces_well_formed_output
    ].

sanity_check(_) ->
    %% @doc vis: echo -n "Foo" | sha256sum
    Encoded = mongoose_bin:encode_crypto(<<"Foo">>),
    <<"201a6b3053cc1422d2c3670b62616221d2290929">> = Encoded.

always_produces_well_formed_output(_) ->
    prop(always_produces_well_formed_output,
         ?FORALL(BinaryBlob, binary(),
                 true == is_well_formed(mongoose_bin:encode_crypto(BinaryBlob)))).

is_well_formed(Binary) ->
    true =:= is_binary(Binary) andalso
    40 =:= byte_size(Binary) andalso
    nomatch == re:run(Binary, "[^0-9a-f]").
