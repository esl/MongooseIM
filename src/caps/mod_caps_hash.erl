-module(mod_caps_hash).

-export([generate/2, encode/1]).
-include("jlib.hrl").

-type alg() :: binary().

-spec generate([exml:element()], alg()) -> binary().
generate(Elements, Alg) ->
    base64:encode(hash(encode(Elements), Alg)).

-spec encode([exml:element()]) -> binary().
encode(Elements) ->
    EncodedItems = lists:sort(lists:map(fun encode_item/1, Elements)),
    << <<EncodedItem/binary>> || {_Priority, EncodedItem} <- EncodedItems >>.

-spec encode_item(exml:element()) -> {Priority :: pos_integer(), binary()} | skip.
encode_item(#xmlel{name = <<"identity">>,
                   attrs = Attrs = #{<<"category">> := Category, <<"type">> := Type}}) ->
    Lang = maps:get(<<"xml:lang">>, Attrs, <<>>),
    Name = maps:get(<<"name">>, Attrs, <<>>),
    {1, << Category/binary, $/, Type/binary, $/, Lang/binary, $/, Name/binary, $< >>};
encode_item(#xmlel{name = <<"feature">>, attrs = #{<<"var">> := Var}}) ->
    {2, << Var/binary, $< >>};
encode_item(Element) ->
    case mongoose_data_forms:is_form(Element) of
        true -> {3, encode_data_form(mongoose_data_forms:parse_form_fields(Element))};
        false -> skip
    end.

-spec encode_data_form(mongoose_data_forms:parsed_form()) -> binary().
encode_data_form(#{type := <<"result">>, kvs := KVs, ns := NS}) ->
    << NS/binary, $<, (encode_form_fields(KVs))/binary >>.

-spec encode_form_fields(mongoose_data_forms:kv_map()) -> binary().
encode_form_fields(KVs) ->
    << << (encode_form_field(K, V))/binary, $< >> || K := V <- maps:iterator(KVs, ordered) >>.

-spec encode_form_field(binary(), [binary()]) -> binary().
encode_form_field(Key, Values) ->
    << << Item/binary, $< >> || Item <- [Key | Values] >>.

-spec hash(binary(), alg()) -> binary().
hash(Data, <<"md5">>) -> erlang:md5(Data);
hash(Data, <<"sha-1">>) -> crypto:hash(sha, Data);
hash(Data, <<"sha-224">>) -> crypto:hash(sha224, Data);
hash(Data, <<"sha-256">>) -> crypto:hash(sha256, Data);
hash(Data, <<"sha-384">>) -> crypto:hash(sha384, Data);
hash(Data, <<"sha-512">>) -> crypto:hash(sha512, Data).
