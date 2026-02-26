-module(mod_caps_hash).

-export([generate/2, encode/1]).

-ignore_xref([encode/1]). % exported for tests

-include("jlib.hrl").

-type alg() :: binary().

-spec generate([exml:element()], alg()) -> binary().
generate(Elements, Alg) ->
    base64:encode(hash(encode(Elements), Alg)).

-spec encode([exml:element()]) -> binary().
encode(Elements) ->
    EncodedItems = lists:sort(lists:map(fun encode_item/1, Elements)),
    iolist_to_binary([Item || {_Priority, Item} <- EncodedItems]).

-spec encode_item(exml:element()) -> {Priority :: pos_integer(), iolist()} | skip.
encode_item(#xmlel{name = ~"identity",
                   attrs = Attrs = #{~"category" := Category, ~"type" := Type}}) ->
    Lang = maps:get(~"xml:lang", Attrs, <<>>),
    Name = maps:get(~"name", Attrs, <<>>),
    {1, [Category, $/, Type, $/, Lang, $/, Name, $<]};
encode_item(#xmlel{name = ~"feature", attrs = #{~"var" := Var}}) ->
    {2, [Var, $<]};
encode_item(Element) ->
    case mongoose_data_forms:is_form(Element) of
        true -> {3, encode_data_form(mongoose_data_forms:parse_form_fields(Element))};
        false -> skip % this could only occur for a non-conformant or custom info response
    end.

-spec encode_data_form(mongoose_data_forms:parsed_form()) -> iolist().
encode_data_form(#{type := ~"result", kvs := KVs, ns := NS}) ->
    [NS, $< | encode_form_fields(KVs)].

-spec encode_form_fields(mongoose_data_forms:kv_map()) -> iolist().
encode_form_fields(KVs) ->
    [encode_form_field(K, V) || K := V <- maps:iterator(KVs, ordered)].

-spec encode_form_field(binary(), [binary()]) -> iolist().
encode_form_field(Key, Values) ->
    [[Item, $<] || Item <- [Key | Values]].

-spec hash(binary(), alg()) -> binary().
hash(Data, ~"md5") -> erlang:md5(Data);
hash(Data, ~"sha-1") -> crypto:hash(sha, Data);
hash(Data, ~"sha-224") -> crypto:hash(sha224, Data);
hash(Data, ~"sha-256") -> crypto:hash(sha256, Data);
hash(Data, ~"sha-384") -> crypto:hash(sha384, Data);
hash(Data, ~"sha-512") -> crypto:hash(sha512, Data).
