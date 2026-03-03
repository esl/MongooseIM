-module(mod_caps_hash).

-export([is_known_hash_alg/1, generate/2, encode/1]).

-ignore_xref([encode/1]). % exported for tests

-include("jlib.hrl").

-spec is_known_hash_alg(mod_caps:hash_alg()) -> boolean().
is_known_hash_alg(Alg) ->
    hash_function(Alg) =/= unknown_alg.

-spec generate([exml:element()], mod_caps:hash_alg()) -> mod_caps:hash_value().
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
    maybe
        true ?= mongoose_data_forms:is_form(Element),
        #{type := ~"result", kvs := KVs, ns := NS} ?=
            mongoose_data_forms:parse_form_fields(Element),
        {3, [NS, $< | encode_form_fields(KVs)]}
    else
        _ -> skip % this could happen for a non-conformant or custom info response
    end.

-spec encode_form_fields(mongoose_data_forms:kv_map()) -> iolist().
encode_form_fields(KVs) ->
    [encode_form_field(K, V) || K := V <- maps:iterator(KVs, ordered)].

-spec encode_form_field(binary(), [binary()]) -> iolist().
encode_form_field(Key, Values) ->
    [[Item, $<] || Item <- [Key | lists:sort(Values)]].

-spec hash(binary(), mod_caps:hash_alg()) -> binary().
hash(Data, Alg) ->
    (hash_function(Alg))(Data). % fails for an unknown algorithm

-spec hash_function(mod_caps:hash_alg()) -> fun((binary()) -> binary()) | unknown_alg.
hash_function(~"md5") -> fun erlang:md5/1;
hash_function(~"sha-1") -> fun(Data) -> crypto:hash(sha, Data) end;
hash_function(~"sha-224") -> fun(Data) -> crypto:hash(sha224, Data) end;
hash_function(~"sha-256") -> fun(Data) -> crypto:hash(sha256, Data) end;
hash_function(~"sha-384") -> fun(Data) -> crypto:hash(sha384, Data) end;
hash_function(~"sha-512") -> fun(Data) -> crypto:hash(sha512, Data) end;
hash_function(_) -> unknown_alg.
