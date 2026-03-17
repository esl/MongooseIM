-module(mod_caps_hash).
-moduledoc """
Encoding and hashing of disco#info response query elements. There are two versions supported:
  - v1 fully implements XEP-0115.
  - v2 fully implements XEP-0390 with the following notes:
    1. Inheritance of the 'xml:lang' attribute is not implemented as it would require
       a broader context than the query elements.
       We are not aware of any existing implementations of this rule as well.
    2. Duplicates are disallowed as in XEP-0115 - see details in comments below.
""".

-xep([{xep, 300}, {version, "1.0.0"}]).

-export([generate/3, encode/2, is_alg_supported/1]).

-ignore_xref([encode/2]). % exported for tests

-type feature() :: binary().
-type identity() :: {binary(), binary(), binary(), binary()}.
-type form() :: {binary(), mongoose_data_forms:kv_map()}.
-type prepared_element() :: {group(), group_element()} | {skip | error, exml:element()}.
-type group() :: feature | identity | form.
-type group_element() :: feature() | identity() | form().

-include("jlib.hrl").

-spec generate([exml:element()], mod_caps:version(), mod_caps:hash_alg()) -> mod_caps:hash_value().
generate(Elements, Version, Alg) ->
    base64:encode(hash(encode(Version, Elements), Alg)).

%% Processing and encoding of the disco#info response

-spec encode(mod_caps:version(), [exml:element()]) -> binary().
encode(Version, Elements) ->
    PreparedEls = lists:map(fun(Element) -> prepare_element(Version, Element) end, Elements),
    case make_groups(PreparedEls) of
        #{error := InvalidElements} ->
            error({invalid_elements, InvalidElements});
        Groups ->
            Result = [encode_group(Version, Group, maps:get(Group, Groups, []))
                      || Group <- ordered_groups(Version)],
            iolist_to_binary(Result)
    end.

-spec make_groups([prepared_element()]) -> #{group() => [group_element()],
                                             skip | error => [exml:element()]}.
make_groups(EncodedEls) ->
    maps:groups_from_list(fun({Group, _}) -> Group end, fun({_, Value}) -> Value end, EncodedEls).

-spec encode_group(mod_caps:version(), group(), [group_element()]) -> iolist().
encode_group(Version, Group, PreparedEls) ->
    case deduplicate_group(Group, PreparedEls) of
        DedupEls when length(DedupEls) =:= length(PreparedEls) ->
            encode_group(Version, [encode(Version, Group, El) || El <- DedupEls]);
        DedupEls ->
            % XEP-0115 5.4 3.c/d requires aborting on any duplicate identity/feature
            %          3.e requires aborting on more than one form with the same FORM_TYPE
            % XEP-0390 has no such rules, but we still apply this rule for consistency, simplicity,
            %          and because duplicate elements don't make sense in disco#info responses
            error({duplicate_elements, PreparedEls -- DedupEls})
    end.

%% XEP-0115 5.1 1-7 specifies sorting rules as below
-spec deduplicate_group(group(), [group_element()]) -> [group_element()].
deduplicate_group(feature, Features) -> lists:usort(Features);
deduplicate_group(identity, Identities) -> lists:usort(Identities);
deduplicate_group(form, Forms) -> lists:ukeysort(1, Forms).

-spec prepare_element(mod_caps:version(), exml:element()) -> prepared_element().
prepare_element(_Version, #xmlel{name = ~"feature", attrs = #{~"var" := Var}}) ->
    {feature, Var};
prepare_element(_Version, #xmlel{name = ~"identity",
                                 attrs = Attrs = #{~"category" := Category, ~"type" := Type}}) ->
    Lang = maps:get(~"xml:lang", Attrs, ~""),
    Name = maps:get(~"name", Attrs, ~""),
    {identity, {Category, Type, Lang, Name}};
prepare_element(Version, Element = #xmlel{children = Children}) ->
    case mongoose_data_forms:parse_form(Element) of
        #{type := ~"result", kvs := KVs, ns := NS} ->
            case lists:any(fun(El) -> is_disallowed_form_element(Version, El) end, Children) of
                true -> {skip_or_error(Version), Element};
                false -> {form, {NS, KVs}}
            end;
        #{type := ~"result", kvs := #{~"FORM_TYPE" := [_|_]}} ->
            % XEP-0115 5.4 3.e Abort on forms with multiple values of FORM_TYPE
            % XEP-0390 4.1 1.3 Abort on forms not adhering to the FORM_TYPE protocol
            {error, Element};
        _ ->
            % XEP-0390 4.1 1.1 Abort on elements not adhering to XEP-0030 or XEP-0128
            % XEP-0115 does not have such a rule, so v1 ignores extra elements
            {skip_or_error(Version), Element}
    end.

-spec is_disallowed_form_element(mod_caps:version(), exml:element()) -> boolean().
is_disallowed_form_element(v2, #xmlel{name = Name}) when Name =:= ~"reported"; Name =:= ~"item" ->
    % XEP-0390 4.1 1.2 Abort on forms containing <reported/> or <item/>
    true;
is_disallowed_form_element(_, El = #xmlel{name = ~"field", attrs = #{~"var" := ~"FORM_TYPE"}}) ->
    % XEP-0115 5.4 3.f Skip forms with non-hidden FORM_TYPE
    % XEP-0390 4.1 1.3 Abort on forms not adhering to the FORM_TYPE protocol
    exml_query:attr(El, ~"type") =/= ~"hidden";
is_disallowed_form_element(_, _) ->
    false.

-spec skip_or_error(mod_caps:version()) -> skip | error.
skip_or_error(v1) -> skip;
skip_or_error(v2) -> error.

%% Ordering and encoding according to XEP-0115 5.1 and XEP-0390 4.1

-spec ordered_groups(mod_caps:version()) -> [group()].
ordered_groups(v1) -> [identity, feature, form];
ordered_groups(v2) -> [feature, identity, form].

%% XEP-0390 4.1 4-6 requires sorting by octet strings
-spec encode_group(mod_caps:version(), [iolist()]) -> iolist().
encode_group(v1, Values) -> Values; % already sorted on deduplication
encode_group(v2, Values) -> [lists:sort(Values), 16#1c].

-spec encode(mod_caps:version(), group(), group_element()) -> iolist().
encode(v1, identity, {Cat, Type, Lang, Name}) ->
    [Cat, $/, Type, $/, Lang, $/, Name, $<];
encode(v2, identity, {Cat, Type, Lang, Name}) ->
    [Cat, 16#1f, Type, 16#1f, Lang, 16#1f, Name, 16#1f, 16#1e];
encode(v1, feature, Var) ->
    [Var, $<];
encode(v2, feature, Var) ->
    [Var, 16#1f];
encode(v1, form, {NS, KVs}) ->
    [NS, $< | encode_form_fields(v1, KVs)];
encode(v2, form, {NS, KVs}) ->
    [encode_form_fields(v2, KVs#{~"FORM_TYPE" => [NS]}), 16#1d].

-spec encode_form_fields(mod_caps:version(), mongoose_data_forms:kv_map()) -> iolist().
encode_form_fields(Version, KVs) ->
    [encode_form_field(Version, [K | lists:sort(Vs)]) || K := Vs <- maps:iterator(KVs, ordered)].

-spec encode_form_field(mod_caps:version(), [binary()]) -> iolist().
encode_form_field(v1, Values) ->
    [[Value, $<] || Value <- Values];
encode_form_field(v2, Values) ->
    [[[Value, 16#1f] || Value <- Values], 16#1e].

%% Hash calculation
%% According to XEP-0300, a supported hash algorithm MUST be defined in one of the sources:
%% 1. https://www.iana.org/assignments/hash-function-text-names/hash-function-text-names.xhtml
%% 2. https://xmpp.org/extensions/xep-0300.html#table-1

-spec is_alg_supported(mod_caps:hash_alg()) -> boolean().
is_alg_supported(~"md5") ->
    true;
is_alg_supported(Alg) ->
    case hash_type(Alg) of
        {ok, Type} -> lists:member(Type, crypto:supports(hashs));
        {error, unknown_alg} -> false
    end.

%% XOF hash sizes are specified in https://datatracker.ietf.org/doc/html/rfc8692#rsa-sigs
-spec hash(binary(), mod_caps:hash_alg()) -> binary().
hash(Data, ~"md5") ->
    erlang:md5(Data);
hash(Data, ~"shake128") ->
    crypto:hash_xof(shake128, Data, 256);
hash(Data, ~"shake256") ->
    crypto:hash_xof(shake256, Data, 512);
hash(Data, Alg) ->
    {ok, Type} = hash_type(Alg),
    crypto:hash(Type, Data).

-spec hash_type(mod_caps:hash_alg()) -> {ok, atom()} | {error, unknown_alg}.
hash_type(~"sha-1") -> {ok, sha};
hash_type(~"sha-224") -> {ok, sha224};
hash_type(~"sha-256") -> {ok, sha256};
hash_type(~"sha-384") -> {ok, sha384};
hash_type(~"sha-512") -> {ok, sha512};
hash_type(~"shake128") -> {ok, shake128};
hash_type(~"shake256") -> {ok, shake256};
hash_type(~"sha3-256") -> {ok, sha3_256};
hash_type(~"sha3-512") -> {ok, sha3_512};
hash_type(~"blake2b-512") -> {ok, blake2b};
hash_type(_) -> {error, unknown_alg}.
