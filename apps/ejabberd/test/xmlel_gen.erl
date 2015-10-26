-module(xmlel_gen).

%% Public
-export([xmlel/1, xmlel/3]).

-include_lib("proper/include/proper.hrl").
-include_lib("exml/include/exml.hrl").

%%
%% Public
%%

xmlel(0) ->
    ?LET({Name, Attrs}, {ascii_text(), xmlel_attrs()},
         normalization_hack(#xmlel{name = list_to_binary(Name),
                                   attrs = Attrs}));

xmlel(Size) ->
    ?LET({Name, Attrs, Children}, {ascii_text(), xmlel_attrs(), xmlel_children(Size)},
         normalization_hack(#xmlel{name = list_to_binary(Name),
                                   attrs = Attrs,
                                   children = Children})).

xmlel(FixedName, FixedAttrs, FixedChildren) ->
    ?LET(Element, ?SIZED(Size, xmlel(Size, FixedName, FixedAttrs, FixedChildren)),
         normalization_hack(Element)).

%%
%% Internal
%%

ascii_text() ->
    non_empty(list(choose($a, $z))).

xmlel_attr() ->
    ?LET({Key, Val}, {ascii_text(), ascii_text()},
         {list_to_binary(Key), list_to_binary(Val)}).

xmlel_attrs_non_unique() ->
    ?LET(Len, choose(1, 5), vector(Len, xmlel_attr())).

xmlel_attrs() ->
    ?SUCHTHAT(Attrs, xmlel_attrs_non_unique(),
              length(lists:ukeysort(1, Attrs)) == length(Attrs)).

xmlel(0, FixedName, FixedAttrs, FixedChildren) ->
    ?LET({Attrs}, {xmlel_attrs()},
         #xmlel{name = list_to_binary(FixedName),
                attrs = Attrs ++ FixedAttrs,
                children = FixedChildren});

xmlel(Size, FixedName, FixedAttrs, FixedChildren) ->
    ?LET({Attrs, Children}, {xmlel_attrs(), xmlel_children(Size)},
         #xmlel{name = list_to_binary(FixedName),
                attrs = Attrs ++ FixedAttrs,
                children = FixedChildren ++ Children}).

xmlel_children(Size) ->
    ?LET(Len, choose(0, 5), vector(Len, xmlel_child(Size))).

xmlel_child(Size) ->
    ?LET(CData, ascii_text(),
         oneof([#xmlcdata{content = list_to_binary(CData)},
                xmlel(Size div 3)])).

normalization_hack(Element) ->
    %% Hack!
    %% Because a exml:to_binary composed with exml:parse might merge cdata fields [1],
    %% we do it once here to reach some kind-of-normalized representation
    %% of the XML element.
    %%
    %% [1] example:
    %%
    %%   > T1#token.vcard.
    %%   {xmlel,<<"vCard">>,
    %%          [{<<"a">>,<<"a">>}],
    %%          [{xmlel,<<"a">>,
    %%                  [{<<"a">>,<<"a">>}],
    %%                  [{xmlcdata,<<"a">>},{xmlcdata,<<"a">>}]}]}
    %%   > exml:parse(exml:to_binary(T1#token.vcard)).
    %%   {ok,{xmlel,<<"vCard">>,
    %%              [{<<"a">>,<<"a">>}],
    %%              [{xmlel,<<"a">>,
    %%                      [{<<"a">>,<<"a">>}],
    %%                      [{xmlcdata,<<"aa">>}]}]}}
    %%
    {ok, Normalized} = exml:parse(exml:to_binary(Element)),
    Normalized.
