-module(xmlel_gen).

-export([xmlel/1, xmlel/3]).

-include_lib("proper/include/proper.hrl").
-include_lib("exml/include/exml.hrl").

ascii_text() ->
    non_empty(list(choose($a, $z))).

xmlel_attr() ->
    ?LET({Key, Val}, {ascii_text(), ascii_text()},
         {list_to_binary(Key), list_to_binary(Val)}).

xmlel_attrs() ->
    ?LET(Len, choose(1, 5), vector(Len, xmlel_attr())).

xmlel(0) ->
    ?LET({Name, Attrs}, {ascii_text(), xmlel_attrs()},
         #xmlel{name = list_to_binary(Name),
                attrs = Attrs});

xmlel(Size) ->
    ?LET({Name, Attrs}, {ascii_text(), xmlel_attrs()},
         #xmlel{name = list_to_binary(Name),
                attrs = Attrs,
                children = xmlel_children(Size)}).

xmlel(FixedName, FixedAttrs, FixedChildren) ->
    ?SIZED(Size, xmlel(Size, FixedName, FixedAttrs, FixedChildren)).

xmlel(0, FixedName, FixedAttrs, FixedChildren) ->
    ?LET({Attrs}, {xmlel_attrs()},
         #xmlel{name = list_to_binary(FixedName),
                attrs = Attrs ++ FixedAttrs,
                children = FixedChildren});

xmlel(Size, FixedName, FixedAttrs, FixedChildren) ->
    ?LET({Attrs}, {xmlel_attrs()},
         #xmlel{name = list_to_binary(FixedName),
                attrs = Attrs ++ FixedAttrs,
                children = FixedChildren ++ xmlel_children(Size)}).

xmlel_children(Size) ->
    ?LET(Len, choose(0, 5), vector(Len, xmlel_child(Size))).

xmlel_child(Size) ->
    ?LET(CData, ascii_text(),
         oneof([#xmlcdata{content = list_to_binary(CData)},
                xmlel(Size div 3)])).
