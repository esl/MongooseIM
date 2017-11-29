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
         #xmlel{name = list_to_binary(Name),
                attrs = Attrs});

xmlel(Size) ->
    ?LET({Name, Attrs, Children}, {ascii_text(), xmlel_attrs(), xmlel_children(Size)},
         #xmlel{name = list_to_binary(Name),
                attrs = Attrs,
                children = join_consecutive_cdata(Children)}).

xmlel(FixedName, FixedAttrs, FixedChildren) ->
    ?SIZED(Size, xmlel(Size, FixedName, FixedAttrs, FixedChildren)).

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
                children = join_consecutive_cdata(FixedChildren ++ Children)}).

xmlel_children(Size) ->
    ?LET(Len, choose(0, 5), vector(Len, xmlel_child(Size))).

xmlel_child(Size) ->
    ?LET(CData, ascii_text(),
         oneof([#xmlcdata{content = list_to_binary(CData)},
                xmlel(Size div 3)])).

join_consecutive_cdata([]) -> [];
join_consecutive_cdata([H|T]) ->
    join_consecutive_cdata(T, [H]).

join_consecutive_cdata([], Acc) -> lists:reverse(Acc);
join_consecutive_cdata([#xmlcdata{content = B} | Tail], [#xmlcdata{content = A} | Acc]) ->
    join_consecutive_cdata(Tail, [#xmlcdata{content = <<A/bytes, B/bytes>>} | Acc]);
join_consecutive_cdata([El | Tail], Acc) ->
    join_consecutive_cdata(Tail, [El | Acc]).
