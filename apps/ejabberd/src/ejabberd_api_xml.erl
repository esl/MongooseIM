-module(ejabberd_api_xml).

-export([serialize/1]).

-include_lib("exml/include/exml.hrl").

serialize(Data) ->
    do_serialize(Data).

do_serialize(Data) ->
    exml:to_iolist(prepare_xmlel(Data)).

prepare_xmlel({ElementName, List}) when is_list(List) ->
    {Attrs, Children} = lists:partition(fun is_attribute/1, List),
    #xmlel{name = to_iolist_compliant(ElementName),
           attrs = [prepare_xmlel(Attr) || Attr <- Attrs],
           children = [prepare_xmlel(Child) || Child <- Children]};
prepare_xmlel(List) when is_list(List) ->
    #xmlel{name = <<"list">>,
           children = [prepare_xmlel(Element) || Element <- List]};
prepare_xmlel({Key, Value}) ->
    {to_iolist_compliant(Key), to_iolist_compliant(Value)};
prepare_xmlel(Other) ->
    #xmlel{name = to_iolist_compliant(Other)}.

is_attribute({_, List}) when is_list(List) ->
    false;
is_attribute({_, _}) ->
    true;
is_attribute(_) ->
    false.

to_iolist_compliant(Atom) when is_atom(Atom) ->
    atom_to_binary(Atom, utf8);
to_iolist_compliant(Int) when is_integer(Int) ->
    integer_to_binary(Int);
to_iolist_compliant(Other) ->
    Other.
