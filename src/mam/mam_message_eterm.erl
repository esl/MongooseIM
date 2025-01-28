%%% @doc Encoding and decoding messages using internal erlang format
-module(mam_message_eterm).
-export([encode/1,
         decode/1]).

-behaviour(mam_message).

-include_lib("exml/include/exml.hrl").

-type old_xmlcdata_format() :: {xmlcdata, Content :: binary()}.
-type old_xmlel_format() :: {xmlel, Name :: binary(), Attrs :: [{binary(),binary()}],
                             Children :: [old_xmlel_format() | old_xmlcdata_format()]}.
encode(Packet) ->
    term_to_binary(Packet).

decode(Bin) ->
    Term = binary_to_term(Bin),
    maybe_convert_old_exml_format(Term).

-spec maybe_convert_old_exml_format(exml:element() | old_xmlel_format()) ->
    exml:element().
maybe_convert_old_exml_format({xmlel, _, Attrs, _} = XmlEl) when is_list(Attrs) ->
    convert_old_exml_format(XmlEl);
maybe_convert_old_exml_format(XmlEl) ->
    XmlEl.

-spec convert_old_exml_format(old_xmlcdata_format()) -> exml:cdata();
                             (old_xmlel_format()) -> exml:element().
convert_old_exml_format({xmlel, Name, Attrs, Children}) ->
    NewAttrs = maps:from_list(Attrs),
    NewChildren = [convert_old_exml_format(C) || C <- Children],
    #xmlel{name = Name, attrs = NewAttrs, children = NewChildren};
convert_old_exml_format({xmlcdata, Content}) ->
    #xmlcdata{content = Content}.
