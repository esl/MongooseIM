%%%----------------------------------------------------------------------
%%% File    : xml.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : XML utils
%%% Created : 20 Nov 2002 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2011   ProcessOne
%%%
%%% This program is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation; either version 2 of the
%%% License, or (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with this program; if not, write to the Free Software
%%% Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
%%%
%%%----------------------------------------------------------------------

-module(xml).
-author('alexey@process-one.net').

-export([remove_cdata/1,
         get_attr/2, get_attr_s/2,
         get_tag_attr/2, get_tag_attr_s/2,
         append_subtags/2,
         replace_tag_attr/3,
         replace_subelement/2]).

-include("jlib.hrl").

-type xmlel_or_cdata() :: jlib:xmlch().

-spec remove_cdata_p(xmlel_or_cdata()) -> boolean().
remove_cdata_p(#xmlel{}) -> true;
remove_cdata_p(_) -> false.

-spec remove_cdata([xmlel_or_cdata()]) -> [xmlel_or_cdata()].
remove_cdata(L) ->
    [E || E <- L, remove_cdata_p(E)].

-spec get_attr(binary() | string(),
              [jlib:binary_pair()]) -> 'false' | {'value', binary() | string()}.
get_attr(AttrName, Attrs) ->
    case lists:keysearch(AttrName, 1, Attrs) of
        {value, {_, Val}} ->
            {value, Val};
        _ ->
            false
    end.


-spec get_attr_s(binary(), list()) -> binary().
get_attr_s(AttrName, Attrs) ->
    case lists:keysearch(AttrName, 1, Attrs) of
        {value, {_, Val}} ->
            Val;
        _ ->
            context_default(AttrName)
    end.

-spec get_tag_attr(binary(), exml:element()) -> 'false' | {'value', binary()}.
get_tag_attr(AttrName, #xmlel{attrs = Attrs}) ->
    get_attr(AttrName, Attrs).


-spec get_tag_attr_s(binary(), exml:element()) -> binary().
get_tag_attr_s(AttrName, #xmlel{attrs = Attrs}) ->
    get_attr_s(AttrName, Attrs).

-spec append_subtags(exml:element(), [xmlel_or_cdata()]) -> exml:element().
append_subtags(XE = #xmlel{children = SubTags1}, SubTags2) ->
    XE#xmlel{children = SubTags1 ++ SubTags2}.

-spec replace_tag_attr(Attr :: binary(), Value :: binary(), exml:element()
                      ) -> exml:element().
replace_tag_attr(Attr, Value, XE = #xmlel{attrs = Attrs}) ->
    Attrs1 = lists:keydelete(Attr, 1, Attrs),
    Attrs2 = [{Attr, Value} | Attrs1],
    XE#xmlel{attrs = Attrs2}.

%% @doc Given an element and a new subelement,
%% replace the instance of the subelement in element with the new subelement.
-spec replace_subelement(exml:element(), exml:element()) -> exml:element().
replace_subelement(XE = #xmlel{children = SubEls}, NewSubEl) ->
    {_, NameNewSubEl, _, _} = NewSubEl,
    SubEls2 = lists:keyreplace(NameNewSubEl, 2, SubEls, NewSubEl),
    XE#xmlel{children = SubEls2}.

-spec context_default(binary() | string()) -> <<>> | [].
context_default(Attr) when is_list(Attr) ->
    "";
context_default(_Attr) ->
    <<>>.
