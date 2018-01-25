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
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
%%% 02111-1307 USA
%%%
%%%----------------------------------------------------------------------

-module(xml).
-author('alexey@process-one.net').

-export([remove_cdata/1,
         get_cdata/1, get_tag_cdata/1,
         get_attr/2, get_attr_s/2,
         get_tag_attr/2, get_tag_attr_s/2,
         get_subtag/2,
         append_subtags/2,
         get_path_s/2,
         replace_tag_attr/3]).

-include("mongoose.hrl").
-include("jlib.hrl").

-type xmlel_or_cdata() :: jlib:xmlch().

-spec remove_cdata_p(xmlel_or_cdata()) -> boolean().
remove_cdata_p(#xmlel{}) -> true;
remove_cdata_p(_) -> false.


-spec remove_cdata([xmlel_or_cdata()]) -> [xmlel_or_cdata()].
remove_cdata(L) -> [E || E <- L, remove_cdata_p(E)].


-spec get_cdata([xmlel_or_cdata()]) -> binary().
get_cdata(L) ->
    list_to_binary(get_cdata(L, "")).


-spec get_cdata([xmlel_or_cdata()], [iolist()]) -> [iolist()].
get_cdata([#xmlcdata{content = CData} | L], S) ->
    get_cdata(L, [S, CData]);
get_cdata([_ | L], S) ->
    get_cdata(L, S);
get_cdata([], S) ->
    S.


-spec get_tag_cdata(exml:element()) -> binary().
get_tag_cdata(#xmlel{children = Els}) ->
    get_cdata(Els).


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


-spec get_subtag(exml:element(), binary()) -> 'false' | exml:element().
get_subtag(#xmlel{children = Els}, Name) ->
    get_subtag1(Els, Name).


-spec get_subtag1([xmlel_or_cdata()], binary()) -> 'false' | exml:element().
get_subtag1([El | Els], Name) ->
    case El of
        #xmlel{name = Name} ->
            El;
        _ ->
            get_subtag1(Els, Name)
    end;
get_subtag1([], _) ->
    false.


-spec append_subtags(exml:element(), [xmlel_or_cdata()]) -> exml:element().
append_subtags(XE = #xmlel{children = SubTags1}, SubTags2) ->
    XE#xmlel{children = SubTags1 ++ SubTags2}.


-spec get_path_s(exml:element(), [{elem, binary()} | {attr, binary()} | cdata]) ->
    iodata() | exml:element().
get_path_s(El, []) ->
    El;
get_path_s(El, [{elem, Name} | Path]) ->
    case get_subtag(El, Name) of
        false ->
            context_default(Name);
        SubEl ->
            get_path_s(SubEl, Path)
    end;
get_path_s(El, [{attr, Name}]) ->
    get_tag_attr_s(Name, El);
get_path_s(El, [cdata]) ->
    get_tag_cdata(El).


-spec replace_tag_attr(Attr :: binary(), Value :: binary(), exml:element()
                      ) -> exml:element().
replace_tag_attr(Attr, Value, XE = #xmlel{attrs = Attrs}) ->
    Attrs1 = lists:keydelete(Attr, 1, Attrs),
    Attrs2 = [{Attr, Value} | Attrs1],
    XE#xmlel{attrs = Attrs2}.


-spec context_default(binary() | string()) -> <<>> | [].
context_default(Attr) when is_list(Attr) ->
    "";
context_default(_Attr) ->
    <<>>.
