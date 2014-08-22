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

-export([element_to_string/1,
         element_to_binary/1,
         crypt/1,
         remove_cdata/1,
         get_cdata/1, get_tag_cdata/1,
         get_attr/2, get_attr_s/2,
         get_tag_attr/2, get_tag_attr_s/2,
         get_subtag/2,
         append_subtags/2,
         get_path_s/2,
         start/0,
         replace_tag_attr/3]).

-export([escape_cdata_and_attr/1]).

-include("ejabberd.hrl").
-include("jlib.hrl").

-type xmlel_or_cdata() :: jlib:xmlel() | jlib:xmlcdata().

-define(ESCAPE_BINARY(CData), crypt(CData)).

-spec start() -> ok.
-ifdef(xml_nif).
start() ->
    SOPath = filename:join(ejabberd:get_so_path(), "xml"),
    case catch erlang:load_nif(SOPath, 0) of
        ok ->
            ok;
        Err ->
            ?WARNING_MSG("unable to load xml NIF: ~p", [Err])
    end.
-else.
start() ->
    ok.
-endif.

-spec escape_cdata_and_attr(jlib:xmlel()) -> #xmlcdata{}.
escape_cdata_and_attr(#xmlel{ children = Children, attrs = Attrs } = Data) ->
    Data#xmlel{ children = [ escape_cdata_and_attr(Child) || Child <- Children ],
                attrs = [escape_attr(Attr) || Attr <- Attrs]};
escape_cdata_and_attr(#xmlcdata{ content = Content }) ->
    exml:escape_cdata(Content);
escape_cdata_and_attr(Elem) ->
    Elem.

escape_attr({Name, Value}) when is_binary(Value) ->
    {Name, exml:escape_attr(Value)};
escape_attr(Attr) -> Attr.

-spec element_to_binary(jlib:xmlel()) -> binary().
element_to_binary(El) ->
    iolist_to_binary(element_to_string(El)).


-spec element_to_string(jlib:xmlel()) -> string() | none().
element_to_string(El) ->
    case catch element_to_string_nocatch(El) of
        {'EXIT', Reason} ->
            erlang:error({badxml, El, Reason});
        Result ->
            Result
    end.


-spec element_to_string_nocatch(jlib:xmlcdata() | jlib:xmlel()) -> string().
element_to_string_nocatch(El) ->
    case El of
        #xmlel{name = Name, attrs = Attrs, children = Els} ->
            if
                Els /= [] ->
                    [$<, Name, attrs_to_list(Attrs), $>,
                     [element_to_string_nocatch(E) || E <- Els],
                     $<, $/, Name, $>];
               true ->
                    [$<, Name, attrs_to_list(Attrs), $/, $>]
               end;
        %% We do not crypt CDATA binary, but we enclose it in XML CDATA
        #xmlcdata{content = CData} when is_binary(CData) ->
            ?ESCAPE_BINARY(CData);
        %% We crypt list and possibly binaries if full XML usage is
        %% disabled unsupported (implies a conversion to list).
        #xmlcdata{content = CData} ->
            crypt(CData)
    end.


-spec attrs_to_list([jlib:binary_pair()]) -> [string()].
attrs_to_list(Attrs) ->
    [attr_to_list(A) || A <- Attrs].


-spec attr_to_list(jlib:binary_pair()) -> string().
attr_to_list({Name, Value}) ->
    [$\s, Name, $=, $', crypt(Value), $'].


-spec crypt(binary() | string()) -> string().
crypt(S) when is_list(S) ->
    [case C of
         $& -> "&amp;";
         $< -> "&lt;";
         $> -> "&gt;";
         $" -> "&quot;";
         $' -> "&apos;";
         _ -> C
     end || C <- S];
crypt(S) when is_binary(S) ->
    crypt(binary_to_list(S)).

%% @doc Returns escape type needed for the text node
%% none, cdata, {cdata, [Positions]}
%% Positions is a list a integer containing positions of CDATA end tokens, so
%% that they can be escaped
-spec cdata_need_escape(binary()) -> 'cdata' | 'none' | {'cdata',string()}.
cdata_need_escape(CData) ->
    cdata_need_escape(CData, 0, false, []).


-spec cdata_need_escape(binary(), non_neg_integer(), boolean(), string()
                       ) -> 'cdata' | 'none' | {'cdata', string()}.
cdata_need_escape(<<>>, _, false, _) ->
    none;
cdata_need_escape(<<>>, _, true, []) ->
    cdata;
cdata_need_escape(<<>>, _, true, CDataEndTokens) ->
    {cdata, lists:reverse(CDataEndTokens)};
cdata_need_escape(<<$],$],$>,Rest/binary>>, CurrentPosition,
                  _XMLEscape, CDataEndTokens) ->
    NewPosition = CurrentPosition + 3,
    cdata_need_escape(Rest, NewPosition, true,
                      [CurrentPosition+1|CDataEndTokens]);
%% Only <, & need to be escaped in XML text node
%% See reference: http://www.w3.org/TR/xml11/#syntax
cdata_need_escape(<<$<,Rest/binary>>, CurrentPosition,
                  _XMLEscape, CDataEndTokens) ->
    cdata_need_escape(Rest, CurrentPosition+1, true, CDataEndTokens);
cdata_need_escape(<<$&,Rest/binary>>, CurrentPosition,
                  _XMLEscape, CDataEndTokens) ->
    cdata_need_escape(Rest, CurrentPosition+1, true, CDataEndTokens);
cdata_need_escape(<<_:8,Rest/binary>>, CurrentPosition,
                  XMLEscape, CDataEndTokens) ->
    cdata_need_escape(Rest, CurrentPosition+1, XMLEscape,
                      CDataEndTokens).

%% @doc escape cdata that contain CDATA end tokens
%% EndTokens is a list of position of end tokens (integer)
%% This is supposed to be a very rare case: You need to generate several
%% fields, splitting it in the middle of the end token.
%% See example: http://en.wikipedia.org/wiki/CDATA#Uses_of_CDATA_sections
-spec escape_cdata(binary(), string()) -> [binary()].
escape_cdata(CData, EndTokens) ->
    escape_cdata(CData, 0, EndTokens, []).
escape_cdata(<<>>, _CurrentPosition, [], Acc) ->
    lists:reverse(Acc);
escape_cdata(Rest, CurrentPosition, [], Acc) ->
    CDATA1 = <<"<![CDATA[">>,
    CDATA2 = <<"]]>">>,
    escape_cdata(<<>>, CurrentPosition, [], [CDATA2, Rest, CDATA1|Acc]);
escape_cdata(CData, Index, [Pos|Positions], Acc) ->
    CDATA1 = <<"<![CDATA[">>,
    CDATA2 = <<"]]>">>,
    Split = Pos-Index,
    {Part, Rest} = split_binary(CData, Split+1),
    %% Note: We build the list in reverse to optimize construction
    escape_cdata(Rest, Pos+1, Positions, [CDATA2, Part, CDATA1|Acc]).


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


-spec get_tag_cdata(jlib:xmlel()) -> binary().
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


-spec get_attr_s(string(), string()) -> string().
get_attr_s(AttrName, Attrs) ->
    case lists:keysearch(AttrName, 1, Attrs) of
        {value, {_, Val}} ->
            Val;
        _ ->
            context_default(AttrName)
    end.


-spec get_tag_attr(binary() | string(), jlib:xmlel()
                  ) -> 'false' | {'value',binary() | string()}.
get_tag_attr(AttrName, #xmlel{attrs = Attrs}) ->
    get_attr(AttrName, Attrs).


-spec get_tag_attr_s(string(), jlib:xmlel()) -> string().
get_tag_attr_s(AttrName, #xmlel{attrs = Attrs}) ->
    get_attr_s(AttrName, Attrs).


-spec get_subtag(jlib:xmlel(), binary()) -> 'false' | jlib:xmlel().
get_subtag(#xmlel{children = Els}, Name) ->
    get_subtag1(Els, Name).


-spec get_subtag1([xmlel_or_cdata()], binary()) -> 'false' | jlib:xmlel().
get_subtag1([El | Els], Name) ->
    case El of
        #xmlel{name = Name} ->
            El;
        _ ->
            get_subtag1(Els, Name)
    end;
get_subtag1([], _) ->
    false.


-spec append_subtags(jlib:xmlel(), [xmlel_or_cdata()]) -> jlib:xmlel().
append_subtags(XE = #xmlel{children = SubTags1}, SubTags2) ->
    XE#xmlel{children = SubTags1 ++ SubTags2}.


-spec get_path_s(jlib:xmlel(), string()) -> string().
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


-spec replace_tag_attr(Attr :: binary(), Value :: binary(), jlib:xmlel()
                      ) -> jlib:xmlel().
replace_tag_attr(Attr, Value, XE = #xmlel{attrs = Attrs}) ->
    Attrs1 = lists:keydelete(Attr, 1, Attrs),
    Attrs2 = [{Attr, Value} | Attrs1],
    XE#xmlel{attrs = Attrs2}.


-spec context_default(binary() | string()) -> <<>> | [].
context_default(Attr) when is_list(Attr) ->
    "";
context_default(_Attr) ->
    <<>>.
