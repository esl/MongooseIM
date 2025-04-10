%%%----------------------------------------------------------------------
%%% File    : jlib.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : General XMPP library.
%%% Created : 23 Nov 2002 by Alexey Shchepin <alexey@process-one.net>
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

-module(jlib).
-author('alexey@process-one.net').
-xep([{xep, 59}, {version, "1.0"}]).
-xep([{xep, 86}, {version, "1.0"}]).
-export([make_result_iq_reply/1,
         make_error_reply/2,
         make_error_reply/3,
         make_invitation/3,
         make_config_change_message/1,
         replace_from_to/3,
         remove_attr/2,
         iq_query_info/1,
         iq_query_or_response_info/1,
         iq_to_xml/1,
         timestamp_to_xml/3,
         rsm_encode/1,
         rsm_decode/1,
         stanza_error/3,
         stanza_error/5,
         stanza_errort/5,
         stream_error/1,
         stream_errort/3,
         maybe_append_delay/4,
         remove_delay_tags/1]).

-export([remove_cdata/1,
         append_subtags/2,
         replace_tag_attr/3,
         replace_subelement/2]).

-ignore_xref([make_result_iq_reply/1]).

-include_lib("exml/include/exml.hrl").
-include_lib("exml/include/exml_stream.hrl"). % only used to define stream types
-include("jlib.hrl").
-include("mongoose.hrl").
-include("mongoose_rsm.hrl").

%% Stream types defined in exml/include/exml_stream.hrl
-type xmlstreamerror()  :: #xmlstreamerror{}.
-type xmlstreamel() :: exml:element() | exml_stream:start() | exml_stream:stop() | xmlstreamerror().

-type iq() :: #iq{}.

-type rsm_in() :: #rsm_in{}.
-type rsm_out() :: #rsm_out{}.

%% Copied from calendar:rfc3339_string() (because it is not exported)
-type rfc3339_string() :: [byte(), ...].

-export_type([xmlstreamel/0, xmlstreamerror/0,
              rsm_in/0, rsm_out/0,
              iq/0,
              rfc3339_string/0]).

-define(IS_EMPTY(X), (X =:= #{})).

-spec make_result_iq_reply(exml:element()) -> exml:element();
                          (iq()) -> iq().
make_result_iq_reply(XE = #xmlel{}) ->
    NewAttrs = make_result_iq_reply_attrs(XE),
    XE#xmlel{attrs = NewAttrs};
make_result_iq_reply(IQ = #iq{}) ->
    IQ#iq{type = result}.

-spec make_result_iq_reply_attrs(exml:element()) -> exml:attrs().
make_result_iq_reply_attrs(#xmlel{attrs = Attrs}) ->
    Attrs1 = swap_from_to_attrs(Attrs),
    Attrs1#{<<"type">> => <<"result">>}.

-spec make_error_reply(exml:element() | mongoose_acc:t(), exml:child()) ->
    exml:element() | {mongoose_acc:t(), exml:element() | {error, {already_an_error, _, _}}}.
make_error_reply(#xmlel{} = Elem, Error) ->
    ?LOG_DEBUG(#{what => make_error_reply,
                 exml_packet => Elem, error_element => Error}),
    make_error_reply_from_element(Elem, Error);
make_error_reply(Acc, Error) ->
    make_error_reply(Acc, mongoose_acc:element(Acc), Error).

make_error_reply(Acc, Packet, Error) ->
    ?LOG_DEBUG(#{what => make_error_reply,
                 acc => Acc, error_element => Error}),
    case mongoose_acc:get(flag, error, false, Acc) of
        true ->
            ?LOG_ERROR(#{what => error_reply_to_error, exml_packet => Packet,
                         reason => Error}),
            {Acc, {error, {already_an_error, Packet, Error}}};
        _ ->
            {mongoose_acc:set(flag, error, true, Acc),
             make_error_reply_from_element(Packet, Error)}
    end.

make_error_reply_from_element(#xmlel{name = Name, children = SubTags} = Element, Error) ->
    NewAttrs = make_error_reply_attrs(Element),
    #xmlel{name = Name, attrs = NewAttrs, children = [Error | SubTags]}.

-spec make_error_reply_attrs(exml:element()) -> exml:attrs().
make_error_reply_attrs(#xmlel{attrs = Attrs}) ->
    Attrs1 = swap_from_to_attrs(Attrs),
    Attrs1#{<<"type">> => <<"error">>}.

-spec make_config_change_message(binary()) -> exml:element().
make_config_change_message(Status) ->
    #xmlel{name = <<"message">>, attrs = #{<<"type">> => <<"groupchat">>},
           children = [#xmlel{name = <<"x">>,
                              attrs = #{<<"xmlns">> => ?NS_MUC_USER},
                              children = [#xmlel{name = <<"status">>,
                                                 attrs = #{<<"code">> => Status}}]}]}.


-spec make_invitation(From :: jid:jid(), Password :: binary(),
                      Reason :: binary()) -> exml:element().
make_invitation(From, Password, Reason) ->
    Children = case Reason of
                    <<>> -> [];
                    _ -> [#xmlel{name = <<"reason">>,
                        children = [#xmlcdata{content = Reason}]}]
    end,
    Elements = [#xmlel{name = <<"invite">>,
                       attrs = #{<<"from">> => jid:to_binary(From)},
                       children = Children}],

    Elements2 = case Password of
        <<>> -> Elements;
        _ -> [#xmlel{name = <<"password">>,
                     children = [#xmlcdata{content = Password}]} | Elements]
                end,

    #xmlel{name = <<"message">>,
           children = [#xmlel{name = <<"x">>,
                              attrs = #{<<"xmlns">> => ?NS_MUC_USER},
                              children = Elements2}]}.

-spec replace_from_to_attrs(From :: binary(),
                            To :: binary() | undefined,
                            exml:attrs()) -> exml:attrs().
replace_from_to_attrs(From, undefined, Attrs) ->
    Attrs1 = maps:remove(<<"to">>, Attrs),
    Attrs1#{<<"from">> => From};
replace_from_to_attrs(From, To, Attrs) ->
    Attrs#{<<"from">> => From, <<"to">> => To}.

-spec swap_from_to_attrs(exml:attrs()) -> exml:attrs().
swap_from_to_attrs(#{<<"from">> := From, <<"to">> := To} = Attrs) ->
    Attrs#{<<"from">> := To, <<"to">> := From};
swap_from_to_attrs(#{<<"from">> := From} = Attrs0) ->
    Attrs1 = maps:remove(<<"from">>, Attrs0),
    Attrs1#{<<"to">> => From};
swap_from_to_attrs(#{<<"to">> := To} = Attrs0) ->
    Attrs1 = maps:remove(<<"to">>, Attrs0),
    Attrs1#{<<"from">> => To};
swap_from_to_attrs(Attrs) ->
    Attrs.

%% Replaces from and to, or ensures they are defined to begin with.
-spec replace_from_to(From :: jid:simple_jid() | jid:jid(),
                      To :: undefined | jid:simple_jid() | jid:jid(),
                      XE :: exml:element()) -> exml:element().
replace_from_to(From, undefined, #xmlel{attrs = Attrs} = Packet) ->
    NewAttrs = replace_from_to_attrs(jid:to_binary(From), undefined, Attrs),
    Packet#xmlel{attrs = NewAttrs};
replace_from_to(From, To, #xmlel{attrs = Attrs} = Packet) ->
    NewAttrs = replace_from_to_attrs(jid:to_binary(From), jid:to_binary(To), Attrs),
    Packet#xmlel{attrs = NewAttrs}.

-spec remove_attr(binary(), exml:element()) -> exml:element().
remove_attr(Attr, XE = #xmlel{attrs = Attrs}) ->
    NewAttrs = maps:remove(Attr, Attrs),
    XE#xmlel{attrs = NewAttrs}.

-spec iq_query_info(exml:element()) -> 'invalid' | 'not_iq' | 'reply' | iq().
iq_query_info(El) ->
    iq_info_internal(El, request).


-spec iq_query_or_response_info(exml:element()) ->
                                'invalid' | 'not_iq' | 'reply' | iq().
iq_query_or_response_info(El) ->
    iq_info_internal(El, any).

-spec make_reply_from_type(binary()) -> {atom(), atom()}.
make_reply_from_type(<<"set">>) ->
    {set, request};
make_reply_from_type(<<"get">>) ->
    {get, request};
make_reply_from_type(<<"result">>) ->
    {result, reply};
make_reply_from_type(<<"error">>) ->
    {error, reply};
make_reply_from_type(_) ->
    {invalid, invalid}.

-spec extract_xmlns([exml:element()]) -> binary().
extract_xmlns([Element]) ->
    exml_query:attr(Element, <<"xmlns">>, <<>>);
extract_xmlns(_) ->
    <<>>.

-spec iq_info_internal(exml:element(), Filter :: 'any' | 'request') ->
                                'invalid' | 'not_iq' | 'reply' | iq().
iq_info_internal(#xmlel{name = Name, children = Els} = Element, Filter) when Name == <<"iq">> ->
    %% Filter is either request or any.  If it is request, any replies
    %% are converted to the atom reply.
    ID = exml_query:attr(Element, <<"id">>, <<>>),
    Type = exml_query:attr(Element, <<"type">>, <<>>),
    Lang = exml_query:attr(Element, <<"xml:lang">>, <<>>),
    {Type1, Class} = make_reply_from_type(Type),
    case {Type1, Class, Filter} of
        {invalid, _, _} ->
            invalid;
        {_, Class, Filter} when Class == request; Filter == any ->
            %% The iq record is a bit strange.  The sub_el field is an
            %% XML tuple for requests, but a list of XML tuples for
            %% responses.
            FilteredEls = remove_cdata(Els),
            {XMLNS, SubEl} =
                case {Class, FilteredEls} of
                    {request, [El2]} ->
                        {exml_query:attr(El2, <<"xmlns">>, <<>>), hd(FilteredEls)};
                    {reply, _} ->
                        %% Find the namespace of the first non-error
                        %% element, if there is one.
                        NonErrorEls = [El ||
                                        #xmlel{name = SubName} = El <- FilteredEls,
                                        SubName /= <<"error">>],
                        {extract_xmlns(NonErrorEls), FilteredEls};
                    _ ->
                        {<<>>, []}
                end,
            case {XMLNS, Class} of
                {<<>>, request} ->
                    invalid;
                _ ->
                    #iq{id = ID,
                        type = Type1,
                        xmlns = XMLNS,
                        lang = Lang,
                        sub_el = SubEl}
            end;
        {_, reply, _} ->
            reply
    end;
iq_info_internal(_, _) ->
    not_iq.

-spec iq_type_to_binary(set|get|result|error) -> invalid | binary().
iq_type_to_binary(set) -> <<"set">>;
iq_type_to_binary(get) -> <<"get">>;
iq_type_to_binary(result) -> <<"result">>;
iq_type_to_binary(error) -> <<"error">>;
iq_type_to_binary(_) -> invalid.

-spec iq_to_xml(iq()) -> exml:element().
iq_to_xml(#iq{id = ID, type = Type, sub_el = SubEl}) when ID /= "" ->
    #xmlel{name = <<"iq">>,
        attrs = #{<<"id">> => ID, <<"type">> => iq_type_to_binary(Type)},
        children = sub_el_to_els(SubEl)};
iq_to_xml(#iq{type = Type, sub_el = SubEl}) ->
    #xmlel{name = <<"iq">>,
        attrs = #{<<"type">> => iq_type_to_binary(Type)},
        children = sub_el_to_els(SubEl)}.

%% @doc Convert `#iq.sub_el' back to `#xmlel.children'.
%% @end
-spec sub_el_to_els([exml:element()] | exml:element()) -> [exml:element()].
%% for requests.
sub_el_to_els(#xmlel{}=E) -> [E];
%% for replies.
sub_el_to_els(Es) when is_list(Es) -> Es.

-spec rsm_decode(exml:element() | iq()) -> none | #rsm_in{}.
rsm_decode(#iq{sub_el = SubEl})->
    rsm_decode(SubEl);
rsm_decode(#xmlel{} = SubEl) ->
    case exml_query:subelement(SubEl, <<"set">>) of
        undefined ->
            none;
        #xmlel{name = <<"set">>, children = SubEls} ->
            lists:foldl(fun rsm_parse_element/2, #rsm_in{}, SubEls)
    end.

-spec rsm_parse_element(exml:element(), rsm_in()) -> rsm_in().
rsm_parse_element(#xmlel{name = <<"max">>, attrs = Attrs} = Elem, RsmIn) when ?IS_EMPTY(Attrs) ->
    CountStr = exml_query:cdata(Elem),
    {Count, _} = string:to_integer(binary_to_list(CountStr)),
    RsmIn#rsm_in{max = Count};
rsm_parse_element(#xmlel{name = <<"before">>, attrs = Attrs} = Elem, RsmIn) when ?IS_EMPTY(Attrs) ->
    UID = exml_query:cdata(Elem),
    RsmIn#rsm_in{direction = before, id = UID};
rsm_parse_element(#xmlel{name = <<"after">>, attrs = Attrs} = Elem, RsmIn) when ?IS_EMPTY(Attrs) ->
    UID = exml_query:cdata(Elem),
    RsmIn#rsm_in{direction = aft, id = UID};
rsm_parse_element(#xmlel{name = <<"index">>, attrs = Attrs} = Elem, RsmIn) when ?IS_EMPTY(Attrs) ->
    IndexStr = exml_query:cdata(Elem),
    {Index, _} = string:to_integer(binary_to_list(IndexStr)),
    RsmIn#rsm_in{index = Index};
rsm_parse_element(_, RsmIn)->
    RsmIn.

-spec rsm_encode(none | rsm_out()) -> [exml:element()].
rsm_encode(none) ->
    [];
rsm_encode(RsmOut) ->
    [#xmlel{name = <<"set">>, attrs = #{<<"xmlns">> => ?NS_RSM},
            children = lists:reverse(rsm_encode_out(RsmOut))}].

-spec rsm_encode_out(rsm_out()) -> [exml:element()].
rsm_encode_out(#rsm_out{count = Count, index = Index, first = First, last = Last})->
    El = rsm_encode_first(First, Index, []),
    El2 = rsm_encode_last(Last, El),
    rsm_encode_count(Count, El2).

-spec rsm_encode_first(First :: undefined | binary(),
                       Index :: 'undefined' | integer(),
                       Arr::[exml:element()]) -> [exml:element()].
rsm_encode_first(undefined, undefined, Arr) ->
    Arr;
rsm_encode_first(First, undefined, Arr) ->
    [#xmlel{name = <<"first">>, children = [#xmlcdata{content = First}]} | Arr];
rsm_encode_first(First, Index, Arr) ->
    [#xmlel{name = <<"first">>, attrs = #{<<"index">> => i2b(Index)},
            children = [#xmlcdata{content = First}]}|Arr].

-spec rsm_encode_last(Last :: 'undefined', Arr :: [exml:element()]) -> [exml:element()].
rsm_encode_last(undefined, Arr) -> Arr;
rsm_encode_last(Last, Arr) ->
    [#xmlel{name = <<"last">>, children = [#xmlcdata{content = Last}]} | Arr].

-spec rsm_encode_count(Count :: 'undefined' | pos_integer(),
                       Arr :: [exml:element()]) -> [exml:element()].
rsm_encode_count(undefined, Arr) -> Arr;
rsm_encode_count(Count, Arr) ->
    [#xmlel{name = <<"count">>, children = [#xmlcdata{content = i2b(Count)}]} | Arr].

-spec i2b(integer()) -> binary().
i2b(I) when is_integer(I) ->
    integer_to_binary(I).

-spec timestamp_to_xml(TimestampString :: rfc3339_string(),
                       FromJID :: jid:simple_jid() | jid:jid() | undefined,
                       Desc :: iodata() | undefined) -> exml:element().
timestamp_to_xml(TimestampString, FromJID, Desc) ->
    Text = case Desc of
               undefined -> [];
               _ -> [#xmlcdata{content = Desc}]
           end,
    From = case FromJID of
               undefined -> #{};
               _ -> #{<<"from">> =>jid:to_binary(FromJID)}
           end,
    #xmlel{name = <<"delay">>,
           attrs = From#{<<"xmlns">> => ?NS_DELAY,
                         <<"stamp">> => list_to_binary(TimestampString)},
           children = Text}.

-spec stanza_error( Code :: binary()
   , Type :: binary()
   , Condition :: binary()
   , SpecTag :: binary()
   , SpecNs :: binary() | undefined) -> exml:element().
stanza_error(Code, Type, Condition, SpecTag, SpecNs) ->
    Er = stanza_error(Code, Type, Condition),
    Spec = #xmlel{ name = SpecTag, attrs = #{<<"xmlns">> => SpecNs}},
    NCh = [Spec | Er#xmlel.children],
    Er#xmlel{children = NCh}.

%% TODO: remove `code' attribute (currently it used for backward-compatibility)

-spec stanza_error( Code :: binary()
                 , Type :: binary()
                 , Condition :: binary() | undefined) -> exml:element().
stanza_error(Code, Type, Condition) ->
  #xmlel{ name = <<"error">>
        , attrs = #{<<"code">> => Code, <<"type">> => Type}
        , children = [ #xmlel{ name = Condition
                            , attrs = #{<<"xmlns">> => ?NS_STANZAS}
                             }]
        }.

-spec stanza_errort( Code :: binary()
                  , Type :: binary()
                  , Condition :: binary()
                  , Lang :: ejabberd:lang()
                  , Text :: binary()) -> exml:element().
stanza_errort(Code, Type, Condition, Lang, Text) ->
  Txt = service_translations:do(Lang, Text),
  #xmlel{ name = <<"error">>
       , attrs = #{<<"code">> => Code, <<"type">> => Type}
       , children = [ #xmlel{ name = Condition
                            , attrs = #{<<"xmlns">> => ?NS_STANZAS}
                             }
                    , #xmlel{ name = <<"text">>
                            , attrs = #{<<"xmlns">> => ?NS_STANZAS}
                            , children = [#xmlcdata{ content = Txt }]
                             }]
        }.

-spec stream_error(Condition :: binary()) -> exml:element().
stream_error(Condition) ->
  #xmlel{ name = <<"stream:error">>
       , children = [ #xmlel{ name = Condition
                            , attrs = #{<<"xmlns">> => ?NS_STREAMS}
                             }
                     ]
        }.

-spec stream_errort( Condition :: binary()
                  , Lang :: ejabberd:lang()
                  , Text :: binary()) -> exml:element().
stream_errort(Condition, Lang, Text) ->
  Txt = service_translations:do(Lang, Text),
  #xmlel{ name = <<"stream:error">>
       , children = [ #xmlel{ name = Condition
                            , attrs = #{<<"xmlns">> => ?NS_STREAMS} }
                    , #xmlel{ name = <<"text">>
                            , attrs = #{ <<"xml:lang">> => Lang
                                       , <<"xmlns">> => ?NS_STREAMS}
                            , children = [ #xmlcdata{ content = Txt} ]}
                     ]
        }.

-spec maybe_append_delay(Packet :: exml:element(),
                         From :: jid:jid(),
                         TS :: integer(),
                         Desc :: undefined | iodata()) -> exml:element().
maybe_append_delay(Packet = #xmlel{children = Children}, From, TS, Desc) ->
    case exml_query:path(Packet, [{element, <<"delay">>}]) of
        undefined ->
            TsString = calendar:system_time_to_rfc3339(TS, [{offset, "Z"}, {unit, microsecond}]),
            DelayTag = jlib:timestamp_to_xml(TsString, From, Desc),
            Packet#xmlel{children = [DelayTag | Children]};
        _ ->
            Packet
    end.

remove_delay_tags(#xmlel{children = Children} = Packet) ->
    Fun = fun(#xmlel{name = <<"delay">>, attrs = #{<<"xmlns">> := ?NS_DELAY}}, Els) ->
                  Els;
             (#xmlel{name = <<"x">>, attrs = #{<<"xmlns">> := ?NS_DELAY91}}, Els) ->
                  Els;
             (R, Els) ->
                  [R | Els]
          end,
    NEls = lists:foldl(Fun, [], Children),
    Packet#xmlel{children = lists:reverse(NEls)}.

-spec remove_cdata([exml:child()]) -> [exml:element()].
remove_cdata(L) ->
    [E || E <- L, remove_cdata_p(E)].

-spec remove_cdata_p(exml:child()) -> boolean().
remove_cdata_p(#xmlel{}) -> true;
remove_cdata_p(_) -> false.

-spec append_subtags(exml:element(), [exml:child()]) -> exml:element().
append_subtags(XE = #xmlel{children = SubTags1}, SubTags2) ->
    XE#xmlel{children = SubTags1 ++ SubTags2}.

-spec replace_tag_attr(Attr :: binary(), Value :: binary(), exml:element()) -> exml:element().
replace_tag_attr(Attr, Value, XE = #xmlel{attrs = Attrs}) ->
    Attrs1 = Attrs#{Attr => Value},
    XE#xmlel{attrs = Attrs1}.

%% @doc Given an element and a new subelement,
%% replace the instance of the subelement in element with the new subelement.
-spec replace_subelement(exml:element(), exml:element()) -> exml:element().
replace_subelement(XE = #xmlel{children = SubEls}, NewSubEl) ->
    {_, NameNewSubEl, _, _} = NewSubEl,
    SubEls2 = lists:keyreplace(NameNewSubEl, 2, SubEls, NewSubEl),
    XE#xmlel{children = SubEls2}.
