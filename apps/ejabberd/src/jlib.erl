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
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
%%% 02111-1307 USA
%%%
%%%----------------------------------------------------------------------

-module(jlib).
-author('alexey@process-one.net').
-xep([{xep, 59}, {version, "1.0"}]).
-xep([{xep, 68}, {version, "1.2"}]).
-xep([{xep, 86}, {version, "1.0"}]).
-export([make_result_iq_reply/1,
         make_error_reply/2,
         make_invitation/3,
         make_config_change_message/1,
         make_voice_approval_form/3,
         form_field/1,
         replace_from_to_attrs/3,
         replace_from_to/3,
         remove_attr/2,
         iq_query_info/1,
         iq_query_or_response_info/1,
         iq_to_xml/1,
         parse_xdata_submit/1,
         timestamp_to_xml/4,
         timestamp_to_mam_xml/4,
         timestamp_to_iso/2,
         now_to_utc_binary/1,
         now_to_utc_string/1,
         datetime_binary_to_timestamp/1,
         decode_base64/1,
         encode_base64/1,
         ip_to_list/1,
         rsm_encode/1,
         rsm_decode/1,
         stanza_error/3,
         stanza_error/5,
         stanza_errort/5,
         stream_error/1,
         stream_errort/3,
         remove_delay_tags/1]).

-include_lib("exml/include/exml.hrl").
-include_lib("exml/include/exml_stream.hrl"). % only used to define stream types
-include("jlib.hrl").
-include("ejabberd.hrl").

%% Stream types defined in exml/include/exml_stream.hrl
-type xmlstreamstart()  :: #xmlstreamstart{}.
-type xmlstreamend()    :: #xmlstreamend{}.
-type xmlstreamel() :: xmlel() | xmlstreamstart() | xmlstreamend().

-type xmlcdata()  :: #xmlcdata{}.

-type xmlch() :: xmlel() | xmlcdata(). % (XML ch)ild

-type binary_pair() :: {binary(), binary()}.

-export_type([xmlel/0, xmlstreamstart/0, xmlstreamend/0, xmlstreamel/0,
              binary_pair/0,
              rsm_in/0, rsm_out/0,
              xmlcdata/0,
              xmlch/0
             ]).

%% Datetime format where all or some elements may be 'false' or integer()
-type maybe_datetime() :: {
                        { 'false' | non_neg_integer(),
                          'false' | non_neg_integer(),
                          'false' | non_neg_integer() },
                      integer(),
                      'false' | integer(),
                      'false' | integer()
                      }.
%% Timezone format where all or some elements may be 'false' or integer()
-type maybe_tz() :: {'false' | non_neg_integer(), 'false' | non_neg_integer()}.

%% Time format where all or some elements may be 'false' or integer()
-type maybe_time() :: {'false' | non_neg_integer(),
                       'false' | non_neg_integer(),
                       'false' | non_neg_integer()}.


-spec make_result_iq_reply(xmlel()) -> xmlel().
make_result_iq_reply(XE = #xmlel{attrs = Attrs}) ->
    NewAttrs = make_result_iq_reply_attrs(Attrs),
    XE#xmlel{attrs = NewAttrs}.


-spec make_result_iq_reply_attrs([binary_pair()]) -> [binary_pair(), ...].
make_result_iq_reply_attrs(Attrs) ->
    To = xml:get_attr(<<"to">>, Attrs),
    From = xml:get_attr(<<"from">>, Attrs),
    Attrs1 = lists:keydelete(<<"to">>, 1, Attrs),
    Attrs2 = lists:keydelete(<<"from">>, 1, Attrs1),
    Attrs3 = case To of
                 {value, ToVal} ->
                     [{<<"from">>, ToVal} | Attrs2];
                 _ ->
                     Attrs2
             end,
    Attrs4 = case From of
                 {value, FromVal} ->
                     [{<<"to">>, FromVal} | Attrs3];
                 _ ->
                     Attrs3
             end,
    Attrs5 = lists:keydelete(<<"type">>, 1, Attrs4),
    [{<<"type">>, <<"result">>} | Attrs5].


-spec make_error_reply(xmlel() | mongoose_acc:t(),
                       xmlcdata() | xmlel()) ->
    xmlel() | mongoose_acc:t().
make_error_reply(#xmlel{name = Name, attrs = Attrs,
                        children = SubTags}, Error) ->
    NewAttrs = make_error_reply_attrs(Attrs),
    #xmlel{name = Name, attrs = NewAttrs, children = SubTags ++ [Error]};
make_error_reply(Acc, Error) ->
    make_error_reply(mongoose_acc:get(to_send, Acc), Error).


-spec make_error_reply_attrs([binary_pair()]) -> [binary_pair(), ...].
make_error_reply_attrs(Attrs) ->
    To = xml:get_attr(<<"to">>, Attrs),
    From = xml:get_attr(<<"from">>, Attrs),
    Attrs1 = lists:keydelete(<<"to">>, 1, Attrs),
    Attrs2 = lists:keydelete(<<"from">>, 1, Attrs1),
    Attrs3 = case To of
                 {value, ToVal} ->
                     [{<<"from">>, ToVal} | Attrs2];
                 _ ->
                     Attrs2
             end,
    Attrs4 = case From of
                 {value, FromVal} ->
                     [{<<"to">>, FromVal} | Attrs3];
                 _ ->
                     Attrs3
             end,
    Attrs5 = lists:keydelete(<<"type">>, 1, Attrs4),
    Attrs6 = [{<<"type">>, <<"error">>} | Attrs5],
    Attrs6.


-spec make_config_change_message(binary()) -> xmlel().
make_config_change_message(Status) ->
    #xmlel{name = <<"message">>, attrs = [{<<"type">>, <<"groupchat">>}],
           children = [#xmlel{name = <<"x">>,
                              attrs = [{<<"xmlns">>, ?NS_MUC_USER}],
                              children = [#xmlel{name = <<"status">>,
                                                 attrs = [{<<"code">>, Status}]}]}]}.


-spec make_invitation(From :: ejabberd:jid(), Password :: binary(),
                      Reason :: binary()) -> xmlel().
make_invitation(From, Password, Reason) ->
    Elements = [#xmlel{name = <<"invite">>,
                       attrs = [{<<"from">>, jid:to_binary(From)}]}],
    Elements2 = case Password of
        <<>> -> Elements;
        _ -> [#xmlel{name = <<"password">>,
                     children = [#xmlcdata{content = Password}]} | Elements]
                end,
    Elements3 = case Reason of
        <<>> -> Elements2;
        _ -> [#xmlel{name = <<"reason">>,
                     children = [#xmlcdata{content = Reason}]} | Elements2]
                end,

    #xmlel{name = <<"message">>,
           children = [#xmlel{name = <<"x">>,
                              attrs = [{<<"xmlns">>, ?NS_MUC_USER}],
                              children = Elements3}]}.


-spec form_field({binary(), binary(), binary()}
               | {binary(), binary()}
               | {binary(), binary(), binary(), binary()}) -> xmlel().
form_field({Var, Type, Value, Label}) ->
    Field = form_field({Var, Type, Value}),
    Field#xmlel{attrs = [{<<"label">>, Label} | Field#xmlel.attrs]};
form_field({Var, Type, Value}) ->
    Field = form_field({Var, Value}),
    Field#xmlel{attrs = [{<<"type">>, Type} | Field#xmlel.attrs]};
form_field({Var, Value}) ->
    #xmlel{name = <<"field">>,
           attrs = [{<<"var">>, Var}],
           children = [#xmlel{name = <<"value">>, children = [#xmlcdata{content = Value}]}]}.


-spec make_voice_approval_form(From :: ejabberd:simple_jid() | ejabberd:jid(),
                               Nick :: binary(), Role :: binary()) -> xmlel().
make_voice_approval_form(From, Nick, Role) ->
  Fields = [{<<"FORM_TYPE">>, <<"hidden">>, ?NS_MUC_REQUEST},
    {<<"muc#role">>, <<"text-single">>, Role, <<"Request role">>},
    {<<"muc#jid">>, <<"jid-single">>, jid:to_binary(From), <<"User ID">>},
    {<<"muc#roomnick">>, <<"text-single">>, Nick, <<"Room Nickname">>},
    {<<"muc#request_allow">>, <<"boolean">>, <<"false">>, <<"Grant voice to this person?">>}
  ],
  #xmlel{name = <<"message">>,
        children = [
          #xmlel{name = <<"x">>,
          attrs = [{<<"xmlns">>, ?NS_XDATA}, {<<"type">>, <<"form">>}],
          children = [#xmlel{name = <<"title">>,
          children = [#xmlcdata{content = <<"Voice request">>}]},
            #xmlel{name = <<"instructions">>,
            children = [#xmlcdata{content = <<"To approve this request",
            " for voice, select the &quot;Grant voice to this person?&quot; checkbox",
            " and click OK. To skip this request, click the cancel button.">>}]} |
            [form_field(El) || El <- Fields]
          ]}
        ]}.


-spec replace_from_to_attrs(From :: binary(), To :: binary(), [binary_pair()]) -> [binary_pair()].
replace_from_to_attrs(From, To, Attrs) ->
    Attrs1 = lists:keydelete(<<"to">>, 1, Attrs),
    Attrs2 = lists:keydelete(<<"from">>, 1, Attrs1),
    Attrs3 = [{<<"to">>, To} | Attrs2],
    Attrs4 = [{<<"from">>, From} | Attrs3],
    Attrs4.


-spec replace_from_to(From :: ejabberd:simple_jid() | ejabberd:jid(),
                      To :: ejabberd:simple_jid() | ejabberd:jid(),
                      XE :: xmlel()) -> xmlel().
replace_from_to(From, To, XE = #xmlel{attrs = Attrs}) ->
    NewAttrs = replace_from_to_attrs(jid:to_binary(From),
                                     jid:to_binary(To),
                                     Attrs),
    XE#xmlel{attrs = NewAttrs}.


-spec remove_attr(binary(), xmlel()) -> xmlel().
remove_attr(Attr, XE = #xmlel{attrs = Attrs}) ->
    NewAttrs = lists:keydelete(Attr, 1, Attrs),
    XE#xmlel{attrs = NewAttrs}.

-spec iq_query_info(xmlel()) -> 'invalid' | 'not_iq' | 'reply' | ejabberd:iq().
iq_query_info(El) ->
    iq_info_internal(El, request).


-spec iq_query_or_response_info(xmlel()) ->
                                'invalid' | 'not_iq' | 'reply' | ejabberd:iq().
iq_query_or_response_info(El) ->
    iq_info_internal(El, any).


-spec iq_info_internal(xmlel(), Filter :: 'any' | 'request') ->
                                'invalid' | 'not_iq' | 'reply' | ejabberd:iq().
iq_info_internal(#xmlel{name = Name, attrs = Attrs,
                        children = Els}, Filter) when Name == <<"iq">> ->
    %% Filter is either request or any.  If it is request, any replies
    %% are converted to the atom reply.
    ID = xml:get_attr_s(<<"id">>, Attrs),
    Type = xml:get_attr_s(<<"type">>, Attrs),
    Lang = xml:get_attr_s(<<"xml:lang">>, Attrs),
    {Type1, Class} = case Type of
                         <<"set">> -> {set, request};
                         <<"get">> -> {get, request};
                         <<"result">> -> {result, reply};
                         <<"error">> -> {error, reply};
                         _ -> {invalid, invalid}
                     end,
    if
        Type1 == invalid ->
            invalid;
        Class == request; Filter == any ->
            %% The iq record is a bit strange.  The sub_el field is an
            %% XML tuple for requests, but a list of XML tuples for
            %% responses.
            FilteredEls = xml:remove_cdata(Els),
            {XMLNS, SubEl} =
                case {Class, FilteredEls} of
                    {request, [#xmlel{attrs = Attrs2}]} ->
                        {xml:get_attr_s(<<"xmlns">>, Attrs2),
                         hd(FilteredEls)};
                    {reply, _} ->
                        %% Find the namespace of the first non-error
                        %% element, if there is one.
                        NonErrorEls = [El ||
                                          #xmlel{name = SubName} = El
                                              <- FilteredEls,
                                          SubName /= <<"error">>],
                        {case NonErrorEls of
                             [NonErrorEl] ->
                                 xml:get_tag_attr_s(<<"xmlns">>, NonErrorEl);
                             _ ->
                                 <<>>
                         end,
                         FilteredEls};
                    _ ->
                        {<<>>, []}
                end,
            if XMLNS == <<>>, Class == request ->
                    invalid;
               true ->
                    #iq{id = ID,
                        type = Type1,
                        xmlns = XMLNS,
                        lang = Lang,
                        sub_el = SubEl}
            end;
        Class == reply, Filter /= any ->
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


-spec iq_to_xml(ejabberd:iq()) -> xmlel().
iq_to_xml(#iq{id = ID, type = Type, sub_el = SubEl}) ->
    if
        ID /= "" ->
            #xmlel{name = <<"iq">>,
                   attrs = [{<<"id">>, ID}, {<<"type">>, iq_type_to_binary(Type)}],
                   children = sub_el_to_els(SubEl)};
        true ->
            #xmlel{name = <<"iq">>,
                   attrs = [{<<"type">>, iq_type_to_binary(Type)}],
                   children = sub_el_to_els(SubEl)}
    end.


%% @doc Convert `#iq.sub_el' back to `#xmlel.children'.
%% @end
-spec sub_el_to_els([xmlel()] | xmlel()) -> [xmlel()].
%% for requests.
sub_el_to_els(#xmlel{}=E) -> [E];
%% for replies.
sub_el_to_els(Es) when is_list(Es) -> Es.


-spec parse_xdata_submit(xmlel()) -> 'invalid' | [{binary(), [binary()]}].
parse_xdata_submit(El) ->
    #xmlel{attrs = Attrs, children = Els} = El,
    case xml:get_attr_s(<<"type">>, Attrs) of
        <<"submit">> ->
            lists:reverse(parse_xdata_fields(Els, []));
        _ ->
            invalid
    end.


-spec parse_xdata_fields([xmlcdata() | xmlel()], [{binary(), [binary()]}]) ->
    [{binary(), [binary()]}].
parse_xdata_fields([], Res) ->
    Res;
parse_xdata_fields([#xmlel{name = Name, attrs = Attrs,
                           children = SubEls} | Els], Res) ->
    case Name of
        <<"field">> ->
            case xml:get_attr_s(<<"var">>, Attrs) of
                <<>> ->
                    parse_xdata_fields(Els, Res);
                Var ->
                    Field =
                        {Var, lists:reverse(parse_xdata_values(SubEls, []))},
                    parse_xdata_fields(Els, [Field | Res])
            end;
        _ ->
            parse_xdata_fields(Els, Res)
    end;
parse_xdata_fields([_ | Els], Res) ->
    parse_xdata_fields(Els, Res).


-spec parse_xdata_values([xmlcdata() | xmlel()], [binary()]) -> [binary()].
parse_xdata_values([], Res) ->
    Res;
parse_xdata_values([#xmlel{name = Name,
                           children = SubEls} | Els], Res) ->
    case Name of
        <<"value">> ->
            Val = xml:get_cdata(SubEls),
            parse_xdata_values(Els, [Val | Res]);
        _ ->
            parse_xdata_values(Els, Res)
    end;
parse_xdata_values([_ | Els], Res) ->
    parse_xdata_values(Els, Res).


-spec rsm_decode(xmlel() | iq()) -> 'none' | #rsm_in{}.
rsm_decode(#iq{sub_el=SubEl})->
    rsm_decode(SubEl);
rsm_decode(#xmlel{}=SubEl) ->
    case xml:get_subtag(SubEl, <<"set">>) of
        false ->
            none;
        #xmlel{name = <<"set">>, children = SubEls} ->
            lists:foldl(fun rsm_parse_element/2, #rsm_in{}, SubEls)
    end.


-spec rsm_parse_element(xmlel(), rsm_in()) -> rsm_in().
rsm_parse_element(#xmlel{name = <<"max">>, attrs = []}=Elem, RsmIn) ->
    CountStr = xml:get_tag_cdata(Elem),
    {Count, _} = string:to_integer(binary_to_list(CountStr)),
    RsmIn#rsm_in{max=Count};
rsm_parse_element(#xmlel{name = <<"before">>,
                         attrs = []}=Elem, RsmIn) ->
    UID = xml:get_tag_cdata(Elem),
    RsmIn#rsm_in{direction=before, id=UID};
rsm_parse_element(#xmlel{name = <<"after">>, attrs = []}=Elem, RsmIn) ->
    UID = xml:get_tag_cdata(Elem),
    RsmIn#rsm_in{direction=aft, id=UID};
rsm_parse_element(#xmlel{name = <<"index">>, attrs = []}=Elem, RsmIn) ->
    IndexStr = xml:get_tag_cdata(Elem),
    {Index, _} = string:to_integer(binary_to_list(IndexStr)),
    RsmIn#rsm_in{index=Index};
rsm_parse_element(_, RsmIn)->
    RsmIn.


-spec rsm_encode('none' | rsm_out()) -> [xmlel()].
rsm_encode(none)->
    [];
rsm_encode(RsmOut)->
    [#xmlel{name = <<"set">>, attrs = [{<<"xmlns">>, ?NS_RSM}],
            children = lists:reverse(rsm_encode_out(RsmOut))}].


-spec rsm_encode_out(rsm_out()) -> [xmlel()].
rsm_encode_out(#rsm_out{count=Count, index=Index, first=First, last=Last})->
    El = rsm_encode_first(First, Index, []),
    El2 = rsm_encode_last(Last, El),
    rsm_encode_count(Count, El2).


-spec rsm_encode_first(First :: undefined | binary(),
                       Index :: 'undefined' | integer(),
                       Arr::[xmlel()]) -> [xmlel()].
rsm_encode_first(undefined, undefined, Arr) ->
    Arr;
rsm_encode_first(First, undefined, Arr) ->
    [#xmlel{name = <<"first">>, children = [#xmlcdata{content = First}]}|Arr];
rsm_encode_first(First, Index, Arr) ->
    [#xmlel{name = <<"first">>, attrs = [{<<"index">>, i2b(Index)}],
            children = [#xmlcdata{content = First}]}|Arr].


-spec rsm_encode_last(Last :: 'undefined', Arr :: [xmlel()]) -> [xmlel()].
rsm_encode_last(undefined, Arr) -> Arr;
rsm_encode_last(Last, Arr) ->
    [#xmlel{name = <<"last">>, children = [#xmlcdata{content = Last}]}|Arr].


-spec rsm_encode_count(Count :: 'undefined' | pos_integer(),
                       Arr :: [xmlel()]) -> [xmlel()].
rsm_encode_count(undefined, Arr)-> Arr;
rsm_encode_count(Count, Arr)->
    [#xmlel{name = <<"count">>, children = [#xmlcdata{content = i2b(Count)}]} | Arr].

-spec i2b(integer()) -> binary().
i2b(I) when is_integer(I) -> list_to_binary(integer_to_list(I)).

-type tzoffset() :: {TZh :: integer(), TZm :: integer()}.
-type tz() :: 'utc' | {Sign :: string() | binary(), tzoffset()} | tzoffset().

-type hour() :: 0..23.
-type minute() :: 0..59.
-type second() :: 0..59.
-type datetime_micro() :: {calendar:date(), {hour(), minute(), second(),
                                             Micro :: non_neg_integer()}}.
%% @doc Timezone = utc | {Sign::string(), {Hours, Minutes}} | {Hours, Minutes}
%% Hours = integer()
%% Minutes = integer()
-spec timestamp_to_iso(calendar:datetime() | datetime_micro(), tz()) -> {string(), io_lib:chars()}.
timestamp_to_iso({{Year, Month, Day}, {Hour, Minute, Second, Micro}}, Timezone) ->
    Timestamp_string =
        lists:flatten(
          io_lib:format("~4..0w-~2..0w-~2..0wT~2..0w:~2..0w:~2..0w.~6..0w",
                        [Year, Month, Day, Hour, Minute, Second, Micro])),
    Timezone_string =
        case Timezone of
            utc -> "Z";
            {Sign, {TZh, TZm}} ->
                io_lib:format("~s~2..0w:~2..0w", [Sign, TZh, TZm]);
            {TZh, TZm} ->
                Sign = case TZh >= 0 of
                           true -> "+";
                           false -> "-"
                       end,
                io_lib:format("~s~2..0w:~2..0w", [Sign, abs(TZh), TZm])
        end,
    {Timestamp_string, Timezone_string};
timestamp_to_iso({{Year, Month, Day}, {Hour, Minute, Second}}, Timezone) ->
    Timestamp_string =
        lists:flatten(
          io_lib:format("~4..0w-~2..0w-~2..0wT~2..0w:~2..0w:~2..0w",
                        [Year, Month, Day, Hour, Minute, Second])),
    Timezone_string =
        case Timezone of
            utc -> "Z";
            {Sign, {TZh, TZm}} ->
                io_lib:format("~s~2..0w:~2..0w", [Sign, TZh, TZm]);
            {TZh, TZm} ->
                Sign = case TZh >= 0 of
                           true -> "+";
                           false -> "-"
                       end,
                io_lib:format("~s~2..0w:~2..0w", [Sign, abs(TZh), TZm])
        end,
    {Timestamp_string, Timezone_string}.

-spec timestamp_to_xml(DateTime :: calendar:datetime() | datetime_micro(),
                       Timezone :: tz(),
                       FromJID :: ejabberd:simple_jid() | ejabberd:jid(),
                       Desc :: iodata()) -> xmlel().
timestamp_to_xml(DateTime, Timezone, FromJID, Desc) ->
    {T_string, Tz_string} = timestamp_to_iso(DateTime, Timezone),
    Text = [#xmlcdata{content = Desc}],
    From = jid:to_binary(FromJID),
    #xmlel{name = <<"delay">>,
           attrs = [{<<"xmlns">>, ?NS_DELAY},
                    {<<"from">>, From},
                    {<<"stamp">>, list_to_binary(T_string ++ Tz_string)}],
           children = Text}.


-spec timestamp_to_mam_xml(DateTime :: calendar:datetime(),
                           Timezone :: tz(),
                           QueryId :: any(),
                           MessageUID :: binary()) -> xmlel().
timestamp_to_mam_xml(DateTime, Timezone, QueryID, MessageUID) ->
    {T_string, Tz_string} = timestamp_to_iso(DateTime, Timezone),
    #xmlel{name = <<"delay">>,
           attrs = [{<<"xmlns">>, ?NS_DELAY},
                    {<<"stamp">>, list_to_binary(T_string ++ Tz_string)},
                    {<<"id">>, MessageUID}] ++
                   [{<<"queryid">>, QueryID} || QueryID =/= undefined, QueryID =/= <<>>]}.

-spec now_to_utc_string(erlang:timestamp()) -> string().
now_to_utc_string({MegaSecs, Secs, MicroSecs}) ->
    {{Year, Month, Day}, {Hour, Minute, Second}} =
        calendar:now_to_universal_time({MegaSecs, Secs, MicroSecs}),
    lists:flatten(
      io_lib:format("~4..0w-~2..0w-~2..0wT~2..0w:~2..0w:~2..0w.~6..0wZ",
                    [Year, Month, Day, Hour, Minute, Second, MicroSecs])).
now_to_utc_binary(Timestamp) ->
    list_to_binary(now_to_utc_string(Timestamp)).


%% @doc yyyy-mm-ddThh:mm:ss[.sss]{Z|{+|-}hh:mm} -> {MegaSecs, Secs, MicroSecs}
-spec datetime_binary_to_timestamp(binary()) -> undefined | erlang:timestamp().
datetime_binary_to_timestamp(TimeBin) ->
    %% Operations on short strings are actually faster than on binaries,
    %% even if we include time for binary_to_list/1
    case catch parse_datetime(binary_to_list(TimeBin)) of
        {'EXIT', _Err} ->
            undefined;
        TimeStamp ->
            TimeStamp
    end.

-spec parse_datetime(string()) -> erlang:timestamp().
parse_datetime(TimeStr) ->
    [Date, Time] = string:tokens(TimeStr, "T"),
    D = parse_date(Date),
    {T, MS, TZH, TZM} = parse_time(Time),
    S = calendar:datetime_to_gregorian_seconds({D, T}),
    S1 = calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}),
    Seconds = (S - S1) - TZH * 60 * 60 - TZM * 60,
    {Seconds div 1000000, Seconds rem 1000000, MS}.

%% @doc yyyy-mm-dd
-spec parse_date(nonempty_string()) -> 'false' | calendar:date().
parse_date(Date) ->
    [Y, M, D] = string:tokens(Date, "-"),
    Date1 = {list_to_integer(Y), list_to_integer(M), list_to_integer(D)},
    case calendar:valid_date(Date1) of
        true ->
            Date1;
        _ ->
            false
    end.


%% @doc hh:mm:ss[.sss]TZD
-spec parse_time(nonempty_string()) -> 'false' | maybe_datetime().
parse_time(Time) ->
    case string:str(Time, "Z") of
        0 ->
            parse_time_with_timezone(Time);
        _ ->
            [T | _] = string:tokens(Time, "Z"),
            {TT, MS} = parse_time1(T),
            {TT, MS, 0, 0}
    end.


-spec parse_time_with_timezone(nonempty_string()) -> 'false' | maybe_datetime().
parse_time_with_timezone(Time) ->
    case string:str(Time, "+") of
        0 ->
            case string:str(Time, "-") of
                0 ->
                    false;
                _ ->
                    parse_time_with_timezone(Time, "-")
            end;
        _ ->
            parse_time_with_timezone(Time, "+")
    end.


-spec parse_time_with_timezone(nonempty_string(),
                               Delim :: [43 | 45, ...]) -> maybe_datetime().
parse_time_with_timezone(Time, Delim) ->
    [T, TZ] = string:tokens(Time, Delim),
    {TZH, TZM} = parse_timezone(TZ),
    {TT, MS} = parse_time1(T),
    case Delim of
        "-" ->
            {TT, MS, -TZH, -TZM};
        "+" ->
            {TT, MS, TZH, TZM}
    end.


-spec parse_timezone(nonempty_string()) -> maybe_tz().
parse_timezone(TZ) ->
    [H, M] = string:tokens(TZ, ":"),
    {[H1, M1], true} = check_list([{H, 12}, {M, 60}]),
    {H1, M1}.


-spec parse_time1(nonempty_string()) -> {maybe_time(), integer()}.
parse_time1(Time) ->
    [HMS | T] =  string:tokens(Time, "."),
    MS = case T of
             [] ->
                 0;
             [Val] ->
                 list_to_integer(string:left(Val, 6, $0))
         end,
    [H, M, S] = string:tokens(HMS, ":"),
    {[H1, M1, S1], true} = check_list([{H, 24}, {M, 60}, {S, 60}]),
    {{H1, M1, S1}, MS}.


-spec check_list([{nonempty_string(), 12 | 24 | 60}, ...]) ->
                                  {['false' | non_neg_integer(), ...], _}.
check_list(List) ->
    lists:mapfoldl(
      fun({L, N}, B)->
              V = list_to_integer(L),
              if
                  (V >= 0) and (V =< N) ->
                      {V, B};
                  true ->
                      {false, false}
              end
      end, true, List).


%%
%% Base64 stuff (based on httpd_util.erl)
%%

%% TODO: optimize binary way
-spec decode_base64(binary() | string()) -> binary() | string().
decode_base64(S) when erlang:is_binary(S)->
    list_to_binary(decode_base64(binary_to_list(S)));
decode_base64(S) ->
    decode1_base64([C || C <- S,
                         C /= $\s,
                         C /= $\t,
                         C /= $\n,
                         C /= $\r]).


-spec decode1_base64(string()) -> string().
decode1_base64([]) ->
    [];
decode1_base64([Sextet1, Sextet2, $=, $=|Rest]) ->
    Bits2x6=
        (d(Sextet1) bsl 18) bor
        (d(Sextet2) bsl 12),
    Octet1=Bits2x6 bsr 16,
    [Octet1|decode1_base64(Rest)];
decode1_base64([Sextet1, Sextet2, Sextet3, $=|Rest]) ->
    Bits3x6=
        (d(Sextet1) bsl 18) bor
        (d(Sextet2) bsl 12) bor
        (d(Sextet3) bsl 6),
    Octet1=Bits3x6 bsr 16,
    Octet2=(Bits3x6 bsr 8) band 16#ff,
    [Octet1, Octet2|decode1_base64(Rest)];
decode1_base64([Sextet1, Sextet2, Sextet3, Sextet4|Rest]) ->
    Bits4x6=
        (d(Sextet1) bsl 18) bor
        (d(Sextet2) bsl 12) bor
        (d(Sextet3) bsl 6) bor
        d(Sextet4),
    Octet1=Bits4x6 bsr 16,
    Octet2=(Bits4x6 bsr 8) band 16#ff,
    Octet3=Bits4x6 band 16#ff,
    [Octet1, Octet2, Octet3|decode1_base64(Rest)];
decode1_base64(_CatchAll) ->
    "".


-spec d(char()) -> char().
d(X) when X >= $A, X =<$Z ->
    X-65;
d(X) when X >= $a, X =<$z ->
    X-71;
d(X) when X >= $0, X =<$9 ->
    X+4;
d($+) -> 62;
d($/) -> 63;
d(_) -> 63.


%% TODO: optimize binary way
-spec encode_base64(binary() | string()) -> binary() | string().
encode_base64(B) when is_binary(B) ->
    list_to_binary(encode_base64(binary_to_list(B)));
encode_base64([]) ->
    [];
encode_base64([A]) ->
    [e(A bsr 2), e((A band 3) bsl 4), $=, $=];
encode_base64([A, B]) ->
    [e(A bsr 2), e(((A band 3) bsl 4) bor (B bsr 4)), e((B band 15) bsl 2), $=];
encode_base64([A, B, C|Ls]) ->
    encode_base64_do(A, B, C, Ls).
encode_base64_do(A, B, C, Rest) ->
    BB = (A bsl 16) bor (B bsl 8) bor C,
    [e(BB bsr 18), e((BB bsr 12) band 63),
     e((BB bsr 6) band 63), e(BB band 63)|encode_base64(Rest)].


-spec e(byte()) -> char() | none().
e(X) when X >= 0, X < 26 -> X+65;
e(X) when X>25, X<52 ->     X+71;
e(X) when X>51, X<62 ->     X-4;
e(62) ->                    $+;
e(63) ->                    $/;
e(X) ->                     exit({bad_encode_base64_token, X}).


%% @doc Convert Erlang inet IP to list
-spec ip_to_list(inet:ip4_address() | {inet:ip_address(), inet:port_number()}
                ) -> string().
ip_to_list({IP, _Port}) ->
    ip_to_list(IP);
ip_to_list({_, _, _, _, _, _, _, _} = Ipv6Address) ->
    inet_parse:ntoa(Ipv6Address);
%% This function clause could use inet_parse too:
ip_to_list({A, B, C, D}) ->
    lists:flatten(io_lib:format("~w.~w.~w.~w", [A, B, C, D]));
ip_to_list(IP) ->
    lists:flatten(io_lib:format("~w", [IP])).


-spec stanza_error( Code :: binary()
   , Type :: binary()
   , Condition :: binary()
   , SpecTag :: binary()
   , SpecNs :: binary() | undefined) -> #xmlel{}.
stanza_error(Code, Type, Condition, SpecTag, SpecNs) ->
    Er = stanza_error(Code, Type, Condition),
    Spec = #xmlel{ name = SpecTag, attrs = [{<<"xmlns">>, SpecNs}]},
    NCh = [Spec | Er#xmlel.children],
    Er#xmlel{children = NCh}.

%% TODO: remove<<"code" attribute (currently it used for backward-compatibility)
-spec stanza_error( Code :: binary()
                 , Type :: binary()
                 , Condition :: binary() | undefined) -> #xmlel{}.
stanza_error(Code, Type, Condition) ->
  #xmlel{ name = <<"error">>
       , attrs = [{<<"code">>, Code}, {<<"type">>, Type}]
       , children = [ #xmlel{ name = Condition
                            , attrs = [{<<"xmlns">>, ?NS_STANZAS}]
                             }]
        }.

-spec stanza_errort( Code :: binary()
                  , Type :: binary()
                  , Condition :: binary()
                  , Lang :: ejabberd:lang()
                  , Text :: binary()) -> #xmlel{}.
stanza_errort(Code, Type, Condition, Lang, Text) ->
  Txt = translate:translate(Lang, Text),
  #xmlel{ name = <<"error">>
       , attrs = [{<<"code">>, Code}, {<<"type">>, Type}]
       , children = [ #xmlel{ name = Condition
                            , attrs = [{<<"xmlns">>, ?NS_STANZAS}]
                             }
                    , #xmlel{ name = <<"text">>
                            , attrs = [{<<"xmlns">>, ?NS_STANZAS}]
                            , children = [#xmlcdata{ content = Txt }]
                             }]
        }.

-spec stream_error(Condition :: binary()) -> #xmlel{}.
stream_error(Condition) ->
  #xmlel{ name = <<"stream:error">>
       , children = [ #xmlel{ name = Condition
                            , attrs = [{<<"xmlns">>, ?NS_STREAMS}]
                             }
                     ]
        }.

-spec stream_errort( Condition :: binary()
                  , Lang :: ejabberd:lang()
                  , Text :: binary()) -> #xmlel{}.
stream_errort(Condition, Lang, Text) ->
  Txt = translate:translate(Lang, Text),
  #xmlel{ name = <<"stream:error">>
       , children = [ #xmlel{ name = Condition
                            , attrs = [{<<"xmlns">>, ?NS_STREAMS}] }
                    , #xmlel{ name = <<"text">>
                            , attrs = [ {<<"xml:lang">>, Lang}
                                      , {<<"xmlns">>, ?NS_STREAMS}]
                            , children = [ #xmlcdata{ content = Txt} ]}
                     ]
        }.

remove_delay_tags(#xmlel{children = Els} = Packet) ->
    NEl = lists:foldl(
             fun(#xmlel{name= <<"delay">>, attrs = Attrs} = R, El)->
                              case xml:get_attr_s(<<"xmlns">>, Attrs) of
                                  ?NS_DELAY ->
                                      El;
                                  _ ->
                                    El ++ [R]
                              end;
                (#xmlel{name= <<"x">>, attrs = Attrs } = R, El) ->
                              case xml:get_attr_s(<<"xmlns">>, Attrs) of
                                  ?NS_DELAY91 ->
                                      El;
                                  _ ->
                                    El ++ [R]
                              end;
                (R, El) ->
                              El ++ [R]
                end, [], Els),
    Packet#xmlel{children=NEl}.
