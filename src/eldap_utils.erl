%%%----------------------------------------------------------------------
%%% File    : eldap_utils.erl
%%% Author  : Mickael Remond <mremond@process-one.net>
%%% Purpose : ejabberd LDAP helper functions
%%% Created : 12 Oct 2006 by Mickael Remond <mremond@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2013   ProcessOne
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

-module(eldap_utils).
-author('mremond@process-one.net').

-export([generate_subfilter/1,
         find_ldap_attrs/2,
         get_ldap_attr/2,
         get_user_part/2,
         make_filter/2,
         make_filter/3,
         get_state/2,
         case_insensitive_match/2,
         deref_aliases/1,
         process_user_filter/2,
         get_search_filter/1,
         decode_octet_string/3,
         uids_domain_subst/2,
         singleton_value/1,
         maybe_list2b/1,
         maybe_b2list/1]).

-ignore_xref([decode_octet_string/3, generate_subfilter/1, make_filter/2, uids_domain_subst/2]).

%% Used to be eldap:filter()
-type filter() :: term().

%% Used to be eldap:eldap_entry()
-type eldap_entry() :: term().

%% Used to be eldap:handle()
-type handle() :: term().

-export_type([handle/0,
              filter/0,
              eldap_entry/0]).

-type dn() :: binary().
-type deref() :: neverDerefAliases | derefInSearching
                 | derefFindingBaseObj | derefAlways.
%% Used to access mongoose_wpool
-type eldap_id() :: {HostType :: mongooseim:host_type(), Tag :: mongoose_wpool:tag()}.

-export_type([dn/0,
              deref/0,
              eldap_id/0]).

%% @doc Generate an 'or' LDAP query on one or several attributes
%% If there is only one attribute
-spec generate_subfilter([{binary()} | {binary(), binary()}]) -> binary().
generate_subfilter([UID]) ->
    subfilter(UID);
%% If there is several attributes
generate_subfilter(UIDs) ->
    iolist_to_binary(["(|", [subfilter(UID) || UID <- UIDs], ")"]).


%% @doc Subfilter for a single attribute
-spec subfilter({binary()} | {binary(), binary()}) -> binary().
subfilter({UIDAttr, UIDAttrFormat}) ->
    %% The default UiDAttrFormat is %u
    <<$(, UIDAttr/binary, $=, UIDAttrFormat/binary, $)>>;
subfilter({UIDAttr}) ->
    %% The default UiDAttrFormat is <<"%u">>
    <<$(, UIDAttr/binary, $=, "%u)">>.


%% @doc Not tail-recursive, but it is not very terribly.
%% It stops finding on the first not empty value.
-spec find_ldap_attrs([{binary()} | {binary(), binary()}],
                      [{binary(), [binary()]}]) -> <<>> | {binary(), binary()}.
find_ldap_attrs([{Attr} | Rest], Attributes) ->
    find_ldap_attrs([{Attr, <<"%u">>} | Rest], Attributes);
find_ldap_attrs([{Attr, Format} | Rest], Attributes) ->
    case get_ldap_attr(Attr, Attributes) of
        Value when Value /= <<>>, Value /= [] ->
            {Value, Format};
        _ ->
            find_ldap_attrs(Rest, Attributes)
    end;
find_ldap_attrs([], _) ->
    <<>>.


-spec get_ldap_attr(binary(), [{binary(), [binary()]}]) -> binary().
get_ldap_attr(LDAPAttr, Attributes) ->
    Res = lists:filter(
            fun({Name, _}) ->
                    case_insensitive_match(Name, LDAPAttr)
            end, Attributes),
    case singleton_value(Res) of
        {_, Value} -> eldap_utils:maybe_list2b(Value);
        _ -> <<>>
    end.


-spec get_user_part(binary(), binary()) -> {ok, binary()} | {error, badmatch}.
get_user_part(String, Pattern) ->
    F = fun(S, P) ->
        {First, _} = binary:match(P, <<"%u">>),
                TailLength = byte_size(P) - (First+1),
        binary:part(S, First, byte_size(S)-TailLength-First+1)
        end,
    case catch F(String, Pattern) of
            {'EXIT', _} ->
                {error, badmatch};
        Result ->
            case catch re:replace(Pattern, <<"%u">>, Result, [global, {return, binary}]) of
                {'EXIT', _} ->
                    {error, badmatch};
                StringRes ->
                    case case_insensitive_match(StringRes, String) of
                        true ->
                            {ok, Result};
                        false ->
                            {error, badmatch}
                    end
            end
    end.


-spec generate_substring_list(binary())
      -> [{'any', binary()} | {'final', binary()} | {'initial', binary()}].
generate_substring_list(Value)->
    Splits = binary:split(Value, <<"*">>, [global]),
    {Acc, S}=case Splits of
        [<<"">>|T]->{[], maybe_b2list(T)};
        [H|T]-> {[{initial, maybe_b2list(H)}], T}
    end,
    lists:reverse(generate_substring_list(S, Acc)).
generate_substring_list([<<"">>], Acc)->
    Acc;
generate_substring_list([Last], Acc)->
    [{final, Last}|Acc];
generate_substring_list([H|T], Acc)->
    generate_substring_list(T, [{any, H}|Acc]).


-spec make_filter([{binary(), [binary()]}], [{binary(), binary()}]) -> any().
make_filter(Data, UIDs) ->
    make_filter(Data, UIDs, 'and').

-spec make_filter([{binary(), [binary()]}], [{binary(), binary()}],
                  'or' | 'and') -> any().
make_filter(Data, UIDs, Op) ->
    NewUIDs = [{U, eldap_filter:do_sub(
                     UF, [{<<"%u">>, <<"*%u*">>, 1}])} || {U, UF} <- UIDs],
    Filter = lists:flatmap(
               traverse_filter_fun(NewUIDs), Data),
    case Filter of
        [F] ->
            F;
        _ ->
            eldap:Op(Filter)
    end.

traverse_filter_fun(NewUIDs) ->
  fun(Entry) ->
    match_filter_name(Entry, NewUIDs)
  end.

match_filter_name({<<"%u">>, [Value | _]}, NewUIDs) when Value /= <<"">> ->
  case eldap_filter:parse(
    generate_subfilter(NewUIDs),
    [{<<"%u">>, Value}]) of
    {ok, F} -> [F];
    _ -> []
  end;
match_filter_name({Name, [Value | _]}, _NewUIDs) when Value /= <<"">> ->
  case binary:match(Value, <<"*">>) of
    nomatch -> [eldap:equalityMatch(maybe_b2list(Name), maybe_b2list(Value))];
    _ -> [eldap:substrings(maybe_b2list(Name),
      generate_substring_list(Value))]
  end;
match_filter_name(_, _) ->
  [].

-spec case_insensitive_match(binary(), binary()) -> boolean().
case_insensitive_match(X, Y) ->
    X1 = string:to_lower(maybe_b2list(X)),
    Y1 = string:to_lower(maybe_b2list(Y)),
  case X1 == Y1 of
    true -> true;
    _-> false
  end.


-spec get_state(mongooseim:host_type(), atom()) -> any().
get_state(HostType, Module) ->
    Proc = gen_mod:get_module_proc(HostType, Module),
    gen_server:call(Proc, get_state).


%% @doc From the list of uids attribute: we look from alias domain (%d) and make
%% the substitution with the actual host domain. This helps when you need to
%% configure many virtual domains.
-spec uids_domain_subst(binary(), [{binary(), binary()}]) ->
                               [{binary(), binary()}].
uids_domain_subst(Host, UIDs) ->
    lists:map(fun({U, V}) ->
                      {U, eldap_filter:do_sub(V, [{<<"%d">>, Host}])};
                  (A) -> A
              end,
              UIDs).

deref_aliases(never) -> neverDerefAliases;
deref_aliases(searching) -> derefInSearching;
deref_aliases(finding) -> derefFindingBaseObj;
deref_aliases(always) -> derefAlways.

process_user_filter(UIDs, RawUserFilter) ->
    SubFilter = generate_subfilter(UIDs),
    case RawUserFilter of
        <<>> ->
            SubFilter;
        F ->
            <<"(&", SubFilter/binary, F/binary, ")">>
    end.

get_search_filter(UserFilter) ->
    eldap_filter:do_sub(UserFilter, [{<<"%u">>, <<"*">>}]).

-spec singleton_value(list()) -> {binary(), binary()} | false.
singleton_value([{K, [V]}]) ->
    {K, V};
singleton_value([{_K, _V} = I]) ->
    I;
singleton_value(_) ->
    false.
%%----------------------------------------
%% Borrowed from asn1rt_ber_bin_v2.erl
%%----------------------------------------

%%% The tag-number for universal types
-define(N_BOOLEAN, 1).
-define(N_INTEGER, 2).
-define(N_BIT_STRING, 3).
-define(N_OCTET_STRING, 4).
-define(N_NULL, 5).
-define(N_OBJECT_IDENTIFIER, 6).
-define(N_OBJECT_DESCRIPTOR, 7).
-define(N_EXTERNAL, 8).
-define(N_REAL, 9).
-define(N_ENUMERATED, 10).
-define(N_EMBEDDED_PDV, 11).
-define(N_SEQUENCE, 16).
-define(N_SET, 17).
-define(N_NumericString, 18).
-define(N_PrintableString, 19).
-define(N_TeletexString, 20).
-define(N_VideotexString, 21).
-define(N_IA5String, 22).
-define(N_UTCTime, 23).
-define(N_GeneralizedTime, 24).
-define(N_GraphicString, 25).
-define(N_VisibleString, 26).
-define(N_GeneralString, 27).
-define(N_UniversalString, 28).
-define(N_BMPString, 30).


-spec decode_octet_string(_, _, list()) -> binary().
decode_octet_string(Buffer, Range, Tags) ->
%    NewTags = new_tags(HasTag, #tag{class=?UNIVERSAL, number=?N_OCTET_STRING}),
    decode_restricted_string(Buffer, Range, Tags).


-spec decode_restricted_string(_, _, list()) -> binary().
decode_restricted_string(Tlv, Range, TagsIn) ->
    Val = match_tags(Tlv, TagsIn),
    Val2 =
        case Val of
            PartList = [_H|_T] -> % constructed val
                collect_parts(PartList);
            Bin ->
                Bin
        end,
    check_and_convert_restricted_string(Val2, Range).


-spec check_and_convert_restricted_string(iolist(), _) -> binary().
check_and_convert_restricted_string(Val, Range) ->
    {StrLen, NewVal} = if is_binary(Val) ->
                              {size(Val), Val};
                         true ->
                              {length(Val), list_to_binary(Val)}
                      end,
    case Range of
        [] -> % No length constraint
            NewVal;
        {Lb, Ub} when StrLen >= Lb, Ub >= StrLen -> % variable length constraint
            NewVal;
        {{Lb, _Ub}, []} when StrLen >= Lb ->
            NewVal;
        {{Lb, _Ub}, _Ext=[Min|_]} when StrLen >= Lb; StrLen >= Min ->
            NewVal;
        {{Lb1, Ub1}, {Lb2, Ub2}} when StrLen >= Lb1, StrLen =< Ub1;
                                   StrLen =< Ub2, StrLen >= Lb2 ->
            NewVal;
        StrLen -> % fixed length constraint
            NewVal;
        {_, _} ->
            exit({error, {asn1, {length, Range, Val}}});
        _Len when is_integer(_Len) ->
            exit({error, {asn1, {length, Range, Val}}});
        _ -> % some strange constraint that we don't support yet
            NewVal
    end.

%%----------------------------------------
%% Decode the in buffer to bits
%%----------------------------------------
match_tags({T, V}, [T]) ->
    V;
match_tags({T, V}, [T|Tt]) ->
    match_tags(V, Tt);
match_tags([{T, V}], [T|Tt]) ->
    match_tags(V, Tt);
match_tags(Vlist = [{T, _V}|_], [T]) ->
    Vlist;
match_tags(Tlv, []) ->
    Tlv;
match_tags({Tag, _V}, [T|_Tt]) ->
    {error, {asn1, {wrong_tag, {Tag, T}}}}.


-spec collect_parts([{_, _}]) -> binary().
collect_parts(TlvList) ->
    collect_parts(TlvList, []).


-spec collect_parts([{_, _}], [any()]) -> binary().
collect_parts([{_, L}|Rest], Acc) when is_list(L) ->
    collect_parts(Rest, [collect_parts(L)|Acc]);
collect_parts([{?N_BIT_STRING, <<Unused, Bits/binary>>}|Rest], _Acc) ->
    collect_parts_bit(Rest, [Bits], Unused);
collect_parts([{_T, V}|Rest], Acc) ->
    collect_parts(Rest, [V|Acc]);
collect_parts([], Acc) ->
    list_to_binary(lists:reverse(Acc)).


-spec collect_parts_bit([{3, binary()}], [binary(), ...], non_neg_integer()) -> binary().
collect_parts_bit([{?N_BIT_STRING, <<Unused, Bits/binary>>}|Rest], Acc, Uacc) ->
    collect_parts_bit(Rest, [Bits|Acc], Unused+Uacc);
collect_parts_bit([], Acc, Uacc) ->
    maybe_list2b([Uacc|lists:reverse(Acc)]).

maybe_b2list(B) when is_binary(B) ->
  binary_to_list(B);
maybe_b2list(L) when is_list(L) ->
  L;
maybe_b2list(O) ->
  {error, {unknown_type, O}}.

maybe_list2b(L) when is_list(L) ->
  list_to_binary(L);
maybe_list2b(B) when is_binary(B) ->
  B;
maybe_list2b(O) ->
  {error, {unknown_type, O}}.
