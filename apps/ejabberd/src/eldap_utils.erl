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
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
%%% 02111-1307 USA
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
         get_opt/3,
         get_opt/4,
         get_mod_opt/3,
         get_mod_opt/4,
         get_config/2,
         decode_octet_string/3,
         uids_domain_subst/2,
         singleton_value/1]).

-include("ejabberd.hrl").
-include("eldap.hrl").

%% @doc Generate an 'or' LDAP query on one or several attributes
%% If there is only one attribute
-spec generate_subfilter([{binary()} | {binary(),binary()}]) -> binary().
generate_subfilter([UID]) ->
    subfilter(UID);
%% If there is several attributes
generate_subfilter(UIDs) ->
    iolist_to_binary(["(|", [subfilter(UID) || UID <- UIDs], ")"]).


%% @doc Subfilter for a single attribute
-spec subfilter({binary()} | {binary(),binary()}) -> binary().
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
        Value when is_binary(Value), Value /= <<>> ->
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
        {_, Value} -> Value;
        _ -> <<>>
    end.


-spec get_user_part(binary(), binary()) -> {ok, binary()} | {error, badmatch}.
get_user_part(String, Pattern) ->
    F = fun(S, P) ->
        {First,_} = binary:match(P,<<"%u">>),
                TailLength = byte_size(P) - (First+1),
        binary:part(S,First,byte_size(S)-TailLength-First+1)
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
      -> [{'any',binary()} | {'final',binary()} | {'initial',binary()}].
generate_substring_list(Value)->
    Splits = binary:split(Value,<<"*">>,[global]),
    {Acc,S}=case Splits of
        [<<"">>|T]->{[],T};
        [H|T]-> {[{initial,H}],T}
    end,
    lists:reverse(generate_substring_list(S,Acc)).
generate_substring_list([<<"">>],Acc)->
    Acc;
generate_substring_list([Last],Acc)->
    [{final,Last}|Acc];
generate_substring_list([H|T],Acc)->
    generate_substring_list(T,[{any,H}|Acc]).


-spec make_filter([{binary(), [binary()]}], [{binary(), binary()}]) -> any().
make_filter(Data, UIDs) ->
    make_filter(Data, UIDs, 'and').

-spec make_filter([{binary(), [binary()]}], [{binary(), binary()}],
                  'or' | 'and') -> any().
make_filter(Data, UIDs, Op) ->
    NewUIDs = [{U, eldap_filter:do_sub(
                     UF, [{<<"%u">>, <<"*%u*">>, 1}])} || {U, UF} <- UIDs],
    Filter = lists:flatmap(
               fun({Name, [Value | _]}) ->
                       case Name of
                           <<"%u">> when Value /= <<"">> ->
                               case eldap_filter:parse(
                                      generate_subfilter(NewUIDs),
                                      [{<<"%u">>, Value}]) of
                                   {ok, F} -> [F];
                                   _ -> []
                               end;
                           _ when Value /= <<"">> ->
                    case binary:match(Value,<<"*">>) of
                        nomatch -> [eldap:equalityMatch(Name,Value)];
                        _ -> [eldap:substrings(Name,generate_substring_list(Value))]
                    end;
                           _ ->
                               []
                       end
               end, Data),
    case Filter of
        [F] ->
            F;
        _ ->
            eldap:Op(Filter)
    end.


-spec case_insensitive_match(binary(), binary()) -> boolean().
case_insensitive_match(X, Y) ->
    X1 = string:to_lower(binary_to_list(X)),
    Y1 = string:to_lower(binary_to_list(Y)),
    if
        X1 == Y1 -> true;
        true -> false
    end.


-spec get_state(binary() | string(),atom()) -> any().
get_state(Server, Module) ->
    Proc = gen_mod:get_module_proc(Server, Module),
    gen_server:call(Proc, get_state).


%% @doc From the list of uids attribute: we look from alias domain (%d) and make
%% the substitution with the actual host domain. This helps when you need to
%% configure many virtual domains.
-spec uids_domain_subst(binary(), [{binary(), binary()}]) ->
                               [{binary(), binary()}].
uids_domain_subst(Host, UIDs) ->
    lists:map(fun({U,V}) ->
                      {U, eldap_filter:do_sub(V,[{<<"%d">>, Host}])};
                  (A) -> A
              end,
              UIDs).


-spec get_opt({atom(), binary()}, list(), fun()) -> any().
get_opt({Key, Host}, Opts, F) ->
    get_opt({Key, Host}, Opts, F, undefined).


-spec get_opt({atom(), binary()}, list(), fun(), any()) -> any().
get_opt({Key, Host}, Opts, F, Default) ->
    case gen_mod:get_opt(Key, Opts, undefined) of
        undefined ->
            case ejabberd_config:get_local_option({Key,Host}) of
                undefined ->
                    Default;
                Val ->
                    prepare_opt_val(Key, Val, F,Default)
            end;
        Val ->
            prepare_opt_val(Key, Val, F, Default)
    end.


-spec get_mod_opt(atom(), list(), fun()) -> any().
get_mod_opt(Key, Opts, F) ->
    get_mod_opt(Key,Opts,F,undefined).


-spec get_mod_opt(atom(), list(), fun(), any()) -> any().
get_mod_opt(Key, Opts, F, Default) ->
    case gen_mod:get_opt(Key, Opts, Default) of
        Default ->
            Default;
        Val ->
            prepare_opt_val(Key, Val, F, Default)
    end.


-type check_fun() :: fun((any()) -> any()) | {module(), atom()}.
-spec prepare_opt_val(any(), any(), check_fun(), any()) -> any().
prepare_opt_val(Opt, Val, F, Default) ->
    Res = case F of
              {Mod, Fun} ->
                  catch Mod:Fun(Val);
              _ ->
                  catch F(Val)
          end,
    case Res of
        {'EXIT', _} ->
            ?ERROR_MSG("Configuration problem:~n"
                      "** Option: ~p~n"
                      "** Invalid value: ~p~n"
                      "** Using as fallback: ~p",
                      [ Opt,
                        Val,
                        Default]),
            Default;
        _ ->
            Res
    end.


-spec get_config(binary(), list()) -> eldap:eldap_config().
get_config(Host, Opts) ->
    Servers = get_opt({ldap_servers, Host}, Opts,
                      fun(L) ->
                              [iolist_to_binary(H) || H <- L]
                      end, [<<"localhost">>]),
    Backups = get_opt({ldap_backups, Host}, Opts,
                      fun(L) ->
                              [iolist_to_binary(H) || H <- L]
                      end, []),
    Encrypt = get_opt({ldap_encrypt, Host}, Opts,
                      fun(tls) -> tls;
                         (starttls) -> starttls;
                         (none) -> none
                      end, none),
    TLSVerify = get_opt({ldap_tls_verify, Host}, Opts,
                        fun(hard) -> hard;
                           (soft) -> soft;
                           (false) -> false
                        end, false),
    TLSCAFile = get_opt({ldap_tls_cacertfile, Host}, Opts,
                        fun iolist_to_binary/1),
    TLSDepth = get_opt({ldap_tls_depth, Host}, Opts,
                       fun(I) when is_integer(I), I>=0 -> I end),
    Port = get_opt({ldap_port, Host}, Opts,
                   fun(I) when is_integer(I), I>0 -> I end,
                   case Encrypt of
                       tls -> ?LDAPS_PORT;
                       starttls -> ?LDAP_PORT;
                       _ -> ?LDAP_PORT
                   end),
    RootDN = get_opt({ldap_rootdn, Host}, Opts,
                     fun iolist_to_binary/1,
                     <<"">>),
    Password = get_opt({ldap_password, Host}, Opts,
                 fun iolist_to_binary/1,
                 <<"">>),
    Base = get_opt({ldap_base, Host}, Opts,
                   fun iolist_to_binary/1,
                   <<"">>),
    DerefAliases = get_opt({deref_aliases, Host}, Opts,
                           fun(never) -> never;
                              (searching) -> searching;
                              (finding) -> finding;
                              (always) -> always
                           end, never),
    #eldap_config{servers = Servers,
                  backups = Backups,
                  tls_options = [{encrypt, Encrypt},
                                 {tls_verify, TLSVerify},
                                 {tls_cacertfile, TLSCAFile},
                                 {tls_depth, TLSDepth}],
                  port = Port,
                  dn = RootDN,
                  password = Password,
                  base = Base,
                  deref_aliases = DerefAliases}.

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


-spec decode_octet_string(_,_,list()) -> binary().
decode_octet_string(Buffer, Range, Tags) ->
%    NewTags = new_tags(HasTag,#tag{class=?UNIVERSAL,number=?N_OCTET_STRING}),
    decode_restricted_string(Buffer, Range, Tags).


-spec decode_restricted_string(_,_,list()) -> binary().
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


-spec check_and_convert_restricted_string(iolist(),_) -> binary().
check_and_convert_restricted_string(Val, Range) ->
    {StrLen,NewVal} = if is_binary(Val) ->
                              {size(Val), Val};
                         true ->
                              {length(Val), list_to_binary(Val)}
                      end,
    case Range of
        [] -> % No length constraint
            NewVal;
        {Lb,Ub} when StrLen >= Lb, Ub >= StrLen -> % variable length constraint
            NewVal;
        {{Lb,_Ub},[]} when StrLen >= Lb ->
            NewVal;
        {{Lb,_Ub},_Ext=[Min|_]} when StrLen >= Lb; StrLen >= Min ->
            NewVal;
        {{Lb1,Ub1},{Lb2,Ub2}} when StrLen >= Lb1, StrLen =< Ub1;
                                   StrLen =< Ub2, StrLen >= Lb2 ->
            NewVal;
        StrLen -> % fixed length constraint
            NewVal;
        {_,_} ->
            exit({error,{asn1,{length,Range,Val}}});
        _Len when is_integer(_Len) ->
            exit({error,{asn1,{length,Range,Val}}});
        _ -> % some strange constraint that we don't support yet
            NewVal
    end.

%%----------------------------------------
%% Decode the in buffer to bits
%%----------------------------------------
match_tags({T,V},[T]) ->
    V;
match_tags({T,V}, [T|Tt]) ->
    match_tags(V,Tt);
match_tags([{T,V}],[T|Tt]) ->
    match_tags(V, Tt);
match_tags(Vlist = [{T,_V}|_], [T]) ->
    Vlist;
match_tags(Tlv, []) ->
    Tlv;
match_tags({Tag,_V},[T|_Tt]) ->
    {error,{asn1,{wrong_tag,{Tag,T}}}}.


-spec collect_parts([{_,_}]) -> binary().
collect_parts(TlvList) ->
    collect_parts(TlvList,[]).


-spec collect_parts([{_,_}],[any()]) -> binary().
collect_parts([{_,L}|Rest],Acc) when is_list(L) ->
    collect_parts(Rest,[collect_parts(L)|Acc]);
collect_parts([{?N_BIT_STRING,<<Unused,Bits/binary>>}|Rest],_Acc) ->
    collect_parts_bit(Rest,[Bits],Unused);
collect_parts([{_T,V}|Rest],Acc) ->
    collect_parts(Rest,[V|Acc]);
collect_parts([],Acc) ->
    list_to_binary(lists:reverse(Acc)).


-spec collect_parts_bit([{3,binary()}],[binary(),...],non_neg_integer()) -> binary().
collect_parts_bit([{?N_BIT_STRING,<<Unused,Bits/binary>>}|Rest],Acc,Uacc) ->
    collect_parts_bit(Rest,[Bits|Acc],Unused+Uacc);
collect_parts_bit([],Acc,Uacc) ->
    list_to_binary([Uacc|lists:reverse(Acc)]).
