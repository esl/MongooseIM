%% Generated by the Erlang ASN.1 BER compiler. Version: 5.0.4
%% Purpose: Encoding and decoding of the types in XmppAddr.

-module('XmppAddr').
-compile(nowarn_unused_vars).
-dialyzer(no_improper_lists).
-include("XmppAddr.hrl").
-asn1_info([{vsn,'5.0.4'},
            {module,'XmppAddr'},
            {options,[{i,"/Users/thomassauren/projects/work/mong/MongooseIM/asngen"},
 ber,
 {outdir,"/Users/thomassauren/projects/work/mong/MongooseIM/asngen"},
 {i,"."},
 {i,"/Users/thomassauren/projects/work/mong/MongooseIM/asn1"}]}]).

-export([encoding_rule/0,maps/0,bit_string_format/0,
         legacy_erlang_types/0]).
-export(['dialyzer-suppressions'/1]).
-export([
enc_XmppAddr/2
]).

-export([
dec_XmppAddr/2
]).

-export([
'id-on-xmppAddr'/0
]).

-export([info/0]).

-export([encode/2,decode/2]).

encoding_rule() -> ber.

maps() -> false.

bit_string_format() -> bitstring.

legacy_erlang_types() -> false.

encode(Type, Data) ->
try iolist_to_binary(element(1, encode_disp(Type, Data))) of
  Bytes ->
    {ok,Bytes}
  catch
    Class:Exception when Class =:= error; Class =:= exit ->
      Stk = erlang:get_stacktrace(),
      case Exception of
        {error,{asn1,Reason}} ->
          {error,{asn1,{Reason,Stk}}};
        Reason ->
         {error,{asn1,{Reason,Stk}}}
      end
end.

decode(Type, Data) ->
try
   Result = decode_disp(Type, element(1, ber_decode_nif(Data))),
   {ok,Result}
  catch
    Class:Exception when Class =:= error; Class =:= exit ->
      Stk = erlang:get_stacktrace(),
      case Exception of
        {error,{asn1,Reason}} ->
          {error,{asn1,{Reason,Stk}}};
        Reason ->
         {error,{asn1,{Reason,Stk}}}
      end
end.

encode_disp('XmppAddr', Data) -> enc_XmppAddr(Data);
encode_disp(Type, _Data) -> exit({error,{asn1,{undefined_type,Type}}}).

decode_disp('XmppAddr', Data) -> dec_XmppAddr(Data);
decode_disp(Type, _Data) -> exit({error,{asn1,{undefined_type,Type}}}).

info() ->
   case ?MODULE:module_info(attributes) of
     Attributes when is_list(Attributes) ->
       case lists:keyfind(asn1_info, 1, Attributes) of
         {_,Info} when is_list(Info) ->
           Info;
         _ ->
           []
       end;
     _ ->
       []
   end.


%%================================
%%  XmppAddr
%%================================
enc_XmppAddr(Val) ->
    enc_XmppAddr(Val, [<<12>>]).

enc_XmppAddr(Val, TagIn) ->
encode_UTF8_string(Val, TagIn).


dec_XmppAddr(Tlv) ->
   dec_XmppAddr(Tlv, [12]).

dec_XmppAddr(Tlv, TagIn) ->
decode_UTF8_string(Tlv, TagIn).

'id-on-xmppAddr'() ->
{1,3,6,1,5,5,7,8,5}.


%%%
%%% Run-time functions.
%%%

'dialyzer-suppressions'(Arg) ->
    ok.

ber_decode_nif(B) ->
    asn1rt_nif:decode_ber_tlv(B).

collect_parts(TlvList) ->
    collect_parts(TlvList, []).

collect_parts([{_,L}|Rest], Acc) when is_list(L) ->
    collect_parts(Rest, [collect_parts(L)|Acc]);
collect_parts([{3,<<Unused,Bits/binary>>}|Rest], _Acc) ->
    collect_parts_bit(Rest, [Bits], Unused);
collect_parts([{_T,V}|Rest], Acc) ->
    collect_parts(Rest, [V|Acc]);
collect_parts([], Acc) ->
    list_to_binary(lists:reverse(Acc)).

collect_parts_bit([{3,<<Unused,Bits/binary>>}|Rest], Acc, Uacc) ->
    collect_parts_bit(Rest, [Bits|Acc], Unused + Uacc);
collect_parts_bit([], Acc, Uacc) ->
    list_to_binary([Uacc|lists:reverse(Acc)]).

decode_UTF8_string(Tlv, TagsIn) ->
    Val = match_tags(Tlv, TagsIn),
    case Val of
        [_|_] = PartList ->
            collect_parts(PartList);
        Bin ->
            Bin
    end.

encode_UTF8_string(UTF8String, TagIn) when is_binary(UTF8String) ->
    encode_tags(TagIn, UTF8String, byte_size(UTF8String));
encode_UTF8_string(UTF8String, TagIn) ->
    encode_tags(TagIn, UTF8String, length(UTF8String)).

encode_length(L) when L =< 127 ->
    {[L],1};
encode_length(L) ->
    Oct = minimum_octets(L),
    Len = length(Oct),
    if
        Len =< 126 ->
            {[128 bor Len|Oct],Len + 1};
        true ->
            exit({error,{asn1,too_long_length_oct,Len}})
    end.

encode_tags([Tag|Trest], BytesSoFar, LenSoFar) ->
    {Bytes2,L2} = encode_length(LenSoFar),
    encode_tags(Trest,
                [Tag,Bytes2|BytesSoFar],
                LenSoFar + byte_size(Tag) + L2);
encode_tags([], BytesSoFar, LenSoFar) ->
    {BytesSoFar,LenSoFar}.

match_tags({T,V}, [T]) ->
    V;
match_tags({T,V}, [T|Tt]) ->
    match_tags(V, Tt);
match_tags([{T,V}], [T|Tt]) ->
    match_tags(V, Tt);
match_tags([{T,_V}|_] = Vlist, [T]) ->
    Vlist;
match_tags(Tlv, []) ->
    Tlv;
match_tags({Tag,_V} = Tlv, [T|_Tt]) ->
    exit({error,{asn1,{wrong_tag,{{expected,T},{got,Tag,Tlv}}}}}).

minimum_octets(0, Acc) ->
    Acc;
minimum_octets(Val, Acc) ->
    minimum_octets(Val bsr 8, [Val band 255|Acc]).

minimum_octets(Val) ->
    minimum_octets(Val, []).
