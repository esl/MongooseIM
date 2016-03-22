-module(mongoose_privacy_serializer).
-export([encode/1, decode/1]).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("mod_privacy.hrl").

-spec encode(#listitem{}) -> {binary(), binary()}.
encode(Item) ->
    ItemName = encode_item_name(Item),
    ItemValue = encode_item_value(Item),
    {ItemName, ItemValue}.

-spec decode({binary(), binary()}) -> #listitem{}.
decode({ItemName, ItemValue}) ->
    Item = decode_item_name(ItemName),
    decode_item_value(Item, ItemValue).

%% Encoding schema:
%%
%%- Order (integer);
%%- Type (char): j - jid, g - group, s - subscription, n - none;
%%- Action (char): a - allow, d - deny;
%%- Match all (char): y - yes, n - no;
%%- Match iq (char): y - yes, n - no;
%%- Match message (char): y - yes, n - no;
%%- Match presence in (char): y - yes, n - no;
%%- Match presence out (char): y - yes, n - no.
%%
%% For example 1jayyyyy:
%% order=1, type=jid, action=allow, match_all=yes, match_iq=yes,
%% match_message=yes, match_presence_in=yes, match_presence_out=yes
encode_item_name(#listitem{type = Type,
              action = Action,
              order = Order,
              match_all = MatchAll,
              match_iq = MatchIQ,
              match_message = MatchMessage,
              match_presence_in = MatchPresenceIn,
              match_presence_out = MatchPresenceOut
             }) ->
    X = [encode_order(Order),
         encode_type(Type),
         encode_action(Action),
         encode_boolean(MatchAll),
         encode_boolean(MatchIQ),
         encode_boolean(MatchMessage),
         encode_boolean(MatchPresenceIn),
         encode_boolean(MatchPresenceOut)],
    erlang:iolist_to_binary(X).


decode_item_name(Bin) ->
    {Order, Bin2} = decode_order(Bin),
    <<CharType, CharAction, CharMatchAll, CharMatchIQ, CharMatchMessage,
      CharMatchPresenceIn, CharMatchPresenceOut>> = Bin2,
    Type             = decode_type(CharType),
    Action           = decode_action(CharAction),
    MatchAll         = decode_boolean(CharMatchAll),
    MatchIQ          = decode_boolean(CharMatchIQ),
    MatchMessage     = decode_boolean(CharMatchMessage),
    MatchPresenceIn  = decode_boolean(CharMatchPresenceIn),
    MatchPresenceOut = decode_boolean(CharMatchPresenceOut),
    #listitem{type = Type,
              action = Action,
              order = Order,
              match_all = MatchAll,
              match_iq = MatchIQ,
              match_message = MatchMessage,
              match_presence_in = MatchPresenceIn,
              match_presence_out = MatchPresenceOut
             }.

encode_order(Order) when is_integer(Order) ->
    integer_to_list(Order).

decode_order(Bin) ->
    mongoose_lib:bin_to_int(Bin).

encode_type(jid)          -> $j;
encode_type(group)        -> $g;
encode_type(subscription) -> $s;
encode_type(none)         -> $n.

decode_type($j) -> jid;
decode_type($g) -> group;
decode_type($s) -> subscription;
decode_type($n) -> none.

encode_action(allow) -> $a;
encode_action(deny)  -> $d.

decode_action($a) -> allow;
decode_action($d) -> deny.

encode_boolean(true) -> $y;
encode_boolean(_)    -> $n.

decode_boolean($y) -> true;
decode_boolean($n) -> false.

encode_item_value(#listitem{type=Type, value=Value}) ->
    encode_item_value_binary(Type, Value).

decode_item_value(Item=#listitem{type=Type}, ItemValue) ->
    Value = decode_item_value_binary(Type, ItemValue),
    Item#listitem{value=Value}.

encode_item_value_binary(none,        _Value) -> <<>>;
encode_item_value_binary(jid,          Value) -> jid:to_binary(Value);
encode_item_value_binary(group,        Value) -> Value;
encode_item_value_binary(subscription, Value) -> encode_subscription(Value).

decode_item_value_binary(none,         _ItemValue) -> none;
decode_item_value_binary(jid,           ItemValue) -> jid:to_lower(jid:from_binary(ItemValue));
decode_item_value_binary(group,         ItemValue) -> ItemValue;
decode_item_value_binary(subscription,  ItemValue) -> decode_subscription(ItemValue).

decode_subscription(<<"n">>) -> none;
decode_subscription(<<"b">>) -> both;
decode_subscription(<<"f">>) -> from;
decode_subscription(<<"t">>) -> to.

encode_subscription(none) -> <<"n">>;
encode_subscription(both) -> <<"b">>;
encode_subscription(from) -> <<"f">>;
encode_subscription(to)   -> <<"t">>.
