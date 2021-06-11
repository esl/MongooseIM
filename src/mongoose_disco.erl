-module(mongoose_disco).

-export([new_acc/5,
         get_features/1,
         add_features/2,
         features_to_xml/1,
         add_items/2,
         items_to_xml/1,
         add_identities/2,
         identities_to_xml/1,
         get_identities/1,
         get_info/4,
         info_list_to_xml/1]).

-include("mongoose.hrl").
-include("jlib.hrl").

-type acc(Elem) :: #{host_type := mongooseim:host_type(),
                     from_jid := jid:jid(),
                     to_jid := jid:jid(),
                     node := binary(),
                     lang := ejabberd:lang(),
                     result := empty | [Elem]}.
-type feature_acc() :: acc(feature()).
-type item_acc() :: acc(item()).
-type identity_acc() :: acc(identity()).

-type feature() :: binary().
-type item() :: #{jid := jid:lserver(), name => binary(), node => binary()}.
-type identity() :: #{category := binary(), type := binary(), name => binary()}.
-type info() :: #{xmlns := binary(), fields := [info_field()]}.
-type info_field() :: #{var := binary(), values := [binary()], label => binary()}.

-export_type([item_acc/0, feature_acc/0, identity_acc/0,
              item/0, feature/0, identity/0,
              info_field/0, info/0]).

-spec new_acc(mongooseim:host_type(), jid:jid(), jid:jid(), binary(), ejabberd:lang()) ->
          acc(feature() | item()).
new_acc(HostType, From, To, Node, Lang) ->
    #{host_type => HostType,
      from_jid => From,
      to_jid => To,
      node => Node,
      lang => Lang,
      result => empty}.

-spec get_features(feature_acc()) -> [feature()].
get_features(#{result := empty}) -> [];
get_features(#{result := Features}) -> Features.

-spec add_features([feature()], feature_acc())  -> feature_acc().
add_features(Features, Acc) ->
    add(Features, Acc).

-spec features_to_xml([feature()]) -> [exml:element()].
features_to_xml(Features) ->
    %% Avoid duplicating features
    [feature_to_xml(Feature) || Feature <- lists:usort(Features)].

feature_to_xml(Feature) when is_binary(Feature) ->
    #xmlel{name = <<"feature">>, attrs = [{<<"var">>, Feature}]}.

-spec add_items([item()], item_acc()) -> item_acc().
add_items(Items, Acc) ->
    add(Items, Acc).

-spec items_to_xml([item()]) -> [exml:element()].
items_to_xml(Items) ->
    %% For each JID, leave only the rightmost item with that JID (the one which was added first).
    %% This is needed as extension modules might add more detailed information about an item
    %% than the default which is obtained from the registered routes and contains only the JID.
    maps:values(maps:from_list([{JID, item_to_xml(Item)} || #{jid := JID} = Item <- Items])).

item_to_xml(Item) ->
    #xmlel{name = <<"item">>,
           attrs = lists:map(fun({Key, Value}) -> {atom_to_binary(Key, utf8), Value} end,
                             maps:to_list(Item))}.

-spec add_identities([identity()], identity_acc()) -> identity_acc().
add_identities(Identities, Acc) ->
    add(Identities, Acc).

-spec identities_to_xml([identity()]) -> [exml:element()].
identities_to_xml(Identities) ->
    lists:map(fun identity_to_xml/1, Identities).

identity_to_xml(Identity) ->
    #xmlel{name = <<"identity">>,
           attrs = lists:map(fun({Key, Value}) -> {atom_to_binary(Key, utf8), Value} end,
                             maps:to_list(Identity))}.

-spec add([Elem], acc(Elem)) -> acc(Elem).
add(Elements, Acc = #{result := empty}) ->
    Acc#{result := Elements};
add(Elements, Acc = #{result := InitialElements}) ->
    Acc#{result := Elements ++ InitialElements}.

-spec get_identities(identity_acc()) -> [identity()].
get_identities(#{result := empty}) -> [];
get_identities(#{result := Identities}) -> Identities.

-spec get_info(mongooseim:host_type(), module() | undefined, binary(), ejabberd:lang()) ->
          [exml:element()].
get_info(HostType, Module, Node, Lang) ->
    InfoList = mongoose_hooks:disco_info(HostType, Module, Node, Lang),
    info_list_to_xml(InfoList).

-spec info_list_to_xml([info()]) -> [exml:element()].
info_list_to_xml(InfoList) ->
    [info_to_xml(Info) || Info <- InfoList].

-spec info_to_xml(info()) -> exml:element().
info_to_xml(#{xmlns := NS, fields := Fields}) ->
    #xmlel{name = <<"x">>,
           attrs = [{<<"xmlns">>, ?NS_XDATA}, {<<"type">>, <<"result">>}],
           children = [form_type_field_xml(NS) |
                       [info_field_to_xml(Field) || Field <- Fields]]}.

-spec info_field_to_xml(info_field()) -> exml:element().
info_field_to_xml(InfoField) ->
    {Values, Attrs} = maps:take(values, InfoField),
    #xmlel{name = <<"field">>,
           attrs = lists:map(fun({Key, Value}) -> {atom_to_binary(Key, utf8), Value} end,
                             maps:to_list(Attrs)),
           children = values_to_xml(Values)}.

-spec values_to_xml([binary()]) -> [exml:element()].
values_to_xml(Values) ->
    [#xmlel{name = <<"value">>, children = [#xmlcdata{content = Value}]} || Value <- Values].

-spec form_type_field_xml(binary()) -> exml:element().
form_type_field_xml(NS) ->
    #xmlel{name = <<"field">>,
           attrs = [{<<"var">>, <<"FORM_TYPE">>}, {<<"type">>, <<"hidden">>}],
           children = [#xmlel{name = <<"value">>,
                              children = [#xmlcdata{content = NS}]}]}.

