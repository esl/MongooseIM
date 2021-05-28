-module(mongoose_disco).

-export([get_local_features/5,
         add_features/2,
         features_to_xml/1,
         add_items/2,
         items_to_xml/1]).

-include("mongoose.hrl").
-include("jlib.hrl").

-type item_acc() :: empty | {result, map()}.
-type feature_acc() :: empty | {result, [feature()]}.
-type item() :: #{jid := jid:lserver(),
                  name => binary(),
                  node => binary()}.
-type feature() :: binary().

-export_type([item_acc/0, feature_acc/0, item/0, feature/0]).

-spec get_local_features(jid:lserver(), jid:jid(), jid:jid(), binary(), ejabberd:lang()) ->
          [feature()].
get_local_features(Host, From, To, Node, Lang) ->
    case mongoose_hooks:disco_local_features(Host, From, To, Node, Lang) of
        empty ->
            [];
        {result, Features} ->
            Features
    end.

-spec add_features([feature()], feature_acc())  -> feature_acc().
add_features(Features, Acc) ->
    case Acc of
        empty ->
            {result, Features};
        {result, InitialFeatures} ->
            {result, Features ++ InitialFeatures}
    end.

-spec features_to_xml([feature()]) -> [exml:element()].
features_to_xml(Features) ->
    %% Avoid duplicating features
    [feature_to_xml(Feature) || Feature <- lists:usort(Features)].

feature_to_xml(Feature) when is_binary(Feature) ->
    #xmlel{name = <<"feature">>, attrs = [{<<"var">>, Feature}]}.

-spec add_items([item()], item_acc())  -> item_acc().
add_items(Items, empty) ->
    {result, add_items_to_map(Items, #{})};
add_items(Items, {result, InitialItemMap}) ->
    {result, add_items_to_map(Items, InitialItemMap)}.

add_items_to_map(Items, InitialItemMap) ->
    ToAdd = maps:from_list([{JID, Item} || Item = #{jid := JID} <- Items]),
    maps:merge(ToAdd, InitialItemMap).

-spec items_to_xml(map()) -> [exml:element()].
items_to_xml(Items) ->
    [item_to_xml(Item) || Item <- maps:values(Items)].

item_to_xml(Item) ->
    #xmlel{name = <<"item">>,
           attrs = lists:map(fun({Key, Value}) -> {atom_to_binary(Key, utf8), Value} end,
                             maps:to_list(Item))}.
