-module(mongoose_disco).

-export([new_item_acc/5,
         get_local_features/5,
         add_features/2,
         features_to_xml/1,
         add_items/2,
         items_to_xml/1,
         identities_to_xml/1]).

-include("mongoose.hrl").
-include("jlib.hrl").

-type feature_acc() :: empty | {result, [feature()]}.
-type feature() :: binary().

-type item_acc() :: #{host_type := mongooseim:host_type(),
                      from_jid := jid:jid(),
                      to_jid := jid:jid(),
                      node := binary(),
                      lang := ejabberd:lang(),
                      result := empty | [item()]}.
-type item() :: #{jid := jid:lserver(), name => binary(), node => binary()}.

-type identity() :: #{category := binary(), type := binary(), name => binary()}.

-export_type([item_acc/0, feature_acc/0, item/0, feature/0, identity/0]).

new_item_acc(HostType, From, To, Node, Lang) ->
    #{host_type => HostType,
      from_jid => From,
      to_jid => To,
      node => Node,
      lang => Lang,
      result => empty}.

%% @doc Run the 'disco_local_features' hook and unpack the results.
%% Used by extension modules which support their own subdomains
%% and add their own features to the end result.
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
add_features(Features, empty) ->
    {result, Features};
add_features(Features, {result, InitialFeatures}) ->
    {result, Features ++ InitialFeatures}.

-spec features_to_xml([feature()]) -> [exml:element()].
features_to_xml(Features) ->
    %% Avoid duplicating features
    [feature_to_xml(Feature) || Feature <- lists:usort(Features)].

feature_to_xml(Feature) when is_binary(Feature) ->
    #xmlel{name = <<"feature">>, attrs = [{<<"var">>, Feature}]}.

-spec add_items([item()], item_acc()) -> item_acc().
add_items(Items, Acc = #{result := empty}) ->
    Acc#{result := Items};
add_items(Items, Acc = #{result := InitialItems}) ->
    Acc#{result := Items ++ InitialItems}.

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

-spec identities_to_xml([identity()]) -> [exml:element()].
identities_to_xml(Identities) ->
    lists:map(fun identity_to_xml/1, Identities).

identity_to_xml(Identity) ->
    #xmlel{name = <<"identity">>,
           attrs = lists:map(fun({Key, Value}) -> {atom_to_binary(Key, utf8), Value} end,
                             maps:to_list(Identity))}.
