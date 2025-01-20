%%% @doc This module is responsible for running disco_* hooks that collect information
%%% for service discovery. It also contains helpers for direct construction of XML elements
%%% representing the advertised services.

-module(mongoose_disco).

%% Hooks wrappers
-export([get_local_identity/5,
         get_sm_identity/5,
         get_local_items/5,
         get_sm_items/5,
         get_local_features/5,
         get_sm_features/5,
         get_muc_features/6,
         get_info/4]).

%% Helpers used by hook handlers for Acc manipulation
-export([add_identities/2,
         add_items/2,
         add_features/2,
         add_info/2]).

%% XML construction helpers
-export([identities_to_xml/1,
         items_to_xml/1,
         features_to_xml/1,
         info_list_to_xml/1]).

-ignore_xref([items_to_xml/1]).

-include("jlib.hrl").

-type feature_acc() :: acc(feature()).
-type feature() :: binary().

-type item_acc() :: acc(item()).
-type item() :: #{jid := jid:lserver(), name => binary(), node => binary()}.

-type identity_acc() :: acc(identity()).
-type identity() :: #{category := binary(), type := binary(), name => binary()}.

-type info_acc() :: #{host_type := mongooseim:host_type(),
                      module := module(),
                      node := binary(),
                      lang := ejabberd:lang(),
                      result := empty | [info()]}.
-type info() :: #{xmlns := binary(), fields := [info_field()]}.
-type info_field() :: #{var := binary(), values := [binary()], label => binary()}.

-type acc(Elem) :: #{host_type := mongooseim:host_type(),
                     from_jid := jid:jid(),
                     to_jid := jid:jid(),
                     node := binary(),
                     lang := ejabberd:lang(),
                     result := empty | [Elem]}.

-export_type([item_acc/0, feature_acc/0, identity_acc/0,
              item/0, feature/0, identity/0,
              info_field/0, info/0, info_acc/0]).

%% Hook wrapper API

-spec get_local_identity(mongooseim:host_type(), jid:jid(), jid:jid(), binary(), ejabberd:lang()) ->
          [exml:element()].
get_local_identity(HostType, From, To, Node, Lang) ->
    InitialAcc = new_acc(HostType, From, To, Node, Lang),
    FinalAcc = mongoose_hooks:disco_local_identity(InitialAcc),
    Identities = extract_result(FinalAcc),
    identities_to_xml(Identities).

-spec get_sm_identity(mongooseim:host_type(), jid:jid(), jid:jid(), binary(), ejabberd:lang()) ->
          [exml:element()].
get_sm_identity(HostType, From, To, Node, Lang) ->
    InitialAcc = new_acc(HostType, From, To, Node, Lang),
    IdentityAcc = mongoose_hooks:disco_sm_identity(InitialAcc),
    Identities = extract_result(IdentityAcc),
    identities_to_xml(Identities).

-spec get_local_items(mongooseim:host_type(), jid:jid(), jid:jid(), binary(), ejabberd:lang()) ->
          {result, [exml:element()]} | empty.
get_local_items(HostType, From, To, Node, Lang) ->
    Acc = new_acc(HostType, From, To, Node, Lang),
    case mongoose_hooks:disco_local_items(Acc) of
        #{result := empty} -> empty;
        #{result := Items} -> {result, items_to_xml(Items)}
    end.

-spec get_sm_items(mongooseim:host_type(), jid:jid(), jid:jid(), binary(), ejabberd:lang()) ->
          {result, [exml:element()]} | empty.
get_sm_items(HostType, From, To, Node, Lang) ->
    Acc = new_acc(HostType, From, To, Node, Lang),
    case mongoose_hooks:disco_sm_items(Acc) of
        #{result := empty} -> empty;
        #{result := Items} -> {result, items_to_xml(Items)}
    end.

-spec get_local_features(mongooseim:host_type(), jid:jid(), jid:jid(), binary(), ejabberd:lang()) ->
          {result, [exml:element()]} | empty.
get_local_features(HostType, From, To, Node, Lang) ->
    Acc = new_acc(HostType, From, To, Node, Lang),
    case mongoose_hooks:disco_local_features(Acc) of
        #{result := empty} -> empty;
        #{result := Features} -> {result, features_to_xml(Features)}
    end.

-spec get_sm_features(mongooseim:host_type(), jid:jid(), jid:jid(), binary(), ejabberd:lang()) ->
          {result, [exml:element()]} | empty.
get_sm_features(HostType, From, To, Node, Lang) ->
    Acc = new_acc(HostType, From, To, Node, Lang),
    case mongoose_hooks:disco_sm_features(Acc) of
        #{result := empty} -> empty;
        #{result := Features} -> {result, features_to_xml(Features)}
    end.

-spec get_muc_features(mongooseim:host_type(), jid:jid(), jid:jid(), binary(), ejabberd:lang(),
                   [mongoose_disco:feature()]) ->
          [exml:element()].
get_muc_features(HostType, From, To, Node, Lang, ExtraFeatures) ->
    InitialAcc = new_acc(HostType, From, To, Node, Lang),
    FinalAcc = mongoose_hooks:disco_muc_features(InitialAcc),
    Features = ExtraFeatures ++ extract_result(FinalAcc),
    features_to_xml(Features).

-spec get_info(mongooseim:host_type(), module() , binary(), ejabberd:lang()) -> [exml:element()].
get_info(HostType, Module, Node, Lang) ->
    InitialAcc = new_info_acc(HostType, Module, Node, Lang),
    FinalAcc = mongoose_hooks:disco_info(InitialAcc),
    InfoList = extract_result(FinalAcc),
    info_list_to_xml(InfoList).

%% Helper API

-spec add_identities([identity()], identity_acc()) -> identity_acc().
add_identities(Identities, Acc) ->
    add(Identities, Acc).

-spec add_items([item()], item_acc()) -> item_acc().
add_items(Items, Acc) ->
    add(Items, Acc).

-spec add_features([feature()], feature_acc()) -> feature_acc().
add_features(Features, Acc) ->
    add(Features, Acc).

-spec add_info([info()], info_acc()) -> info_acc().
add_info(InfoList, Acc) ->
    add(InfoList, Acc).

%% XML construction API

-spec identities_to_xml([identity()]) -> [exml:element()].
identities_to_xml(Identities) ->
    lists:map(fun identity_to_xml/1, Identities).

-spec items_to_xml([item()]) -> [exml:element()].
items_to_xml(Items) ->
    %% For each JID, leave only the rightmost item with that JID (the one which was added first).
    %% This is needed as extension modules might add more detailed information about an item
    %% than the default which is obtained from the registered routes and contains only the JID.
    maps:values(maps:from_list([{JID, item_to_xml(Item)} || #{jid := JID} = Item <- Items])).

-spec features_to_xml([feature()]) -> [exml:element()].
features_to_xml(Features) ->
    %% Avoid duplicating features
    [feature_to_xml(Feature) || Feature <- lists:usort(Features)].

-spec info_list_to_xml([info()]) -> [exml:element()].
info_list_to_xml(InfoList) ->
    [info_to_xml(Info) || Info <- InfoList].

%% Acc manipulation

-spec new_acc(mongooseim:host_type(), jid:jid(), jid:jid(), binary(), ejabberd:lang()) ->
          acc(any()).
new_acc(HostType, From, To, Node, Lang) ->
    #{host_type => HostType,
      from_jid => From,
      to_jid => To,
      node => Node,
      lang => Lang,
      result => empty}.

-spec new_info_acc(mongooseim:host_type(), module(), binary(), ejabberd:lang()) -> info_acc().
new_info_acc(HostType, Module, Node, Lang) ->
    #{host_type => HostType,
      module => Module,
      node => Node,
      lang => Lang,
      result => empty}.

-spec add([Elem], acc(Elem)) -> acc(Elem);
         ([info()], info_acc()) -> info_acc().
add(Elements, Acc = #{result := empty}) ->
    Acc#{result := Elements};
add(Elements, Acc = #{result := InitialElements}) ->
    Acc#{result := Elements ++ InitialElements}.

-spec extract_result(acc(Elem)) -> [Elem];
                    (info_acc()) -> [info()].
extract_result(#{result := empty}) -> [];
extract_result(#{result := Elements}) -> Elements.

%% Conversion to XML

feature_to_xml(Feature) when is_binary(Feature) ->
    #xmlel{name = <<"feature">>, attrs = #{<<"var">> => Feature}}.

item_to_xml(Item) ->
    #xmlel{name = <<"item">>,
           attrs = #{atom_to_binary(Key, utf8) => Value || Key := Value <- Item}}.

identity_to_xml(Identity) ->
    #xmlel{name = <<"identity">>,
           attrs = #{atom_to_binary(Key, utf8) => Value || Key := Value <- Identity}}.

-spec info_to_xml(info()) -> exml:element().
info_to_xml(#{xmlns := NS, fields := Fields}) ->
    mongoose_data_forms:form(#{type => <<"result">>, ns => NS, fields => Fields}).
