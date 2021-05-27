-module(mongoose_disco).

-export([get_local_features/5,
         add_features/2,
         features_to_xml/1]).

-include("mongoose.hrl").
-include("jlib.hrl").

-type acc() :: empty | {error, any()} | {result, [exml:element()]}.
-type feature() :: binary().

get_local_features(Host, From, To, Node, Lang) ->
    case mongoose_hooks:disco_local_features(Host, From, To, Node, Lang) of
        empty ->
            [];
        {error, Reason} ->
            ?LOG_ERROR(#{what => get_local_features_failed,
                         from_jid => jid:to_binary(From),
                         to_jid => jid:to_binary(To),
                         node => Node,
                         lang => Lang,
                         reason => Reason}),
            [];
        {result, Features} ->
            Features
    end.

-spec add_features([feature()], acc())  -> acc().
add_features(Features, Acc) ->
    case Acc of
        {error, _} = Error ->
            Error;
        empty ->
            {result, Features};
        {result, InitialFeatures} ->
            {result, Features ++ InitialFeatures}
    end.

features_to_xml(FeatureList) ->
    %% Avoid duplicating features
    [#xmlel{name = <<"feature">>, attrs = [{<<"var">>, Feat}]} ||
                  Feat <- lists:usort(
                            lists:map(
                              fun({{Feature, _Host}}) ->
                                  Feature;
                                 (Feature) when is_binary(Feature) ->
                          Feature
                              end, FeatureList))].
