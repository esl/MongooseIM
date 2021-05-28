-module(mongoose_disco).

-export([get_local_features/5,
         add_features/2,
         features_to_xml/1]).

-include("mongoose.hrl").
-include("jlib.hrl").

-type acc() :: empty | {result, [exml:element()]}.
-type feature() :: binary().

-spec get_local_features(jid:lserver(), jid:jid(), jid:jid(), binary(), ejabberd:lang()) ->
          [feature()].
get_local_features(Host, From, To, Node, Lang) ->
    case mongoose_hooks:disco_local_features(Host, From, To, Node, Lang) of
        empty ->
            [];
        {result, Features} ->
            Features
    end.

-spec add_features([feature()], acc())  -> acc().
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
    [#xmlel{name = <<"feature">>, attrs = [{<<"var">>, Feature}]} ||
        Feature <- lists:usort(Features)].
