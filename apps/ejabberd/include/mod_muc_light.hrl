-type configuration() :: [{Key :: atom(), Value :: term()}].
-type affiliation() :: owner | member | none.
-type affiliation_tuple() :: {{LUser :: binary(), LServer :: binary(), <<>>}, affiliation()}.
-type affiliations() :: [affiliation_tuple()].
-type validation_error() :: {error, {Key :: binary(), Reason :: atom()}}.

-define(NS_MUC_LIGHT, <<"mongooseim:muc:light">>).
