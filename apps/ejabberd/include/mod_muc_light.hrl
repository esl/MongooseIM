-type configuration() :: [{Key :: atom(), Value :: term()}].
-type affiliation() :: owner | member | none.
-type affiliated_user() :: {{LUser :: binary(), LServer :: binary(), <<>>}, affiliation()}.
-type affiliated_users() :: [affiliated_user()].
-type validation_error() :: {error, {Key :: binary(), Reason :: atom()}}.

-define(NS_MUC_LIGHT, <<"mongooseim:muc:light">>).
