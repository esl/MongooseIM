-module(mongoose_graphql_vcard_user_query).
-behaviour(mongoose_graphql).

-export([execute/4]).

-import(mongoose_graphql_helper, [make_error/2, null_to_default/2]).

-ignore_xref([execute/4]).

-include("../mongoose_graphql_types.hrl").
-include("mongoose.hrl").
-include("jlib.hrl").

execute(#{user := CallerJID}, vcard, <<"getVcard">>, #{<<"user">> := UserJID}) ->
    UserJID2 = null_to_default(UserJID, CallerJID),
    case mod_vcard_api:get_vcard(UserJID2) of
        {ok, _} = Vcard -> Vcard;
        Error -> make_error(Error, #{<<"user">> => jid:to_binary(UserJID2)})
    end.
