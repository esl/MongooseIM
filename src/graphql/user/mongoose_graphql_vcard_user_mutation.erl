-module(mongoose_graphql_vcard_user_mutation).
-behaviour(mongoose_graphql).

-include("mod_vcard.hrl").

-export([execute/4]).

-import(mongoose_graphql_helper, [make_error/2, format_result/2, null_to_default/2]).

-ignore_xref([execute/4]).

-include("../mongoose_graphql_types.hrl").
-include("mongoose.hrl").
-include("jlib.hrl").

execute(#{user := CallerJID}, vcard, <<"setVcard">>, #{<<"vcard">> := VCARD}) ->
    case mod_vcard_api:set_vcard(CallerJID, VCARD) of
        {ok, _} = Vcard -> Vcard;
        {error, not_found} ->
            make_error({error, "User does not exist"}, #{user => CallerJID});
        _ ->
            make_error({error, "Internal server error"}, #{user => CallerJID})
    end.
