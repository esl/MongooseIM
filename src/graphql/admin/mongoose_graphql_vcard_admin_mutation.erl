-module(mongoose_graphql_vcard_admin_mutation).
-behaviour(mongoose_graphql).

-include("mod_vcard.hrl").

-export([execute/4]).

-import(mongoose_graphql_helper, [make_error/2]).

-ignore_xref([execute/4]).

-include("../mongoose_graphql_types.hrl").
-include("mongoose.hrl").
-include("jlib.hrl").

execute(_Ctx, vcard, <<"setVcard">>, #{<<"user">> := CallerJID, <<"vcard">> := VcardInput}) ->
    case mod_vcard_api:set_vcard(CallerJID, VcardInput) of
        {ok, _} = Vcard -> Vcard;
        Error -> make_error(Error, #{user => jid:to_binary(CallerJID)})
    end.
