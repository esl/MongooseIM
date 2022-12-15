-module(mongoose_graphql_vcard_admin_query).
-behaviour(mongoose_graphql).

-export([execute/4]).

-import(mongoose_graphql_helper, [make_error/2]).

-ignore_xref([execute/4]).

-include("../mongoose_graphql_types.hrl").
-include("mongoose.hrl").
-include("jlib.hrl").

execute(_Ctx, vcard, <<"getVcard">>, #{<<"user">> := CallerJID}) ->
    case mod_vcard_api:get_vcard(CallerJID) of
        {ok, _} = Vcard -> Vcard;
        Error ->
            make_error(Error, #{user => jid:to_binary(CallerJID)})
    end.
