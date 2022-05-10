-module(mongoose_graphql_vcard_admin_query).
-behaviour(mongoose_graphql).

-export([execute/4]).

-import(mongoose_graphql_helper, [make_error/2, format_result/2, null_to_default/2]).

-ignore_xref([execute/4]).

-include("../mongoose_graphql_types.hrl").
-include("mongoose.hrl").
-include("jlib.hrl").

execute(_Ctx, vcard, <<"getVcard">>, #{<<"user">> := CallerJID}) ->
    case mod_vcard_api:get_vcard(CallerJID) of
        {ok, _} = Vcard -> Vcard;
        {ErrorCode, ErrorMessage} ->
            make_error({ErrorCode, ErrorMessage}, #{user => CallerJID})
    end.
