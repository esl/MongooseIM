-module(mongoose_graphql_private_user_mutation).
-behaviour(mongoose_graphql).

-export([execute/4]).

-ignore_xref([execute/4]).

-include("../mongoose_graphql_types.hrl").

-import(mongoose_graphql_helper, [make_error/2, format_result/2]).

execute(#{user := CallerJID}, _Obj, <<"setPrivate">>, #{<<"elementString">> := Element}) ->
    case mod_private_api:private_set(CallerJID, Element) of
        {ok, _} = Result -> Result;
        Error -> make_error(Error, #{user => jid:to_binary(CallerJID),
                                     element => exml:to_binary(Element)})
    end.
