-module(mongoose_graphql_vcard_admin_mutation).
-behaviour(mongoose_graphql).

-include("mod_vcard.hrl").

-export([execute/4]).

-ignore_xref([execute/4]).

-include("../mongoose_graphql_types.hrl").
-include("mongoose.hrl").
-include("jlib.hrl").

-define(UNKNOWN_DOMAIN_RESULT, {unknown_domain, "Domain not found"}).

execute(_Ctx, vcard, <<"setVcard">>,
        #{<<"user">> := #jid{luser = LUser, lserver = LServer} = _CallerJID,
          <<"vcard">> := Vcard}) ->
    case mongoose_domain_api:get_domain_host_type(LServer) of
        {ok, HostType} ->
            mod_vcard_api:set_vcard(HostType, LUser, LServer, Vcard),
            {ok, mod_vcard_api:get_vcard(HostType, LUser, LServer)}
       end.
