-module(mongoose_graphql_helper).
-export([check_user/1]).

-include("jlib.hrl").

check_user(#jid{lserver = LServer} = Jid) ->
    case mongoose_domain_api:get_domain_host_type(LServer) of
        {ok, HostType} ->
            check_auth(HostType, Jid);
        _ ->
            {error, #{what => unknown_domain, domain => LServer}}
    end.

check_auth(HostType, Jid) ->
   case ejabberd_auth:does_user_exist(HostType, Jid, stored) of
       true ->
           {ok, HostType};
       false ->
            {error, #{what => non_existing_user, jid => jid:to_binary(Jid)}}
    end.
