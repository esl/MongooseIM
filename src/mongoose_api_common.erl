%%%-------------------------------------------------------------------
%%% @author ludwikbukowski
%%% @copyright (C) 2016, Erlang Solutions Ltd.
%%% Created : 20. Jul 2016 10:16
%%%-------------------------------------------------------------------
%%% @doc Utilities for the REST API

-module(mongoose_api_common).
-author("ludwikbukowski").

%% API
-export([get_auth_details/1,
         is_known_auth_method/1,
         make_unauthorized_response/2,
         check_password/2]).

%%--------------------------------------------------------------------
%% Authorization
%%--------------------------------------------------------------------

-spec get_auth_details(cowboy_req:req()) ->
    {basic, User :: binary(), Password :: binary()} | undefined.
get_auth_details(Req) ->
    case cowboy_req:parse_header(<<"authorization">>, Req) of
        {basic, _User, _Password} = Details ->
            Details;
        _ ->
            undefined
    end.

-spec is_known_auth_method(atom()) -> boolean().
is_known_auth_method(basic) -> true;
is_known_auth_method(_) -> false.

make_unauthorized_response(Req, State) ->
    {{false, <<"Basic realm=\"mongooseim\"">>}, Req, State}.

-spec check_password(jid:jid() | error, binary()) -> {true, mongoose_credentials:t()} | false.
check_password(error, _) ->
    false;
check_password(JID, Password) ->
    {LUser, LServer} = jid:to_lus(JID),
    case mongoose_domain_api:get_domain_host_type(LServer) of
        {ok, HostType} ->
            Creds0 = mongoose_credentials:new(LServer, HostType, #{}),
            Creds1 = mongoose_credentials:set(Creds0, username, LUser),
            Creds2 = mongoose_credentials:set(Creds1, password, Password),
            case ejabberd_auth:authorize(Creds2) of
                {ok, Creds} -> {true, Creds};
                _ -> false
            end;
        {error, not_found} -> false
    end.
