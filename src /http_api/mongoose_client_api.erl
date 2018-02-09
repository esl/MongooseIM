-module(mongoose_client_api).

-export([init/3]).
-export([content_types_provided/2]).
-export([is_authorized/2]).
-export([options/2]).
-export([allowed_methods/2]).
-export([to_json/2]).
-export([rest_init/2]).

-include("mongoose.hrl").
-include("jlib.hrl").

init(_Transport, _Req, _Opts) ->
    {upgrade, protocol, cowboy_rest}.

rest_init(Req, _HandlerOpts) ->
    State = #{},
    case cowboy_req:header(<<"origin">>, Req) of
        {undefined, Req1} ->
            {ok, Req1, State};
        {Origin, Req1} ->
            Req2 = set_cors_headers(Origin, Req1),
            {ok, Req2, State}
    end.

set_cors_headers(Origin, Req) ->
    %% set CORS headers
    Headers = [{<<"access-control-allow-origin">>, Origin},
               {<<"access-control-allow-methods">>, <<"GET, OPTIONS">>},
               {<<"access-control-allow-credentials">>, <<"true">>},
               {<<"access-control-allow-headers">>, <<"authorization, content-type">>}
              ],

    lists:foldl(fun set_cors_header/2, Req, Headers).

set_cors_header({Header, Value}, Req) ->
    cowboy_req:set_resp_header(Header, Value, Req).

allowed_methods(Req, State) ->
    {[<<"OPTIONS">>, <<"GET">>], Req, State}.

content_types_provided(Req, State) ->
    {[
      {{<<"application">>, <<"json">>, '*'}, to_json}
     ], Req, State}.

options(Req, State) ->
    {ok, Req, State}.

to_json(Req, User) ->
    {<<"{}">>, Req, User}.

%%--------------------------------------------------------------------
%% Authorization
%%--------------------------------------------------------------------

% @doc cowboy callback
is_authorized(Req, State) ->
    {HTTPMethod, Req2} = cowboy_req:method(Req),
    {ok, AuthDetails, Req3} = mongoose_api_common:get_auth_details(Req2),
    case AuthDetails == undefined of
        true ->
            mongoose_api_common:make_unauthorized_response(Req3, State);
        false ->
            authorize(AuthDetails, HTTPMethod, Req3, State)
    end.

authorize({AuthMethod, Creds} = AuthDetails, HTTPMethod, Req, State) ->
    case do_authorize(AuthDetails, HTTPMethod) of
        noauth ->
            {true, Req, State};
        true ->
            {User, _} = Creds,
            {true, Req, State#{user => User, jid => jid:from_binary(User)}};
        false ->
            mongoose_api_common:make_unauthorized_response(Req, State)
    end.

do_authorize({AuthMethod, Creds}, HTTPMethod) ->
    case is_noauth_http_method(HTTPMethod) of
        true ->
            noauth;
        false ->
            check_password(Creds) andalso
            mongoose_api_common:is_known_auth_method(AuthMethod)
    end.

check_password(undefined) -> false;
check_password({User, Password}) ->
    #jid{luser = RawUser, lserver = Server} = jid:from_binary(User),
    Creds0 = mongoose_credentials:new(Server),
    Creds1 = mongoose_credentials:set(Creds0, username, RawUser),
    Creds2 = mongoose_credentials:set(Creds1, password, Password),
    case ejabberd_auth:authorize(Creds2) of
        {ok, _} -> true;
        _ -> false
    end.

% Constraints
is_noauth_http_method(<<"OPTIONS">>) -> true;
is_noauth_http_method(_) -> false.

