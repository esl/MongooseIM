-module(mongoose_client_api).

-export([init/2]).
-export([content_types_provided/2]).
-export([is_authorized/2]).
-export([options/2]).
-export([allowed_methods/2]).
-export([to_json/2]).
-export([bad_request/2]).
-export([bad_request/3]).
-export([forbidden_request/2]).
-export([forbidden_request/3]).
-export([json_to_map/1]).

-ignore_xref([allowed_methods/2, content_types_provided/2, forbidden_request/3,
              options/2, to_json/2]).

-include("mongoose.hrl").
-include("jlib.hrl").

init(Req, _Opts) ->
    State = #{},
    case cowboy_req:header(<<"origin">>, Req) of
        undefined ->
            {cowboy_rest, Req, State};
        Origin ->
            Req1 = set_cors_headers(Origin, Req),
            {cowboy_rest, Req1, State}
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

bad_request(Req, State) ->
    bad_request(Req, <<"Bad request. The details are unknown.">>, State).

bad_request(Req, Reason, State) ->
    reply(400, Req, Reason, State).

forbidden_request(Req, State) ->
    forbidden_request(Req, <<>>, State).

forbidden_request(Req, Reason, State) ->
    reply(403, Req, Reason, State).

reply(StatusCode, Req, Body, State) ->
    maybe_report_error(StatusCode, Req, Body),
    Req1 = set_resp_body_if_missing(Body, Req),
    Req2 = cowboy_req:reply(StatusCode, Req1),
    {stop, Req2, State#{was_replied => true}}.

set_resp_body_if_missing(Body, Req) ->
    case cowboy_req:has_resp_body(Req) of
        true ->
            Req;
        false ->
            cowboy_req:set_resp_body(Body, Req)
    end.

maybe_report_error(StatusCode, Req, Body) when StatusCode >= 400 ->
    ?LOG_WARNING(#{what => reply_error,
                   stacktrace => element(2, erlang:process_info(self(), current_stacktrace)),
                   code => StatusCode, req => Req, reply_body => Body});
maybe_report_error(_StatusCode, _Req, _Body) ->
    ok.

%%--------------------------------------------------------------------
%% Authorization
%%--------------------------------------------------------------------

% @doc cowboy callback
is_authorized(Req, State) ->
    HTTPMethod = cowboy_req:method(Req),
    AuthDetails = mongoose_api_common:get_auth_details(Req),
    case AuthDetails of
        undefined ->
            mongoose_api_common:make_unauthorized_response(Req, State);
        {AuthMethod, User, Password} ->
            authorize(AuthMethod, User, Password, HTTPMethod, Req, State)
    end.

authorize(AuthMethod, User, Password, HTTPMethod, Req, State) ->
    MaybeJID = jid:from_binary(User),
    case do_authorize(AuthMethod, MaybeJID, Password, HTTPMethod) of
        noauth ->
            {true, Req, State};
        {true, Creds} ->
            {true, Req, State#{user => User, jid => MaybeJID, creds => Creds}};
        false ->
            mongoose_api_common:make_unauthorized_response(Req, State)
    end.

do_authorize(AuthMethod, MaybeJID, Password, HTTPMethod) ->
    case is_noauth_http_method(HTTPMethod) of
        true ->
            noauth;
        false ->
            mongoose_api_common:is_known_auth_method(AuthMethod) andalso
                mongoose_api_common:check_password(MaybeJID, Password)
    end.

% Constraints
is_noauth_http_method(<<"OPTIONS">>) -> true;
is_noauth_http_method(_) -> false.

%% -------------------------------------------------------------------
%% @doc
%% Decode JSON binary into map
%% @end
%% -------------------------------------------------------------------
-spec json_to_map(JsonBin :: binary()) -> {ok, Map :: maps:map()} | {error, invalid_json}.

json_to_map(JsonBin) ->
    case catch jiffy:decode(JsonBin, [return_maps]) of
        Map when is_map(Map) ->
            {ok, Map};
        _ ->
            {error, invalid_json}
    end.
