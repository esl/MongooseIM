-module(mongoose_client_api).

-export([init/3]).
-export([content_types_provided/2]).
-export([is_authorized/2]).
-export([to_json/2]).

-include("ejabberd.hrl").

%% yes, there is no other option, this API has to run over encrypted connection
init({ssl, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_rest}.

is_authorized(Req, State) ->
    Auth = cowboy_req:parse_header(<<"authorization">>, Req),
    ?WARNING_MSG("Auth: ~p", [Auth]),
    case Auth of
        {ok, undefined, _} ->
            make_unauthorized_response(Req, State);
        {ok, AuthDetails, Req2} ->
           do_authorize(AuthDetails, Req2, State)
    end.

content_types_provided(Req, State) ->
	{[
		{<<"application/json">>, to_json}
	], Req, State}.

to_json(Req, User) ->
    {<<"{}">>, Req, User}.

do_authorize({<<"basic">>, {User, Password}}, Req, State) ->
    case jid:from_binary(User) of
        error ->
            make_unauthorized_response(Req, State);
        JID ->
            do_check_password(jid:to_lus(JID), Password, Req, State)
    end;
do_authorize(_, Req, State) ->
    make_unauthorized_response(Req, State).

do_check_password({User, Server}, Password, Req, State) ->
    case ejabberd_auth:check_password(User, Server, Password) of
        true ->
            {true, Req, User};
        _ ->
            make_unauthorized_response(Req, State)
    end.

make_unauthorized_response(Req, State) ->
    {{false, <<"Basic realm=\"mongooseim\"">>}, Req, State}.

