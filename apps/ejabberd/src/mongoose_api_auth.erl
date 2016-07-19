%%%-------------------------------------------------------------------
%%% @author ludwikbukowski
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. Jul 2016 17:55
%%%-------------------------------------------------------------------
-module(mongoose_api_auth).
-author("ludwikbukowski").
-include("mongoose_api.hrl").
-include("jlib.hrl").

%% API
-export([is_authorized/2]).

is_authorized(Req, State) ->
    Auth = cowboy_req:parse_header(<<"authorization">>, Req),
    case Auth of
        {ok, undefined, _} ->
            make_unauthorized_response(Req, State);
        {ok, AuthDetails, Req2} ->
            do_authorize(AuthDetails, Req2, State)
    end.

do_authorize({<<"basic">>, {User, Password}}, Req, State) ->
    case jid:from_binary(User) of
        error ->
            make_unauthorized_response(Req, State);
        JID ->
            do_check_password(JID, Password, Req, State)
    end;
do_authorize(_, Req, State) ->
    make_unauthorized_response(Req, State).

do_check_password(#jid{luser = User, lserver = Server} = JID,
    Password, Req, State) ->
    case ejabberd_auth:check_password(User, Server, Password) of
        true ->
            {true, Req, State#backend_state{entity = JID}};
        _ ->
            make_unauthorized_response(Req, State)
    end.

make_unauthorized_response(Req, State) ->
    {{false, <<"Basic realm=\"mongooseim\"">>}, Req, State}.