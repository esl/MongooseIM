-module(mongoose_client_api_contacts).
-behaviour(cowboy_rest).

-export([trails/0]).
-export([init/2]).
-export([content_types_provided/2]).
-export([content_types_accepted/2]).
-export([is_authorized/2]).
-export([allowed_methods/2]).

-export([forbidden_request/2]).

-export([to_json/2]).
-export([from_json/2]).
-export([delete_resource/2]).

-include("mongoose.hrl").
-include("jlib.hrl").
-include_lib("exml/include/exml.hrl").

trails() ->
    mongoose_client_api_contacts_doc:trails().

init(Req, Opts) ->
    mongoose_client_api:init(Req, Opts).

is_authorized(Req, State) ->
    mongoose_client_api:is_authorized(Req, State).

content_types_provided(Req, State) ->
    {[
      {{<<"application">>, <<"json">>, '*'}, to_json}
     ], Req, State}.

content_types_accepted(Req, State) ->
    {[
      {{<<"application">>, <<"json">>, '*'}, from_json}
     ], Req, State}.

allowed_methods(Req, State) ->
    {[<<"OPTIONS">>, <<"GET">>, <<"POST">>, <<"PUT">>, <<"DELETE">>],
     Req, State}.

forbidden_request(Req, State) ->
    Req1 = cowboy_req:reply(403, Req),
    {stop, Req1, State}.

to_json(Req, #{jid := Caller} = State) ->
    CJid = jid:to_binary(Caller),
    Method = cowboy_req:method(Req),
    Jid = cowboy_req:binding(jid, Req),
    case Jid of
        undefined ->
            {ok, Res} = handle_request(Method, Jid, undefined, CJid),
            {jiffy:encode(lists:flatten([Res])), Req, State};
        _ ->
            Req2 = cowboy_req:reply(404, Req),
            {stop, Req2, State}
    end.


from_json(Req, #{jid := Caller} = State) ->
    CJid = jid:to_binary(Caller),
    Method = cowboy_req:method(Req),
    {ok, Body, Req1} = cowboy_req:read_body(Req),
    case mongoose_client_api:json_to_map(Body) of
        {ok, JSONData} ->
            Jid = case maps:get(<<"jid">>, JSONData, undefined) of
                      undefined -> cowboy_req:binding(jid, Req1);
                      J when is_binary(J) -> J;
                      _ -> undefined
                  end,
            Action = maps:get(<<"action">>, JSONData, undefined),
            handle_request_and_respond(Method, Jid, Action, CJid, Req1, State);
        _ ->
            mongoose_client_api:bad_request(Req1, State)
    end.

%% @doc Called for a method of type "DELETE"
delete_resource(Req, #{jid := Caller} = State) ->
    CJid = jid:to_binary(Caller),
    Jid = cowboy_req:binding(jid, Req),
    case Jid of
        undefined ->
            handle_multiple_deletion(CJid, get_requested_contacts(Req), Req, State);
        _ ->
            handle_single_deletion(CJid, Jid, Req, State)
    end.

handle_multiple_deletion(_, undefined, Req, State) ->
    mongoose_client_api:bad_request(Req, State);
handle_multiple_deletion(CJid, ToDelete, Req, State) ->
    case handle_request(<<"DELETE">>, ToDelete, undefined, CJid) of
        {ok, NotDeleted} ->
            RespBody = #{not_deleted => NotDeleted},
            Req2 = cowboy_req:set_resp_body(jiffy:encode(RespBody), Req),
            Req3 = cowboy_req:set_resp_header(<<"content-type">>, <<"application/json">>, Req2),
            {true, Req3, State};
        Other ->
            serve_failure(Other, Req, State)
    end.

handle_single_deletion(_, undefined, Req, State) ->
    mongoose_client_api:bad_request(Req, State);
handle_single_deletion(CJid, ToDelete, Req, State) ->
    case handle_request(<<"DELETE">>, ToDelete, undefined, CJid) of
        ok ->
            {true, Req, State};
        Other ->
            serve_failure(Other, Req, State)
    end.

handle_request_and_respond(_, undefined, _, _, Req, State) ->
    mongoose_client_api:bad_request(Req, State);
handle_request_and_respond(Method, Jid, Action, CJid, Req, State) ->
    case handle_request(Method, to_binary(Jid), Action, CJid) of
        ok ->
            {true, Req, State};
        not_implemented ->
            Req2 = cowboy_req:reply(501, Req),
            {stop, Req2, State};
        not_found ->
            Req2 = cowboy_req:reply(404, Req),
            {stop, Req2, State}
    end.

serve_failure(not_implemented, Req, State) ->
    Req2 = cowboy_req:reply(501, Req),
    {stop, Req2, State};
serve_failure(not_found, Req, State) ->
    Req2 = cowboy_req:reply(404, Req),
    {stop, Req2, State};
serve_failure({error, ErrorType, Msg}, Req, State) ->
    ?ERROR_MSG("Error while serving http request: ~p: ~s", [ErrorType, Msg]),
    Req2 = cowboy_req:reply(500, Req),
    {stop, Req2, State}.

get_requested_contacts(Req) ->
    Body = get_whole_body(Req, <<"">>),
    case mongoose_client_api:json_to_map(Body) of
        {ok, #{<<"to_delete">> :=  ResultJids}} when is_list(ResultJids) ->
            case [X || X <- ResultJids, is_binary(X)] =:= ResultJids of
                true ->
                    ResultJids;
                false ->
                    undefined
            end;
        _ ->
            undefined
    end.

get_whole_body(Req, Acc) ->
    case cowboy_req:read_body(Req) of
        {ok, Data, _Req2} ->
            <<Data/binary, Acc/binary>>;
        {more, Data, Req2} ->
            get_whole_body(Req2, <<Data/binary, Acc/binary>>)
    end.

handle_request(<<"GET">>, undefined, undefined, CJid) ->
    mongoose_commands:execute(CJid, list_contacts, #{caller => CJid});
handle_request(<<"POST">>, Jid, undefined, CJid) ->
    mongoose_commands:execute(CJid, add_contact, #{caller => CJid,
        jid => Jid});
handle_request(<<"DELETE">>, Jids, _Action, CJid) when is_list(Jids) ->
    mongoose_commands:execute(CJid, delete_contacts, #{caller => CJid,
        jids => Jids});
handle_request(Method, Jid, Action, CJid) ->
    case jid_exists(CJid, Jid) of
        true ->
            handle_contact_request(Method, Jid, Action, CJid);
        false -> not_found
    end.

handle_contact_request(<<"PUT">>, Jid, <<"invite">>, CJid) ->
    mongoose_commands:execute(CJid, subscription, #{caller => CJid,
        jid => Jid, action => atom_to_binary(subscribe, latin1)});
handle_contact_request(<<"PUT">>, Jid, <<"accept">>, CJid) ->
    mongoose_commands:execute(CJid, subscription, #{caller => CJid,
        jid => Jid, action => atom_to_binary(subscribed, latin1)});
handle_contact_request(<<"DELETE">>, Jid, undefined, CJid) ->
    mongoose_commands:execute(CJid, delete_contact, #{caller => CJid,
        jid => Jid});
handle_contact_request(_, _, _, _) ->
    not_implemented.

to_binary(S) when is_binary(S) ->
    S;
to_binary(S) ->
    list_to_binary(S).

-spec jid_exists(binary(), binary()) -> boolean().
jid_exists(CJid, Jid) ->
    FJid = jid:from_binary(CJid),
    Res = mod_roster:get_roster_entry(FJid#jid.luser, FJid#jid.lserver, Jid),
    Res =/= does_not_exist.
