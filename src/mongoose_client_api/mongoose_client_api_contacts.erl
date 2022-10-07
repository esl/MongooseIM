-module(mongoose_client_api_contacts).

-behaviour(mongoose_client_api).
-export([routes/0]).

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

-ignore_xref([from_json/2, to_json/2, trails/0, forbidden_request/2]).

-type req() :: cowboy_req:req().
-type state() :: map().

-spec routes() -> mongoose_http_handler:routes().
routes() ->
    [{"/contacts/[:jid]", ?MODULE, #{}}].

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

-spec forbidden_request(req(), state()) -> {stop, req(), state()}.
forbidden_request(Req, State) ->
    Req1 = cowboy_req:reply(403, Req),
    {stop, Req1, State}.

-spec to_json(req(), state()) -> {iodata() | stop, req(), state()}.
to_json(Req, State) ->
    Method = cowboy_req:method(Req),
    Jid = cowboy_req:binding(jid, Req),
    case Jid of
        undefined ->
            {ok, Res} = handle_request(Method, State),
            {jiffy:encode(Res), Req, State};
        _ ->
            Req2 = cowboy_req:reply(404, Req),
            {stop, Req2, State}
    end.

-spec from_json(req(), state()) -> {true | stop, req(), state()}.
from_json(Req, State) ->
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
            handle_request_and_respond(Method, Jid, Action, Req1, State);
        _ ->
            mongoose_client_api:bad_request(Req1, State)
    end.

%% @doc Called for a method of type "DELETE"
-spec delete_resource(req(), state()) -> {true | stop, req(), state()}.
delete_resource(Req, State) ->
    Jid = cowboy_req:binding(jid, Req),
    case Jid of
        undefined ->
            handle_multiple_deletion(get_requested_contacts(Req), Req, State);
        _ ->
            handle_single_deletion(Jid, Req, State)
    end.

-spec handle_multiple_deletion(undefined | [jid:literal_jid()], req(), state()) ->
           {true | stop, req(), state()}.
handle_multiple_deletion(undefined, Req, State) ->
    mongoose_client_api:bad_request(Req, State);
handle_multiple_deletion(ToDelete, Req, State = #{jid := CJid}) ->
    NotDeleted = delete_contacts(CJid, ToDelete),
    RespBody = #{not_deleted => NotDeleted},
    Req2 = cowboy_req:set_resp_body(jiffy:encode(RespBody), Req),
    Req3 = cowboy_req:set_resp_header(<<"content-type">>, <<"application/json">>, Req2),
    {true, Req3, State}.

-spec handle_single_deletion(undefined | jid:literal_jid(), req(), state()) ->
          {true | stop, req(), state()}.
handle_single_deletion(undefined, Req, State) ->
    mongoose_client_api:bad_request(Req, State);
handle_single_deletion(ToDelete, Req, State = #{jid := CJid}) ->
    ok = delete_contact(CJid, ToDelete),
    {true, Req, State}.

-spec handle_request_and_respond(Method :: binary(), jid:literal_jid() | undefined,
                                 Action :: binary() | undefined, req(), state()) ->
          {true | stop, req(), state()}.
handle_request_and_respond(_, undefined, _, Req, State) ->
    mongoose_client_api:bad_request(Req, State);
handle_request_and_respond(Method, Jid, Action, Req, State) ->
    case handle_request(Method, Jid, Action, State) of
        ok ->
            {true, Req, State};
        not_implemented ->
            Req2 = cowboy_req:reply(501, Req),
            {stop, Req2, State};
        not_found ->
            Req2 = cowboy_req:reply(404, Req),
            {stop, Req2, State}
    end.

-spec get_requested_contacts(req()) -> [jid:literal_jid()] | undefined.
get_requested_contacts(Req) ->
    Body = get_whole_body(Req, <<"">>),
    case mongoose_client_api:json_to_map(Body) of
        {ok, #{<<"to_delete">> :=  ResultJids}} when is_list(ResultJids) ->
            case [X || X <- ResultJids, is_binary(X)] of
                ResultJids ->
                    ResultJids;
                _ ->
                    undefined
            end;
        _ ->
            undefined
    end.

-spec get_whole_body(req(), binary()) -> binary().
get_whole_body(Req, Acc) ->
    case cowboy_req:read_body(Req) of
        {ok, Data, _Req2} ->
            <<Data/binary, Acc/binary>>;
        {more, Data, Req2} ->
            get_whole_body(Req2, <<Data/binary, Acc/binary>>)
    end.

-spec handle_request(binary(), state()) -> {ok, [jiffy:json_object()]} | {error, any()}.
handle_request(<<"GET">>, #{jid := CallerJid}) ->
    list_contacts(CallerJid).

-spec handle_request(Method :: binary(), jid:literal_jid() | undefined,
                     Action :: binary() | undefined, state()) ->
          ok | not_found | not_implemented | {error, any()}.
handle_request(<<"POST">>, Jid, undefined, #{jid := CallerJid}) ->
    add_contact(CallerJid, Jid);
handle_request(Method, Jid, Action, #{jid := CallerJid, creds := Creds}) ->
    HostType = mongoose_credentials:host_type(Creds),
    case contact_exists(HostType, CallerJid, jid:from_binary(Jid)) of
        true ->
            handle_contact_request(Method, Jid, Action, CallerJid);
        false -> not_found
    end.

handle_contact_request(<<"PUT">>, Jid, <<"invite">>, CJid) ->
    subscription(CJid, Jid, atom_to_binary(subscribe, latin1));
handle_contact_request(<<"PUT">>, Jid, <<"accept">>, CJid) ->
    subscription(CJid, Jid, atom_to_binary(subscribed, latin1));
handle_contact_request(_, _, _, _) ->
    not_implemented.

-spec contact_exists(mongooseim:host_type(), jid:jid(), jid:jid() | error) -> boolean().
contact_exists(_, _, error) -> false;
contact_exists(HostType, CallerJid, Jid) ->
    LJid = jid:to_lower(Jid),
    Res = mod_roster:get_roster_entry(HostType, CallerJid, LJid, short),
    Res =/= does_not_exist andalso Res =/= error.

%% Internal functions

-spec list_contacts(jid:jid()) -> {ok, [jiffy:json_object()]} | {error, any()}.
list_contacts(Caller) ->
    case mod_roster_api:list_contacts(Caller) of
        {ok, Rosters} ->
            {ok, lists:map(fun roster_info/1, Rosters)};
        {ErrorCode, _Msg} ->
            {error, ErrorCode}
    end.

-spec roster_info(mod_roster:roster()) -> jiffy:json_object().
roster_info(Roster) ->
    #{jid := Jid, subscription := Sub, ask := Ask} = mod_roster:item_to_map(Roster),
    #{jid => jid:to_binary(Jid), subscription => Sub, ask => Ask}.

-spec add_contact(jid:jid(), jid:literal_jid()) -> ok | {error, any()}.
add_contact(Caller, JabberID) ->
    add_contact(Caller, JabberID, <<>>, []).

add_contact(CallerJid, Other, Name, Groups) ->
    case jid:from_binary(Other) of
        error ->
            {error, invalid_jid};
        Jid ->
            Res = mod_roster_api:add_contact(CallerJid, Jid, Name, Groups),
            skip_result_msg(Res)
    end.

-spec delete_contacts(jid:jid(), [jid:literal_jid()]) -> [jid:literal_jid()].
delete_contacts(Caller, ToDelete) ->
    maybe_delete_contacts(Caller, ToDelete, []).

maybe_delete_contacts(_, [], NotDeleted) -> NotDeleted;
maybe_delete_contacts(Caller, [H | T], NotDeleted) ->
    case delete_contact(Caller, H) of
        ok ->
            maybe_delete_contacts(Caller, T, NotDeleted);
        _Error ->
            maybe_delete_contacts(Caller, T, NotDeleted ++ [H])
    end.

-spec delete_contact(jid:jid(), jid:literal_jid()) -> ok | {error, any()}.
delete_contact(CallerJID, Other) ->
    case jid:from_binary(Other) of
        error ->
            {error, invalid_jid};
        Jid ->
            Res = mod_roster_api:delete_contact(CallerJID, Jid),
            skip_result_msg(Res)
    end.

-spec subscription(jid:jid(), jid:literal_jid(), binary()) -> ok | {error, any()}.
subscription(CallerJID, Other, Action) ->
    case decode_action(Action) of
        error ->
            {error, {bad_request, <<"invalid action">>}};
        Act ->
            case jid:from_binary(Other) of
                error ->
                    {error, invalid_jid};
                Jid ->
                    Res = mod_roster_api:subscription(CallerJID, Jid, Act),
                    skip_result_msg(Res)
            end
    end.

decode_action(<<"subscribe">>) -> subscribe;
decode_action(<<"subscribed">>) -> subscribed;
decode_action(_) -> error.

skip_result_msg({ok, _Msg}) -> ok;
skip_result_msg({ErrCode, _Msg}) -> {error, ErrCode}.
