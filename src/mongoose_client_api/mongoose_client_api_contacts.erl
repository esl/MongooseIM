-module(mongoose_client_api_contacts).

-behaviour(mongoose_client_api).
-export([routes/0]).

-behaviour(cowboy_rest).
-export([trails/0,
         init/2,
         is_authorized/2,
         content_types_provided/2,
         content_types_accepted/2,
         allowed_methods/2,
         to_json/2,
         from_json/2,
         delete_resource/2]).

-ignore_xref([from_json/2, to_json/2, trails/0]).

-import(mongoose_client_api, [parse_body/1, try_handle_request/3, throw_error/2]).

-type req() :: cowboy_req:req().
-type state() :: map().

-spec routes() -> mongoose_http_handler:routes().
routes() ->
    [{"/contacts/[:jid]", ?MODULE, #{}}].

trails() ->
    mongoose_client_api_contacts_doc:trails().

-spec init(req(), state()) -> {cowboy_rest, req(), state()}.
init(Req, Opts) ->
    mongoose_client_api:init(Req, Opts).

-spec is_authorized(req(), state()) -> {true | {false, iodata()}, req(), state()}.
is_authorized(Req, State) ->
    mongoose_client_api:is_authorized(Req, State).

-spec content_types_provided(req(), state()) ->
          {[{{binary(), binary(), '*'}, atom()}], req(), state()}.
content_types_provided(Req, State) ->
    {[
      {{<<"application">>, <<"json">>, '*'}, to_json}
     ], Req, State}.

-spec content_types_accepted(req(), state()) ->
          {[{{binary(), binary(), '*'}, atom()}], req(), state()}.
content_types_accepted(Req, State) ->
    {[
      {{<<"application">>, <<"json">>, '*'}, from_json}
     ], Req, State}.

-spec allowed_methods(req(), state()) -> {[binary()], req(), state()}.
allowed_methods(Req, State) ->
    {[<<"OPTIONS">>, <<"GET">>, <<"POST">>, <<"PUT">>, <<"DELETE">>],
     Req, State}.

%% @doc Called for a method of type "GET"
-spec to_json(req(), state()) -> {iodata() | stop, req(), state()}.
to_json(Req, State) ->
    try_handle_request(Req, State, fun handle_get/2).

%% @doc Called for a method of type "POST" or "PUT"
-spec from_json(req(), state()) -> {true | stop, req(), state()}.
from_json(Req, State) ->
    F = case cowboy_req:method(Req) of
            <<"POST">> -> fun handle_post/2;
            <<"PUT">> -> fun handle_put/2
        end,
    try_handle_request(Req, State, F).

%% @doc Called for a method of type "DELETE"
-spec delete_resource(req(), state()) -> {true | stop, req(), state()}.
delete_resource(Req, State) ->
    try_handle_request(Req, State, fun handle_delete/2).

%% Internal functions

handle_get(Req, State = #{jid := UserJid}) ->
    Bindings = cowboy_req:bindings(Req),
    assert_no_jid(Bindings),
    {ok, Contacts} = mod_roster_api:list_contacts(UserJid),
    {jiffy:encode(lists:map(fun roster_info/1, Contacts)), Req, State}.

handle_post(Req, State = #{jid := UserJid}) ->
    Args = parse_body(Req),
    ContactJid = get_jid(Args),
    case mod_roster_api:add_contact(UserJid, ContactJid, <<>>, []) of
        {user_not_exist, Reason} ->
            throw_error(not_found, Reason);
        {ok, _} ->
            {true, Req, State}
    end.

handle_put(Req, State = #{jid := UserJid}) ->
    Bindings = cowboy_req:bindings(Req),
    ContactJid = get_jid(Bindings),
    Args = parse_body(Req),
    Action = get_action(Args),
    assert_contact_exists(UserJid, ContactJid),
    {ok, _} =  mod_roster_api:subscription(UserJid, ContactJid, Action),
    {true, Req, State}.

handle_delete(Req, State = #{jid := UserJid}) ->
    Bindings = cowboy_req:bindings(Req),
    case try_get_jid(Bindings) of
        undefined ->
            Args = parse_body(Req),
            ContactJids = get_jids_to_delete(Args),
            NotDeleted = delete_contacts(UserJid, ContactJids),
            RespBody = #{not_deleted => lists:map(fun jid:to_binary/1, NotDeleted)},
            Req2 = cowboy_req:set_resp_body(jiffy:encode(RespBody), Req),
            Req3 = cowboy_req:set_resp_header(<<"content-type">>, <<"application/json">>, Req2),
            {true, Req3, State};
        ContactJid ->
            case mod_roster_api:delete_contact(UserJid, ContactJid) of
                {contact_not_found, Reason} ->
                    throw_error(not_found, Reason);
                {ok, _} ->
                    {true, Req, State}
            end
    end.

-spec roster_info(mod_roster:roster()) -> jiffy:json_value(). %% returns jiffy:json_object()
roster_info(Roster) ->
    #{jid := Jid, subscription := Sub, ask := Ask} = mod_roster:item_to_map(Roster),
    #{jid => jid:to_binary(Jid), subscription => Sub, ask => Ask}.

-spec delete_contacts(jid:jid(), [jid:jid()]) -> [jid:jid()].
delete_contacts(UserJid, ContactJids) ->
    lists:filter(fun(ContactJid) ->
                          case mod_roster_api:delete_contact(UserJid, ContactJid) of
                              {contact_not_found, _Reason} ->
                                  true;
                              {ok, _} ->
                                  false
                          end
                 end, ContactJids).

get_jid(#{jid := JidBin}) ->
    parse_jid(JidBin);
get_jid(#{}) ->
    throw_error(bad_request, <<"Missing JID">>).

try_get_jid(#{jid := JidBin}) ->
    parse_jid(JidBin);
try_get_jid(#{}) ->
    undefined.

assert_no_jid(#{jid := _}) ->
    throw_error(not_found, <<"JID provided but not supported">>);
assert_no_jid(#{}) ->
    ok.

assert_contact_exists(UserJid, ContactJid) ->
    case mod_roster_api:get_contact(UserJid, ContactJid) of
        {ok, _} -> ok;
        {_Error, Msg} -> throw_error(not_found, Msg)
    end.

get_jids_to_delete(#{to_delete := JidsBin}) ->
    lists:map(fun parse_jid/1, JidsBin).

parse_jid(JidBin) ->
    case jid:from_binary(JidBin) of
        error -> throw_error(bad_request, <<"Invalid JID: ", JidBin/binary>>);
        Jid -> Jid
    end.

get_action(#{action := ActionBin}) ->
    decode_action(ActionBin);
get_action(#{}) ->
    throw_error(bad_request, <<"Missing action">>).

decode_action(<<"invite">>) -> subscribe;
decode_action(<<"accept">>) -> subscribed;
decode_action(_) -> throw_error(bad_request, <<"Invalid action">>).
