-module(mongoose_admin_api_contacts).

-behaviour(mongoose_admin_api).
-export([routes/1]).

-behaviour(cowboy_rest).
-export([init/2,
         is_authorized/2,
         content_types_provided/2,
         content_types_accepted/2,
         allowed_methods/2,
         to_json/2,
         from_json/2,
         delete_resource/2]).

-ignore_xref([to_json/2, from_json/2]).

-import(mongoose_admin_api, [parse_body/1, try_handle_request/3, throw_error/2]).

-type req() :: cowboy_req:req().
-type state() :: mongoose_admin_api:state().

-spec routes(state()) -> mongoose_http_handler:routes().
routes(State) ->
    [{"/contacts/:user/[:contact]", ?MODULE, State},
     {"/contacts/:user/:contact/manage", ?MODULE, State#{suffix => manage}}].

-spec init(req(), state()) -> {cowboy_rest, req(), state()}.
init(Req, State) ->
    mongoose_admin_api:init(Req, State).

-spec is_authorized(req(), state()) -> {true | {false, iodata()}, req(), state()}.
is_authorized(Req, State) ->
    mongoose_admin_api:is_authorized(Req, State).

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
    {[<<"OPTIONS">>, <<"GET">>, <<"POST">>, <<"PUT">>, <<"DELETE">>], Req, State}.

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

handle_get(Req, State) ->
    Bindings = cowboy_req:bindings(Req),
    UserJid = get_user_jid(Bindings),
    case mod_roster_api:list_contacts(UserJid) of
        {ok, Rosters} ->
            {jiffy:encode(lists:map(fun roster_info/1, Rosters)), Req, State};
        {unknown_domain, Reason} ->
            throw_error(not_found, Reason)
    end.

handle_post(Req, State) ->
    Bindings = cowboy_req:bindings(Req),
    UserJid = get_user_jid(Bindings),
    Args = parse_body(Req),
    ContactJid = get_jid(Args),
    case mod_roster_api:add_contact(UserJid, ContactJid, <<>>, []) of
        {unknown_domain, Reason} ->
            throw_error(not_found, Reason);
        {user_not_exist, Reason} ->
            throw_error(not_found, Reason);
        {ok, _} ->
            {true, Req, State}
    end.

handle_put(Req, State) ->
    Bindings = cowboy_req:bindings(Req),
    UserJid = get_user_jid(Bindings),
    ContactJid = get_contact_jid(Bindings),
    Args = parse_body(Req),
    Action = get_action(Args, State),
    case perform_action(UserJid, ContactJid, Action, State) of
        {unknown_domain, Reason} ->
            throw_error(not_found, Reason);
        {user_not_exist, Reason} ->
            throw_error(not_found, Reason);
        {contact_not_found, Reason} ->
            throw_error(not_found, Reason);
        {ok, _} ->
            {true, Req, State}
    end.

handle_delete(Req, State) ->
    Bindings = cowboy_req:bindings(Req),
    UserJid = get_user_jid(Bindings),
    ContactJid = get_contact_jid(Bindings),
    case mod_roster_api:delete_contact(UserJid, ContactJid) of
        {contact_not_found, Reason} ->
            throw_error(not_found, Reason);
        {ok, _} ->
            {true, Req, State}
    end.

perform_action(UserJid, ContactJid, Action, #{suffix := manage}) ->
    mod_roster_api:set_mutual_subscription(UserJid, ContactJid, Action);
perform_action(UserJid, ContactJid, Action, #{}) ->
    mod_roster_api:subscription(UserJid, ContactJid, Action).

-spec roster_info(mod_roster:roster()) -> jiffy:json_value(). %% returns jiffy:json_object()
roster_info(Roster) ->
    #{jid := Jid, subscription := Sub, ask := Ask} = mod_roster:item_to_map(Roster),
    #{jid => jid:to_binary(Jid), subscription => Sub, ask => Ask}.

get_jid(#{jid := JidBin}) ->
    case jid:from_binary(JidBin) of
        error -> throw_error(bad_request, <<"Invalid JID">>);
        Jid -> Jid
    end;
get_jid(#{}) ->
    throw_error(bad_request, <<"Missing JID">>).

get_user_jid(#{user := User}) ->
    case jid:from_binary(User) of
        error -> throw_error(bad_request, <<"Invalid user JID">>);
        Jid -> Jid
    end.

get_contact_jid(#{contact := Contact}) ->
    case jid:from_binary(Contact) of
        error -> throw_error(bad_request, <<"Invalid contact JID">>);
        Jid -> Jid
    end;
get_contact_jid(#{}) ->
    throw_error(bad_request, <<"Missing contact JID">>).

get_action(#{action := ActionBin}, State) ->
    decode_action(ActionBin, maps:get(suffix, State, no_suffix));
get_action(#{}, _State) ->
    throw_error(bad_request, <<"Missing action">>).

decode_action(<<"subscribe">>, no_suffix) -> subscribe;
decode_action(<<"subscribed">>, no_suffix) -> subscribed;
decode_action(<<"connect">>, manage) -> connect;
decode_action(<<"disconnect">>, manage) -> disconnect;
decode_action(_, _) -> throw_error(bad_request, <<"Invalid action">>).
