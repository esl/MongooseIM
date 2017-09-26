-module(mongoose_client_api_contacts).

-export([init/3]).
-export([rest_init/2]).
-export([content_types_provided/2]).
-export([content_types_accepted/2]).
-export([is_authorized/2]).
-export([allowed_methods/2]).

-export([forbidden_request/2]).

-export([to_json/2]).
-export([from_json/2]).
-export([delete_resource/2]).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include_lib("exml/include/exml.hrl").

init(_Transport, _Req, _Opts) ->
    {upgrade, protocol, cowboy_rest}.

rest_init(Req, HandlerOpts) ->
    mongoose_client_api:rest_init(Req, HandlerOpts).

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
    cowboy_req:reply(403, Req),
    {halt, Req, State}.

to_json(Req, #{jid := Caller} = State) ->
    CJid = jid:to_binary(Caller),
    {Method, _} = cowboy_req:method(Req),
    {Jid, _} = cowboy_req:binding(jid, Req),
    case Jid of
        undefined ->
            {ok, Res} = handle_request(Method, Jid, undefined, CJid),
            {jiffy:encode(lists:flatten([Res])), Req, State};
        _ ->
            {ok, Req2} = cowboy_req:reply(404, Req),
            {halt, Req2, State}
    end.


from_json(Req, #{jid := Caller} = State) ->
    CJid = jid:to_binary(Caller),
    {Method, Req2} = cowboy_req:method(Req),
    {ok, Body, Req1} = cowboy_req:body(Req),
    JSONData = jiffy:decode(Body, [return_maps]),
    Jid = case maps:get(<<"jid">>, JSONData, undefined) of
              undefined ->
                  {J, _} = cowboy_req:binding(jid, Req1),
                  J;
              J -> J
          end,
    Action = maps:get(<<"action">>, JSONData, undefined),
    handle_request_and_respond(Method, Jid, Action, CJid, Req2, State).

%% @doc Called for a method of type "DELETE"
delete_resource(Req, #{jid := Caller} = State) ->
    {Jid, Req2} = cowboy_req:binding(jid, Req),
    CJid = jid:to_binary(Caller),
    handle_request_and_respond(<<"DELETE">>, Jid, undefined, CJid, Req2, State).

handle_request_and_respond(Method, Jid, Action, CJid, Req, State) ->
    case handle_request(Method, to_binary(Jid), Action, CJid) of
        ok ->
            {true, Req, State};
        not_implemented ->
            {ok, Req2} = cowboy_req:reply(501, Req),
            {halt, Req2, State};
        not_found ->
            {ok, Req2} = cowboy_req:reply(404, Req),
            {halt, Req2, State}
    end.


handle_request(<<"GET">>, undefined, undefined, CJid) ->
    mongoose_commands:execute(CJid, list_contacts, #{caller => CJid});
handle_request(<<"POST">>, Jid, undefined, CJid) ->
    mongoose_commands:execute(CJid, add_contact, #{caller => CJid,
        jid => Jid});
handle_request(Method, Jid, Action, CJid) ->
    case jid_exists(CJid, Jid) of
        true -> handle_contact_request(Method, Jid, Action, CJid);
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
