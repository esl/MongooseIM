-module(mongoose_admin_api_messages).

-behaviour(mongoose_admin_api).
-export([routes/1]).

-behaviour(cowboy_rest).
-export([init/2,
         is_authorized/2,
         content_types_provided/2,
         content_types_accepted/2,
         allowed_methods/2,
         to_json/2,
         from_json/2]).

-ignore_xref([to_json/2, from_json/2]).

-import(mongoose_admin_api, [parse_body/1, parse_qs/1, try_handle_request/3, throw_error/2]).

-type req() :: cowboy_req:req().
-type state() :: mongoose_admin_api:state().

-spec routes(state()) -> mongoose_http_handler:routes().
routes(State) ->
    [{"/messages/:owner/:with", ?MODULE, State},
     {"/messages/[:owner]", ?MODULE, State}].

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
    {[<<"OPTIONS">>, <<"GET">>, <<"POST">>], Req, State}.

%% @doc Called for a method of type "GET"
-spec to_json(req(), state()) -> {iodata() | stop, req(), state()}.
to_json(Req, State) ->
    try_handle_request(Req, State, fun handle_get/2).

%% @doc Called for a method of type "POST"
-spec from_json(req(), state()) -> {true | stop, req(), state()}.
from_json(Req, State) ->
    try_handle_request(Req, State, fun handle_post/2).

%% Internal functions

handle_get(Req, State) ->
    Bindings = cowboy_req:bindings(Req),
    OwnerJid = get_owner_jid(Bindings),
    WithJid = get_with_jid(Bindings),
    Args = parse_qs(Req),
    Limit = get_limit(Args),
    Before = get_before(Args),
    case mongoose_stanza_api:lookup_recent_messages(OwnerJid, WithJid, Before, Limit, true) of
        {ok, {Rows, _Limit}} ->
            Messages = lists:map(fun row_to_map/1, Rows),
            {jiffy:encode(Messages), Req, State};
        {unknown_user, Msg} ->
            throw_error(bad_request, Msg)
    end.

handle_post(Req, State) ->
    Args = parse_body(Req),
    From = get_caller(Args),
    To = get_to(Args),
    Body = get_body(Args),
    case mongoose_stanza_api:send_chat_message(undefined, From, To, Body) of
        {ok, _} ->
            {true, Req, State};
        {_Error, Msg} ->
            throw_error(bad_request, Msg)
    end.

-spec row_to_map(mod_mam:message_row()) -> map().
row_to_map(#{id := Id, jid := From, packet := Msg}) ->
    Jbin = jid:to_binary(From),
    {Msec, _} = mod_mam_utils:decode_compact_uuid(Id),
    MsgId = exml_query:attr(Msg, <<"id">>, <<>>),
    Body = exml_query:path(Msg, [{element, <<"body">>}, cdata]),
    #{sender => Jbin, timestamp => round(Msec / 1000000), message_id => MsgId, body => Body}.

get_limit(#{limit := LimitBin}) ->
    try
        Limit = binary_to_integer(LimitBin),
        true = Limit >= 0,
        Limit
    catch
        _:_ -> throw_error(bad_request, <<"Invalid limit">>)
    end;
get_limit(#{}) -> 100.

get_before(#{before := BeforeBin}) ->
    try
        1000000 * binary_to_integer(BeforeBin)
    catch
        _:_ -> throw_error(bad_request, <<"Invalid value of 'before'">>)
    end;
get_before(#{}) -> undefined.

get_owner_jid(#{owner := Owner}) ->
    case jid:from_binary(Owner) of
        error -> throw_error(bad_request, <<"Invalid owner JID">>);
        OwnerJid -> OwnerJid
    end;
get_owner_jid(#{}) -> throw_error(not_found, <<"Missing owner JID">>).

get_with_jid(#{with := With}) ->
    case jid:from_binary(With) of
        error -> throw_error(bad_request, <<"Invalid interlocutor JID">>);
        WithJid -> WithJid
    end;
get_with_jid(#{}) -> undefined.

get_caller(#{caller := Caller}) ->
    case jid:from_binary(Caller) of
        error -> throw_error(bad_request, <<"Invalid sender JID">>);
        CallerJid -> CallerJid
    end;
get_caller(#{}) -> throw_error(bad_request, <<"Missing sender JID">>).

get_to(#{to := To}) ->
    case jid:from_binary(To) of
        error -> throw_error(bad_request, <<"Invalid recipient JID">>);
        ToJid -> ToJid
    end;
get_to(#{}) -> throw_error(bad_request, <<"Missing recipient JID">>).

get_body(#{body := Body}) -> Body;
get_body(#{}) -> throw_error(bad_request, <<"Missing message body">>).
