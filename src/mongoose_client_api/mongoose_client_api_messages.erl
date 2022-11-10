-module(mongoose_client_api_messages).

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
         from_json/2]).

%% Used by mongoose_client_api_sse
-export([encode/2]).

-ignore_xref([to_json/2, from_json/2, trails/0]).

-import(mongoose_client_api, [parse_body/1, parse_qs/1, try_handle_request/3, throw_error/2]).

-type req() :: cowboy_req:req().
-type state() :: mongoose_admin_api:state().

-include_lib("exml/include/exml.hrl").

-spec routes() -> mongoose_http_handler:routes().
routes() ->
    [{"/messages/[:with]", ?MODULE, #{}}].

trails() ->
    mongoose_client_api_messages_doc:trails().

-spec init(req(), state()) -> {cowboy_rest, req(), state()}.
init(Req, State) ->
    mongoose_client_api:init(Req, State).

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
    {[<<"OPTIONS">>, <<"GET">>, <<"POST">>], Req, State}.

%% @doc Called for a method of type "GET"
-spec to_json(req(), state()) -> {iodata() | stop, req(), state()}.
to_json(Req, State) ->
    try_handle_request(Req, State, fun handle_get/2).

%% @doc Called for a method of type "POST"
-spec from_json(req(), state()) -> {true | stop, req(), state()}.
from_json(Req, State) ->
    try_handle_request(Req, State, fun handle_post/2).

handle_get(Req, State = #{jid := OwnerJid}) ->
    Bindings = cowboy_req:bindings(Req),
    WithJid = get_with_jid(Bindings),
    Args = parse_qs(Req),
    Limit = get_limit(Args),
    Before = get_before(Args),
    {ok, {Rows, _Limit}} =
        mongoose_stanza_api:lookup_recent_messages(OwnerJid, WithJid, Before, Limit, false),
    Resp = [make_json_msg(Msg, MAMId) || #{id := MAMId, packet := Msg} <- Rows],
    {jiffy:encode(Resp), Req, State}.

handle_post(Req, State = #{jid := UserJid}) ->
    Args = parse_body(Req),
    To = get_to(Args),
    Body = get_body(Args),
    {ok, Resp} = mongoose_stanza_api:send_chat_message(UserJid, undefined, To, Body),
    Req2 = cowboy_req:set_resp_body(jiffy:encode(Resp), Req),
    {true, Req2, State}.

get_limit(#{limit := LimitBin}) ->
    try
        Limit = binary_to_integer(LimitBin),
        true = Limit >= 0,
        Limit
    catch
        _:_ -> throw_error(bad_request, <<"Invalid limit">>)
    end;
get_limit(#{}) -> 50.

get_before(#{before := BeforeBin}) ->
    try
        1000 * binary_to_integer(BeforeBin)
    catch
        _:_ -> throw_error(bad_request, <<"Invalid value of 'before'">>)
    end;
get_before(#{}) -> undefined.

get_with_jid(#{with := With}) ->
    case jid:from_binary(With) of
        error -> throw_error(bad_request, <<"Invalid interlocutor JID">>);
        WithJid -> WithJid
    end;
get_with_jid(#{}) -> undefined.

get_to(#{to := To}) ->
    case jid:from_binary(To) of
        error -> throw_error(bad_request, <<"Invalid recipient JID">>);
        ToJid -> ToJid
    end;
get_to(#{}) -> throw_error(bad_request, <<"Missing recipient JID">>).

get_body(#{body := Body}) -> Body;
get_body(#{}) -> throw_error(bad_request, <<"Missing message body">>).

make_json_msg(Msg, MAMId) ->
    {Microsec, _} = mod_mam_utils:decode_compact_uuid(MAMId),
    encode(Msg, Microsec div 1000).

-spec encode(exml:item(), integer()) -> map().
encode(Msg, Timestamp) ->
    BodyTag = exml_query:path(Msg, [{element, <<"body">>}]),
    L = [{<<"from">>, exml_query:attr(Msg, <<"from">>)},
          {<<"to">>, exml_query:attr(Msg, <<"to">>)},
          {<<"id">>, exml_query:attr(Msg, <<"id">>)},
          {<<"body">>, exml_query:cdata(BodyTag)},
          {<<"timestamp">>, Timestamp}] ++ extensions(Msg) ++ thread_and_parent(Msg),
    maps:from_list(L).

extensions(Msg) ->
    SmackNS = <<"http://www.jivesoftware.com/xmlns/xmpp/properties">>,
    RawMsgProps = exml_query:subelement_with_name_and_ns(Msg, <<"properties">>, SmackNS),
    case RawMsgProps of
        #xmlel{children = Children} ->
            Props = [convert_prop_child(Child) || Child <- Children],
            [{<<"properties">>, maps:from_list(Props)}];
        _ ->
            []
    end.

thread_and_parent(Msg) ->
    case exml_query:path(Msg, [{element, <<"thread">>}, cdata]) of
        undefined -> [];
        Thread -> [{<<"thread">>, Thread} | parent(Msg)]
    end.

parent(Msg) ->
    case exml_query:path(Msg, [{element, <<"thread">>}, {attr, <<"parent">>}]) of
        undefined -> [];
        ThreadParent -> [{<<"parent">>, ThreadParent}]
    end.

convert_prop_child(Child)->
    Name = exml_query:path(Child, [{element, <<"name">>}, cdata]),
    Value = exml_query:path(Child, [{element, <<"value">>}, cdata]),
    {Name, Value}.
