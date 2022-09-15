-module(mongoose_admin_api_stanzas).
-behaviour(cowboy_rest).

-export([init/2,
         is_authorized/2,
         content_types_accepted/2,
         allowed_methods/2,
         from_json/2]).

-ignore_xref([to_json/2, from_json/2]).

-import(mongoose_admin_api, [parse_body/1, try_handle_request/3, throw_error/2]).

-type req() :: cowboy_req:req().
-type state() :: map().

-spec init(req(), state()) -> {cowboy_rest, req(), state()}.
init(Req, Opts) ->
    mongoose_admin_api:init(Req, Opts).

-spec is_authorized(req(), state()) -> {true | {false, iodata()}, req(), state()}.
is_authorized(Req, State) ->
    mongoose_admin_api:is_authorized(Req, State).

-spec content_types_accepted(req(), state()) ->
          {[{{binary(), binary(), '*'}, atom()}], req(), state()}.
content_types_accepted(Req, State) ->
    {[
      {{<<"application">>, <<"json">>, '*'}, from_json}
     ], Req, State}.

-spec allowed_methods(req(), state()) -> {[binary()], req(), state()}.
allowed_methods(Req, State) ->
    {[<<"OPTIONS">>, <<"POST">>], Req, State}.

%% @doc Called for a method of type "POST"
-spec from_json(req(), state()) -> {true | stop, req(), state()}.
from_json(Req, State) ->
    try_handle_request(Req, State, fun handle_post/2).

%% Internal functions

handle_post(Req, State) ->
    Args = parse_body(Req),
    Stanza = get_stanza(Args),
    From = get_from_jid(Stanza),
    To = get_to_jid(Stanza),
    case mongoose_stanza_helper:route(From, To, Stanza, true) of
        {error, #{what := unknown_domain}} ->
            throw_error(bad_request, <<"Unknown domain">>);
        {error, #{what := unknown_user}} ->
            throw_error(bad_request, <<"Unknown user">>);
        {ok, _} ->
            {true, Req, State}
    end.

get_stanza(#{stanza := BinStanza}) ->
    case exml:parse(BinStanza) of
        {ok, Stanza} ->
            Stanza;
        {error, _} ->
            throw_error(bad_request, <<"Malformed stanza">>)
    end;
get_stanza(#{}) -> throw_error(bad_request, <<"Missing stanza">>).

get_from_jid(Stanza) ->
    case exml_query:attr(Stanza, <<"from">>) of
        undefined ->
            throw_error(bad_request, <<"Missing sender JID">>);
        JidBin ->
            case jid:from_binary(JidBin) of
                error -> throw_error(bad_request, <<"Invalid sender JID">>);
                Jid -> Jid
            end
    end.

get_to_jid(Stanza) ->
    case exml_query:attr(Stanza, <<"to">>) of
        undefined ->
            throw_error(bad_request, <<"Missing recipient JID">>);
        JidBin ->
            case jid:from_binary(JidBin) of
                error -> throw_error(bad_request, <<"Invalid recipient JID">>);
                Jid -> Jid
            end
    end.
