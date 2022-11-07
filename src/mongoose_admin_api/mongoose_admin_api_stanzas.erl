-module(mongoose_admin_api_stanzas).

-behaviour(mongoose_admin_api).
-export([routes/1]).

-behaviour(cowboy_rest).
-export([init/2,
         is_authorized/2,
         content_types_accepted/2,
         allowed_methods/2,
         from_json/2]).

-ignore_xref([to_json/2, from_json/2]).

-import(mongoose_admin_api, [parse_body/1, try_handle_request/3, throw_error/2]).

-type req() :: cowboy_req:req().
-type state() :: mongoose_admin_api:state().

-spec routes(state()) -> mongoose_http_handler:routes().
routes(State) ->
    [{"/stanzas", ?MODULE, State}].

-spec init(req(), state()) -> {cowboy_rest, req(), state()}.
init(Req, State) ->
    mongoose_admin_api:init(Req, State).

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
    case mongoose_stanza_api:send_stanza(undefined, Stanza) of
        {ok, _} ->
            {true, Req, State};
        {_Error, Msg} ->
            throw_error(bad_request, Msg)
    end.

get_stanza(#{stanza := BinStanza}) ->
    case exml:parse(BinStanza) of
        {ok, Stanza} ->
            Stanza;
        {error, _} ->
            throw_error(bad_request, <<"Malformed stanza">>)
    end;
get_stanza(#{}) ->
    throw_error(bad_request, <<"Missing stanza">>).
