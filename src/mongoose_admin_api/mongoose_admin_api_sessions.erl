-module(mongoose_admin_api_sessions).

-behaviour(mongoose_admin_api).
-export([routes/1]).

-behaviour(cowboy_rest).
-export([init/2,
         is_authorized/2,
         content_types_provided/2,
         allowed_methods/2,
         to_json/2,
         delete_resource/2]).

-ignore_xref([to_json/2, from_json/2]).

-import(mongoose_admin_api, [try_handle_request/3, throw_error/2]).

-type req() :: cowboy_req:req().
-type state() :: mongoose_admin_api:state().

-spec routes(state()) -> mongoose_http_handler:routes().
routes(State) ->
    [{"/sessions/:domain/[:username]/[:resource]", ?MODULE, State}].

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

-spec allowed_methods(req(), state()) -> {[binary()], req(), state()}.
allowed_methods(Req, State) ->
    {[<<"OPTIONS">>, <<"GET">>, <<"DELETE">>], Req, State}.

%% @doc Called for a method of type "GET"
-spec to_json(req(), state()) -> {iodata() | stop, req(), state()}.
to_json(Req, State) ->
    try_handle_request(Req, State, fun handle_get/2).

%% @doc Called for a method of type "DELETE"
-spec delete_resource(req(), state()) -> {true | stop, req(), state()}.
delete_resource(Req, State) ->
    try_handle_request(Req, State, fun handle_delete/2).

%% Internal functions

handle_get(Req, State) ->
    #{domain := Domain} = cowboy_req:bindings(Req),
    {ok, Sessions} = mongoose_session_api:list_resources(Domain),
    {jiffy:encode(Sessions), Req, State}.

handle_delete(Req, State) ->
    #{domain := Domain} = Bindings = cowboy_req:bindings(Req),
    UserName = get_user_name(Bindings),
    Resource = get_resource(Bindings),
    case mongoose_session_api:kick_session(jid:make(UserName, Domain, Resource), <<"kicked">>) of
        {ok, _} ->
            {true, Req, State};
        {no_session, Reason} ->
            throw_error(not_found, Reason)
    end.

get_user_name(#{username := UserName}) -> UserName;
get_user_name(#{}) -> throw_error(bad_request, <<"Missing user name">>).

%% Resource is matched first, so it is not possible for it to be missing
get_resource(#{resource := Resource}) -> Resource.
