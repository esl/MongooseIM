-module(mongoose_admin_api_users).

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

-import(mongoose_admin_api, [parse_body/1, try_handle_request/3, throw_error/2, resource_created/4]).

-type req() :: cowboy_req:req().
-type state() :: mongoose_admin_api:state().

-spec routes(state()) -> mongoose_http_handler:routes().
routes(State) ->
    [{"/users/:domain/[:username]", ?MODULE, State}].

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
    #{domain := Domain} = cowboy_req:bindings(Req),
    Users = mongoose_account_api:list_users(Domain),
    {_, UsersList} = Users,
    {jiffy:encode(UsersList), Req, State}.

handle_post(Req, State) ->
    #{domain := Domain} = cowboy_req:bindings(Req),
    Args = parse_body(Req),
    UserName = get_user_name(Args),
    Password = get_password(Args),
    case mongoose_account_api:register_user(UserName, Domain, Password) of
        {exists, Reason} ->
            throw_error(denied, Reason);
        {invalid_jid, Reason} ->
            throw_error(bad_request, Reason);
        {cannot_register, Reason} ->
            throw_error(denied, Reason);
        {ok, Result} ->
            Path = [cowboy_req:uri(Req), "/", UserName],
            resource_created(Req, State, Path, Result)
    end.

handle_put(Req, State) ->
    #{domain := Domain} = Bindings = cowboy_req:bindings(Req),
    UserName = get_user_name(Bindings),
    Args = parse_body(Req),
    Password = get_new_password(Args),
    case mongoose_account_api:change_password(UserName, Domain, Password) of
        {empty_password, Reason} ->
            throw_error(bad_request, Reason);
        {invalid_jid, Reason} ->
            throw_error(bad_request, Reason);
        {not_allowed, Reason} ->
            throw_error(denied, Reason);
        {ok, _} ->
            {true, Req, State}
    end.

handle_delete(Req, State) ->
    #{domain := Domain} = Bindings = cowboy_req:bindings(Req),
    UserName = get_user_name(Bindings),
    case mongoose_account_api:unregister_user(UserName, Domain) of
        {invalid_jid, Reason} ->
            throw_error(bad_request, Reason);
        {not_allowed, Reason} ->
            throw_error(denied, Reason);
        {ok, _} ->
            {true, Req, State}
    end.

get_user_name(#{username := UserName}) -> UserName;
get_user_name(#{}) -> throw_error(bad_request, <<"Missing user name">>).

get_password(#{password := Password}) -> Password;
get_password(#{}) -> throw_error(bad_request, <<"Missing password">>).

get_new_password(#{newpass := Password}) -> Password;
get_new_password(#{}) -> throw_error(bad_request, <<"Missing new password">>).
