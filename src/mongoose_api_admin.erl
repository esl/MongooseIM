%%%-------------------------------------------------------------------
%%% @author ludwikbukowski
%%% @copyright (C) 2016, Erlang Solutions Ltd.
%%% Created : 05. Jul 2016 12:59
%%%-------------------------------------------------------------------

%% @doc MongooseIM REST HTTP API for administration.
%% This module implements cowboy REST callbacks and
%% passes the requests on to the http api backend module.
%% @end
-module(mongoose_api_admin).
-author("ludwikbukowski").
-behaviour(cowboy_rest).

%% ejabberd_cowboy exports
-export([cowboy_router_paths/2]).

%% cowboy_rest exports
-export([allowed_methods/2,
         content_types_provided/2,
         terminate/3,
         init/2,
         options/2,
         content_types_accepted/2,
         delete_resource/2,
         is_authorized/2]).
%% local callbacks
-export([to_json/2, from_json/2]).
-include("mongoose_api.hrl").
-include("mongoose.hrl").

-import(mongoose_api_common, [error_response/3,
                              error_response/4,
                              action_to_method/1,
                              method_to_action/1,
                              error_code/1,
                              process_request/6,
                              parse_request_body/1]).

-type credentials() :: {Username :: binary(), Password :: binary()} | any.

%%--------------------------------------------------------------------
%% ejabberd_cowboy callbacks
%%--------------------------------------------------------------------

%% @doc This is implementation of ejabberd_cowboy callback.
%% Returns list of all available http paths.
%%-spec cowboy_router_paths(ejabberd_cowboy:path(), ejabberd_cowboy:options()) ->
%%    ejabberd_cowboy:implemented_result().
%%cowboy_router_paths(Base, Opts) ->
%%    mongoose_api_common:cowboy_router_paths(admin, Base, Opts).

%% @doc This is implementation of ejabberd_cowboy callback.
%% Returns list of all available http paths.
-spec cowboy_router_paths(
                          ejabberd_cowboy:path(),
                          ejabberd_cowboy:options()) ->
                             ejabberd_cowboy:implemented_result().
cowboy_router_paths(Base, Opts) ->
    ejabberd_hooks:add(register_command, global, mongoose_api_common, reload_dispatches, 50),
    ejabberd_hooks:add(unregister_command, global, mongoose_api_common, reload_dispatches, 50),
    try
        Commands = mongoose_commands:list(admin),
        [handler_path(Base, Command, Opts) || Command <- Commands]
    catch
        Class:Err:StackTrace ->
            ?LOG_ERROR(#{what => getting_command_list_error,
                         class => Class, reason => Err, stacktrace => StackTrace}),
            []
    end.

%%--------------------------------------------------------------------
%% cowboy_rest callbacks
%%--------------------------------------------------------------------

init(Req, Opts) ->
    Bindings = maps:to_list(cowboy_req:bindings(Req)),
    CommandCategory = proplists:get_value(command_category, Opts),
    CommandSubCategory = proplists:get_value(command_subcategory, Opts),
    Auth = proplists:get_value(auth, Opts, any),
    State = #http_api_state{allowed_methods = mongoose_api_common:get_allowed_methods(admin),
                            bindings = Bindings,
                            command_category = CommandCategory,
                            command_subcategory = CommandSubCategory,
                            auth = Auth},
    {cowboy_rest, Req, State}.

options(Req, State) ->
    Req1 = set_cors_headers(Req),
    {ok, Req1, State}.

set_cors_headers(Req) ->
    Req1 = cowboy_req:set_resp_header(<<"Access-Control-Allow-Methods">>,
                                      <<"GET, OPTIONS, PUT, POST, DELETE">>, Req),
    Req2 = cowboy_req:set_resp_header(<<"Access-Control-Allow-Origin">>,
                                      <<"*">>, Req1),
    cowboy_req:set_resp_header(<<"Access-Control-Allow-Headers">>,
                               <<"Content-Type">>, Req2).

allowed_methods(Req, #http_api_state{command_category = Name} = State) ->
    CommandList = mongoose_commands:list(admin, Name),
    AllowedMethods = [action_to_method(mongoose_commands:action(Command))
                      || Command <- CommandList],
    {[<<"OPTIONS">> | AllowedMethods], Req, State}.

content_types_provided(Req, State) ->
    CTP = [{{<<"application">>, <<"json">>, '*'}, to_json}],
    {CTP, Req, State}.

content_types_accepted(Req, State) ->
    CTA = [{{<<"application">>, <<"json">>, '*'}, from_json}],
    {CTA, Req, State}.

terminate(_Reason, _Req, _State) ->
    ok.

%% @doc Called for a method of type "DELETE"
delete_resource(Req, #http_api_state{command_category = Category,
                                     command_subcategory = SubCategory,
                                     bindings = B} = State) ->
    Arity = length(B),
    Cmds = mongoose_commands:list(admin, Category, method_to_action(<<"DELETE">>), SubCategory),
    [Command] = [C || C <- Cmds, mongoose_commands:arity(C) == Arity],
    process_request(whatever, <<"DELETE">>, [], Command, Req, State).


%%--------------------------------------------------------------------
%% Authorization
%%--------------------------------------------------------------------

% @doc Cowboy callback
is_authorized(Req, State) ->
    ControlCreds = get_control_creds(State),
    AuthDetails = mongoose_api_common:get_auth_details(Req),
    case authorize(ControlCreds, AuthDetails) of
        true ->
            {true, Req, State};
        false ->
            mongoose_api_common:make_unauthorized_response(Req, State)
    end.

-spec authorize(credentials(), {AuthMethod :: atom(),
                                Username :: binary(),
                                Password :: binary()}) -> boolean().
authorize(any, _) -> true;
authorize(_, undefined) -> false;
authorize(ControlCreds, {AuthMethod, User, Password}) ->
    compare_creds(ControlCreds, {User, Password}) andalso
        mongoose_api_common:is_known_auth_method(AuthMethod).

% @doc Checks if credentials are the same (if control creds are 'any'
% it is equal to everything).
-spec compare_creds(credentials(), credentials() | undefined) -> boolean().
compare_creds({User, Pass}, {User, Pass}) -> true;
compare_creds(_, _) -> false.

get_control_creds(#http_api_state{auth = Creds}) ->
    Creds.

%%--------------------------------------------------------------------
%% Cowboy
%%--------------------------------------------------------------------

%% @doc Called for a method of type "GET"
to_json(Req, #http_api_state{entity = Caller0,
                             bindings = B} = State) ->
    {Caller, QVals} = check_caller(Caller0, Req),
    AllArgs = B ++ QVals,
    mongoose_api_common:to_json(Req, Caller, AllArgs, State).

%% @doc Called for a method of type "POST" and "PUT"
from_json(Req, #http_api_state{entity = Caller0,
                               bindings = B} = State) ->
    case parse_request_body(Req) of
        {error, _R}->
            error_response(bad_request, ?BODY_MALFORMED, Req, State);
        {Params, _} ->
            {Caller, QVals} = check_caller(Caller0, Req),
            AllArgs = B ++ Params ++ QVals,
            mongoose_api_common:from_json(Req, Caller, AllArgs, State)
    end.

%%--------------------------------------------------------------------
%% Internal
%%--------------------------------------------------------------------

check_caller(admin, Req) ->
    QVals0 = cowboy_req:parse_qs(Req),
    QVals1 = [{binary_to_existing_atom(K, utf8), V} || {K, V} <- QVals0],
    case proplists:get_value(caller, QVals1) of
        undefined ->
            {admin, QVals1};
        BinJid ->
            {jid:from_binary(BinJid), proplists:delete(caller, QVals1)}
    end.

-spec handler_path(ejabberd_cowboy:path(), mongoose_commands:t(), [{atom(), term()}]) ->
    ejabberd_cowboy:route().
handler_path(Base, Command, ExtraOpts) ->
    {[Base, mongoose_api_common:create_url_path(Command)],
     ?MODULE, [{command_category, mongoose_commands:category(Command)},
               {command_subcategory, mongoose_commands:subcategory(Command)} | ExtraOpts]}.


