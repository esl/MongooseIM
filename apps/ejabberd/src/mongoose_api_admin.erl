%%%-------------------------------------------------------------------
%%% @author ludwikbukowski
%%% @copyright (C) 2016, Erlang Solutions Ltd.
%%% @doc
%%%
%%% @end
%%% Created : 05. Jul 2016 12:59
%%%-------------------------------------------------------------------
%% @doc MongooseIM REST HTTP API for administration.
%% This module implements cowboy REST callbacks and
%% passes the requests on to the http api backend module.

-module(mongoose_api_admin).
-author("ludwikbukowski").

%% ejabberd_cowboy exports
-export([cowboy_router_paths/2]).

%% cowboy_rest exports
-export([allowed_methods/2,
         content_types_provided/2,
         rest_terminate/2,
         init/3,
         rest_init/2,
         content_types_accepted/2,
         delete_resource/2]).

%% local callbacks
-export([to_json/2, from_json/2]).
-include("mongoose_api.hrl").
-include("ejabberd.hrl").

-import(mongoose_api_common, [error_response/3, error_response/4, action_to_method/1, method_to_action/1, error_code/1]).

%%--------------------------------------------------------------------
%% ejabberd_cowboy callbacks
%%--------------------------------------------------------------------

%% @doc This is implementation of ejabberd_cowboy callback. Returns list of all available http paths.
-spec cowboy_router_paths(ejabberd_cowboy:path(), ejabberd_cowboy:options()) ->
    ejabberd_cowboy:implemented_result() | ejabberd_cowboy:default_result().
cowboy_router_paths(Base, _Opts) ->
    ejabberd_hooks:add(register_command, global, mongoose_api_utils, reload_dispatches, 50),
    ejabberd_hooks:add(unregister_command, global, mongoose_api_utils, reload_dispatches, 50),
        try
            Commands = mongoose_commands:list(admin),
            [handler_path(Base, Command) || Command <- Commands]
        catch
            _:Err ->
                ?ERROR_MSG("Error occured when getting the commands list: ~p~n", [Err]),
                []
        end.

%%--------------------------------------------------------------------
%% cowboy_rest callbacks
%%--------------------------------------------------------------------

init({_Transport, _}, Req, Opts) ->
    {upgrade, protocol, cowboy_rest, Req, Opts}.

rest_init(Req, Opts) ->
    {Bindings, Req1} = cowboy_req:bindings(Req),
    CommandCategory =
        case lists:keytake(command_category, 1, Opts) of
            {value, {command_category, Name},  _Opts1} ->
                Name;
            false ->
                undefined
        end,
    State = #http_api_state{allowed_methods = mongoose_api_common:get_allowed_methods(admin),
        bindings = Bindings, command_category = CommandCategory},
    {ok, Req1, State}.

allowed_methods(Req, #http_api_state{command_category = Name} = State) ->
    CommandList = mongoose_commands:list(admin, Name),
    AllowedMethods = [action_to_method(mongoose_commands:action(Command)) || Command <- CommandList],
    {AllowedMethods, Req, State}.

content_types_provided(Req, State) ->
    CTP = [{{<<"application">>, <<"json">>, '*'}, to_json}],
    {CTP, Req, State}.

content_types_accepted(Req, State) ->
    CTA = [{{<<"application">>, <<"json">>, '*'}, from_json}],
    {CTA, Req, State}.

rest_terminate(_Req, _State) ->
    ok.

%% @doc Called for a method of type "DELETE"
delete_resource(Req, #http_api_state{command_category = Category} = State) ->
    [Command] = mongoose_commands:list(admin, Category, method_to_action(<<"DELETE">>)),
    mongoose_api_common:process_request(<<"DELETE">>, Command, Req, State).

%%--------------------------------------------------------------------
%% Internal funs
%%--------------------------------------------------------------------

%% @doc Called for a method of type "GET"
to_json(Req, #http_api_state{command_category = Category} = State) ->
    [Command] = mongoose_commands:list(admin, Category, method_to_action(<<"GET">>)),
    mongoose_api_common:process_request(<<"GET">>, Command, Req, State).


%% @doc Called for a method of type "POST" and "PUT"
from_json(Req, #http_api_state{command_category = Category} = State) ->
    {Method, Req2} = cowboy_req:method(Req),
    [Command] = mongoose_commands:list(admin, Category, method_to_action(Method)),
    mongoose_api_common:process_request(Method, Command, Req2, State).


-spec handler_path(ejabberd_cowboy:path(), mongoose_command()) -> ejabberd_cowboy:path().
handler_path(Base, Command) ->
    {[Base, mongoose_api_common:create_admin_url_path(Command)],
        ?MODULE, [{command_category, mongoose_commands:category(Command)}]}.


