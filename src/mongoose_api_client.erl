%%%-------------------------------------------------------------------
%%% @author ludwikbukowski
%%% @copyright (C) 2016, Erlang Solutions Ltd.
%%%
%%% @end
%%% Created : 19. Jul 2016 17:55
%%%-------------------------------------------------------------------
%% @doc MongooseIM REST HTTP API for clients.
%% This module implements cowboy REST callbacks and
%% passes the requests on to the http api backend module.
%% It provides also client authorization mechanism

-module(mongoose_api_client).
-author("ludwikbukowski").
-include("mongoose_api.hrl").
-include("jlib.hrl").
-include("mongoose.hrl").

%% ejabberd_cowboy exports
-export([cowboy_router_paths/2, to_json/2, from_json/2]).

%% API
-export([is_authorized/2,
         init/2,
         allowed_methods/2,
         content_types_provided/2,
         content_types_accepted/2,
         rest_terminate/2,
         delete_resource/2]).

-import(mongoose_api_common, [action_to_method/1,
                              method_to_action/1,
                              error_code/1,
                              process_request/4,
                              error_response/4,
                              parse_request_body/1]).

%%--------------------------------------------------------------------
%% ejabberd_cowboy callbacks
%%--------------------------------------------------------------------

%% @doc This is implementation of ejabberd_cowboy callback.
%% Returns list of all available http paths.
-spec cowboy_router_paths(ejabberd_cowboy:path(), ejabberd_cowboy:options()) ->
    ejabberd_cowboy:implemented_result().
cowboy_router_paths(Base, _Opts) ->
    ejabberd_hooks:add(register_command, global, mongoose_api_common, reload_dispatches, 50),
    ejabberd_hooks:add(unregister_command, global, mongoose_api_common, reload_dispatches, 50),
    try
        Commands = mongoose_commands:list(user),
        [handler_path(Base, Command) || Command <- Commands]
    catch
        _:Err ->
            ?ERROR_MSG("Error occured when getting the commands list: ~p~n", [Err]),
            []
    end.


%%--------------------------------------------------------------------
%% cowboy_rest callbacks
%%--------------------------------------------------------------------


init(Req, Opts) ->
    Bindings = maps:to_list(cowboy_req:bindings(Req)),
    CommandCategory =
        case lists:keytake(command_category, 1, Opts) of
            {value, {command_category, Name},  _Opts1} ->
                Name;
            false ->
                undefined
        end,
    State = #http_api_state{allowed_methods = mongoose_api_common:get_allowed_methods(user),
        bindings = Bindings, command_category = CommandCategory},
    {cowboy_rest, Req, State}.

allowed_methods(Req, #http_api_state{command_category = Name} = State) ->
    CommandList = mongoose_commands:list(user, Name),
    AllowedMethods = [action_to_method(mongoose_commands:action(Command))
                      || Command <- CommandList],
    {AllowedMethods, Req, State}.

content_types_provided(Req, State) ->
    CTP = [{{<<"application">>, <<"json">>, '*'}, to_json}],
    {CTP, Req, State}.

content_types_accepted(Req, State) ->
    CTA = [{{<<"application">>, <<"json">>, '*'}, from_json}],
    {CTA, Req, State}.

rest_terminate(_Req, _State) ->
    ok.

is_authorized(Req, State) ->
    AuthDetails = cowboy_req:parse_header(<<"authorization">>, Req),
    do_authorize(AuthDetails, Req, State).

%% @doc Called for a method of type "DELETE"
delete_resource(Req, #http_api_state{command_category = Category,
                                     command_subcategory = SubCategory,
                                     bindings = B} = State) ->
    Arity = length(B),
    Cmds = mongoose_commands:list(user, Category, method_to_action(<<"DELETE">>), SubCategory),
    [Command] = [C || C <- Cmds, arity(C) == Arity],
    mongoose_api_common:process_request(<<"DELETE">>, Command, Req, State).

%%--------------------------------------------------------------------
%% internal funs
%%--------------------------------------------------------------------

%% @doc Called for a method of type "GET"
to_json(Req, #http_api_state{command_category = Category,
                             command_subcategory = SubCategory,
                             bindings = B} = State) ->
    Arity = length(B),
    Cmds = mongoose_commands:list(user, Category, method_to_action(<<"GET">>), SubCategory),
    [Command] = [C || C <- Cmds, arity(C) == Arity],
    mongoose_api_common:process_request(<<"GET">>, Command, Req, State).


%% @doc Called for a method of type "POST" and "PUT"
from_json(Req, #http_api_state{command_category = Category,
                               command_subcategory = SubCategory,
                               bindings = B} = State) ->
    Method = cowboy_req:method(Req),
    case parse_request_body(Req) of
        {error, _R}->
            error_response(bad_request, ?BODY_MALFORMED, Req, State);
        {Params, _} ->
        Arity = length(B) + length(Params),
        Cmds = mongoose_commands:list(user, Category, method_to_action(Method), SubCategory),
        case [C || C <- Cmds, arity(C) == Arity] of
            [Command] ->
                process_request(Method, Command, {Params, Req}, State);
            [] ->
                error_response(not_found, ?ARGS_LEN_ERROR, Req, State)
        end
    end.

arity(C) ->
    % we don't have caller in bindings (we know it from authorisation),
    % so it doesn't count when checking arity
    Args = mongoose_commands:args(C),
    length([N || {N, _} <- Args, N =/= caller]).

do_authorize({basic, User, Password}, Req, State) ->
    case jid:from_binary(User) of
        error ->
            make_unauthorized_response(Req, State);
        JID ->
            do_check_password(JID, Password, Req, State)
    end;
do_authorize(_, Req, State) ->
    make_unauthorized_response(Req, State).

do_check_password(#jid{luser = User, lserver = Server} = JID,
    Password, Req, State) ->
    case ejabberd_auth:check_password(User, Server, Password) of
        true ->
            {true, Req, State#http_api_state{entity = jid:to_binary(JID)}};
        _ ->
            make_unauthorized_response(Req, State)
    end.

make_unauthorized_response(Req, State) ->
    {{false, <<"Basic realm=\"mongooseim\"">>}, Req, State}.

-spec handler_path(ejabberd_cowboy:path(), mongoose_commands:t()) -> ejabberd_cowboy:route().
handler_path(Base, Command) ->
    {[Base, mongoose_api_common:create_user_url_path(Command)],
        ?MODULE, [{command_category, mongoose_commands:category(Command)}]}.

