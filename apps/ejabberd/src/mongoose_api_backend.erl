%%%-------------------------------------------------------------------
%%% @author ludwikbukowski
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. Jul 2016 12:59
%%%-------------------------------------------------------------------
-module(mongoose_api_backend).
-author("ludwikbukowski").
-record(backend_state, {allowed_methods, bindings}).

%% ejabberd_cowboy exports
-export([cowboy_router_paths/2]).

%% cowboy_rest exports
-export([allowed_methods/2, content_types_provided/2, rest_terminate/2, init/3, rest_init/2]).


-define(COMMANDS_ENGINE, mongoose_commands).
-include("mongoose_commands.hrl").

%%--------------------------------------------------------------------
%% ejabberd_cowboy callbacks
%%--------------------------------------------------------------------

cowboy_router_paths(Base, Opts) ->
    Handlers = mongoose_api:list_commands(admin),
    [pa:bind(fun register_handler/2, Base), Handlers].

%%--------------------------------------------------------------------
%% cowboy_rest callbacks
%%--------------------------------------------------------------------
init({_Transport, http}, Req, Opts) ->
    {upgrade, protocol, cowboy_rest, Req,
        #backend_state{allowed_methods = get_allowed_methods()}}.


rest_init(Req, Opts) ->
    {Bindings, Req1} = cowboy_req:bindings(Req),
    State = #backend_state{bindings=Bindings},
    {ok, Req1, State}.


allowed_methods(Req, State) ->
    {State#backend_state.allowed_methods, Req, State}.

content_types_provided(Req, State) ->
    CTP = [{{<<"application">>, <<"json">>, '*'}, to_json},
        {{<<"application">>, <<"xml">>, '*'}, to_xml}],
    {CTP, Req, State}.

rest_terminate(_Req, _State) ->
    ok.


%% Helpers

register_handler(Base, Handler) ->
    {Base, Handler#mongoose_command.resource}.

get_allowed_methods() ->
    Commands = ?COMMANDS_ENGINE:list_commands(admin),
    [translate_action(Command#mongoose_command.action) || Command <- Commands].

%% Resources

available_metrics(_Bindings) ->
    {Hosts, Metrics} = get_available_hosts_metrics(),
    Global = get_available_global_metrics(),
    Reply = [{hosts, Hosts}, {metrics, Metrics}, {global, Global}],
    {ok, Reply}.


translate_action(get) -> <<"GET">>;
translate_action(set) -> <<"POST">>;
translate_action(delete) -> <<"DELETE">>;
translate_action(send) -> <<"POST">>;
translate_action(_) -> undefined.


