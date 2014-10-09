%%%===================================================================
%%% @copyright (C) 2014, Erlang Solutions Ltd.
%%% @doc HTTP(S) reverse proxy for MongooseIM's Cowboy listener
%%% @end
%%%===================================================================
-module(mod_revproxy).
-behaviour(gen_mod).
-behaviour(cowboy_http_handler).

%% gen_mod callbacks
-export([start/2,
         stop/1]).

%% cowboy_http_handler callbacks
-export([init/3,
         handle/2,
         terminate/3]).

-record(state, {}).
-type state() :: #state{}.
-type option() :: {atom(), any()}.

%%--------------------------------------------------------------------
%% gen_mod callbacks
%%--------------------------------------------------------------------
-spec start(ejabberd:host(), [option()]) -> any().
start(Host, Opts) ->
    Rules = gen_mod:get_opt(rules, Opts, []),
    Module = gen_mod:get_module_proc(Host, ?MODULE),
    mod_revproxy_rules:compile(Module, Rules),
    ok.

stop(_Host) ->
    ok.

%%--------------------------------------------------------------------
%% cowboy_http_handler callbacks
%%--------------------------------------------------------------------
%% @todo specs

init(_Transport, Req, _Opts) ->
    {ok, Req, #state{}}.

handle(Req, State) ->
    {Host, Req2} = cowboy_req:header(<<"host">>, Req),
    error_logger:info_msg("Host: ~p~n", [Host]),
    {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------
