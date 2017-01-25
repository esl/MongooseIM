%%%==============================================================================
%%% File    : mim_ct_rest.erl
%%% Author  : Piotr Nosek <piotr.nosek@erlang-solutions.com>
%%% Description : Service mocking authentication REST API
%%% Created : 6 Aug 2014 by <piotr.nosek@erlang-solutions.com>
%%%==============================================================================

-module(mim_ct_rest).

-behaviour(gen_server).

%% gen_server exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% API
-export([start/2, stop/0]).
-export([listen/0, verify/1, cancel_listen/0, fail/0]).
-export([op/1, consume_fail/0, get_basic_auth/0]).
-export([check_password/3, get_password/2, user_exists/2, set_password/3,
         remove_user/2, remove_user_validate/3, register/3]).
-export([do/1]).

-record(state, {
          basic_auth = <<>> :: binary(),
          users = [] :: [{{User :: binary(), Server :: binary()}, Password :: binary()}],
          rest_listeners = [] :: [pid()],
          fail_once = false :: boolean() | middle
         }).

%%% -------------------------------------
%%% API
%%% -------------------------------------

start(BasicAuth, Config) ->
    application:ensure_all_started(ssl),
    application:ensure_all_started(lhttpc),
    {ok, _} = gen_server:start({local, mim_ct_rest_server}, ?MODULE, [BasicAuth], []),
    Config.

stop() ->
    Pid = whereis(mim_ct_rest_server),
    Ref = erlang:monitor(process, Pid),
    gen_server:call(Pid, stop),
    receive
        {'DOWN', Ref, process, Pid, _} ->
            ok
    after timer:seconds(5) ->
            error(timeout)
    end.

listen() ->
    gen_server:call(mim_ct_rest_server, listen).

verify([]) ->
    ok;
verify([Op | ROps] = L) ->
    receive
        Op -> verify(ROps);
        WrongOp -> throw({invalid_op, WrongOp, L})
    after
        5000 -> throw({empty_mailbox, L})
    end.

cancel_listen() ->
    gen_server:call(mim_ct_rest_server, cancel_listen).

fail() ->
    gen_server:call(mim_ct_rest_server, {fail, true}).

op(Op) ->
    gen_server:call(mim_ct_rest_server, {op, Op}).

consume_fail() ->
    gen_server:call(mim_ct_rest_server, consume_fail).

get_basic_auth() ->
    gen_server:call(mim_ct_rest_server, get_basic_auth).

check_password(U, S, P) ->
    gen_server:call(mim_ct_rest_server, {check_password, U, S, P}).

get_password(U, S) ->
    gen_server:call(mim_ct_rest_server, {get_password, U, S}).

user_exists(U, S) ->
    gen_server:call(mim_ct_rest_server, {user_exists, U, S}).

set_password(U, S, P) ->
    gen_server:call(mim_ct_rest_server, {set_password, U, S, P}).

remove_user(U, S) ->
    gen_server:call(mim_ct_rest_server, {remove_user, U, S}).

remove_user_validate(U, S, P) ->
    gen_server:call(mim_ct_rest_server, {remove_user_validate, U, S, P}).

register(U, S, P) ->
    gen_server:call(mim_ct_rest_server, {register, U, S, P}).

do(Fun) ->
    gen_server:call(mim_ct_rest_server, {do, Fun}).

%%% -------------------------------------
%%% gen_server callbacks
%%% -------------------------------------

init([BasicAuth]) ->
    application:ensure_all_started(cowboy),
    DispatchEJD = cowboy_router:compile([
            {'_', [{"/auth/:method/", mim_ct_rest_handler, []}]}
        ]),

    {ok, _} = cowboy:start_http(tests_listener, 5, [{port, 12000}],
                                [{env, [{dispatch, DispatchEJD}]}]),

    {ok, #state{ basic_auth = list_to_binary(BasicAuth) }}.

handle_call(listen, {From, _}, State) ->
    {reply, ok, State#state{ rest_listeners = [From | State#state.rest_listeners] }};
handle_call(cancel_listen, {From, _}, State) ->
    {reply, ok, State#state{ rest_listeners = lists:delete(From, State#state.rest_listeners) }};
handle_call({fail, Type}, _From, State) ->
    {reply, ok, State#state{ fail_once = Type }};
handle_call({op, Op}, _From, State) ->
    lists:foreach(fun(Listener) ->
                          Listener ! Op
                  end, State#state.rest_listeners),
    {reply, ok, State};
handle_call(consume_fail, _From, State) ->
    {reply, State#state.fail_once, State#state{ fail_once = false }};
handle_call(get_basic_auth, _From, State) ->
    {reply, State#state.basic_auth, State};
handle_call({check_password, U, S, P}, _From, State) ->
    case lists:keyfind({U, S}, 1, State#state.users) of
        {_, P} -> {reply, true, State};
        _ -> {reply, false, State}
    end;
handle_call({get_password, U, S}, _From, State) ->
    Pass = case lists:keyfind({U, S}, 1, State#state.users) of
               false -> false;
               {_, P} -> P
           end,
    {reply, Pass, State};
handle_call({user_exists, U, S}, _From, State) ->
    {reply, lists:keyfind({U, S}, 1, State#state.users) =/= false, State};
handle_call({set_password, U, S, P}, _From, #state{ users = Users } = State) ->
    {reply, ok, State#state{ users = lists:keystore({U, S}, 1, Users, {{U, S}, P}) }};
handle_call({remove_user, U, S}, _From, State) ->
    {reply, ok, State#state{ users = lists:keydelete({U, S}, 1, State#state.users) }};
handle_call({remove_user_validate, U, S, P}, _From, #state{ users = Users } = State) ->
    {Reply, StateN} = case lists:keyfind({U, S}, 1, Users) of
                          {_, P} ->
                              {ok, State#state{ users = lists:keydelete(
                                                          {U, S}, 1, Users) }};
                          false ->
                              {not_found, State};
                          _ ->
                              {not_allowed, State}
                      end,
    {reply, Reply, StateN};
handle_call({register, U, S, P}, _From, #state{ users = Users } = State) ->
    case lists:keyfind({U, S}, 1, Users) of
        {_, _} -> {reply, conflict, State};
        false -> {reply, ok, State#state{ users = [{{U, S}, P} | Users] }}
    end;
handle_call({do, Fun}, _From, State) ->
    {reply, Fun(), State};
handle_call(stop, _, State) ->
    {stop, shutdown, ok, State}.

handle_cast(_, State) ->
    {noreply, State}.

handle_info(_, State) ->
    {noreply, State}.

code_change(_, State, _) ->
    {ok, State}.

terminate(_Reason, _State) ->
    cowboy:stop_listener(tests_listener).

