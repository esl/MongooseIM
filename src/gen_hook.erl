-module(gen_hook).

-behaviour(gen_server).

%% External exports
-export([start_link/0,
         add_handler/5,
         delete_handler/5,
         add_handlers/1,
         delete_handlers/1,
         run_fold/4]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         code_change/3,
         handle_info/2,
         terminate/2]).

%% exported for unit tests only
-export([error_running_hook/4]).

-include("mongoose.hrl").

-type hook_name() :: atom().
-type hook_tag() :: any().

%% while Accumulator is not limited to any type, it's recommended to use maps.
-type hook_acc() :: any().
-type hook_params() :: map().
-type hook_extra() :: map().

-type hook_fn_ret_value() :: {ok | stop, NewAccumulator :: hook_acc()}.
-type hook_fn() :: %% see run_fold/4 documentation
    fun((Accumulator :: hook_acc(),
         ExecutionParameters :: hook_params(),
         ExtraParameters :: hook_extra()) -> hook_fn_ret_value()).

-type key() :: {HookName :: atom(),
                Tag :: any()}.

-record(hook_handler, {key :: key(),
                       %% 'prio' field must go right after the 'key',
                       %% this is required for the proper sorting.
                       prio :: pos_integer(),
                       fn :: hook_fn(),
                       extra :: map()}).

-export_type([hook_fn/0]).

-define(TABLE, ?MODULE).
%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Add a handler for a hook.
%% Priority is used to sort the calls (lower numbers are executed first).
-spec add_handler(HookName :: hook_name(),
                  Tag :: hook_tag(),
                  Function :: hook_fn(),
                  Extra :: hook_extra(),
                  Priority :: pos_integer()) -> ok.
add_handler(HookName, Tag, Function, Extra, Priority) when is_atom(HookName),
                                                           is_function(Function, 3),
                                                           is_map(Extra),
                                                           is_integer(Priority),
                                                           Priority > 0 ->
    NewExtra = extend_extra(HookName, Tag, Extra),
    Handler = #hook_handler{key = hook_key(HookName, Tag),
                            prio = Priority,
                            fn = Function,
                            extra = NewExtra},
    gen_server:call(?MODULE, {add_handler, Handler}).

%% @doc Delete a hook handler.
%% It is important to indicate exactly the same information than when the call was added.
-spec delete_handler(HookName :: hook_name(),
                     Tag :: hook_tag(),
                     Function :: hook_fn(),
                     Extra :: hook_extra(),
                     Priority :: pos_integer()) -> ok.
delete_handler(HookName, Tag, Function, Extra, Priority) when is_atom(HookName),
                                                              is_function(Function, 3),
                                                              is_map(Extra),
                                                              is_integer(Priority),
                                                              Priority > 0 ->
    NewExtra = extend_extra(HookName, Tag, Extra),
    Handler = #hook_handler{key = hook_key(HookName, Tag),
                            prio = Priority,
                            fn = Function,
                            extra = NewExtra},
    gen_server:call(?MODULE, {delete_handler, Handler}).

-spec add_handlers([{HookName :: hook_name(),
                     Tag :: hook_tag(),
                     Function :: hook_fn(),
                     Extra :: hook_extra(),
                     Priority :: pos_integer()}]) -> ok.
add_handlers(List) ->
    [add_handler(HookName, Tag, Function, Extra, Priority) ||
        {HookName, Tag, Function, Extra, Priority} <- List, is_atom(HookName),
                                                            is_function(Function, 3),
                                                            is_map(Extra),
                                                            is_integer(Priority),
                                                            Priority > 0],
    ok.

-spec delete_handlers([{HookName :: hook_name(),
                        Tag :: hook_tag(),
                        Function :: hook_fn(),
                        Extra :: hook_extra(),
                        Priority :: pos_integer()}]) -> ok.
delete_handlers(List) ->
    [delete_handler(HookName, Tag, Function, Extra, Priority) ||
        {HookName, Tag, Function, Extra, Priority} <- List, is_atom(HookName),
                                                            is_function(Function, 3),
                                                            is_map(Extra),
                                                            is_integer(Priority),
                                                            Priority > 0],
    ok.

%% @doc Run hook handlers in order of priority (lower number means higher priority).
%%  * if hook handler returns {ok, NewAcc}, the NewAcc value is used
%%    as an accumulator parameter for the following hook handler.
%%  * if a hook handler returns {stop, NewAcc}, execution stops immediately
%%    without invoking lower priority hook handlers.
%%  * if hook handler crashes, the error is logged and the next hook handler
%%    is executed.
%% Note that every hook handler MUST return a valid Acc. If hook handler is not
%% interested in changing Acc parameter (or even if Acc is not used for a hook
%% at all), it must return (pass through) an unchanged input accumulator value.
-spec run_fold(HookName :: hook_name(),
               Tag :: hook_tag(),
               Acc :: hook_acc(),
               Params :: hook_params()) -> hook_fn_ret_value().
run_fold(HookName, Tag, Acc, Params) ->
    Key = hook_key(HookName, Tag),
    case ets:lookup(?TABLE, Key) of
        [{_, Ls}] ->
            mongoose_metrics:increment_generic_hook_metric(Tag, HookName),
            run_hook(Ls, Acc, Params);
        [] ->
            {ok, Acc}
    end.

%%%----------------------------------------------------------------------
%%% gen_server callback functions
%%%----------------------------------------------------------------------

init([]) ->
    ets:new(?TABLE, [named_table, {read_concurrency, true}]),
    {ok, no_state}.

handle_call({add_handler, #hook_handler{key = Key} = HookHandler}, _From, State) ->
    Reply = case ets:lookup(?TABLE, Key) of
                [{_, Ls}] ->
                    case lists:member(HookHandler, Ls) of
                        true ->
                            ok;
                        false ->
                            %% NB: lists:merge/2 returns sorted list!
                            NewLs = lists:merge(Ls, [HookHandler]),
                            ets:insert(?TABLE, {Key, NewLs}),
                            ok
                    end;
                [] ->
                    NewLs = [HookHandler],
                    ets:insert(?TABLE, {Key, NewLs}),
                    create_hook_metric(Key),
                    ok
            end,
    {reply, Reply, State};
handle_call({delete_handler, #hook_handler{key = Key} = HookHandler}, _From, State) ->
    Reply = case ets:lookup(?TABLE, Key) of
                [{_, Ls}] ->
                    NewLs = lists:delete(HookHandler, Ls),
                    ets:insert(?TABLE, {Key, NewLs}),
                    ok;
                [] ->
                    ok
            end,
    {reply, Reply, State};
handle_call(Request, From, State) ->
    ?UNEXPECTED_CALL(Request, From),
    {reply, bad_request, State}.

handle_cast(Msg, State) ->
    ?UNEXPECTED_CAST(Msg),
    {noreply, State}.

handle_info(Info, State) ->
    ?UNEXPECTED_INFO(Info),
    {noreply, State}.

terminate(_Reason, _State) ->
    ets:delete(?TABLE),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------
-spec run_hook([#hook_handler{}], hook_acc(), hook_params()) -> hook_fn_ret_value().
run_hook([], Acc, _Params) ->
    {ok, Acc};
run_hook([#hook_handler{fn = Function, extra = Extra} = Handler | Ls], Acc, Params) ->
    case apply_hook_function(Function, Acc, Params, Extra) of
        {'EXIT', Reason} ->
            ?MODULE:error_running_hook(Reason, Handler, Acc, Params),
            run_hook(Ls, Acc, Params);
        {stop, NewAcc} ->
            {stop, NewAcc};
        {ok, NewAcc} ->
            run_hook(Ls, NewAcc, Params)
    end.

-spec apply_hook_function(hook_fn(), hook_acc(), hook_params(), hook_extra()) ->
    hook_fn_ret_value() | {'EXIT', Reason :: any()}.
apply_hook_function(Function, Acc, Params, Extra) ->
    safely:apply(Function, [Acc, Params, Extra]).

error_running_hook(Reason, Handler, Acc, Params) ->
    ?LOG_ERROR(#{what => hook_failed,
                 text => <<"Error running hook">>,
                 handler => Handler,
                 acc => Acc,
                 params => Params,
                 reason => Reason}).

-spec hook_key(hook_name(), hook_tag()) -> key().
hook_key(HookName, Tag) -> {HookName, Tag}.

extend_extra(HookName, Tag, OriginalExtra) ->
    ExtendedExtra = #{hook_name => HookName, hook_tag => Tag},
    %% KV pairs of the OriginalExtra map will remain unchanged,
    %% only the new keys from the ExtendedExtra map will be added
    %% to the NewExtra map
    maps:merge(ExtendedExtra, OriginalExtra).

-spec create_hook_metric(Key :: key()) -> any().
create_hook_metric({HookName, Tag}) ->
    mongoose_metrics:create_generic_hook_metric(Tag, HookName).
