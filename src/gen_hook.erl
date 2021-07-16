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
-type hook_tag() :: mongoose:host_type() | global.

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

-type hook_tuple() :: {HookName :: hook_name(),
                       Tag :: hook_tag(),
                       Function :: hook_fn(),
                       Extra :: hook_extra(),
                       Priority :: pos_integer()}.

-type hook_list() :: [hook_tuple()].

-export_type([hook_fn/0, hook_list/0]).

-record(hook_handler, {key :: key(),
                       %% 'prio' field must go right after the 'key',
                       %% this is required for the proper sorting.
                       prio :: pos_integer(),
                       module :: module(),
                       function :: atom(), %% function name
                       extra :: map()}).

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
add_handler(HookName, Tag, Function, Extra, Priority) ->
    add_handler({HookName, Tag, Function, Extra, Priority}).

-spec add_handlers(hook_list()) -> ok.
add_handlers(List) ->
    [add_handler(HookTuple) || HookTuple <- List],
    ok.

-spec add_handler(hook_tuple()) -> ok.
add_handler(HookTuple) ->
    Handler = make_hook_handler(HookTuple),
    gen_server:call(?MODULE, {add_handler, Handler}).

%% @doc Delete a hook handler.
%% It is important to indicate exactly the same information than when the call was added.
-spec delete_handler(HookName :: hook_name(),
                     Tag :: hook_tag(),
                     Function :: hook_fn(),
                     Extra :: hook_extra(),
                     Priority :: pos_integer()) -> ok.
delete_handler(HookName, Tag, Function, Extra, Priority)  ->
    delete_handler({HookName, Tag, Function, Extra, Priority}).

-spec delete_handlers(hook_list()) -> ok.
delete_handlers(List) ->
    [delete_handler(HookTuple) || HookTuple <- List],
    ok.

-spec delete_handler(hook_tuple()) -> ok.
delete_handler(HookTuple) ->
    Handler = make_hook_handler(HookTuple),
    gen_server:call(?MODULE, {delete_handler, Handler}).

%% @doc Run hook handlers in order of priority (lower number means higher priority).
%%  * if a hook handler returns {ok, NewAcc}, the NewAcc value is used
%%    as an accumulator parameter for the following hook handler.
%%  * if a hook handler returns {stop, NewAcc}, execution stops immediately
%%    without invoking lower priority hook handlers.
%%  * if a hook handler crashes, the error is logged and the next hook handler
%%    is executed.
%% Note that every hook handler MUST return a valid Acc. If a hook handler is not
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
run_hook([Handler | Ls], Acc, Params) ->
    case apply_hook_function(Handler, Acc, Params) of
        {'EXIT', Reason} ->
            ?MODULE:error_running_hook(Reason, Handler, Acc, Params),
            run_hook(Ls, Acc, Params);
        {stop, NewAcc} ->
            {stop, NewAcc};
        {ok, NewAcc} ->
            run_hook(Ls, NewAcc, Params)
    end.

-spec apply_hook_function(#hook_handler{}, hook_acc(), hook_params()) ->
    hook_fn_ret_value() | {'EXIT', Reason :: any()}.
apply_hook_function(#hook_handler{module = Module, function = Function, extra = Extra},
                    Acc, Params) ->
    safely:apply(Module, Function, [Acc, Params, Extra]).

error_running_hook(Reason, Handler, Acc, Params) ->
    ?LOG_ERROR(#{what => hook_failed,
                 text => <<"Error running hook">>,
                 handler => Handler,
                 acc => Acc,
                 params => Params,
                 reason => Reason}).

-spec make_hook_handler(hook_tuple()) -> #hook_handler{}.
make_hook_handler({HookName, Tag, Function, Extra, Priority} = HookTuple)
        when is_atom(HookName), is_binary(Tag) or (Tag =:= global),
             is_function(Function, 3), is_map(Extra),
             is_integer(Priority), Priority > 0 ->
    NewExtra = extend_extra(HookTuple),
    {Module, FunctionName} = check_hook_function(Function),
    #hook_handler{key = hook_key(HookName, Tag),
                  prio = Priority,
                  module = Module,
                  function = FunctionName,
                  extra = NewExtra}.

-spec check_hook_function(hook_fn()) -> {module(), atom()}.
check_hook_function(Function) when is_function(Function, 3) ->
    case erlang:fun_info(Function, type) of
        {type, external} ->
            {module, Module} = erlang:fun_info(Function, module),
            {name, FunctionName} = erlang:fun_info(Function, name),
            case code:ensure_loaded(Module) of
                {module, Module} -> ok;
                Error ->
                    throw_error(#{what => module_is_not_loaded,
                                  module => Module, error => Error})
            end,
            case erlang:function_exported(Module, FunctionName, 3) of
                true -> ok;
                false ->
                    throw_error(#{what => function_is_not_exported,
                                  function => Function})
            end,
            {Module, FunctionName};
        {type, local} ->
            throw_error(#{what => only_external_function_references_allowed,
                          function => Function})
    end.

-spec throw_error(map()) -> no_return().
throw_error(ErrorMap) ->
    error(ErrorMap).

-spec hook_key(HookName :: hook_name(), Tag :: hook_tag()) -> key().
hook_key(HookName, Tag) ->
    {HookName, Tag}.

-spec extend_extra(hook_tuple()) -> hook_extra().
extend_extra({HookName, Tag, _Function, OriginalExtra, _Priority}) ->
    ExtraExtension = case Tag of
                         global -> #{hook_name => HookName, hook_tag => Tag};
                         HostType when is_binary(HostType) ->
                             #{hook_name => HookName, hook_tag => Tag,
                               host_type => HostType}
                     end,
    %% KV pairs of the OriginalExtra map will remain unchanged,
    %% only the new keys from the ExtraExtension map will be added
    %% to the NewExtra map
    maps:merge(ExtraExtension, OriginalExtra).

-spec create_hook_metric(Key :: key()) -> any().
create_hook_metric({HookName, Tag}) ->
    mongoose_metrics:create_generic_hook_metric(Tag, HookName).
