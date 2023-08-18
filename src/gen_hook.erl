-module(gen_hook).

-behaviour(gen_server).

%% External exports
-export([start_link/0,
         add_handler/5,
         delete_handler/5,
         add_handlers/1,
         delete_handlers/1,
         run_fold/4]).
-export([reload_hooks/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         code_change/3,
         handle_info/2,
         terminate/2]).

%% exported for unit tests only
-export([error_running_hook/5]).

-ignore_xref([start_link/0, add_handlers/1, delete_handlers/1]).

-include("safely.hrl").
-include("mongoose.hrl").

-type hook_name() :: atom().
-type hook_tag() :: mongooseim:host_type_or_global().

%% while Accumulator is not limited to any type, it's recommended to use maps.
-type hook_acc() :: any().
-type hook_params() :: map().
-type hook_extra() :: map().
-type extra() :: #{hook_name := hook_name(),
                   hook_tag := hook_tag(),
                   host_type => mongooseim:host_type(),
                   _ => _}.

-type hook_fn_ret() :: hook_fn_ret(hook_acc()).
-type hook_fn_ret(Acc) :: {ok | stop, Acc}.
-type hook_fn() :: %% see run_fold/4 documentation
    fun((Accumulator :: hook_acc(),
         ExecutionParameters :: hook_params(),
         ExtraParameters :: extra()) -> hook_fn_ret()).

-type key() :: {HookName :: atom(),
                Tag :: any()}.

-type hook_tuple() :: hook_tuple(hook_fn()).
-type hook_tuple(HookFn) :: {HookName :: hook_name(),
                             Tag :: hook_tag(),
                             Function :: HookFn,
                             Extra :: hook_extra(),
                             Priority :: pos_integer()}.

-type hook_list() :: hook_list(hook_fn()).
-type hook_list(HookFn) :: [hook_tuple(HookFn)].

-export_type([hook_fn/0,
              hook_list/0,
              hook_list/1,
              hook_fn_ret/0,
              hook_fn_ret/1,
              hook_tuple/0,
              extra/0]).

-record(hook_handler, {prio :: pos_integer(),
                       hook_fn :: hook_fn(),
                       extra :: extra()}).

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
add_handler({HookName, Tag, _, _, _} = HookTuple) ->
    Handler = make_hook_handler(HookTuple),
    Key = hook_key(HookName, Tag),
    gen_server:call(?MODULE, {add_handler, Key, Handler}).

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
delete_handler({HookName, Tag, _, _, _} = HookTuple) ->
    Handler = make_hook_handler(HookTuple),
    Key = hook_key(HookName, Tag),
    gen_server:call(?MODULE, {delete_handler, Key, Handler}).

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
               Params :: hook_params()) -> hook_fn_ret().
run_fold(HookName, Tag, Acc, Params) ->
    Key = hook_key(HookName, Tag),
    case persistent_term:get(?MODULE, #{}) of
        #{Key := Ls} ->
            mongoose_metrics:increment_generic_hook_metric(Tag, HookName),
            run_hook(Ls, Acc, Params, Key);
        _ ->
            {ok, Acc}
    end.

reload_hooks() ->
    gen_server:call(?MODULE, reload_hooks).

%%%----------------------------------------------------------------------
%%% gen_server callback functions
%%%----------------------------------------------------------------------

init([]) ->
    erlang:process_flag(trap_exit, true), %% We need to make sure that terminate is called in tests
    {ok, #{}}.

handle_call({add_handler, Key, #hook_handler{} = HookHandler}, _From, State) ->
    NewState =
        case maps:get(Key, State, []) of
            [] ->
                NewLs = [HookHandler],
                create_hook_metric(Key),
                maps:put(Key, NewLs, State);
            Ls ->
                case lists:search(fun_is_handler_equal_to(HookHandler), Ls) of
                    {value, _} ->
                        ?LOG_WARNING(#{what => duplicated_handler,
                                       key => Key, handler => HookHandler}),
                        State;
                    false ->
                        %% NOTE: sort *only* on the priority,
                        %% order of other fields is not part of the contract
                        NewLs = lists:keymerge(#hook_handler.prio, Ls, [HookHandler]),
                        maps:put(Key, NewLs, State)
                end
        end,
    maybe_insert_immediately(NewState),
    {reply, ok, NewState};
handle_call({delete_handler, Key, #hook_handler{} = HookHandler}, _From, State) ->
    NewState =
        case maps:get(Key, State, []) of
            [] ->
                State;
            Ls ->
                %% NOTE: The straightforward handlers comparison would compare
                %% the function objects, which is not well-defined in OTP.
                %% So we do a manual comparison on the MFA of the funs,
                %% by using `erlang:fun_info/2`
                Pred = fun_is_handler_equal_to(HookHandler),
                {_, NewLs} = lists:partition(Pred, Ls),
                maps:put(Key, NewLs, State)
        end,
    maybe_insert_immediately(NewState),
    {reply, ok, NewState};
handle_call(reload_hooks, _From, State) ->
    persistent_term:put(?MODULE, State),
    {reply, ok, State};
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
    persistent_term:erase(?MODULE),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

%% @doc This call inserts the new hooks map immediately only if an existing map was already present.
%% This simplifies tests: at startup we wait until all hooks have been accumulated
%% before inserting them all at once, while during tests we don't need to remember
%% to reload on every change
maybe_insert_immediately(State) ->
    case persistent_term:get(?MODULE, hooks_not_set) of
        hooks_not_set ->
            ok;
        _ ->
            persistent_term:put(?MODULE, State)
    end.

-spec run_hook([#hook_handler{}], hook_acc(), hook_params(), key()) -> hook_fn_ret().
run_hook([], Acc, _Params, _Key) ->
    {ok, Acc};
run_hook([Handler | Ls], Acc, Params, Key) ->
    case apply_hook_function(Handler, Acc, Params) of
        {ok, NewAcc} ->
            run_hook(Ls, NewAcc, Params, Key);
        {stop, NewAcc} ->
            {stop, NewAcc};
        {exception, Info} ->
            ?MODULE:error_running_hook(Info, Handler, Acc, Params, Key),
            run_hook(Ls, Acc, Params, Key)
    end.

-spec apply_hook_function(#hook_handler{}, hook_acc(), hook_params()) ->
    hook_fn_ret() | safely:exception().
apply_hook_function(#hook_handler{hook_fn = HookFn, extra = Extra},
                    Acc, Params) ->
    ?SAFELY(HookFn(Acc, Params, Extra)).

error_running_hook(Info, Handler, Acc, Params, Key) ->
    ?LOG_ERROR(Info#{what => hook_failed,
                     text => <<"Error running hook">>,
                     key => Key,
                     handler => Handler,
                     acc => Acc,
                     params => Params}).

-spec make_hook_handler(hook_tuple()) -> #hook_handler{}.
make_hook_handler({HookName, Tag, Function, Extra, Priority} = HookTuple)
        when is_atom(HookName), is_binary(Tag) or (Tag =:= global),
             is_function(Function, 3), is_map(Extra),
             is_integer(Priority), Priority > 0 ->
    NewExtra = extend_extra(HookTuple),
    check_hook_function(Function),
    #hook_handler{prio = Priority,
                  hook_fn = Function,
                  extra = NewExtra}.

-spec fun_is_handler_equal_to(#hook_handler{}) -> fun((#hook_handler{}) -> boolean()).
fun_is_handler_equal_to(#hook_handler{prio = P0, hook_fn = HookFn0, extra = Extra0}) ->
    Mod0 = erlang:fun_info(HookFn0, module),
    Name0 = erlang:fun_info(HookFn0, name),
    fun(#hook_handler{prio = P1, hook_fn = HookFn1, extra = Extra1}) ->
            P0 =:= P1 andalso Extra0 =:= Extra1 andalso
            Mod0 =:= erlang:fun_info(HookFn1, module) andalso
            Name0 =:= erlang:fun_info(HookFn1, name)
    end.

-spec check_hook_function(hook_fn()) -> ok.
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
            end;
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
