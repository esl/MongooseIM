-module(mongoose_iq_handler).
-include("mongoose.hrl").
-include("jlib.hrl").

%%----------------------------------------------------------------------
%% Types
%%----------------------------------------------------------------------

-record(iq_handler, {handler_fn :: handler_fn(),
                     extra :: map(),
                     execution_method :: execution_method()}).

-type t() :: #iq_handler{}.

-type handler_fn() :: fun((Acc :: mongoose_acc:t(),
                           From :: jid:jid(),
                           To :: jid:jid(),
                           IQ :: jlib:iq(),
                           Extra :: map()) -> {NewAcc :: mongoose_acc:t(),
                                               IQResp :: ignore | jlib:iq()}).

-type execution_method() :: no_queue | parallel | {one_queue, pid()} | {queues, [pid()]}.

-type execution_type() :: no_queue | parallel | one_queue  | {queues, pos_integer()}.

-export_type([t/0, handler_fn/0, execution_type/0]).

%%----------------------------------------------------------------------
%% API
%%----------------------------------------------------------------------

-export([new/3,
         delete/1,
         process_iq/5,
         add_extra/2,
         execute_handler/5]).
%% Getters
-export([module/1, extra/1]).

-ignore_xref([module/1]).

-spec new(IQHandlerFn :: handler_fn(),
          Extra :: map(),
          ExecutionType :: execution_type()) -> t().
new(IQHandlerFn, Extra, ExecutionType) ->
    ExecutionMethod = execution_method(ExecutionType),
    #iq_handler{handler_fn = IQHandlerFn, extra = Extra,
                execution_method = ExecutionMethod}.

-spec delete(IQHandler :: t()) -> ok.
delete(#iq_handler{execution_method = ExecutionMethod}) ->
    case ExecutionMethod of
        {one_queue, Pid} -> mongoose_iq_worker:stop(Pid);
        {queues, Pids} ->
            [mongoose_iq_worker:stop(Pid) || Pid <- Pids],
            ok;
        _ -> ok
    end.

-spec module(t()) -> module().
module(#iq_handler{handler_fn = Fn}) ->
    {module, Mod} = erlang:fun_info(Fn, module),
    Mod.

-spec process_iq(Handler :: t(),
                 Acc :: mongoose_acc:t(),
                 From :: jid:jid(),
                 To :: jid:jid(),
                 IQ :: jlib:iq()) -> mongoose_acc:t().
process_iq(#iq_handler{execution_method = ExecutionMethod} = Handler,
           Acc, From, To, IQ) ->
    case ExecutionMethod of
        no_queue ->
            ?MODULE:execute_handler(Handler, Acc, From, To, IQ);
        {one_queue, Pid} ->
            mongoose_iq_worker:process_iq(Pid, Handler, Acc, From, To, IQ),
            Acc;
        {queues, Pids} ->
            Pid = lists:nth(rand:uniform(length(Pids)), Pids),
            mongoose_iq_worker:process_iq(Pid, Handler, Acc, From, To, IQ),
            Acc;
        parallel ->
            spawn(?MODULE, execute_handler, [Handler, Acc, From, To, IQ]),
            Acc
    end.

-spec extra(t()) -> map().
extra(#iq_handler{ extra = Extra }) ->
    Extra.

-spec add_extra(t(), map()) -> t().
add_extra(#iq_handler{ extra = OldExtra } = Handler, Extra) ->
    %% KV pairs from the OldExtra map will remain unchanged, only
    %% the new keys from Extra map will be added to the NewExtra map
    NewExtra = maps:merge(Extra, OldExtra),
    Handler#iq_handler{extra = NewExtra}.

-spec execute_handler(Handler :: t(),
                      Acc :: mongoose_acc:t(),
                      From :: jid:jid(),
                      To :: jid:jid(),
                      IQ :: jlib:iq()) -> mongoose_acc:t().
execute_handler(#iq_handler{handler_fn = IQHandlerFn, extra = Extra},
                Acc, From, To, IQ = #iq{sub_el = SubEl, lang = Lang}) ->
    try IQHandlerFn(Acc, From, To, IQ, Extra) of
        {Acc1, ignore} ->
            Acc1;
        {Acc1, ResIQ} ->
            reply(From, To, Acc1, ResIQ)
    catch
        Class:Reason:StackTrace ->
            ?LOG_WARNING(#{what => process_iq_error, from => From, to => To, iq => IQ,
                           acc => Acc, extra => Extra, handler_function => IQHandlerFn,
                           class => Class, reason => Reason, stacktrace => StackTrace}),
            ErrorMsg = <<"The server could not process the IQ stanza">>,
            ErrorEl = mongoose_xmpp_errors:internal_server_error(Lang, ErrorMsg),
            ErrorIQ = IQ#iq{type = error, sub_el = [SubEl, ErrorEl]},
            reply(From, To, Acc, ErrorIQ)
    end.

-spec reply(jid:jid(), jid:jid(), mongoose_acc:t(), jlib:iq()) -> mongoose_acc:t().
reply(From, To, Acc, IQReply) ->
    ejabberd_router:route(To, From, Acc, jlib:iq_to_xml(IQReply)).

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------
-spec execution_method(execution_type()) -> execution_method().
execution_method(ExecutionType) ->
    case ExecutionType of
        no_queue -> no_queue;
        parallel -> parallel;
        one_queue ->
            {ok, Pid} = mongoose_iq_worker:start(),
            {one_queue, Pid};
        {queues, N} ->
            Pids = lists:map(fun(_) ->
                                 {ok, Pid} = mongoose_iq_worker:start(),
                                 Pid
                             end, lists:seq(1, N)),
            {queues, Pids}
    end.
