%%==============================================================================
%% Copyright 2018 Erlang Solutions Ltd.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%==============================================================================
-module(mongoose_cassandra_worker).
-author('rafal.slota@erlang-solutions.com').

-include("mongoose_logger.hrl").
-include_lib("cqerl/include/cqerl.hrl").

%% ====================================================================
%% Exports
%% ====================================================================

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% gen_server API
-export([write/5, write_async/5, read/7]).

-behaviour(gen_server).

%% ====================================================================
%% Internal records and definitions
%% ====================================================================

-record(state, {
          pool_name       :: mongoose_cassandra:pool_name(),
          inflight = #{}  :: #{request_id() => request()},
          cql_tags = #{}  :: #{cql_tag() => request_id()},
          monitors = #{}  :: #{cql_mref() => request_id()}
         }).

-record(write_action, {
          query           :: cql_query(),
          %% Rows left to send
          pending_rows    :: mongoose_cassandra:rows(),
          %% Rows that were send to Cassandra and are awaiting response (write confirmation)
          current_rows    :: mongoose_cassandra:rows()
         }).

-record(read_action, {
          query           :: cql_query(),
          last_result     :: cql_result() | undefined %% Last result handle from cqerl
         }).

-record(request, {
          id              :: request_id(),
          action          :: action(),
          opts = #{}      :: options(),
          cql_mref        :: cql_mref()       | undefined,
          tag             :: cql_tag()        | undefined,
          from            :: caller(),
          retry_left      :: non_neg_integer(),
          timeout_tref    :: reference()      | undefined
         }).

-define(DEFAULT_OPTS, #{
          batch_size      => 20,
          retry           => 3,
          timeout         => timer:minutes(1),
          %% cqerl options
          consistency     => one,
          batch_mode      => unlogged
         }).


%% By how much (%) should we resize batch size when Cassandra returns 'batch to big' error
-define(BATCH_SHRINK_RATIO, 0.5).

%% ====================================================================
%% Types
%% ====================================================================

%% API types

-type options() :: #{
               timeout     => non_neg_integer(), %% milliseconds
               retry       => non_neg_integer(), %% how many retry attempts should be made
               batch_size  => non_neg_integer(), %% Due to Cassandra query size limits, this value may be
               %% adjusted on error (last query retry will use batch size
               consistency => consistency_level() | consistency_level_int(),
               batch_mode  => batch_mode() | batch_mode_int()
              }.

-export_type([options/0]).

%% Internal types

-type context_id()  :: term() | undefined. %% This value is used as a hash base in worker selection
-type query_str()   :: binary().
-type caller()  :: {pid(), term()} | undefined.

-type request_id()      :: reference().
-type request()         :: #request{}.
-type write_action()    :: #write_action{}.
-type read_action()     :: #read_action{}.
-type action()          :: read_action() | write_action().

-type cql_result()  :: void | #cql_result{}.
-type cql_query()   :: #cql_query{}.
-type cql_mref()    :: reference().
-type cql_tag()     :: term(). %% Normally it's a reference(), but let's not depend on that

-type worker_state()    :: #state{}.
-type process_type()    :: cqerl_client.
-type error_type()      :: {down, process_type()} | cancel | cqerl_error.
-type error_reason()    :: {Code :: non_neg_integer(), Details :: binary(), any()} | timeout | term().


%%====================================================================
%% API functions
%%====================================================================


%% --------------------------------------------------------
%% @doc Execute batch write query to Cassandra (insert, update or delete).
%% Note that Cassandra doesn't like big batches, therefore this function will try to
%% split given rows into batches of 20 rows and will fall back to smaller batches if
%% Cassandra rejects the query due to its size being to big.
%% @end
%% --------------------------------------------------------
-spec write(mongoose_cassandra:pool_name(), context_id(), query_str(),
            mongoose_cassandra:rows(), options()) ->
                   ok | {error, Reason :: term()}.
write(PoolName, ContextId, QueryStr, Rows, Opts) ->
    Opts1 = prepare_options(Opts),
    Call = {write, QueryStr, Rows, Opts1},
    mongoose_cassandra_pool:call_query(PoolName, ContextId, Call).

%% --------------------------------------------------------
%% @doc Same as @see write/5 but asynchronous (without response).
%% @end
%% --------------------------------------------------------
-spec write_async(mongoose_cassandra:pool_name(), context_id(), query_str(),
                  mongoose_cassandra:rows(), options()) ->
                         ok.
write_async(PoolName, ContextId, QueryStr, Rows, Opts) ->
    Opts1 = prepare_options(Opts),
    Cast = {write, QueryStr, Rows, Opts1},
    mongoose_cassandra_pool:cast_query(PoolName, ContextId, Cast).

%% --------------------------------------------------------
%% @doc Execute read query to Cassandra (select).
%% This functions behaves much like the lists:foldl/3 but the input are pages from result of given
%% query. Therefore each execution of given fun gets list of several result rows (by default 100 at
%% most).
%% @end
%% --------------------------------------------------------
-spec read(mongoose_cassandra:pool_name(), context_id(), query_str(),
           mongoose_cassandra:parameters(), mongoose_cassandra:fold_fun(),
           mongoose_cassandra:fold_accumulator(), options()) ->
                  {ok, mongoose_cassandra:fold_accumulator()} | {error, Reason :: term()}.
read(PoolName, ContextId, QueryStr, Params, Fun, AccIn, Opts) ->
    Opts1 = prepare_options(Opts),
    Call = {read, QueryStr, Params, Opts1},
    do_read(PoolName, ContextId, Call, Fun, AccIn).

do_read(PoolName, ContextId, Call, Fun, AccIn) ->
    case mongoose_cassandra_pool:call_query(PoolName, ContextId, Call) of
        {finished, Result} ->
            NextAcc = Fun(cqerl:all_rows(Result), AccIn),
            {ok, NextAcc};
        {partial, Req, Result} ->
            NextAcc = Fun(cqerl:all_rows(Result), AccIn),
            do_read(PoolName, ContextId, {continue, Req}, Fun, NextAcc);
        Other ->
            Other
    end.

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([PoolName]) ->
    {ok, #state{pool_name = PoolName}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({write, QueryStr, Rows, Opts}, From, State = #state{}) ->
    Action = new_write_action(QueryStr, Rows),
    Request = new_request(From, Opts, Action),
    RequestId = Request#request.id,

    NewRequest = schedule_timeout(maps:get(timeout, Opts), Request),
    NewState = update_req(NewRequest, State),
    {noreply, process_request(RequestId, NewState)};
handle_call({read, QueryStr, Params, Opts}, From, State = #state{}) ->
    Action = new_read_action(QueryStr, Params),
    Request = new_request(From, Opts, Action),
    RequestId = Request#request.id,

    NewRequest = schedule_timeout(maps:get(timeout, Opts), Request),
    NewState = update_req(NewRequest, State),
    {noreply, process_request(RequestId, NewState)};
handle_call({continue, Req}, From, State = #state{}) ->
    RequestId = Req#request.id,
    NewReq = Req#request{from = From},
    NewState = update_req(NewReq, State),
    {noreply, process_request(RequestId, NewState)};
handle_call(Msg, _From, State) ->
    ?WARNING_MSG("Unexpected call ~p", [Msg]),
    {noreply, State}.


%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------

handle_cast({write, QueryStr, Rows, Opts}, #state{} = State) ->
    Action = new_write_action(QueryStr, Rows),
    Request = new_request(undefined, Opts, Action),
    RequestId = Request#request.id,

    NewState = State#state{inflight = maps:put(RequestId, Request, State#state.inflight)},
    {noreply, process_request(RequestId, NewState)};
handle_cast(Msg, State) ->
    ?WARNING_MSG("Unexpected cast ~p", [Msg]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------

handle_info({cancel, ReqId, Reason}, #state{} = St) ->
    {noreply, handle_cancel(ReqId, Reason, St)};

handle_info({result, _, _} = R, #state{} = St) ->
    {noreply, handle_result(R, St)};

handle_info({error, _, _} = Er, #state{} = St) ->
    {noreply, handle_error(Er, St)};

handle_info({'DOWN', _MRef,  _,  _,  _} = Down, #state{} = St) ->
    {noreply, handle_down(Down, St)};

handle_info({retry, ReqId}, #state{} = St) ->
    case maps:get(ReqId, St#state.inflight, undefined) of
        undefined ->
            ?WARNING_MSG("Unexpected retry request for ~p", [ReqId]),
            {noreply, St};
        #request{retry_left = TryCount} = Req ->
            NextRequest = Req#request{retry_left = max(TryCount - 1, 0)},
            NextState = update_req(NextRequest, St),
            {noreply, process_request(ReqId, NextState)}
    end;

handle_info(Msg, State) ->
    ?WARNING_MSG("Unexpected info ~p", [Msg]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ====================================================================
%% Implementation
%% ====================================================================

%% Get DBMS client and sends next Cassandra query for given request.
-spec process_request(request_id(), worker_state()) -> worker_state().
process_request(ReqId, #state{pool_name = PoolName} = State) ->
    Req = maps:get(ReqId, State#state.inflight),
    #request{action = Action, opts = Opts} = Req,

    try
        Client = {ClientPid, _} = get_client(PoolName),
        MRef = erlang:monitor(process, ClientPid),
        {Tag, NextAction} = send_next_request(Client, Action, Opts),

        NextReq = Req#request{
                    action = NextAction,
                    tag = Tag,
                    cql_mref = MRef
                   },
        State#state{
          inflight = maps:put(ReqId, NextReq, State#state.inflight),
          cql_tags = maps:put(Tag, ReqId, State#state.cql_tags),
          monitors = maps:put(MRef, ReqId, State#state.monitors)
         }
    catch
        _:Reason ->
            %% Logging will be handled in do_handle_error/4
            do_handle_error({down, cqerl_client}, Reason, Req, State)
    end.

%% Sends cqerl query for given action().
-spec send_next_request(cqerl:client(), action(), options()) -> {cql_tag(), action()}.
send_next_request(Client, #write_action{} = Action, Opts) ->
    #write_action{
       current_rows = FailedRows,
       pending_rows = PendingRows,
       query = Query
      } = Action,
    Rows = FailedRows ++ PendingRows,
    BatchSize = maps:get(batch_size, Opts),

    {NewRows, Tail} = lists:split(min(BatchSize, length(Rows)), Rows),
    QueryBatch = #cql_query_batch{
                    mode = maps:get(batch_mode, Opts),
                    consistency = maps:get(consistency, Opts),
                    queries = [Query#cql_query{values = NewRow} || NewRow <- NewRows]
                   },
    Tag = cqerl:send_query(Client, QueryBatch),

    NextAction = Action#write_action{current_rows = NewRows, pending_rows = Tail},
    {Tag, NextAction};
send_next_request(Client, #read_action{} = Action, _Opts) ->
    #read_action{query = Query} = Action,

    Tag =
        case Action#read_action.last_result of
            undefined ->
                cqerl:send_query(Client, Query);
            Result ->
                cqerl:fetch_more_async(Result)
        end,

    {Tag, Action}.

%% Handles query cancel requests (normally scheduled timeout)
-spec handle_cancel(request_id(), Reason :: term(), worker_state()) -> worker_state().
handle_cancel(ReqId, Reason, State) ->
    case maps:get(ReqId, State#state.inflight, undefined) of
        undefined ->
            State;
        #request{tag = Tag} = Req ->
            ?WARNING_MSG("query_type=~s, tag=~p, ~s, action=aborting, abort_reason=~s",
                         [query_type(Req), Tag, error_text(cancel, Reason, Req), Reason]),
            cleanup_request(ReqId, State)
    end.

-spec handle_result({result, cql_tag(), cql_result()}, worker_state()) -> worker_state().
handle_result({result, Tag, Result} = R, #state{} = State) ->
    case maps:get(Tag, State#state.cql_tags, undefined) of
        undefined ->
            ?WARNING_MSG("unexpected result ~p", [R]),
            State;
        ReqId ->
            Req = #request{cql_mref = MRef} = maps:get(ReqId, State#state.inflight),
            erlang:demonitor(MRef, [flush]),
            do_handle_result(Result, Req, State)
    end.

%% Handles all 'result' responses from DBMS. Depending on the state of request,
%% initiates next query or returs response to the caller.
-spec do_handle_result(cql_result(), request(), worker_state()) -> worker_state().
do_handle_result(Result, Req, State) ->
    #request{action = Action, id = ReqId} = Req,

    IsFinished = is_finished(Req, Result),
    case {IsFinished, Action} of
        %% Continue with unfinished queries
        {false, #write_action{} = A} ->
            NextAction = A#write_action{current_rows = []}, %% Remove just saved rows
            NextReq = Req#request{action = NextAction},
            NextState = update_req(NextReq, State),

            process_request(ReqId, NextState);
        {false, #read_action{} = A} ->

            NextAction = A#read_action{last_result = Result},
            NextReq = Req#request{action = NextAction},
            NextState = update_req(NextReq, State),
            maybe_reply(Req, {partial, NextReq, Result}),
            NextState;

        %% Reply and cleanup for finished queries
        {true, #write_action{}} ->
            maybe_reply(Req, ok),
            cleanup_request(ReqId, State);
        {true, #read_action{}} ->
            maybe_reply(Req, {finished, Result}),
            cleanup_request(ReqId, State)

    end.

-spec handle_error({error, cql_tag(), Reason :: term()}, worker_state()) -> worker_state().
handle_error({error, Tag, Reason} = Error, #state{} = State) ->
    case maps:get(Tag, State#state.cql_tags, undefined) of
        undefined ->
            ?WARNING_MSG("unexpected error ~p", [Error]),
            State;
        ReqId ->
            Req = #request{cql_mref = MRef} = maps:get(ReqId, State#state.inflight),
            erlang:demonitor(MRef, [flush]),
            do_handle_error(cqerl_error, Reason, Req, State)
    end.

-spec handle_down({'DOWN', reference(),  process,  pid(),  Info :: term()},
                  worker_state()) -> worker_state().
handle_down({'DOWN', MRef,  _,  _Pid,  Info} = _Down, State) ->
    case maps:get(MRef, State#state.monitors, undefined) of
        undefined ->
            State;
        ReqId ->
            Req = #request{cql_mref = MRef} = maps:get(ReqId, State#state.inflight),
            do_handle_error({down, cqerl_client}, Info, Req, State)
    end.

-spec do_handle_error(error_type(), error_reason(), request(), worker_state()) -> worker_state().
do_handle_error(Type, Reason, Req, State) ->
    #request{tag = Tag, id = ReqId, opts = Opts, retry_left = RetryLeft} = Req,

    case retry_info(Type, Reason, Req, State) of
        {abort, AbortReason, NextState} ->
            ?WARNING_MSG("query_type=~s, tag=~p, ~s, action=aborting, abort_reason=~s",
                         [query_type(Req), Tag, error_text(Type, Reason, Req), AbortReason]),
            maybe_reply(Req, {error, Reason}),
            cleanup_request(Req, NextState);
        {retry, WaitFor, NextState} ->
            ?WARNING_MSG("query_type=~s, tag=~p, ~s, action=retrying, retry_left=~p "
                         "request_opts=~p",
                         [query_type(Req), Tag, error_text(Type, Reason, Req), RetryLeft, Opts]),
            schedule_retry(ReqId, WaitFor, NextState)
    end.

%% Sends given reply is caller is known.
-spec maybe_reply(request(), Result :: ok | {ok, term()} |
                  {error, term()} | {finished, term()} | {partial, request(), term()}) -> any().
maybe_reply(#request{from = undefined}, _Result) ->
    ok;
maybe_reply(#request{from = From}, Result) ->
    gen_server:reply(From, Result).

%% Removes all data and monitors associated with the request.
-spec cleanup_request(request() | request_id(), worker_state()) -> worker_state().
cleanup_request(#request{id = ReqId}, State) ->
    cleanup_request(ReqId, State);
cleanup_request(ReqId, State) ->
    #state{inflight = Inflight, monitors = Monitors, cql_tags = Tags} = State,
    #request{tag = Tag, cql_mref = CqlMRef, timeout_tref = TRef} = maps:get(ReqId, Inflight),
    catch demonitor(CqlMRef), %% If may be unset at this point and we don't really care about it
    catch erlang:cancel_timer(TRef),

    NextInflight = maps:remove(ReqId, Inflight),
    NextMonitors = maps:remove(CqlMRef, Monitors),
    NextTags = maps:remove(Tag, Tags),

    State#state{inflight = NextInflight, monitors = NextMonitors, cql_tags = NextTags}.

%% Checks whether request requires more queries to complete.
-spec is_finished(request(), cql_result()) -> boolean().
is_finished(_Req = #request{action = #write_action{pending_rows = []}}, _Result) ->
    true;
is_finished(_Req = #request{action = #write_action{}}, _Result) ->
    false;
is_finished(_Req = #request{action = #read_action{}}, Result) ->
    not cqerl:has_more_pages(Result).

%% For given query error, returns whether request should be retried and if so - how soon.
-spec retry_info(error_type(), error_reason(), request(), worker_state()) ->
                        {abort, AbortReason :: term(), worker_state()} |
                        {retry, Timeout :: non_neg_integer(), worker_state()}.
retry_info(_, _, #request{retry_left = 0} = _Req, State) ->
    {abort, retry_limit_exeeded, State};
retry_info({down, cqerl_client}, _Reason, _Req, State) ->
    {retry, 5 + rand:uniform(20), State};
retry_info(cqerl_error, {16#1100 = _WriteTimout, _, _}, _Req, State) ->
    {retry, 10 + rand:uniform(50), State};
retry_info(cqerl_error, {16#1200 = _ReadTimout, _, _}, _Req, State) ->
    {retry, 10 + rand:uniform(50), State};
retry_info(cqerl_error, {16#2200 = _WriteToBig, _, _}, Req, State) ->
    #request{id = ReqId, opts = Opts, retry_left = RetryLeft} = Req,
    BatchSize = maps:get(batch_size, Opts),
    NewBatchSize =
        case RetryLeft of
            1 -> %% This is the last try, let's use batch size of 1
                1;
            _ -> max(1, round(BatchSize * ?BATCH_SHRINK_RATIO))
        end,

    NextOpts = maps:put(batch_size, NewBatchSize, Opts),
    NextReq = Req#request{opts = NextOpts},
    NextState = State#state{inflight = maps:put(ReqId, NextReq, State#state.inflight)},

    {retry, 0, NextState};
retry_info(cqerl_error, {connection_closed, _}, _Req, State) ->
    {retry, 5 + rand:uniform(20), State};
retry_info(_Type, _Reason, _Req, State) ->
    {abort, unknown_error, State}.

%% Returns log message for given query error.
error_text(cancel = Category, Reason, _Request) ->
    io_lib:format("status=error, category=~s, details=~p",
                  [Category, Reason]);
error_text({down, ProcessType}, Reason, Request) ->
    MRef = Request#request.cql_mref,
    io_lib:format("status=error, category=~s, details=~p, process_type=~p, mref=~p",
                  [process_down, Reason, ProcessType, MRef]);
error_text(cqerl_error, Reason, _Request) ->
    cql_error_text(Reason).

%% Returns log message for given cqerl specific query error.
cql_error_text({ErrorCode, ErrorText, _}) when is_binary(ErrorText) ->
    CodeText = response_code_to_binary(ErrorCode),
    HexErrorCode = "0x" ++ integer_to_list(ErrorCode, 16),
    io_lib:format("status=error, category=~s, details=~s, code=~p",
                  [CodeText, ErrorText, HexErrorCode]);
cql_error_text({ErrorCode, _, ErrorData}) ->
    cql_error_text({ErrorCode, <<"undefined">>, ErrorData});
cql_error_text({Category, Reason}) ->
    io_lib:format("status=error, category=~s, details=~p",
                  [Category, Reason]);
cql_error_text(UnknownReason) ->
    io_lib:format("status=error, category=~s, details=~p",
                  [unknown, UnknownReason]).


%% ====================================================================
%% Helpers
%% ====================================================================

-spec get_client(mongoose_cassandra:pool_name()) ->
                        cqerl:client() | no_return().
get_client(PoolName) ->
    get_client_loop(PoolName, 0).

-spec get_client_loop(mongoose_cassandra:pool_name(), non_neg_integer()) ->
                             cqerl:client() | no_return().
get_client_loop(PoolName, RetryNo) when RetryNo >= 500 ->
    error(cannot_get_cqerl_client, [PoolName, RetryNo]);
get_client_loop(PoolName, RetryNo) ->
    try cqerl:get_client(PoolName) of
        {ok, {Pid, _Ref} = Client} ->
            case is_process_alive(Pid) of
                true -> Client;
                _ -> throw({dead, Pid})
            end
    catch
        E:R -> ?INFO_MSG("error getting client: ~p retry: ~p", [{E, R}, RetryNo]),
               Wait = rand:uniform(10),
               timer:sleep(Wait),
               get_client_loop(PoolName, RetryNo + 1)
    end.

-spec schedule_retry(request_id(), non_neg_integer(), worker_state()) -> worker_state().
schedule_retry(ReqId, Time, State) ->
    erlang:send_after(Time, self(), {retry, ReqId}),
    State.

-spec prepare_options(options()) -> options().
prepare_options(Opts) ->
    maps:merge(?DEFAULT_OPTS, Opts).

-spec new_request(From :: caller(), options(), action()) -> request().
new_request(From, Opts, Action) ->
    RequestId = erlang:make_ref(),

    #request{
       id = RequestId,
       action = Action,
       opts = Opts,
       from = From,
       retry_left = maps:get(retry, Opts)
      }.

-spec new_write_action(query_str(), mongoose_cassandra:rows()) -> write_action().
new_write_action(QueryStr, Rows) ->
    #write_action{
       query = #cql_query{statement = QueryStr},
       pending_rows = Rows,
       current_rows = []
      }.

-spec new_read_action(query_str(), mongoose_cassandra:parameters()) -> read_action().
new_read_action(QueryStr, Params) ->
    #read_action{
       query = #cql_query{statement = QueryStr, values = Params}
      }.

schedule_timeout(Timeout, Req = #request{id = ReqId}) ->
    TRef = erlang:send_after(Timeout, self(), {cancel, ReqId, timeout}),
    Req#request{timeout_tref = TRef}.

update_req(Req = #request{id = ReqId}, State = #state{inflight = Inflight}) ->
    State#state{inflight = maps:put(ReqId, Req, Inflight)}.

query_type(#request{action = #write_action{}}) ->
    write;
query_type(#request{action = #read_action{}}) ->
    read.

%% Source: https://stackoverflow.com/questions/48304330/list-of-cassandra-error-codes
response_code_to_binary(Code) when is_integer(Code) ->
    case Code of
        16#0000 -> <<"serverError">>;
        16#000A -> <<"protocolError">>;
        16#0100 -> <<"badCredentials">>;
        16#1000 -> <<"unavailableException">>;
        16#1001 -> <<"overloaded">>;
        16#1002 -> <<"isBootstrapping">>;
        16#1003 -> <<"truncateError">>;
        16#1100 -> <<"writeTimeout">>;
        16#1200 -> <<"readTimeout">>;
        16#1300 -> <<"readFailure">>;
        16#1400 -> <<"functionFailure">>;
        16#1500 -> <<"writeFailure">>;
        16#2000 -> <<"syntaxError">>;
        16#2100 -> <<"unauthorized">>;
        16#2200 -> <<"invalid">>;
        16#2300 -> <<"configError">>;
        16#2400 -> <<"alreadyExists">>;
        16#2500 -> <<"unprepared">>;
        _       -> <<"unknownCode=0x", (integer_to_binary(Code, 16))/bytes>>
    end;
response_code_to_binary(_) ->
    <<"invalidErrorCode">>.
