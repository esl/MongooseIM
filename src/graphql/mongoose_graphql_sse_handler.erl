%% @doc An SSE handler for GraphQL subscriptions.
%% The graphql request is prepared, and then executed.
%%
%% 1. The first execution should return 'null' in 'data', and a new Stream in 'aux'.
%% 2. Then, whenever an Event is received, the prepared GraphQL request
%%    is executed again, this time with the Stream and the Event in the context.
%%    The resolver should then return either 'null' or the processed Event to send to the client.
%% 3. Upon termination, the request is executed one last time with the 'terminate' event.
%%    This is an opportunity to clean up all stream resources.
-module(mongoose_graphql_sse_handler).

-behaviour(lasse_handler).
-export([init/3,
         handle_notify/2,
         handle_info/2,
         handle_error/3,
         terminate/3]).

-include("mongoose.hrl").

-type req() :: cowboy_req:req().
-type state() :: #{atom() => term()}.

-spec init(state(), any(), cowboy_req:req()) ->
          {ok, req(), state()} |
          {shutdown, cowboy:http_status(), cowboy:http_headers(), iodata(), req(), state()}.
init(#{sse_idle_timeout := Timeout} = State, _LastEvtId, Req) ->
    process_flag(trap_exit, true), % needed for 'terminate' to be called
    cowboy_req:cast({set_options, #{
        idle_timeout => Timeout
    }}, Req),
    case cowboy_req:method(Req) of
        <<"GET">> ->
            case mongoose_graphql_handler:check_auth_header(Req, State) of
                {ok, State2} ->
                    case mongoose_graphql_handler:gather(Req) of
                        {ok, Req2, Decoded} ->
                            run_request(Decoded, Req2, State2);
                        {error, Reason} ->
                            reply_error(Reason, Req, State)
                    end;
                {error, Reason} ->
                    reply_error(Reason, Req, State)
            end;
        _ ->
            {ok, Req, State} % lasse returns 405: Method Not Allowed
    end.

run_request(#{document := undefined}, Req, State) ->
    reply_error(make_error(decode, no_query_supplied), Req, State);
run_request(GQLReq, Req, #{schema_endpoint := EpName, authorized := AuthStatus} = State) ->
    Ep = mongoose_graphql:get_endpoint(EpName),
    Ctx = maps:get(schema_ctx, State, #{}),
    GQLReq2 = GQLReq#{authorized => AuthStatus, ctx => Ctx#{method => sse}},
    case mongoose_graphql:prepare(Ep, GQLReq2) of
        {error, Reason} ->
            reply_error(Reason, Req, State);
        {ok, GQLReq3 = #{ctx := Ctx2}} ->
            case mongoose_graphql:execute(Ep, GQLReq3) of
                {ok, #{aux := [{stream, Stream}]}} ->
                    Ctx3 = Ctx2#{stream => Stream},
                    {ok, Req, State#{id => 1, ep => Ep, req => GQLReq3#{ctx := Ctx3}}};
                {ok, Response} ->
                    Body = mongoose_graphql_response:term_to_json(Response),
                    {shutdown, 200, #{}, Body, Req, State}
            end
    end.

-spec handle_notify(term(), state()) -> {nosend, state()}.
handle_notify(Msg, State) ->
    ?UNEXPECTED_INFO(Msg),
    {nosend, State}.

-spec handle_info(term(), state()) -> {nosend, state()} | {send, lasse_handler:event(), state()}.
handle_info(Event, State = #{ep := Ep, req := Req = #{ctx := Ctx}, id := Id}) ->
    Ctx1 = Ctx#{event => Event},
    {ok, #{data := Data} = Response} = mongoose_graphql:execute(Ep, Req#{ctx := Ctx1}),
    case has_non_null_value(Data) of
        false ->
            {nosend, State};
        true ->
            EventData = mongoose_graphql_response:term_to_json(Response),
            SseEvent = #{id => integer_to_binary(Id), data => EventData},
            {send, SseEvent, State#{id := Id + 1}}
    end.

%% Check if there is any value that is non-null. Any list is considered non-null.
has_non_null_value(M) when is_map(M) ->
    lists:any(fun has_non_null_value/1, maps:values(M));
has_non_null_value(V) -> V =/= null.

-spec handle_error(iodata(), term(), state()) -> ok.
handle_error(Msg, Reason, _State) ->
    ?LOG_ERROR(#{what => mongoose_graphql_sse_handler_failed,
                 reason => Reason, text => Msg}).

-spec terminate(term(), req(), state()) -> ok.
terminate(_Reason, _Req, #{ep := Ep, req := Req = #{ctx := Ctx}}) ->
    Ctx1 = Ctx#{event => terminate},
    {ok, #{aux := [{stream, closed}]}} = mongoose_graphql:execute(Ep, Req#{ctx := Ctx1}),
    ok;
terminate(_Reason, _Req, #{}) ->
    ok.

make_error(Phase, Term) ->
    #{error_term => Term, phase => Phase}.

reply_error(Reason, Req, State) ->
    {Code, Error} = mongoose_graphql_errors:format_error(Reason),
    Body = jiffy:encode(#{errors => [Error]}),
    {shutdown, Code, #{}, Body, Req, State}.
