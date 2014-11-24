-module(mongoose_api).

%% ejabberd_cowboy callbacks
-export([cowboy_router_paths/2]).

%% cowboy_rest callbacks
-export([init/3,
         rest_init/2,
         rest_terminate/2]).

-export([content_types_provided/2]).

-export([to_xml/2,
         to_json/2]).

-record(state, {handler, opts}).

-type prefix() :: string().
-type route() :: {string(), options()}.
-type routes() :: [route()].
-type bindings() :: proplists:proplist().
-type options() :: [any()].
-type response() :: {ok, any()} | {error, atom()}.
-export_type([prefix/0, routes/0, route/0, bindings/0, options/0, response/0]).

-callback prefix() -> prefix().
-callback routes() -> routes().
-callback handle_get(bindings(), options()) -> response().

%%--------------------------------------------------------------------
%% ejabberd_cowboy callbacks
%%--------------------------------------------------------------------
cowboy_router_paths(Base, Opts) ->
    Handlers = gen_mod:get_opt(handlers, Opts, []),
    lists:flatmap(pa:bind(fun register_handler/2, Base), Handlers).

register_handler(Base, Handler) ->
    [{[Base, Handler:prefix(), Path], ?MODULE, [{handler, Handler}|Opts]}
     || {Path, Opts} <- Handler:routes()].

%%--------------------------------------------------------------------
%% cowboy_rest callbacks
%%--------------------------------------------------------------------
init({_Transport, http}, Req, Opts) ->
    {upgrade, protocol, cowboy_rest, Req, Opts}.

rest_init(Req, Opts) ->
    case lists:keytake(handler, 1, Opts) of
        {value, {handler, Handler}, Opts1} ->
            State = #state{handler = Handler, opts = Opts1},
            {ok, Req, State};
        false ->
            erlang:throw(no_handler_defined)
    end.

rest_terminate(_Req, _State) ->
    ok.

content_types_provided(Req, State) ->
    CTP = [{{<<"application">>, <<"json">>, '*'}, to_json},
           {{<<"application">>, <<"xml">>, '*'}, to_xml}],
    {CTP, Req, State}.

to_json(Req, State) ->
    handle_get(mongoose_api_json, Req, State).

to_xml(Req, State) ->
    handle_get(mongoose_api_xml, Req, State).

%%--------------------------------------------------------------------
%% HTTP verbs handlers
%%--------------------------------------------------------------------
handle_get(Serializer, Req, #state{opts=Opts}=State) ->
    {Bindings, Req1} = cowboy_req:bindings(Req),
    Result = call(handle_get, [Bindings, Opts], State),
    handle_result(Result, Serializer, Req1, State).

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------
handle_result({ok, Result}, Serializer, Req, State) ->
    serialize(Result, Serializer, Req, State);
handle_result({error, Error}, _Serializer, Req, State) ->
    error_response(Error, Req, State);
handle_result(no_call, _Serializer, Req, State) ->
    error_response(not_implemented, Req, State).

serialize(Data, Serializer, Req, State) ->
    {Serializer:serialize(Data), Req, State}.

call(Function, Args, #state{handler=Handler}) ->
    try
        apply(Handler, Function, Args)
    catch error:undef ->
        no_call
    end.

%%--------------------------------------------------------------------
%% Error responses
%%--------------------------------------------------------------------
error_response(Code, Req, State) when is_integer(Code) ->
    {ok, Req1} = cowboy_req:reply(Code, Req),
    {halt, Req1, State};
error_response(Reason, Req, State) ->
    error_response(error_code(Reason), Req, State).

error_code(not_found)       -> 404;
error_code(not_implemented) -> 501.
