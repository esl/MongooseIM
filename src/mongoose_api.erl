%%==============================================================================
%% Copyright 2014 Erlang Solutions Ltd.
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
-module(mongoose_api).

-behaviour(cowboy_rest).

%% ejabberd_cowboy callbacks
-export([cowboy_router_paths/2]).

%% cowboy_rest callbacks
-export([init/2,
         terminate/3,
         allowed_methods/2,
         content_types_provided/2,
         content_types_accepted/2,
         delete_resource/2]).

-export([to_xml/2,
         to_json/2,
         from_json/2]).

-record(state, {handler, opts, bindings}).

-type prefix()   :: string().
-type route()    :: {string(), options()}.
-type routes()   :: [route()].
-type bindings() :: proplists:proplist().
-type options()  :: [any()].
-type method()   :: get | post | put | patch | delete.
-type methods()  :: [method()].
-type response() :: ok | {ok, any()} | {error, atom()}.
-export_type([prefix/0, routes/0, route/0, bindings/0, options/0, response/0, methods/0]).

-callback prefix() -> prefix().
-callback routes() -> routes().
-callback handle_options(bindings(), options()) -> methods().
-callback handle_get(bindings(), options()) -> response().
-callback handle_post(term(), bindings(), options()) -> response().
-callback handle_put(term(), bindings(), options()) -> response().
-callback handle_delete(bindings(), options()) -> response().

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
init(Req, Opts) ->
    case lists:keytake(handler, 1, Opts) of
        {value, {handler, Handler}, Opts1} ->
            Bindings = maps:to_list(cowboy_req:bindings(Req)),
            State = #state{handler=Handler, opts=Opts1, bindings=Bindings},
            {cowboy_rest, Req, State}; % upgrade protocol
        false ->
            erlang:throw(no_handler_defined)
    end.

terminate(_Reason, _Req, _State) ->
    ok.

allowed_methods(Req, #state{bindings=Bindings, opts=Opts}=State) ->
    case call(handle_options, [Bindings, Opts], State) of
        no_call ->
            allowed_methods_from_exports(Req, State);
        Methods ->
            allowed_methods_from_module(Methods, Req, State)
    end.

content_types_provided(Req, State) ->
    CTP = [{{<<"application">>, <<"json">>, '*'}, to_json},
           {{<<"application">>, <<"xml">>, '*'}, to_xml}],
    {CTP, Req, State}.

content_types_accepted(Req, State) ->
    CTA = [{{<<"application">>, <<"json">>, '*'}, from_json}],
    {CTA, Req, State}.

delete_resource(Req, State) ->
    handle_delete(Req, State).

%%--------------------------------------------------------------------
%% content_types_provided/2 callbacks
%%--------------------------------------------------------------------
to_json(Req, State) ->
    handle_get(mongoose_api_json, Req, State).

to_xml(Req, State) ->
    handle_get(mongoose_api_xml, Req, State).

%%--------------------------------------------------------------------
%% content_types_accepted/2 callbacks
%%--------------------------------------------------------------------
from_json(Req, State) ->
    handle_unsafe(mongoose_api_json, Req, State).

%%--------------------------------------------------------------------
%% HTTP verbs handlers
%%--------------------------------------------------------------------
handle_get(Serializer, Req, #state{opts=Opts, bindings=Bindings}=State) ->
    Result = call(handle_get, [Bindings, Opts], State),
    handle_result(Result, Serializer, Req, State).

handle_unsafe(Deserializer, Req, State) ->
    Method = cowboy_req:method(Req),
    {ok, Body, Req1} = cowboy_req:read_body(Req),
    case Deserializer:deserialize(Body) of
        {ok, Data} ->
            handle_unsafe(Method, Data, Req1, State);
        {error, _Reason} ->
            error_response(bad_request, Req1, State)
    end.

handle_unsafe(Method, Data, Req, #state{opts=Opts, bindings=Bindings}=State) ->
    case method_callback(Method) of
        not_implemented ->
            error_response(not_implemented, Req, State);
        Callback ->
            Result = call(Callback, [Data, Bindings, Opts], State),
            handle_result(Result, Req, State)
    end.

handle_delete(Req, #state{opts=Opts, bindings=Bindings}=State) ->
    Result = call(handle_delete, [Bindings, Opts], State),
    handle_result(Result, Req, State).

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------
handle_result({ok, Result}, Serializer, Req, State) ->
    serialize(Result, Serializer, Req, State);
handle_result(Other, _Serializer, Req, State) ->
    handle_result(Other, Req, State).

handle_result(ok, Req, State) ->
    {true, Req, State};
handle_result({error, Error}, Req, State) ->
    error_response(Error, Req, State);
handle_result(no_call, Req, State) ->
    error_response(not_implemented, Req, State).

allowed_methods_from_module(Methods, Req, State) ->
    Methods1 = case lists:member(get, Methods) of
        true  -> [head | Methods];
        false -> Methods
    end,
    Methods2 = [options | Methods1],
    {methods_to_binary(Methods2), Req, State}.

allowed_methods_from_exports(Req, #state{handler=Handler}=State) ->
    Exports = Handler:module_info(exports),
    Methods = lists:foldl(fun collect_allowed_methods/2, [options], Exports),
    {methods_to_binary(Methods), Req, State}.

collect_allowed_methods({handle_get, 2}, Acc) ->
    [head, get | Acc];
collect_allowed_methods({handle_post, 3}, Acc) ->
    [post | Acc];
collect_allowed_methods({handle_put, 3}, Acc) ->
    [put | Acc];
collect_allowed_methods({handle_delete, 2}, Acc) ->
    [delete | Acc];
collect_allowed_methods(_Other, Acc) ->
    Acc.

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
    Req1 = cowboy_req:reply(Code, Req),
    {stop, Req1, State};
error_response(Reason, Req, State) ->
    error_response(error_code(Reason), Req, State).

error_code(bad_request)     -> 400;
error_code(not_found)       -> 404;
error_code(conflict)        -> 409;
error_code(unprocessable)   -> 422;
error_code(not_implemented) -> 501.

methods_to_binary(Methods) ->
    [method_to_binary(Method) || Method <- Methods].

method_to_binary(get)     -> <<"GET">>;
method_to_binary(post)    -> <<"POST">>;
method_to_binary(put)     -> <<"PUT">>;
method_to_binary(delete)  -> <<"DELETE">>;
method_to_binary(patch)   -> <<"PATCH">>;
method_to_binary(options) -> <<"OPTIONS">>;
method_to_binary(head)    -> <<"HEAD">>.

method_callback(<<"POST">>) -> handle_post;
method_callback(<<"PUT">>)  -> handle_put;
method_callback(_Other)     -> not_implemented.
