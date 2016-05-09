%%%===================================================================
%%% @copyright (C) 2016, Erlang Solutions Ltd.
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%% http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%===================================================================
-module(mod_http_notification_requestor).
-author("bartek").

-behaviour(gen_server).

%% API
-export([start_link/1, request/2]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include("ejabberd.hrl").
-include("jlib.hrl").

-define(SERVER, ?MODULE).

-record(state, {connection, httphost, timeout, pathprefix}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Opts) ->
    gen_server:start_link(?MODULE, Opts, []).

request(Pid, Req) ->
    gen_server:call(Pid, Req).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init(Opts) ->
    {_, HttpHost} = proplists:lookup(http_host, Opts),
    {_, Timeout} = proplists:lookup(timeout, Opts),
    {_, PathPrefix} = proplists:lookup(path_prefix, Opts),
    {ok, Conn} = fusco:start_link(HttpHost, []),
    {ok, #state{httphost = HttpHost, timeout = Timeout, pathprefix = PathPrefix, connection = Conn}}.

handle_call(Request, _From, #state{connection = Connection, pathprefix = Path, timeout = Timeout} = State) ->
    {Host, Sender, Receiver, Message} = Request,
    Res = make_req(Connection, Host, Path, Sender, Receiver, Message, Timeout),
    {reply, Res, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

make_req(Connection, Host, Path, Sender, Receiver, Message, Timeout) ->
    Query = <<"author=", Sender/binary, "&server=", Host/binary, "&receiver=", Receiver/binary, "&message=",
              Message/binary>>,
    ?INFO_MSG("Making request '~s' for user ~s@~s...", [Path, Sender, Host]),
    Header = [{<<"Content-Type">>, <<"application/x-www-form-urlencoded">>}],
    case fusco:request(Connection, <<Path/binary>>, "POST", Header, Query, 2, Timeout) of
        {ok, {{Code, _Reason}, _RespHeaders, RespBody, _, _}} ->
            ?INFO_MSG("Request result: ~s: ~p", [Code, RespBody]),
            ok;
        Else ->
            Else
    end.

