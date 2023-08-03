%% @doc Backend module for mod_jingle_sip
%% @author Michal Piotrowski <michal.piotrowski@erlang-solutions.com>
%%
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
-module(mod_jingle_sip_backend).

-include("mongoose.hrl").

-type call_id() :: binary().
-type incoming_request() :: {node(), binary()}.
-type outgoing_handle() :: binary().
-type dialog_hangle() :: term().

-export([init/2]).
-export([set_incoming_request/5]).
-export([set_incoming_handle/2]).
-export([set_outgoing_request/4]).
-export([set_outgoing_handle/4]).
-export([set_outgoing_accepted/1]).
-export([set_incoming_accepted/1]).
-export([get_incoming_request/2]).
-export([get_outgoing_handle/2]).
-export([get_session_info/2]).
-export([remove_session/1]).

-ignore_xref([remove_session/1]).

-define(MAIN_MODULE, mod_jingle_sip).

-callback init(mongooseim:host_type(), gen_mod:module_opts()) -> ok.

-callback set_incoming_request(CallID :: call_id(), ReqID :: binary(),
                               From :: jid:jid(), To :: jid:jid(), exml:element()) ->
    ok | {error, any()}.

-callback set_outgoing_request(CallID :: call_id(), ReqID :: binary(),
                               From :: jid:jid(), To :: jid:jid()) ->
    ok | {error, any()}.

-callback set_incoming_handle(CallID :: call_id(), DialogHandle :: dialog_hangle()) ->
    ok | {error, any()}.

-callback set_outgoing_handle(CallID :: call_id(), DialogHandle :: dialog_hangle(),
                              From :: jid:jid(), To :: jid:jid()) ->
    ok | {error, any()}.

-callback set_incoming_accepted(CallID :: call_id()) ->
    ok | {error, any()}.

-callback set_outgoing_accepted(CallID :: call_id()) ->
    ok | {error, any()}.

-callback get_incoming_request(call_id(), jid:jid()) ->
    {ok, undefined | incoming_request()} | {error, not_found}.

-callback get_outgoing_handle(call_id(), jid:jid()) ->
    {ok, undefined | outgoing_handle()} | {error, not_found}.

-callback get_session_info(call_id(), jid:jid()) ->
    {ok, map()} | {error, any()}.

-callback remove_session(call_id()) -> ok.

-spec init(mongooseim:host_type(), gen_mod:module_opts()) -> ok.
init(Host, Opts) ->
    Args = [Host, Opts],
    mongoose_backend:init(global, ?MAIN_MODULE, [], Opts),
    mongoose_backend:call(global, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec set_incoming_request(CallID :: call_id(), ReqID :: binary(),
                           From :: jid:jid(), To :: jid:jid(), exml:element()) ->
    ok | {error, any()}.
set_incoming_request(CallID, ReqID, From, To, JingleEl) ->
    Args = [CallID, ReqID, From, To, JingleEl],
    mongoose_backend:call(global, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec set_outgoing_request(CallID :: call_id(), ReqID :: binary(),
                           From :: jid:jid(), To :: jid:jid()) ->
    ok | {error, any()}.
set_outgoing_request(CallID, ReqID, From, To) ->
    Args = [CallID, ReqID, From, To],
    mongoose_backend:call(global, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec set_incoming_handle(CallID :: call_id(), DialogHandle :: dialog_hangle()) ->
    ok | {error, any()}.
set_incoming_handle(CallID, DialogHandle) ->
    Args = [CallID, DialogHandle],
    mongoose_backend:call(global, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec set_outgoing_handle(CallID :: call_id(), DialogHandle :: dialog_hangle(),
                          From :: jid:jid(), To :: jid:jid()) ->
    ok | {error, any()}.
set_outgoing_handle(CallID, DialogHandle, From, To) ->
    Args = [CallID, DialogHandle, From, To],
    mongoose_backend:call(global, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec set_incoming_accepted(CallID :: call_id()) ->
    ok | {error, any()}.
set_incoming_accepted(CallID) ->
    Args = [CallID],
    mongoose_backend:call(global, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec set_outgoing_accepted(CallID :: call_id()) ->
    ok | {error, any()}.
set_outgoing_accepted(CallID) ->
    Args = [CallID],
    mongoose_backend:call(global, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec get_incoming_request(call_id(), jid:jid()) -> {ok, undefined | incoming_request()} |
                                                    {error, not_found}.
get_incoming_request(CallID, User) ->
    Args = [CallID, User],
    mongoose_backend:call(global, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec get_outgoing_handle(call_id(), jid:jid()) -> {ok, undefined | outgoing_handle()} |
                                                   {error, not_found}.
get_outgoing_handle(SID, User) ->
    Args = [SID, User],
    mongoose_backend:call(global, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec get_session_info(call_id(), jid:jid()) ->
    {ok, map()} | {error, any()}.
get_session_info(SID, User) ->
    Args = [SID, User],
    mongoose_backend:call(global, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec remove_session(call_id()) -> ok.
remove_session(CallID) ->
    Args = [CallID],
    mongoose_backend:call(global, ?MAIN_MODULE, ?FUNCTION_NAME, Args).
