%%==============================================================================
%% Copyright 2016 Erlang Solutions Ltd.
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

-module(mod_event_pusher).
-author('konrad.zemek@erlang-solutions.com').

-behaviour(gen_mod).
-behaviour(mongoose_module_metrics).

-include("mod_event_pusher_events.hrl").
-include("mongoose_config_spec.hrl").

-type backend() :: http | push | rabbit | sns.
-type event() :: #user_status_event{} | #chat_event{} | #unack_msg_event{}.
-type metadata() :: #{atom() => atom() | binary() | number()}.
-type push_event_params() :: #{event := event()}.
-type push_event_acc() :: #{acc := mongoose_acc:t(), metadata := metadata()}.
-export_type([event/0, metadata/0, push_event_acc/0, push_event_params/0]).

-export([deps/2, start/2, stop/1, config_spec/0, push_event/2]).

-export([config_metrics/1]).

-ignore_xref([behaviour_info/1]).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------

%% @doc Pushes the event to each backend registered with the event_pusher.
-spec push_event(mongoose_acc:t(), event()) -> mongoose_acc:t().
push_event(Acc, Event) ->
    #{acc := NewAcc} = mongoose_hooks:push_event(Acc, Event),
    NewAcc.

%%--------------------------------------------------------------------
%% gen_mod API
%%--------------------------------------------------------------------

-spec deps(mongooseim:host_type(), gen_mod:module_opts()) -> gen_mod_deps:deps().
deps(_HostType, Opts) ->
    [{backend_module(Backend), BackendOpts, hard} || {Backend, BackendOpts} <- maps:to_list(Opts)].

-spec start(mongooseim:host_type(), gen_mod:module_opts()) -> any().
start(HostType, _Opts) ->
    mod_event_pusher_hook_translator:add_hooks(HostType).

-spec stop(mongooseim:host_type()) -> any().
stop(HostType) ->
    mod_event_pusher_hook_translator:delete_hooks(HostType).

-spec config_spec() -> mongoose_config_spec:config_section().
config_spec() ->
    BackendItems = [{atom_to_binary(B, utf8),
                     (backend_module(B)):config_spec()} || B <- all_backends()],
    #section{items = maps:from_list(BackendItems)}.

-spec config_metrics(mongooseim:host_type()) -> [{gen_mod:opt_key(), gen_mod:opt_value()}].
config_metrics(HostType) ->
    case gen_mod:get_module_opts(HostType, ?MODULE) of
        Empty when Empty =:= #{} ->
            [{none, none}];
        Opts ->
            [{backend, Backend} || Backend <- maps:keys(Opts)]
    end.

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

-spec backend_module(backend()) -> module().
backend_module(http) -> mod_event_pusher_http;
backend_module(push) -> mod_event_pusher_push;
backend_module(rabbit) -> mod_event_pusher_rabbit;
backend_module(sns) -> mod_event_pusher_sns.

all_backends() ->
    [http, push, rabbit, sns].
