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

-include("jlib.hrl").
-include("mod_event_pusher_events.hrl").

-export([deps/2, start/2, stop/1, push_event/3]).

%%--------------------------------------------------------------------
%% Callbacks
%%--------------------------------------------------------------------

-callback push_event(Acc :: mongoose_acc:t(), Host :: jid:lserver(), Event :: event()) -> any().

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------

%% @doc Pushes the event to each backend registered with the event_pusher.
-spec push_event(mongoose_acc:t(), Host :: jid:server(), Event :: event()) -> mongoose_acc:t().
push_event(Acc, Host, Event) ->
    [B:push_event(Acc, Host, Event) || B <- ets:lookup_element(ets_name(Host), backends, 2)],
    Acc.

%%--------------------------------------------------------------------
%% gen_mod API
%%--------------------------------------------------------------------

-spec deps(Host :: jid:server(), Opts :: proplists:proplist()) -> gen_mod:deps_list().
deps(_Host, Opts) ->
    Backends = get_backends(Opts),
    BackendDeps = [{B, DepOpts, hard} || {B, DepOpts} <- Backends],
    [{mod_event_pusher_hook_translator, hard} | BackendDeps].

-spec start(Host :: jid:server(), Opts :: proplists:proplist()) -> any().
start(Host, Opts) ->
    create_ets(Host),
    Backends = get_backends(Opts),
    ets:insert(ets_name(Host), {backends, [B || {B, _} <- Backends]}),
    ejabberd_hooks:add(push_event, Host, ?MODULE, push_event, 90).

-spec stop(Host :: jid:server()) -> any().
stop(Host) ->
    ejabberd_hooks:delete(push_event, Host, ?MODULE, push_event, 90),
    ets:delete(ets_name(Host)).

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

-spec get_backends(Opts :: proplists:proplist()) -> [{module(), proplists:proplist()}].
get_backends(Opts) ->
    {backends, Backends0} = lists:keyfind(backends, 1, Opts),
    lists:foldr(fun add_backend/2, [], Backends0).

add_backend({http, Opts}, BackendList) ->
    % http backend is treated somewhat differently - we allow configuration settins as if there
    % were many modules, while here we put together a single list of settings for the http event
    % pusher module. Thus, you can configure event pusher like:
    %{mod_event_pusher,
    %   [{backends,
    %       [{http,
    %           [{path, "/push_here"},
    %            {callback_module, mod_event_pusher_http_one},
    %            {pool_name, http_pool}]
    %       },
    %        {http,
    %           [{path, "/push_there"},
    %            {callback_module, mod_event_pusher_http_two},
    %            {pool_name, http_pool}]
    %        }
    %      ]
    %   }]
    HttpModName = translate_backend(http),
    case lists:keyfind(HttpModName, 1, BackendList) of
        false ->
            [{HttpModName, [{configs, [Opts]}]} | BackendList];
        {HttpModName, [{configs, O}]} ->
            lists:keyreplace(HttpModName, 1, BackendList,
                {HttpModName, [{configs, [Opts | O]}]})
    end;
add_backend({Mod, Opts}, BackendList) ->
    [{translate_backend(Mod), Opts} | BackendList].

-spec translate_backend(Backend :: atom()) -> module().
translate_backend(Backend) ->
    list_to_atom(?MODULE_STRING ++ "_" ++ atom_to_list(Backend)).

-spec ets_name(Host :: jid:server()) -> atom().
ets_name(Host) ->
    gen_mod:get_module_proc(Host, ?MODULE).

-spec create_ets(Host :: jid:server()) -> any().
create_ets(Host) ->
    Self = self(),
    Heir = case whereis(ejabberd_sup) of
               undefined -> none;
               Self -> none;
               Pid -> Pid
           end,
    ets:new(ets_name(Host), [public, named_table, {read_concurrency, true}, {heir, Heir, testing}]).
