%%==============================================================================
%% Copyright 2017 Erlang Solutions Ltd.
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

-module(mod_global_distrib_disco).
-author('konrad.zemek@erlang-solutions.com').

-behaviour(gen_mod).

-include("mongoose.hrl").
-include("jlib.hrl").

-export([start/2, stop/1, get_disco_items/5]).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------

-spec start(Host :: jid:server(), Opts :: list()) -> any().
start(Host, Opts) ->
    mod_global_distrib_utils:start(?MODULE, Host, Opts, fun start/0).

-spec stop(Host :: jid:server()) -> any().
stop(Host) ->
    mod_global_distrib_utils:stop(?MODULE, Host, fun stop/0).

%%--------------------------------------------------------------------
%% Hooks implementation
%%--------------------------------------------------------------------

-spec get_disco_items(Acc :: term(), From :: jid:jid(), To :: jid:jid(),
                      Node :: binary(), ejabberd:lang()) -> {result, [exml:element()]} | term().
get_disco_items({result, Nodes}, _From, _To, <<"">>, _Lang) ->
    {ok, Domains} = mod_global_distrib_mapping:all_domains(),
    NameSet = gb_sets:from_list([exml_query:attr(Node, <<"jid">>) || Node <- Nodes]),
    FilteredDomains = [Domain || Domain <- Domains, not gb_sets:is_member(Domain, NameSet)],
    ?DEBUG("Adding global domains ~p to disco results", [FilteredDomains]),
    NewNodes =
        lists:foldl(
          fun(Domain, Acc) ->
                  [#xmlel{name  = <<"item">>, attrs = [{<<"jid">>, Domain}]} | Acc]
          end,
          Nodes,
          FilteredDomains),
    {result, NewNodes};
get_disco_items(empty, From, To, Node, Lang) ->
    get_disco_items({result, []}, From, To, Node, Lang);
get_disco_items(Acc, _From, _To, _Node, _Lang) ->
    Acc.

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

-spec start() -> any().
start() ->
    Host = opt(global_host),
    ejabberd_hooks:add(disco_local_items, Host, ?MODULE, get_disco_items, 99).

-spec stop() -> any().
stop() ->
    Host = opt(global_host),
    ejabberd_hooks:delete(disco_local_items, Host, ?MODULE, get_disco_items, 99).

-spec opt(Key :: atom()) -> term().
opt(Key) ->
    mod_global_distrib_utils:opt(?MODULE, Key).
