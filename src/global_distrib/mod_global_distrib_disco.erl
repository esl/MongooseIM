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
-behaviour(mongoose_module_metrics).

-include("mongoose.hrl").
-include("jlib.hrl").

-export([start/2, stop/1, disco_local_items/1]).

-ignore_xref([disco_local_items/1]).

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

-spec disco_local_items(mongoose_disco:item_acc()) -> mongoose_disco:item_acc().
disco_local_items(Acc = #{from_jid := From, to_jid := To, node := <<>>}) ->
    Domains = domains_for_disco(To#jid.lserver, From),
    ?LOG_DEBUG(#{what => gd_domains_fetched_for_disco, domains => Domains}),
    Items = [#{jid => Domain} || Domain <- Domains],
    mongoose_disco:add_items(Items, Acc);
disco_local_items(Acc) ->
    Acc.

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

-spec start() -> any().
start() ->
    Host = opt(global_host),
    ejabberd_hooks:add(disco_local_items, Host, ?MODULE, disco_local_items, 99).

-spec stop() -> any().
stop() ->
    Host = opt(global_host),
    ejabberd_hooks:delete(disco_local_items, Host, ?MODULE, disco_local_items, 99).

-spec opt(Key :: atom()) -> term().
opt(Key) ->
    mod_global_distrib_utils:opt(?MODULE, Key).

-spec domains_for_disco(Host :: jid:lserver(), From :: jid:jid()) -> Domains :: [binary()].
domains_for_disco(_Host, #jid{ luser = <<>> } = _From) ->
    %% Currently all non-user entities may discover all services
    mod_global_distrib_mapping:all_domains();
domains_for_disco(Host, _From) ->
    case gen_mod:get_module_opt(Host, mod_disco, users_can_see_hidden_services, true) of
        true ->
            mod_global_distrib_mapping:all_domains();
        false ->
            mod_global_distrib_mapping:public_domains()
    end.

