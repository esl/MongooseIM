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

-export([start/2, stop/1, hooks/1, deps/2, disco_local_items/3]).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------

-spec start(mongooseim:host_type(), gen_mod:module_opts()) -> any().
start(_HostType, _Opts) ->
    ok.

-spec stop(mongooseim:host_type()) -> any().
stop(_HostType) ->
    ok.

-spec deps(mongooseim:host_type(), gen_mod:module_opts()) -> gen_mod_deps:deps().
deps(_HostType, Opts) ->
    [{mod_global_distrib_utils, Opts, hard}].

%%--------------------------------------------------------------------
%% Hooks implementation
%%--------------------------------------------------------------------

-spec disco_local_items(Acc, Params, Extra) -> {ok, Acc} when
                        Acc :: mongoose_disco:item_acc(),
                        Params :: map(),
                        Extra :: map().
disco_local_items(Acc = #{host_type := HostType, from_jid := From, node := <<>>}, _, _) ->
    Domains = domains_for_disco(HostType, From),
    ?LOG_DEBUG(#{what => gd_domains_fetched_for_disco, domains => Domains}),
    Items = [#{jid => Domain} || Domain <- Domains],
    NewAcc = mongoose_disco:add_items(Items, Acc),
    {ok, NewAcc};
disco_local_items(Acc, _, _) ->
    {ok, Acc}.

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

hooks(HostType) ->
    [{disco_local_items, HostType, fun ?MODULE:disco_local_items/3, #{}, 99}].

-spec domains_for_disco(mongooseim:host_type(), From :: jid:jid()) -> Domains :: [binary()].
domains_for_disco(_HostType, #jid{ luser = <<>> } = _From) ->
    %% Currently all non-user entities may discover all services
    mod_global_distrib_mapping:all_domains();
domains_for_disco(HostType, _From) ->
    %% mod_disco is running because it is the only caller of 'disco_local_items'
    case gen_mod:get_module_opt(HostType, mod_disco, users_can_see_hidden_services) of
        true ->
            mod_global_distrib_mapping:all_domains();
        false ->
            mod_global_distrib_mapping:public_domains()
    end.
