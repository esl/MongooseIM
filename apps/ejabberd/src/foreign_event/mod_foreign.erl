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

-module(mod_foreign).
-author('szymon.mentel@erlang-solutions.com').
-behaviour(gen_mod).

-include("jlib.hrl").
-include("ejabberd.hrl").

-define(DEFAULT_SUBHOST, <<"foreign.@HOST@">>).

-export([start/2, stop/1]).

%% Hook implementations
-export([get_disco_items/5]).

%%--------------------------------------------------------------------
%% Types
%%--------------------------------------------------------------------

-type foreign_request() :: jlib:xmlel().
-type foreign_response() :: jlib:xmlel().
-type on_response() :: fun((foreign_response()) -> term()).

%%--------------------------------------------------------------------
%% Callbacks
%%--------------------------------------------------------------------

-callback make_request(foreign_request(), on_response()) -> ok | {error, term()}.

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------

-spec start(Host :: ejabberd:server(), Opts :: list()) -> any().
start(Host, Opts) ->
    IQDisc = gen_mod:get_opt(iqdisc, Opts, one_queue),
    SubHost = subhost(Host),
    mod_disco:register_subhost(Host, SubHost),
    mongoose_subhosts:register(Host, SubHost),
    ejabberd_hooks:add(disco_local_items, Host, ?MODULE, get_disco_items, 90).


-spec stop(Host :: ejabberd:server()) -> any().
stop(Host) ->
    SubHost = subhost(Host),
    ejabberd_hooks:delete(disco_local_items, Host, ?MODULE, get_disco_items, 90),
    mongoose_subhosts:unregister(SubHost),
    mod_disco:unregister_subhost(Host, SubHost).

-spec get_disco_items(Acc :: term(), From :: ejabberd:jid(), To :: ejabberd:jid(),
                      Node :: binary(), ejabberd:lang()) -> {result, [jlib:xmlel()]} | term().
get_disco_items({result, Nodes}, _From, #jid{lserver = Host} = _To, <<"">>, Lang) ->
    Item = #xmlel{name  = <<"item">>,
                  attrs = [{<<"jid">>, subhost(Host)}, {<<"name">>, my_disco_name(Lang)}]},
    {result, [Item | Nodes]};
get_disco_items(empty, From, To, Node, Lang) ->
    get_disco_items({result, []}, From, To, Node, Lang);
get_disco_items(Acc, _From, _To, _Node, _Lang) ->
    Acc.

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

-spec subhost(Host :: ejabberd:server()) -> binary().
subhost(Host) ->
    gen_mod:get_module_opt_subhost(Host, ?MODULE, ?DEFAULT_SUBHOST).


-spec my_disco_name(ejabberd:lang()) -> binary().
my_disco_name(Lang) ->
    translate:translate(Lang, <<"Foreign-Event">>).

