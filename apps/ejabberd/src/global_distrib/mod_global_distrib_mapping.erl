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

-module(mod_global_distrib_mapping).
-author('konrad.zemek@erlang-solutions.com').

-behaviour(gen_mod).

-include("ejabberd.hrl").
-include("jlib.hrl").

-define(DOMAIN_TAB, mod_global_distrib_domain_cache_tab).
-define(JID_TAB, mod_global_distrib_jid_cache_tab).

-export([start/2, stop/1]).
-export([for_domain/1, insert_for_domain/2, delete_for_domain/2]).
-export([for_jid/1, insert_for_jid/2, delete_for_jid/2, clear_cache_for_jid/1]).
-export([register_subhost/2, unregister_subhost/2, user_present/2, user_not_present/5]).

%%--------------------------------------------------------------------
%% Callbacks
%%--------------------------------------------------------------------

-callback put_session(DatabasePool :: atom(), JID :: binary(), Host :: binary()) -> ok.
-callback get_session(DatabasePool :: atom(), JID :: binary()) -> {ok, Host :: binary()} | term().
-callback delete_session(DatabasePool :: atom(), JID :: binary(), Host :: binary()) -> ok.

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------

start(Host, Opts0) ->
    Opts = [{database_pool, global}, {cache_missed, false},
            {domain_lifetime_seconds, 600}, {jid_lifetime_seconds, 5}, {max_jids, 10000} | Opts0],
    mod_global_distrib_utils:start(?MODULE, Host, Opts, fun start/0).

stop(Host) ->
    mod_global_distrib_utils:stop(?MODULE, Host, fun stop/0).

for_domain(Domain) when is_binary(Domain) ->
    cache_tab:lookup(?DOMAIN_TAB, Domain, fun() -> get_session(Domain) end).

insert_for_domain(Domain, Host) when is_binary(Domain), is_binary(Host) ->
    cache_tab:insert(?DOMAIN_TAB, Domain, Host, fun() -> put_session(Domain, Host) end).

delete_for_domain(Domain, Host) when is_binary(Domain), is_binary(Host) ->
    cache_tab:delete(?DOMAIN_TAB, Domain, fun() -> delete_session(Domain, Host) end).

for_jid(#jid{} = Jid) -> for_jid(jid:to_lower(Jid));
for_jid({_, _, _} = Jid) ->
    BinJid = jid:to_binary(Jid),
    LookupInDB = fun(BJid) -> fun() -> get_session(BJid) end end,
    case cache_tab:lookup(?JID_TAB, BinJid, LookupInDB(BinJid)) of
        {ok, _} = Result -> Result;
        Other ->
            case jid:to_bare(Jid) of
                Jid -> Other;
                BareJid -> cache_tab:lookup(?JID_TAB, BinJid, LookupInDB(jid:to_binary(BareJid)))
            end
    end.

insert_for_jid(#jid{} = Jid, Host) -> insert_for_jid(jid:to_lower(Jid), Host);
insert_for_jid({_, _, _} = Jid, Host) when is_binary(Host) ->
    lists:foreach(
      fun(BinJid) ->
              cache_tab:insert(?JID_TAB, BinJid, Host, fun() -> put_session(BinJid, Host) end)
      end,
      normalize_jid(Jid)).

clear_cache_for_jid(Jid) when is_binary(Jid) ->
    cache_tab:dirty_delete(?JID_TAB, Jid).

delete_for_jid(#jid{} = Jid, Host) -> delete_for_jid(jid:to_lower(Jid), Host);
delete_for_jid({_, _, _} = Jid, Host) when is_binary(Host) ->
    lists:foreach(
      fun(BinJid) ->
              cache_tab:delete(?JID_TAB, BinJid, fun() -> delete_session(BinJid, Host) end)
      end,
      normalize_jid(Jid)).

%%--------------------------------------------------------------------
%% Hooks implementation
%%--------------------------------------------------------------------

-spec user_present(Acc :: map(), UserJID :: ejabberd:jid()) -> ok.
user_present(Acc, #jid{} = UserJid) ->
    insert_for_jid(UserJid, opt(local_host)),
    Acc.

-spec user_not_present(Acc :: map(),
                       User     :: ejabberd:luser(),
                       Server   :: ejabberd:lserver(),
                       Resource :: ejabberd:lresource(),
                       _Status :: any()) -> ok.
user_not_present(Acc, User, Host, Resource, _Status) ->
    UserJid = {User, Host, Resource},
    delete_for_jid(UserJid, opt(local_host)),
    Acc.

register_subhost(_, SubHost) ->
    IsSubhostOf =
        fun(Host) ->
                case binary:match(SubHost, Host) of
                    {Start, Length} -> Start + Length == byte_size(SubHost);
                    _ -> false
                end
        end,

    GlobalHost = opt(global_host),
    case lists:filter(IsSubhostOf, ?MYHOSTS) of
        [GlobalHost] -> insert_for_domain(SubHost, opt(local_host));
        _ -> ok
    end.

unregister_subhost(_, SubHost) ->
    delete_for_domain(SubHost, opt(local_host)).

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

start() ->
    Host = opt(global_host),
    gen_mod:start_backend_module(?MODULE, [{backend, cassandra}]),

    CacheMissed = opt(cache_missed),
    DomainLifetime = opt(domain_lifetime_seconds),
    JidLifetime = opt(jid_lifetime_seconds),
    MaxJids = opt(max_jids),

    ejabberd_hooks:add(register_subhost, global, ?MODULE, register_subhost, 90),
    ejabberd_hooks:add(unregister_subhost, Host, ?MODULE, unregister_subhost, 90),
    ejabberd_hooks:add(user_available_hook, Host, ?MODULE, user_present, 90),
    ejabberd_hooks:add(unset_presence_hook, Host, ?MODULE, user_not_present, 90),

    cache_tab:new(?DOMAIN_TAB, [{cache_missed, CacheMissed}, {life_time, DomainLifetime}]),
    cache_tab:new(?JID_TAB, [{cache_missed, CacheMissed}, {life_time, JidLifetime},
                             {max_size, MaxJids}]).

stop() ->
    Host = opt(global_host),

    cache_tab:delete(?JID_TAB),
    cache_tab:delete(?DOMAIN_TAB),

    ejabberd_hooks:delete(unset_presence_hook, Host, ?MODULE, user_not_present, 90),
    ejabberd_hooks:delete(user_available_hook, Host, ?MODULE, user_present, 90),
    ejabberd_hooks:delete(unregister_subhost, Host, ?MODULE, unregister_subhost, 90),
    ejabberd_hooks:delete(register_subhost, global, ?MODULE, register_subhost, 90).


normalize_jid({_, _, _} = FullJid) ->
    case jid:to_bare(FullJid) of
        FullJid -> [jid:to_binary(FullJid)];
        BareJid -> [jid:to_binary(FullJid), jid:to_binary(BareJid)]
    end.

opt(Key) ->
    mod_global_distrib_utils:opt(?MODULE, Key).

get_session(Key) ->
    mod_global_distrib_mapping_backend:get_session(opt(database_pool), Key).

put_session(Key, Value) ->
    mod_global_distrib_mapping_backend:put_session(opt(database_pool), Key, Value).

delete_session(Key, Value) ->
    mod_global_distrib_mapping_backend:delete_session(opt(database_pool), Key, Value).
