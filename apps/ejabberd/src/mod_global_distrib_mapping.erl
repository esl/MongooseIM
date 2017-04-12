-module(mod_global_distrib_mapping).

-behaviour(gen_mod).

-include("jlib.hrl").

-define(DOMAIN_TAB, mod_global_distrib_domain_cache_tab).
-define(JID_TAB, mod_global_distrib_jid_cache_tab).

-export([start/3, stop/0]).
-export([for_domain/1, insert_for_domain/2, delete_for_domain/2]).
-export([for_jid/1, insert_for_jid/2, delete_for_jid/2, clear_cache_for_jid/1]).

start(DomainLifetimeSeconds, JidLifetimeSeconds, MaxJids) ->
    cache_tab:new(?DOMAIN_TAB, [{cache_missed, false}, {life_time, DomainLifetimeSeconds}]),
    cache_tab:new(?JID_TAB, [{cache_missed, false}, {life_time, JidLifetimeSeconds},
                             {max_size, MaxJids}]),
    ok.

stop() ->
    cache_tab:delete(?DOMAIN_TAB),
    cache_tab:delete(?JID_TAB),
    ok.

for_domain(Domain) when is_binary(Domain) ->
    cache_tab:lookup(?DOMAIN_TAB, Domain,
                     fun() -> mod_global_distrib_backend:get_session(Domain) end).

insert_for_domain(Domain, Host) when is_binary(Domain), is_binary(Host) ->
    cache_tab:insert(?DOMAIN_TAB, Domain, Host,
                     fun() -> mod_global_distrib_backend:put_session(Domain, Host) end).

delete_for_domain(Domain, Host) when is_binary(Domain), is_binary(Host) ->
    cache_tab:delete(?DOMAIN_TAB, Domain,
                     fun() -> mod_global_distrib_backend:delete_session(Domain, Host) end).

for_jid(#jid{} = Jid) -> for_jid(jid:to_lower(Jid));
for_jid({_, _, _} = Jid) ->
    BinJid = jid:to_binary(Jid),
    LookupInDB = fun(BJid) -> fun() -> mod_global_distrib_backend:get_session(BJid) end end,
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
            InsertToDB = fun() -> mod_global_distrib_backend:put_session(BinJid, Host) end,
            cache_tab:insert(?JID_TAB, BinJid, Host, InsertToDB)
        end,
        normalize_jid(Jid)).

clear_cache_for_jid(Jid) when is_binary(Jid) ->
    cache_tab:dirty_delete(?JID_TAB, Jid).

delete_for_jid(#jid{} = Jid, Host) -> delete_for_jid(jid:to_lower(Jid), Host);
delete_for_jid({_, _, _} = Jid, Host) when is_binary(Host) ->
    lists:foreach(
        fun(BinJid) ->
            DeleteFromDB = fun() -> mod_global_distrib_backend:delete_session(BinJid, Host) end,
            cache_tab:delete(?JID_TAB, BinJid, DeleteFromDB)
        end,
        normalize_jid(Jid)).

normalize_jid({_, _, _} = FullJid) ->
    case jid:to_bare(FullJid) of
        FullJid -> [jid:to_binary(FullJid)];
        BareJid -> [jid:to_binary(FullJid), jid:to_binary(BareJid)]
    end.
