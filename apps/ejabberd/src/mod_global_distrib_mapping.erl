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
    LookupInDB =
        fun() ->
                case mod_global_distrib_backend:get_session(BinJid) of
                    {ok, _} = Result -> Result;
                    Other ->
                        case jid:to_bare(Jid) of
                            Jid -> Other;
                            BareJid ->
                                mod_global_distrib_backend:get_session(jid:to_binary(BareJid))
                        end
                end
        end,
    cache_tab:lookup(?JID_TAB, BinJid, LookupInDB).

insert_for_jid(#jid{} = Jid, Host) -> insert_for_jid(jid:to_lower(Jid), Host);
insert_for_jid({_, _, _} = Jid, Host) when is_binary(Host) ->
    BinJid = jid:to_binary(Jid),
    InsertToDB =
        fun() ->
                lists:all(fun(Res) -> Res =:= ok end,
                          [mod_global_distrib_backend:put_session(BJid, Host)
                           || BJid <- normalize_jid(Jid, BinJid)])
        end,
    cache_tab:insert(?JID_TAB, BinJid, Host, InsertToDB).

clear_cache_for_jid(Jid) when is_binary(Jid) ->
    cache_tab:dirty_delete(?JID_TAB, Jid).

delete_for_jid(#jid{} = Jid, Host) -> delete_for_jid(jid:to_lower(Jid), Host);
delete_for_jid({_, _, _} = Jid, Host) when is_binary(Host) ->
    BinJid = jid:to_binary(Jid),
    DeleteFromDB =
        fun() ->
                lists:all(fun(Res) -> Res =:= ok end,
                          [mod_global_distrib_backend:delete_session(BJid, Host)
                           || BJid <- normalize_jid(Jid, BinJid)])
        end,
    cache_tab:delete(?JID_TAB, BinJid, DeleteFromDB).

normalize_jid({_, _, _} = FullJid, BinFullJid) ->
    case jid:to_bare(FullJid) of
        FullJid -> [BinFullJid];
        BareJid -> [BinFullJid, jid:to_binary(BareJid)]
    end.
