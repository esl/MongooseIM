-module(mod_global_distrib).

-behaviour(gen_mod).

-include("ejabberd.hrl").
-include("jlib.hrl").

-callback put_session(JID :: binary(), Host :: binary()) -> ok.
-callback get_session(JID :: binary()) -> {ok, Host :: binary()} | term().
-callback delete_session(JID :: binary(), Host :: binary()) -> ok.

-export([start/2, stop/1, maybe_reroute_subhost/1, maybe_wrap_message/1, user_present/2,
         register_subhost/2, maybe_unwrap_message/1, unregister_subhost/2, user_not_present/5]).

-spec start(Host :: ejabberd:server(), Opts :: list()) -> any().
start(Host, Opts) ->
    {global_host, GlobalHostList} = lists:keyfind(global_host, 1, Opts),
    case unicode:characters_to_binary(GlobalHostList) of
        Host ->
            gen_mod:start_backend_module(?MODULE, [{backend, cassandra} | Opts]),

            Self = self(),
            Heir = case whereis(ejabberd_sup) of
                       undefined -> none;
                       Self -> none;
                       Pid -> Pid
                   end,

            ets:new(?MODULE, [named_table, public, {read_concurrency, true}, {heir, Heir, testing}]),
            [ets:insert(?MODULE, {K, unicode:characters_to_binary(V)}) || {K, V} <- Opts],

            LocalHost = opt(local_host),

            ejabberd_hooks:add(filter_packet, global, ?MODULE, maybe_reroute_subhost, 99),
            ejabberd_hooks:add(register_subhost, global, ?MODULE, register_subhost, 90),
            ejabberd_hooks:add(unregister_subhost, Host, ?MODULE, unregister_subhost, 90),
            ejabberd_hooks:add(filter_local_packet, Host, ?MODULE, maybe_wrap_message, 99),
            ejabberd_hooks:add(filter_local_packet, LocalHost, ?MODULE, maybe_unwrap_message, 99),
            ejabberd_hooks:add(user_available_hook, Host, ?MODULE, user_present, 90),
            ejabberd_hooks:add(unset_presence_hook, Host, ?MODULE, user_not_present, 90);

        _ -> ok
    end.

-spec stop(Host :: ejabberd:server()) -> any().
stop(Host) ->
    case opt(global_host) of
        Host ->
            LocalHost = opt(local_host),
            ejabberd_hooks:add(unset_presence_hook, Host, ?MODULE, user_not_present, 90),
            ejabberd_hooks:delete(user_available_hook, Host, ?MODULE, user_present, 90),
            ejabberd_hooks:delete(filter_local_packet, LocalHost, ?MODULE, maybe_unwrap_message, 99),
            ejabberd_hooks:delete(filter_local_packet, Host, ?MODULE, maybe_wrap_message, 99),
            ejabberd_hooks:delete(unregister_subhost, Host, ?MODULE, unregister_subhost, 90),
            ejabberd_hooks:delete(register_subhost, global, ?MODULE, register_subhost, 90),
            ejabberd_hooks:delete(filter_packet, global, ?MODULE, maybe_reroute_subhost, 99),
            ets:delete(?MODULE);

        _ -> ok
    end.

maybe_reroute_subhost(drop) -> drop;
maybe_reroute_subhost({_From, To, _Packet} = FPacket) ->
    LocalHost = opt(local_host),
    case mod_global_distrib_backend:get_session(To#jid.lserver) of
        {ok, [#{host := LocalHost}]} -> FPacket;
        {ok, [#{host := TargetHost}]} -> wrap_message(opt(cookie), LocalHost, TargetHost, FPacket);
        _ -> FPacket
    end.

maybe_unwrap_message(drop) -> drop;
maybe_unwrap_message({_From, _To, Packet} = FPacket) ->
    Cookie = opt(cookie),
    case {exml_query:attr(Packet, <<"type">>), exml_query:attr(Packet, <<"distrib">>)} of
        {<<"error">>, Cookie} -> unwrap_error_message(FPacket);
        {_, Cookie} -> unwrap_message(FPacket);
        _ -> FPacket
    end.

maybe_wrap_message(drop) -> drop;
maybe_wrap_message({_, To, _Packet} = FPacket) ->
    LocalHost = opt(local_host),
    case lookup_jid(To) of
        LocalHost -> FPacket;
        undefined -> FPacket;
        TargetHost -> wrap_message(opt(cookie), LocalHost, TargetHost, FPacket)
    end.

unwrap_error_message({_From, _To, #xmlel{children = [Child, Error]}}) ->
    ToBin = exml_query:attr(Child, <<"from">>),
    FromBin = exml_query:attr(Child, <<"to">>),
    UpdatedAttrs = lists:ukeysort(1, [{<<"from">>, FromBin}, {<<"to">>, ToBin},
                                      {<<"type">>, <<"error">>} | Child#xmlel.attrs]),
    UpdatedChild = Child#xmlel{attrs = UpdatedAttrs, children = Child#xmlel.children ++ [Error]},
    ejabberd_router:route(jid:from_binary(FromBin), jid:from_binary(ToBin), UpdatedChild),
    drop.

unwrap_message({_From, _To, #xmlel{children = [Child]}}) ->
    From = jid:from_binary(exml_query:attr(Child, <<"from">>)),
    To = jid:from_binary(exml_query:attr(Child, <<"to">>)),
    ?DEBUG("Unwrapping message from=~s [~s] to=~s [~s]",
           [exml_query:attr(Child, <<"from">>), jid:to_binary(_From),
            exml_query:attr(Child, <<"to">>), jid:to_binary(_To)]),
    ejabberd_router:route(From, To, Child),
    drop.

lookup_jid({_, _, _} = FullJid) ->
    case mod_global_distrib_backend:get_session(jid:to_binary(FullJid)) of
        {ok, [#{host := Result}]} -> Result;
        _ ->
            BareJid = jid:to_bare(FullJid),
            case mod_global_distrib_backend:get_session(jid:to_binary(BareJid)) of
                {ok, [#{host := BareResult}]} -> BareResult;
                _ -> undefined
            end
    end;
lookup_jid(#jid{} = Jid) ->
    lookup_jid(jid:to_lower(Jid)).

normalize_jid(#jid{} = Jid) ->
    normalize_jid(jid:to_lower(Jid));
normalize_jid({_, _, _} = FullJid) ->
    Jids =
        case jid:to_bare(FullJid) of
            FullJid -> [FullJid];
            BareJid -> [FullJid, BareJid]
        end,
    [jid:to_binary(Jid) || Jid <- Jids].


wrap_message(Cookie, LocalHost, TargetHost, {From, To, Packet}) ->
    wrap_message(Cookie, LocalHost, TargetHost, jlib:replace_from_to(From, To, Packet));
wrap_message(Cookie, LocalHost, TargetHost, Child) ->
    WrappedMessage = #xmlel{name = <<"message">>,
                            attrs = [{<<"distrib">>, Cookie},
                                     {<<"from">>, LocalHost},
                                     {<<"to">>, TargetHost}],
                            children = [Child]},
    ?DEBUG("Wrapping message from=~s [~s] to=~s [~s]",
           [exml_query:attr(Child, <<"from">>), LocalHost,
            exml_query:attr(Child, <<"to">>), TargetHost]),
    ejabberd_router:route(jid:from_binary(LocalHost), jid:from_binary(TargetHost), WrappedMessage),
    drop.

-spec user_present(Acc :: map(), UserJID :: ejabberd:jid()) -> ok.
user_present(Acc, #jid{} = UserJid) ->
    LocalHost = opt(local_host),
    [mod_global_distrib_backend:put_session(Jid, LocalHost) || Jid <- normalize_jid(UserJid)],
    Acc.

-spec user_not_present(Acc :: map(),
                       User     :: ejabberd:luser(),
                       Server   :: ejabberd:lserver(),
                       Resource :: ejabberd:lresource(),
                       _Status :: any()) -> ok.
user_not_present(Acc, User, Host, Resource, _Status) ->
    LocalHost = opt(local_host),
    UserJid = {User, Host, Resource},
    [mod_global_distrib_backend:delete_session(Jid, LocalHost) || Jid <- normalize_jid(UserJid)],
    Acc.

register_subhost(_, SubHost) ->
    IsSubhostOf = fun(Host) ->
                          case binary:match(SubHost, Host) of
                              {Start, Length} -> Start + Length == byte_size(SubHost);
                              _ -> false
                          end
                  end,

    GlobalHost = opt(global_host),
    case lists:filter(IsSubhostOf, ?MYHOSTS) of
        [GlobalHost] -> mod_global_distrib_backend:put_session(SubHost, opt(local_host));
        _ -> ok
    end.

unregister_subhost(_, SubHost) ->
    mod_global_distrib_backend:delete_session(SubHost, opt(local_host)).

opt(Key) ->
    try ets:lookup_element(?MODULE, Key, 2) catch _:_ -> undefined end.
