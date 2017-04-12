-module(mod_global_distrib_bounce).

-behaviour(gen_mod).

-include("ejabberd.hrl").
-include("jlib.hrl").

-export([start/2, stop/1]).
-export([remove_message5/5, remove_message4/4, maybe_store_message/1, maybe_resend_message/1]).

-spec start(Host :: ejabberd:server(), Opts :: list()) -> any().
start(Host, Opts) ->
    {global_host, GlobalHostList} = lists:keyfind(global_host, 1, Opts),
    case unicode:characters_to_binary(GlobalHostList) of
        Host ->
            Self = self(),
            Heir = case whereis(ejabberd_sup) of
                       undefined -> none;
                       Self -> none;
                       Pid -> Pid
                   end,

            ets:new(?MODULE, [named_table, public, {read_concurrency, true}, {heir, Heir, testing}]),
            [ets:insert(?MODULE, {K, unicode:characters_to_binary(V)}) || {K, V} <- Opts],

            LocalHost = opt(local_host),

            ejabberd_hooks:add(filter_packet, global, ?MODULE, maybe_resend_message, 1),
            ejabberd_hooks:add(filter_local_packet, Host, ?MODULE, maybe_store_message, 80),
            ejabberd_hooks:add(user_receive_packet, LocalHost, ?MODULE, remove_message5, 99),
            ejabberd_hooks:add(s2s_send_packet, LocalHost, ?MODULE, remove_message4, 99);

        _ -> ok
    end.

-spec stop(Host :: ejabberd:server()) -> any().
stop(Host) ->
    case opt(global_host) of
        Host ->
            LocalHost = opt(local_host),

            ejabberd_hooks:delete(s2s_send_packet, LocalHost, ?MODULE, remove_message4, 99),
            ejabberd_hooks:delete(user_receive_packet, LocalHost, ?MODULE, remove_message5, 99),
            ejabberd_hooks:delete(filter_local_packet, Host, ?MODULE, maybe_store_message, 80),
            ejabberd_hooks:delete(filter_packet, global, ?MODULE, maybe_resend_message, 1),
            ets:delete(?MODULE);

        _ -> ok
    end.

remove_message5(Acc, _Jid, From, To, Packet) ->
    remove_message4(Acc, From, To, Packet).

remove_message4(Acc, From, _To, Packet) ->
    ?DEBUG("Removing message from=~s, to=~s from buffer: successful delivery",
           [jid:to_binary(From), jid:to_binary(_To)]),
    ets:delete(?MODULE, storage_key(From, Packet)),
    Acc.

maybe_resend_message(drop) -> drop;
maybe_resend_message({_From, To, Packet} = FPacket) ->
    case exml_query:attr(Packet, <<"type">>) of
        <<"error">> ->
            StorageKey = storage_key(To, Packet),
            case ets:lookup(?MODULE, StorageKey) of
                [] -> FPacket;
                [{_, _StoredPacket, _TTL = 0}] ->
                    ets:delete(?MODULE, StorageKey),
                    FPacket;
                [{_, StoredPacket, _TTL}] ->
                    resend_message(StorageKey, StoredPacket),
                    drop
            end;
        _ ->
            FPacket
    end.

maybe_store_message(drop) -> drop;
maybe_store_message({_From, _To, Packet} = FPacket) ->
    case exml_query:attr(Packet, <<"distrib_ttl">>) of
        undefined -> store_message(FPacket);
        _ -> FPacket
    end.

resend_message(StorageKey, StoredPacket) ->
    TTL = ets:update_counter(?MODULE, StorageKey, {3, -1}),
    FromBin = exml_query:attr(StoredPacket, <<"from">>),
    ToBin = exml_query:attr(StoredPacket, <<"to">>),
    ?DEBUG("Scheduling resend of message from=~s to=~s retries_left=~B", [FromBin, ToBin, TTL]),
    mod_global_distrib_mapping:clear_cache_for_jid(ToBin),
    timer:apply_after(200, ejabberd_router, route,
                      [jid:from_binary(FromBin), jid:from_binary(ToBin), StoredPacket]). %% TODO

store_message({From, To, Packet0}) ->
    Packet = jlib:replace_from_to(From, To, maybe_set_id(Packet0)),
    ets:insert_new(?MODULE, {storage_key(From, Packet), Packet, 4}), %% TODO: TTL
    {From, To, Packet}.

maybe_set_id(Packet) ->
    case exml_query:attr(Packet, <<"id">>) of
        undefined -> set_id(Packet);
        _ -> Packet
    end.

set_id(Packet) ->
    NewId = uuid:uuid_to_string(uuid:get_v4(), binary_nodash),
    Packet#xmlel{attrs = [{<<"id">>, NewId} | Packet#xmlel.attrs]}.

storage_key(From, Packet) when is_binary(From) ->
    {From, exml_query:attr(Packet, <<"id">>)};
storage_key(From, Packet) ->
    storage_key(jid:to_binary(From), Packet).

opt(Key) ->
    try ets:lookup_element(?MODULE, Key, 2) catch _:_ -> undefined end.
