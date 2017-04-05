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

remove_message4(Acc, _From, _To, Packet) ->
    ets:delete(?MODULE, storage_key(Packet)),
    Acc.

maybe_resend_message(drop) -> drop;
maybe_resend_message({_From, _To, Packet} = FPacket) ->
    case exml_query:attr(Packet, <<"distrib_ttl">>, <<"0">>) of
        <<"0">> -> FPacket;
        BinTTL  ->
            case exml_query:attr(Packet, <<"type">>) of
                <<"error">> ->
                    case ets:lookup(?MODULE, storage_key(Packet)) of
                        [] -> FPacket;
                        [{_, StoredPacket}] ->
                            resend_message(StoredPacket, FPacket, binary_to_integer(BinTTL)),
                            drop
                    end;
                _ ->
                    FPacket
            end
    end.

maybe_store_message(drop) -> drop;
maybe_store_message({_From, _To, Packet} = FPacket) ->
    case exml_query:attr(Packet, <<"distrib_ttl">>) of
        undefined -> store_message(FPacket);
        _ -> FPacket
    end.

resend_message(StoredPacket, {From, To, _Packet}, TTL) ->
    BouncedPacket = set_ttl(StoredPacket, TTL - 1),
    timer:apply_after(100, ejabberd_router, route, [To, From, BouncedPacket]). %% TODO

store_message({From, To, Packet0}) ->
    Packet = maybe_set_id(set_ttl(Packet0, 4)), %% TODO: TTL
    ets:insert(?MODULE, {storage_key(Packet), Packet}),
    {From, To, Packet}.

maybe_set_id(Packet) ->
    case exml_query:attr(Packet, <<"id">>) of
        undefined -> set_id(Packet);
        Id -> {Packet, Id}
    end.

set_id(Packet) ->
    NewId = uuid:uuid_to_string(uuid:get_v4(), binary_nodash),
    Packet#xmlel{attrs = [{<<"id">>, NewId} | Packet#xmlel.attrs]}.

set_ttl(Packet, NewTTL) when is_integer(NewTTL) ->
    TTLTuple = {<<"distrib_ttl">>, integer_to_binary(NewTTL)},
    Packet#xmlel{attrs = lists:keystore(<<"distrib_ttl">>, 1, Packet#xmlel.attrs, TTLTuple)}.

storage_key(Packet) ->
    {exml_query:attr(Packet, <<"from">>), exml_query:attr(Packet, <<"id">>)}.

opt(Key) ->
    try ets:lookup_element(?MODULE, Key, 2) catch _:_ -> undefined end.
