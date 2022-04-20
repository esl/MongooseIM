%% @doc Utilities related to listener configuration options

-module(mongoose_listener_config).

-export([ensure_ip_options/1,
         verify_unique_listeners/1,
         address_family/1,
         listener_id/1]).

%% @doc Fill in IP-related options that can be calculated automatically.
%% Apart from these options, the input should be a complete listener configuration.
-spec ensure_ip_options(map()) -> mongoose_listener:options().
ensure_ip_options(Opts = #{ip_address := IPAddr, ip_version := 4}) ->
    {ok, IPTuple} = inet:parse_ipv4_address(IPAddr),
    Opts#{ip_tuple => IPTuple};
ensure_ip_options(Opts = #{ip_address := IPAddr, ip_version := 6}) ->
    {ok, IPTuple} = inet:parse_ipv6_address(IPAddr),
    Opts#{ip_tuple => IPTuple};
ensure_ip_options(Opts = #{ip_address := IPAddr}) ->
    {ok, IPTuple} = inet:parse_address(IPAddr),
    Opts#{ip_tuple => IPTuple,
          ip_version => ip_version(IPTuple)};
ensure_ip_options(Opts = #{ip_version := 6}) ->
    ensure_ip_options(Opts#{ip_address => "::"});
ensure_ip_options(Opts) ->
    ensure_ip_options(Opts#{ip_address => "0"}).

ip_version(T) when tuple_size(T) =:= 4 -> 4;
ip_version(T) when tuple_size(T) =:= 8 -> 6.

%% @doc Verify that all listeners have unique socket addresses
-spec verify_unique_listeners([mongoose_listener:options()]) -> [mongoose_listener:options()].
verify_unique_listeners(Listeners) ->
    Counts = lists:foldl(fun(L, Cts) ->
                                 maps:update_with(listener_id(L), fun(Ct) -> Ct + 1 end, 1, Cts)
                         end, #{}, Listeners),
    case [K || {K, V} <- maps:to_list(Counts), V > 1] of
        [] -> Listeners;
        Dups -> error(#{what => duplicate_listeners, duplicates => Dups,
                        text => <<"Some listeners have duplicate listening socket addresses">>})
    end.

-spec address_family(4 | 6) -> inet:address_family().
address_family(4) -> inet;
address_family(6) -> inet6.

%% @doc Create a unique ID based on the listening socket address
-spec listener_id(mongoose_listener:options()) -> mongoose_listener:id().
listener_id(#{port := Port, ip_tuple := IPTuple, proto := Proto}) ->
    {Port, IPTuple, Proto}.
