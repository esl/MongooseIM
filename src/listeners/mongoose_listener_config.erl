%% @doc Utilities related to listener configuration options
-module(mongoose_listener_config).

-export([ip_version/1, ensure_ip_options/1, verify_unique_listeners/1]).

%% @doc Fill in IP-related options that can be calculated automatically.
%% Apart from these options, the input should be a complete listener configuration.
-spec ensure_ip_options(map()) -> mongoose_listener:options().
ensure_ip_options(Opts = #{ip_address := IPAddr, ip_version := 4}) ->
    {ok, IPTuple} = inet:parse_ipv4_address(IPAddr),
    Opts#{ip_tuple => IPTuple, ip_version := inet};
ensure_ip_options(Opts = #{ip_address := IPAddr, ip_version := 6}) ->
    {ok, IPTuple} = inet:parse_ipv6_address(IPAddr),
    Opts#{ip_tuple => IPTuple, ip_version := inet6};
ensure_ip_options(Opts = #{ip_address := IPAddr}) ->
    {ok, IPTuple} = inet:parse_address(IPAddr),
    Opts#{ip_tuple => IPTuple,
          ip_version => ip_version(IPTuple)};
ensure_ip_options(Opts = #{ip_version := 6}) ->
    ensure_ip_options(Opts#{ip_address => "::", ip_version := inet6});
ensure_ip_options(Opts) ->
    ensure_ip_options(Opts#{ip_address => "0", ip_version => inet}).

ip_version(T) when tuple_size(T) =:= 4 -> inet;
ip_version(T) when tuple_size(T) =:= 8 -> inet6.

%% @doc Verify that all listeners have unique socket addresses
-spec verify_unique_listeners([mongoose_listener:options()]) -> [mongoose_listener:options()].
verify_unique_listeners(Listeners) ->
    Counts = lists:foldl(fun(L, Cts) ->
                                 maps:update_with(mongoose_listener:listener_id(L), fun(Ct) -> Ct + 1 end, 1, Cts)
                         end, #{}, Listeners),
    case [K || {K, V} <- maps:to_list(Counts), V > 1] of
        [] -> Listeners;
        Dups -> error(#{what => duplicate_listeners, duplicates => Dups,
                        text => <<"Some listeners have duplicate listening socket addresses">>})
    end.
