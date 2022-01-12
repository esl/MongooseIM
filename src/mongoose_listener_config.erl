%% @doc Utilities related to listener configuration options

-module(mongoose_listener_config).

-export([ensure_ip_options/1,
         verify_unique_listeners/1,
         prepare_opts/1,
         filter_socket_opts/1,
         listener_id/1]).

-type listener() :: #{port := inet:port_number(),
                      ip_tuple := inet:ip_address(),
                      ip_address := string(),
                      ip_version := 4 | 6,
                      proto := proto(),
                      any() => any()}.
-type listener_id() :: {inet:port_number(), inet:ip_address(), proto()}.
-type proto() :: tcp | udp.

-export_type([listener/0, listener_id/0, proto/0]).

%% @doc Fill in IP-related options that can be calculated automatically.
%% Apart from these options, the input should be a complete listener configuration.
-spec ensure_ip_options(map()) -> listener().
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
-spec verify_unique_listeners([listener()]) -> [listener()].
verify_unique_listeners(Listeners) ->
    Counts = lists:foldl(fun(L, Cts) ->
                                 maps:update_with(listener_id(L), fun(Ct) -> Ct + 1 end, 1, Cts)
                         end, #{}, Listeners),
    case [K || {K, V} <- maps:to_list(Counts), V > 1] of
        [] -> Listeners;
        Dups -> error(#{what => duplicate_listeners, duplicates => Dups,
                        text => <<"Some listeners have duplicate listening socket addresses">>})
    end.

%% @doc Convert listener configuration to options that can be passed to listener modules
-spec prepare_opts(listener()) -> proplists:proplist().
prepare_opts(Listener) ->
    lists:flatmap(fun prepare_opt/1, maps:to_list(Listener)).

prepare_opt({tls, Opts}) -> Opts;
prepare_opt({ip_version, 4}) -> [inet];
prepare_opt({ip_version, 6}) -> [inet6];
prepare_opt({ip_tuple, Val}) -> [{ip, Val}];
prepare_opt(Opt) -> [Opt].

%% @doc Filter listener options, leaving only socket-related ones
-spec filter_socket_opts(proplists:proplist()) -> proplists:proplist().
filter_socket_opts(Opts) ->
    lists:filter(fun filter_socket_opt/1, Opts).

filter_socket_opt(inet) -> true;
filter_socket_opt(inet6) -> true;
filter_socket_opt({ip, _}) -> true;
filter_socket_opt({backlog, _}) -> true;
filter_socket_opt(_) -> false.

%% @doc Create a unique ID based on the listening socket address
-spec listener_id(listener()) -> listener_id().
listener_id(#{port := Port, ip_tuple := IPTuple, proto := Proto}) ->
    {Port, IPTuple, Proto}.
