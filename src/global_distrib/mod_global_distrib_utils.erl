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

-module(mod_global_distrib_utils).
-author('konrad.zemek@erlang-solutions.com').

-include("mongoose.hrl").
-include("jlib.hrl").

-behaviour(gen_mod).

%% gen_mod callbacks
-export([start/2, stop/1]).

-export([
         opt/2, host_type/0, create_ets/1, create_ets/2,
         cast_or_call/2, cast_or_call/3, cast_or_call/4,
         any_binary_to_atom/1, resolve_endpoints/1,
         binary_to_metric_atom/1, ensure_metric/2, recipient_to_worker_key/2,
         server_to_mgr_name/1, server_to_sup_name/1, maybe_update_mapping/2,
         parse_address/1
        ]).

-export([getaddrs/2]).

-ignore_xref([cast_or_call/3, cast_or_call/4]).

-type domain_name() :: string().
-type endpoint() :: {inet:ip_address() | domain_name(), inet:port_number()}.

-export_type([endpoint/0]).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------

-spec binary_to_metric_atom(binary()) -> atom().
binary_to_metric_atom(Binary) ->
    List = lists:filtermap(fun
                               ($.) -> {true, $_};
                               ($ ) -> {true, $_};
                               (C) when C > 31, C < 127 -> {true, C};
                               (_) -> false
                           end,
                           unicode:characters_to_list(Binary)),
    list_to_atom(List).

ensure_metric(Metric, Type) ->
    mongoose_metrics:ensure_subscribed_metric(global, Metric, Type).

-spec any_binary_to_atom(binary()) -> atom().
any_binary_to_atom(Binary) ->
    binary_to_atom(base64:encode(Binary), latin1).

-spec start(mongooseim:host_type(), gen_mod:module_opts()) -> ok.
start(HostType, #{global_host := GlobalHost, local_host := LocalHost}) ->
    check_host(LocalHost),
    check_host(GlobalHost),
    persistent_term:put({mod_global_distrib, host_type}, HostType).

-spec stop(mongooseim:host_type()) -> any().
stop(_HostType) ->
    persistent_term:erase({mod_global_distrib, host_type}).

-spec opt(module(), gen_mod:opt_key() | gen_mod:key_path()) -> gen_mod:opt_value().
opt(Module, Key) ->
    gen_mod:get_module_opt(host_type(), Module, Key).

host_type() ->
    persistent_term:get({mod_global_distrib, host_type}).

-spec cast_or_call(Target :: pid() | atom(), Message :: term()) -> any().
cast_or_call(Target, Message) ->
    cast_or_call(Target, Message, 500).

-spec cast_or_call(Target :: pid() | atom(), Message :: term(),
                   SyncWatermark :: non_neg_integer()) ->
                           any().
cast_or_call(Target, Message, SyncWatermark) ->
    cast_or_call(Target, Message, SyncWatermark, 5000).

-spec cast_or_call(Target :: pid() | atom(), Message :: term(),
                   SyncWatermark :: non_neg_integer(), Timeout :: pos_integer() | infinity) ->
                          any().
cast_or_call(Target, Message, SyncWatermark, Timeout) when is_atom(Target), Target =/= undefined ->
    cast_or_call(whereis(Target), Message, SyncWatermark, Timeout);
cast_or_call(Target, Message, SyncWatermark, Timeout) when is_pid(Target) ->
    case process_info(Target, message_queue_len) of
        {_, X} when X > SyncWatermark -> gen_server:call(Target, Message, Timeout);
        {_, _} -> gen_server:cast(Target, Message)
    end.

-spec create_ets(Names :: [atom()] | atom()) -> any().
create_ets(Names) ->
    create_ets(Names, set).

-spec create_ets(Names :: [atom()] | atom(), Type :: atom()) -> any().
create_ets(Names, Type) when is_list(Names) ->
    [create_ets(Name, Type) || Name <- Names];
create_ets(Name, Type) ->
    ejabberd_sup:create_ets_table(Name, [named_table, public, Type, {read_concurrency, true}]).

-spec resolve_endpoints([{inet:ip_address() | string(), inet:port_number()}]) -> [endpoint()].
resolve_endpoints(Endpoints) when is_list(Endpoints) ->
    lists:flatmap(fun resolve_endpoint/1, Endpoints).

resolve_endpoint({Addr, _Port} = E) when is_tuple(Addr) ->
    [E];
resolve_endpoint({Addr, Port}) ->
    case to_ip_tuples(Addr) of
        {ok, IpAddrs} ->
            Resolved = [{IpAddr, Port} || IpAddr <- IpAddrs],
            ?LOG_IF(info, is_domain(Addr), #{what => gd_resolve_endpoint,
                                             text => <<"GD resolved address to IPs">>,
                                             address => Addr, ip_addresses => IpAddrs}),
            Resolved;
        {error, {Reasonv6, Reasonv4}} ->
            ?LOG_ERROR(#{what => gd_resolve_endpoint_failed,
                         text => <<"GD Cannot convert address to IP addresses">>,
                         address => Addr,
                         ipv6_reason => inet:format_error(Reasonv6),
                         ipv4_reason => inet:format_error(Reasonv4)}),
            error({domain_not_resolved, {Reasonv6, Reasonv4}})
    end.


-spec recipient_to_worker_key(jid:jid() | jid:ljid(), jid:lserver()) -> binary().
recipient_to_worker_key(#jid{} = Jid, GlobalHost) ->
    recipient_to_worker_key(jid:to_lower(Jid), GlobalHost);
recipient_to_worker_key({_, GlobalHost, _} = Jid, GlobalHost) ->
    jid:to_binary(Jid);
recipient_to_worker_key({_, Domain, _}, _GlobalHost) ->
    Domain.

-spec server_to_mgr_name(Server :: jid:lserver()) -> atom().
server_to_mgr_name(Server) ->
    gen_mod:get_module_proc(Server, mod_global_distrib_server_mgr).

-spec server_to_sup_name(Server :: jid:lserver()) -> atom().
server_to_sup_name(Server) ->
    gen_mod:get_module_proc(Server, mod_global_distrib_server_sup).

%% IMPORTANT! Regarding mod_global_distrib_mapping:insert_for_*/2:
%% These functions with arity 2 will call cache update functions with
%% dummy update functions, so they will result only in cache update with
%% no backend side effects.
-spec maybe_update_mapping(From :: jid:jid(), mongoose_acc:t()) -> any().
maybe_update_mapping(_From, #{name := <<"presence">>, type := <<"unavailable">>}) ->
    ok;
maybe_update_mapping(#jid{luser = <<>>, lserver = LServer} = From, Acc) ->
    case opt(mod_global_distrib, global_host) of
        LServer -> ok;
        _ ->
            ensure_domain_inserted(Acc, From#jid.lserver)
    end;
maybe_update_mapping(From, Acc) ->
    case mod_global_distrib_mapping:for_jid(From) of
        error ->
            case mod_global_distrib:find_metadata(Acc, origin) of
                %% Lack of 'global_distrib' indicates 100% local routing...
                {error, undefined} ->
                    %% .. so we can insert From into cache with local host as mapping
                    mod_global_distrib_mapping:cache_jid(From, local_host());
                {ok, Origin} ->
                    mod_global_distrib_mapping:cache_jid(From, Origin)
            end;
        _ ->
            ok
    end.

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

-spec ensure_domain_inserted(mongoose_acc:t(), jid:lserver()) -> ok.
ensure_domain_inserted(Acc, Domain) ->
    case mod_global_distrib_mapping:for_domain(Domain) of
        error ->
            %% See the comments in the last match of maybe_update_mapping/2 function
            case mod_global_distrib:find_metadata(Acc, origin) of
                {error, undefined} ->
                    mod_global_distrib_mapping:cache_domain(Domain, local_host());
                {ok, Origin} ->
                    mod_global_distrib_mapping:cache_domain(Domain, Origin)
            end;
        _ ->
            ok
    end.

%% @doc Check that the host is hosted by the server
-spec check_host(jid:lserver()) -> ok.
check_host(Domain) ->
    %% There is no clause for a dynamic domain as this module can't be started with dynamic domains
    case mongoose_domain_api:get_domain_host_type(Domain) of
        {error, not_found} ->
            error(#{what => check_host_failed,
                    msg => <<"Domain is not hosted by the server">>,
                    domain => Domain});
        {ok, Domain} ->
            ok
    end.

get_addrs_in_parallel(Addr) ->
    %% getaddrs could be pretty slow, so do in parallel
    %% Also, limit the time it could be running to 5 seconds
    %% (would fail with reason timeout if it takes too long)
    F = fun(Ver) -> ?MODULE:getaddrs(Addr, Ver) end,
    [V6, V4] = mongoose_lib:pmap(F, [inet6, inet]),
    {simplify_result(V6), simplify_result(V4)}.

simplify_result({ok, Res}) -> Res;
simplify_result(Res) -> Res.

-spec to_ip_tuples(Addr :: inet:ip_address() | string()) ->
                         {ok, [inet:ip_address()]} | {error, {V6 :: atom(), V4 :: atom()}}.
to_ip_tuples(Addr) ->
    case get_addrs_in_parallel(Addr) of
        {{error, Reason6}, {error, Reason4}} ->
            {error, {Reason6, Reason4}};
        {Addrs, {error, Msg}} ->
            ?LOG_DEBUG(#{what => resolv_error, address => Addr, reason => Msg,
                         text => <<"IPv4 address resolution error">>}),
            Addrs;
        {{error, Msg}, Addrs} ->
            ?LOG_DEBUG(#{what => resolv_error, address => Addr, reason => Msg,
                         text => <<"IPv6 address resolution error">>}),
            Addrs;
        {{ok, Addrs6}, {ok, Addrs4}} ->
            {ok, Addrs6 ++ Addrs4}
    end.

-spec parse_address(binary() | string() | inet:ip_address()) ->
    {ip, inet:ip_address()} | {domain, domain_name()}.
parse_address(DomainOrIp) when is_binary(DomainOrIp) ->
    parse_address(binary_to_list(DomainOrIp));
parse_address(Ip) when is_tuple(Ip) ->
    {ip, Ip};
parse_address(DomainOrIp) ->
    case inet:parse_address(DomainOrIp) of
        {error, einval} ->
            {domain, DomainOrIp};
        {ok, Ip} ->
            {ip, Ip}
    end.

is_domain(DomainOrIp) ->
    case parse_address(DomainOrIp) of
        {domain, _} ->
            true;
        _ ->
            false
    end.

local_host() ->
    opt(mod_global_distrib, local_host).

getaddrs(Addr, Type) ->
    case inet:getaddrs(Addr, Type) of
        {ok, Addrs} -> {ok, lists:usort(Addrs)};
        {error, Reason} -> {error, Reason}
    end.
