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

-include("ejabberd.hrl").
-include("jlib.hrl").

-export([
         start/4, deps/4, stop/3, opt/2, cast_or_call/2, cast_or_call/3, cast_or_call/4,
         create_ets/1, create_ets/2, any_binary_to_atom/1, resolve_endpoints/1,
         binary_to_metric_atom/1, ensure_metric/2, recipient_to_worker_key/2,
         maybe_update_mapping/2
        ]).

-type endpoint() :: {inet:ip_address(), inet:port_number()}.

-export_type([endpoint/0]).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------

-spec binary_to_metric_atom(binary()) -> atom().
binary_to_metric_atom(Binary) ->
    List = lists:filtermap(fun
                               ($.) -> {true, $_};
                               (C) when C > 31, C < 127 -> {true, C};
                               (_) -> false
                           end,
                           unicode:characters_to_list(Binary)),
    list_to_atom(List).

ensure_metric(Metric, Type) ->
    case mongoose_metrics:ensure_metric(global, Metric, Type) of
        ok ->
            Reporters = exometer_report:list_reporters(),
            Interval = mongoose_metrics:get_report_interval(),
            lists:foreach(
              fun(Reporter) ->
                      mongoose_metrics:subscribe_metric(Reporter, {[global | Metric], Type, []},
                                                        Interval)
              end,
              Reporters);
        Other ->
            Other
    end.

-spec any_binary_to_atom(binary()) -> atom().
any_binary_to_atom(Binary) ->
    binary_to_atom(base64:encode(Binary), latin1).

-spec start(module(), Host :: ejabberd:lserver(), Opts :: proplists:proplist(),
            StartFun :: fun(() -> any())) -> any().
start(Module, Host, Opts, StartFun) ->
    check_host(global_host, Opts),
    check_host(local_host, Opts),

    {global_host, GlobalHostList} = lists:keyfind(global_host, 1, Opts),
    case unicode:characters_to_binary(GlobalHostList) of
        Host ->
            create_ets(Module),
            populate_opts_ets(Module, Opts),
            StartFun();
        _ ->
            ok
    end.

-spec stop(module(), Host :: ejabberd:lserver(), StopFun :: fun(() -> any())) ->
                  any().
stop(Module, Host, StopFun) ->
    case catch opt(Module, global_host) of
        Host ->
            StopFun(),
            ets:delete(Module);
        _ ->
            ok
    end.

-spec deps(module(), Host :: ejabberd:lserver(), Opts :: proplists:proplist(),
           DepsFun :: fun((proplists:proplist()) -> gen_mod:deps_list())) ->
                           gen_mod:deps_list().
deps(_Module, Host, Opts, DepsFun) ->
    {global_host, GlobalHostList} = lists:keyfind(global_host, 1, Opts),
    case unicode:characters_to_binary(GlobalHostList) of
        Host -> DepsFun(Opts);
        _ -> []
    end.

-spec opt(module(), Key :: atom()) -> Value :: term().
opt(Module, Key) ->
    try ets:lookup_element(Module, Key, 2)
    catch
        error:badarg ->
            error(atom_to_list(Module) ++ " required option unset: " ++ atom_to_list(Key))
    end.

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
    Self = self(),
    Heir = case whereis(ejabberd_sup) of
               undefined -> none;
               Self -> none;
               Pid -> Pid
           end,

    ets:new(Name, [named_table, public, Type, {read_concurrency, true}, {heir, Heir, testing}]).

-spec resolve_endpoints([{inet:ip_address() | string(), inet:port_number()}]) ->
                               [endpoint()].
resolve_endpoints(Endpoints) ->
    lists:map(
      fun({Addr, Port}) ->
              case to_ip_tuple(Addr) of
                  {ok, IpAddr} ->
                      {IpAddr, Port};
                  {error, {Reasonv6, Reasonv4}} ->
                      ?ERROR_MSG("Cannot convert ~p to IP address: IPv6: ~s. IPv4: ~s.",
                                 [Addr, inet:format_error(Reasonv6), inet:format_error(Reasonv4)]),
                      error({Reasonv6, Reasonv4})
              end
      end,
      Endpoints).

-spec recipient_to_worker_key(jid() | ljid(), ejabberd:lserver()) -> binary().
recipient_to_worker_key(#jid{} = Jid, GlobalHost) ->
    recipient_to_worker_key(jid:to_lower(Jid), GlobalHost);
recipient_to_worker_key({_, GlobalHost, _} = Jid, GlobalHost) ->
    jid:to_binary(Jid);
recipient_to_worker_key({_, Domain, _}, _GlobalHost) ->
    Domain.

-spec maybe_update_mapping(From :: jid(), mongoose_acc:t()) -> any().
maybe_update_mapping(_From, #{name := <<"presence">>, type := <<"unavailable">>}) ->
    ok;
maybe_update_mapping(From, Acc) ->
    Origin = mod_global_distrib:get_metadata(Acc, origin),
    case mod_global_distrib_mapping:for_jid(From) of
        error -> mod_global_distrib_mapping:insert_for_jid(From, Origin);
        _ -> ok
    end.

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

-spec check_host(local_host | global_host, Opts :: proplists:proplist()) -> true.
check_host(Key, Opts) ->
    {Key, HostList} = lists:keyfind(Key, 1, Opts),
    Host = unicode:characters_to_binary(HostList),
    lists:member(Host, ?MYHOSTS) orelse error(HostList ++ " is not a member of the host list").

-spec populate_opts_ets(module(), Opts :: proplists:proplist()) -> any().
populate_opts_ets(Module, Opts) ->
    [ets:insert(Module, {Key, translate_opt(Value)}) || {Key, Value} <- Opts].

-spec translate_opt(term()) -> term().
translate_opt([Elem | _] = Opt) when is_list(Elem) ->
    [translate_opt(E) || E <- Opt];
translate_opt(Opt) when is_list(Opt) ->
    case catch unicode:characters_to_binary(Opt) of
        Bin when is_binary(Bin) -> Bin;
        _ -> Opt
    end;
translate_opt(Opt) ->
    Opt.

-spec to_ip_tuple(Addr :: inet:ip_address() | string()) ->
                         {ok, inet:ip_address()} | {error, {V6 :: atom(), V4 :: atom()}}.
to_ip_tuple(Addr) ->
    case inet:getaddr(Addr, inet6) of
        {ok, Av6} -> {ok, Av6};
        {error, Reasonv6} ->
            case inet:getaddr(Addr, inet) of
                {ok, Av4} -> {ok, Av4};
                {error, Reasonv4} -> {error, {Reasonv6, Reasonv4}}
            end
    end.

