%%==============================================================================
%% Copyright 2014 Erlang Solutions Ltd.
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
-module(mongoose_api_metrics).

%% mongoose_api callbacks
-export([prefix/0,
         routes/0,
         handle_options/2,
         handle_get/2]).

%% internal exports
-export([available_metrics/1,
         sum_metrics/1,
         sum_metric/1,
         host_metric/1,
         host_metrics/1]).

%%--------------------------------------------------------------------
%% mongoose_api callbacks
%%--------------------------------------------------------------------
-spec prefix() -> mongoose_api:prefix().
prefix() ->
    "/metrics".

-spec routes() -> mongoose_api:routes().
routes() ->
    [{"/", [available_metrics]},
     {"/all", [sum_metrics]},
     {"/all/:metric", [sum_metric]},
     {"/host/:host/:metric", [host_metric]},
     {"/host/:host", [host_metrics]}].

-spec handle_options(mongoose_api:bindings(), mongoose_api:options()) ->
    mongoose_api:methods().
handle_options(_Bindings, [_Command]) ->
    [get].

-spec handle_get(mongoose_api:bindings(), mongoose_api:options()) ->
    mongoose_api:response().
handle_get(Bindings, [Command]) ->
    ?MODULE:Command(Bindings).

%%--------------------------------------------------------------------
%% mongoose_api commands actual handlers
%%--------------------------------------------------------------------
available_metrics(_Bindings) ->
    {Hosts, Metrics} = get_available_hosts_metrics(),
    Reply = [{hosts, Hosts}, {metrics, Metrics}],
    {ok, Reply}.

sum_metrics(_Bindings) ->
    Metrics = {metrics, get_sum_metrics()},
    {ok, Metrics}.

sum_metric(Bindings) ->
    {metric, Metric} = lists:keyfind(metric, 1, Bindings),
    try
        case get_sum_metric(binary_to_existing_atom(Metric, utf8)) of
            {error, _, _} ->
                {error, not_found};
            Value ->
                {ok, {metric, Value}}
        end
    catch error:badarg ->
        {error, not_found}
    end.

host_metric(Bindings) ->
    {host, Host} = lists:keyfind(host, 1, Bindings),
    {metric, Metric} = lists:keyfind(metric, 1, Bindings),
    try
        MetricAtom = binary_to_existing_atom(Metric, utf8),
        Value = folsom_metrics:get_metric_value({Host, MetricAtom}),
        {ok, {metric, Value}}
    catch error:badarg ->
        {error, not_found}
    end.

host_metrics(Bindings) ->
    {host, Host} = lists:keyfind(host, 1, Bindings),
    case get_host_metrics(Host) of
        [] ->
            {error, not_found};
        Metrics ->
            {ok, {metrics, Metrics}}
    end.

%%--------------------------------------------------------------------
%% internal functions
%%--------------------------------------------------------------------
-spec get_available_hosts() -> [ejabberd:server()].
get_available_hosts() ->
    HostsSet = lists:foldl(fun({Host, _Metric}, Hosts) ->
                    ordsets:add_element(Host, Hosts)
            end, ordsets:new(), folsom_metrics:get_metrics()),
    ordsets:to_list(HostsSet).

-spec get_available_metrics(Host :: ejabberd:server()) -> [any()].
get_available_metrics(Host) ->
    Metrics = [Metric || {CurrentHost, Metric} <- folsom_metrics:get_metrics(),
                         CurrentHost =:= Host],
    lists:reverse(Metrics).

-spec get_available_hosts_metrics() -> {[any(),...], [any()]}.
get_available_hosts_metrics() ->
    Hosts = get_available_hosts(),
    Metrics = get_available_metrics(hd(Hosts)),
    {Hosts, Metrics}.

-spec get_sum_metrics() -> [{_,_}].
get_sum_metrics() ->
    {Hosts, Metrics} = get_available_hosts_metrics(),
    Sum = lists:foldl(fun({_Host, Metric}=Name, Dict) ->
                    Value = folsom_metrics:get_metric_value(Name),
                    case orddict:is_key(Metric, Dict) of
                        false ->
                            orddict:store(Metric, Value, Dict);
                        true ->
                            OldValue = orddict:fetch(Metric, Dict),
                            NewMetric = update_sum_metric(OldValue, Value),
                            orddict:store(Metric, NewMetric, Dict)
                    end
            end, orddict:new(), [{H, M} || H <- Hosts, M <- Metrics]),
    orddict:to_list(Sum).

-spec get_sum_metric(atom()) -> any().
get_sum_metric(Metric) ->
    {Hosts, _Metrics} = get_available_hosts_metrics(),
    lists:foldl(fun(Host, Acc) ->
                Value = folsom_metrics:get_metric_value({Host, Metric}),
                update_sum_metric(Acc, Value)
        end, nil, Hosts).

update_sum_metric(nil, Value) ->
    Value;
update_sum_metric(OldValue, Value) when is_integer(Value) ->
    OldValue+Value;
update_sum_metric([{count, OldCount},{one, OldOne}],
                  [{count, Count},{one, One}]) ->
    [{count, OldCount+Count}, {one, OldOne+One}];
update_sum_metric(OldValue, _Value) ->
    OldValue.

-spec get_host_metrics('undefined' | ejabberd:server()) -> [{_,_}].
get_host_metrics(Host) ->
    Metrics = folsom_metrics:get_metrics_value(Host),
    [{Name, Value} || {{_Host, Name}, Value} <- Metrics].
