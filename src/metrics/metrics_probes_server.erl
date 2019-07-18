%%==============================================================================
%% Copyright 2019 Erlang Solutions Ltd.
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
-module(metrics_probes_server).
-behaviour(exometer_probe).

-include("mongoose_logger.hrl").

%% exometer_entry callbacks
%% exometer_probe callbacks
-export([
         behaviour/0,
         probe_init/3,
         probe_terminate/1,
         probe_get_value/2,
         probe_get_datapoints/1,
         probe_update/2,
         probe_reset/1,
         probe_sample/1,
         probe_setopts/3,
         probe_handle_msg/2,
         probe_code_change/3
        ]).

-type datapoint() :: atom().
-record(state, {
    datapoints = [] :: [datapoint()],
    data = #{} :: #{datapoint() => integer()},
    sampling :: function(),
    ref :: reference() | undefined
}).

-spec behaviour() -> exometer:behaviour().
behaviour() ->
    probe.

probe_init(_Name, _Type, Opts) ->
    FunSampling = case proplists:get_value(sampling, Opts) of
                      F when is_function(F) -> F;
                      F -> error({invalid_sampling_fun_given, F})
                  end,
    DataPoints = case proplists:get_value(datapoints, Opts) of
                      D when is_list(D) -> D;
                      D -> error({invalid_datapoints_given, D})
                  end,
    {ok, #state{datapoints = DataPoints,
                sampling = FunSampling,
                data = #{}}}.

probe_terminate(_) -> ok.

probe_get_value(DPs, #state{data = Data} = S) ->
    {ok, probe_get_value_(Data, DPs), S}.

probe_get_value_(Data, DPs) ->
    maps:to_list(maps:with(DPs, Data)).

probe_get_datapoints(#state{datapoints = DPs}) ->
    {ok, DPs}.

probe_update(_, _) ->
    {error, not_supported}.

probe_reset(S) ->
    {ok, S#state{data = #{}}}.


probe_sample(#state{sampling = Fun, ref = undefined} = S) ->
    {_Pid, Ref} =
        spawn_monitor(
            fun() ->
                exit({sample, Fun()})
            end),
    {ok, S#state{ref = Ref}};
probe_sample(#state{} = S) ->
    {ok, S}.

probe_setopts(_Entry, Opts, S) ->
    DPs = proplists:get_value(datapoints, Opts, S#state.datapoints),
    {ok, S#state{datapoints = DPs}}.

probe_handle_msg({'DOWN', Ref, _, _, {sample, Data}}, #state{ref = Ref} = S) ->
    {ok, S#state{ref = undefined, data = Data}};
probe_handle_msg({'DOWN', Ref, _, _, Reason}, #state{sampling = Fun, ref = Ref} = S) ->
    ?WARNING_MSG("Probe sampling ~p died with reason ~p: ", [Fun, Reason]),
    {ok, S#state{ref = undefined}};

probe_handle_msg(_, S) ->
    {ok, S}.

probe_code_change(_, S, _) -> {ok, S}.
