-module(mongoose_admin_api_metrics).

-behaviour(mongoose_admin_api).
-export([routes/1]).

-behaviour(cowboy_rest).
-export([init/2,
         is_authorized/2,
         content_types_provided/2,
         allowed_methods/2,
         to_json/2]).

-ignore_xref([to_json/2, from_json/2]).

-import(mongoose_admin_api, [parse_body/1, try_handle_request/3, throw_error/2, resource_created/4]).

-type req() :: cowboy_req:req().
-type state() :: mongoose_admin_api:state().

-include("mongoose.hrl").

-spec routes(state()) -> mongoose_http_handler:routes().
routes(State) ->
    [{"/metrics/", ?MODULE, State},
     {"/metrics/all/[:metric]", ?MODULE, State#{suffix => all}},
     {"/metrics/global/[:metric]", ?MODULE, State#{suffix => global}},
     {"/metrics/host_type/:host_type/[:metric]", ?MODULE, State}].

-spec init(req(), state()) -> {cowboy_rest, req(), state()}.
init(Req, State) ->
    mongoose_admin_api:init(Req, State).

-spec is_authorized(req(), state()) -> {true | {false, iodata()}, req(), state()}.
is_authorized(Req, State) ->
    mongoose_admin_api:is_authorized(Req, State).

-spec content_types_provided(req(), state()) ->
          {[{{binary(), binary(), '*'}, atom()}], req(), state()}.
content_types_provided(Req, State) ->
    {[
      {{<<"application">>, <<"json">>, '*'}, to_json}
     ], Req, State}.

-spec allowed_methods(req(), state()) -> {[binary()], req(), state()}.
allowed_methods(Req, State) ->
    {[<<"OPTIONS">>, <<"HEAD">>, <<"GET">>], Req, State}.

%% @doc Called for a method of type "GET"
-spec to_json(req(), state()) -> {iodata() | stop, req(), state()}.
to_json(Req, State) ->
    try_handle_request(Req, State, fun handle_get/2).

%% Internal functions

handle_get(Req, State = #{suffix := all}) ->
    Bindings = cowboy_req:bindings(Req),
    case get_metric_name(Bindings) of
        {metric, Metric} ->
            case mongoose_metrics:get_aggregated_values(Metric) of
                [] ->
                    throw_error(not_found, <<"Metric not found">>);
                Value ->
                    {jiffy:encode(#{metric => prepare_value(Value)}), Req, State}
            end;
        all_metrics ->
            Values = get_sum_metrics(),
            {jiffy:encode(#{metrics => Values}), Req, State}
    end;
handle_get(Req, State = #{suffix := global}) ->
    Bindings = cowboy_req:bindings(Req),
    handle_get_values(Req, State, Bindings, global);
handle_get(Req, State) ->
    Bindings = cowboy_req:bindings(Req),
    case Bindings of
        #{host_type := HostType} ->
            handle_get_values(Req, State, Bindings, HostType);
        #{} ->
            {HostTypes, Metrics} = get_available_host_type_metrics(),
            Global = get_available_global_metrics(),
            Reply = #{host_types => HostTypes, metrics => Metrics, global => Global},
            {jiffy:encode(Reply), Req, State}
    end.

handle_get_values(Req, State, Bindings, HostType) ->
    case get_metric_name(Bindings) of
        {metric, Metric} ->
            case mongoose_metrics:get_metric_value(HostType, Metric) of
                {ok, Value} ->
                    {jiffy:encode(#{metric => prepare_value(Value)}), Req, State};
                _Other ->
                    throw_error(not_found, <<"Metric not found">>)
            end;
        all_metrics ->
            case mongoose_metrics:get_metric_values(HostType) of
                [] ->
                    throw_error(not_found, <<"No metrics found">>);
                Metrics ->
                    Values = prepare_metrics(Metrics),
                    {jiffy:encode(#{metrics => Values}), Req, State}
            end
    end.

-spec get_sum_metrics() -> map().
get_sum_metrics() ->
    {_HostTypes, Metrics} = get_available_host_type_metrics(),
    maps:from_list([{Metric, get_sum_metric(Metric)} || Metric <- Metrics]).

-spec get_sum_metric(atom()) -> map().
get_sum_metric(Metric) ->
    maps:from_list(mongoose_metrics:get_aggregated_values(Metric)).

-spec get_available_metrics(HostType :: mongooseim:host_type()) -> [any()].
get_available_metrics(HostType) ->
    mongoose_metrics:get_host_type_metric_names(HostType).

-spec get_available_host_type_metrics() -> {[any(), ...], [any()]}.
get_available_host_type_metrics() ->
    HostTypes = get_available_host_types(),
    Metrics = [Metric || [Metric] <- get_available_metrics(hd(HostTypes))],
    {HostTypes, Metrics}.

get_available_global_metrics() ->
    [Metric || [Metric] <- mongoose_metrics:get_global_metric_names()].

-spec get_available_host_types() -> [mongooseim:host_type()].
get_available_host_types() ->
    ?ALL_HOST_TYPES.

prepare_metrics(Metrics) ->
    maps:from_list([{prepare_name(NameParts, FullName), prepare_value(Value)}
                    || {FullName = [_HostType | NameParts], Value} <- Metrics]).

prepare_name(NameParts, FullName) ->
    try
        ToStrings = [atom_to_list(NamePart) || NamePart <- NameParts],
        list_to_binary(string:join(ToStrings, "."))
    catch Class:Reason:Stacktrace ->
        erlang:raise(Class, {failed_to_prepare_name, FullName, Reason}, Stacktrace)
    end.

prepare_value(KVs) ->
    maps:from_list([{prepare_key(K), V} || {K, V} <- KVs]).

prepare_key(K) when is_integer(K) -> integer_to_binary(K);
prepare_key(K) when is_atom(K) -> atom_to_binary(K).

get_metric_name(#{metric := MetricBin}) ->
    try {metric, binary_to_existing_atom(MetricBin)}
    catch _:_ -> throw_error(not_found, <<"Metric not found">>)
    end;
get_metric_name(#{}) ->
    all_metrics.
