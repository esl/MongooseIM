%%%-------------------------------------------------------------------
%%% @author Ludwik Bukowski <ludwik.bukowski@erlang-solutions.com>
%%% @copyright (C) 2015, Ludwik Bukowski
%%% @doc XEP-0202: Entity Time
%%% @end
%%%-------------------------------------------------------------------
-module(mod_time).
-author('ludwik.bukowski@erlang-solutions.com').

-behaviour(gen_mod).
-behaviour(mongoose_module_metrics).

-export([start/2]).
-export([stop/1]).
-export([supported_features/0]).
-export([config_spec/0]).
-export([process_local_iq/5]).

-ignore_xref([process_local_iq/5]).

-include("jlib.hrl").
-include("mongoose_config_spec.hrl").

-xep([{xep, 202}, {version, "2.0"}]).
-xep([{xep, 82}, {version, "1.1.1"}]).

-spec start(HostType :: mongooseim:host_type(), gen_mod:module_opts()) -> ok | {error, atom()}.
start(HostType, #{iqdisc := IQDisc}) ->
    gen_iq_handler:add_iq_handler_for_domain(HostType, ?NS_TIME, ejabberd_local,
                                             fun ?MODULE:process_local_iq/5, #{}, IQDisc).

-spec stop(HostType :: mongooseim:host_type()) -> ok | {error, not_registered}.
stop(HostType) ->
    gen_iq_handler:remove_iq_handler_for_domain(HostType, ?NS_TIME, ejabberd_local).

-spec supported_features() -> [atom()].
supported_features() ->
    [dynamic_domains].

-spec config_spec() -> mongoose_config_spec:config_section().
config_spec() ->
    #section{
       items = #{<<"iqdisc">> => mongoose_config_spec:iqdisc()},
       defaults = #{<<"iqdisc">> => one_queue}
    }.

process_local_iq(Acc, _From, _To, #iq{type = set, sub_el = SubEl} = IQ, _Extra) ->
    {Acc, IQ#iq{type = error, sub_el = [SubEl, mongoose_xmpp_errors:not_allowed()]}};

process_local_iq(Acc, _From, _To, #iq{type = get} = IQ, _Extra) ->
    {UTC, TZODiff} = calculate_time(),
    R = IQ#iq{type = result,
          sub_el =
          [#xmlel{name = <<"time">>,
                  attrs = [{<<"xmlns">>, ?NS_TIME}],
                  children =
                  [#xmlel{name = <<"tzo">>, attrs = [],
                          children = [#xmlcdata{content = list_to_binary(TZODiff)}]},
                   #xmlel{name = <<"utc">>, attrs = [],
                          children =
                          [#xmlcdata{content = list_to_binary(UTC)}]}]}]},
    {Acc, R}.

%% Internals
calculate_time() ->
    SystemTime = erlang:system_time(second),
    UTCString = calendar:system_time_to_rfc3339(SystemTime, [{offset, "Z"}]),
    LocalString = calendar:system_time_to_rfc3339(SystemTime),
    DateTimeLength = length(UTCString) - 1,
    {UTCString, string:slice(LocalString, DateTimeLength)}.
