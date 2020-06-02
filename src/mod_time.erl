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
-export([start/2, stop/1, process_local_iq/4]).
-include("mongoose.hrl").
-include("jlib.hrl").
-xep([{xep, 202}, {version, "2.0"}]).
-xep([{xep, 82}, {version, "1.1"}]).
start(Host, Opts) ->
    mod_disco:register_feature(Host, ?NS_TIME),
    IQDisc = gen_mod:get_opt(iqdisc, Opts,
                             one_queue),
    gen_iq_handler:add_iq_handler(ejabberd_local, Host,
                                  ?NS_TIME, ?MODULE, process_local_iq, IQDisc).


stop(Host) ->
    mod_disco:unregister_feature(Host, ?NS_TIME),
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host,
                                     ?NS_TIME).


process_local_iq(_From, _To, Acc, #iq{type = set, sub_el = SubEl} = IQ) ->
    {Acc, IQ#iq{type = error, sub_el = [SubEl, mongoose_xmpp_errors:not_allowed()]}};

process_local_iq(_From, _To, Acc, #iq{type = get} = IQ) ->
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
