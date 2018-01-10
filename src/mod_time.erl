%%%-------------------------------------------------------------------
%%% @author Ludwik Bukowski <ludwik.bukowski@erlang-solutions.com>
%%% @copyright (C) 2015, Ludwik Bukowski
%%% @doc XEP-0202: Entity Time
%%% @end
%%%-------------------------------------------------------------------
-module(mod_time).
-author('ludwik.bukowski@erlang-solutions.com').
-behaviour(gen_mod).
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
                          children = [#xmlcdata{content = iolist_to_binary(TZODiff)}]},
                   #xmlel{name = <<"utc">>, attrs = [],
                          children =
                          [#xmlcdata{content = UTC}]}]}]},
    {Acc, R}.

%% Internals
calculate_time() ->
    Now = p1_time_compat:timestamp(),
    NowUniversal = calendar:now_to_universal_time(Now),
    NowLocal = calendar:now_to_local_time(Now),
    {UTCTime, UTCDiff} = jlib:timestamp_to_iso(NowUniversal, utc),
    UTC = list_to_binary(UTCTime ++ UTCDiff),
    SecondsDiff = difference_in_secs(NowLocal, NowUniversal),
    {Hd, Md, _} = calendar:seconds_to_time(abs(SecondsDiff)),
    {_, TZODiff} = jlib:timestamp_to_iso({{2000, 1, 1},
                                          {0, 0, 0}},
                                         {sign(SecondsDiff), {Hd, Md}}),
    {UTC, TZODiff}.

difference_in_secs(LocalTime, UniversalTime) ->
    LocalSeconds = calendar:datetime_to_gregorian_seconds(LocalTime),
    UniversalSeconds = calendar:datetime_to_gregorian_seconds(UniversalTime),
    LocalSeconds - UniversalSeconds.


sign(N) when N < 0 -> <<"-">>;
sign(_) -> <<"+">>.
