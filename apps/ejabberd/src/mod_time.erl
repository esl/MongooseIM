%%%-------------------------------------------------------------------
%%% @author Ludwik Bukowski <ludwik.bukowski@erlang-solutions.com>
%%% @copyright (C) 2015, Ludwik Bukowski
%%% @doc XEP-0202: Entity Time
%%% @end
%%%-------------------------------------------------------------------
-module(mod_time).
-author('ludwik.bukowski@erlang-solutions.com').
-behaviour(gen_mod).
-export([start/2, stop/1, process_local_iq/3]).
-include("ejabberd.hrl").
-include("jlib.hrl").
-xep([{xep, 202}, {version,"2.0"}]).
-xep([{xep, 82}, {version,"1.1"}]).
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


process_local_iq(_From, _To, #iq{type = set, sub_el = SubEl} = IQ) ->
    IQ#iq{type = error, sub_el = [SubEl, ?ERR_NOT_ALLOWED]};

process_local_iq(_From, _To, #iq{type = get} = IQ) ->
    {UTC, TZO_diff} = calculate_time(),
    IQ#iq{type = result,
          sub_el =
          [#xmlel{name = <<"time">>,
                  attrs = [{<<"xmlns">>, ?NS_TIME}],
                  children =
                  [#xmlel{name = <<"tzo">>, attrs = [],
                          children = [#xmlcdata{content = iolist_to_binary(TZO_diff)}]},
                   #xmlel{name = <<"utc">>, attrs = [],
                          children =
                          [#xmlcdata{content = UTC}]}]}]}.

%% Internals
calculate_time() ->
    Now = now(),
    Now_universal = calendar:now_to_universal_time(Now),
    Now_local = calendar:now_to_local_time(Now),
    {UTC_time, UTC_diff} = jlib:timestamp_to_iso(Now_universal, utc),
    UTC = list_to_binary(UTC_time ++ UTC_diff),
    Seconds_diff = difference_in_secs(Now_local, Now_universal),
    {Hd, Md, _} = calendar:seconds_to_time(abs(Seconds_diff)),
    {_, TZO_diff} = jlib:timestamp_to_iso({{2000, 1, 1},
                                           {0, 0, 0}},
                                          {sign(Seconds_diff), {Hd, Md}}),
    {UTC, TZO_diff}.

difference_in_secs(LocalTime, UniversalTime) ->
    LocalSeconds = calendar:datetime_to_gregorian_seconds(LocalTime),
    UniversalSeconds = calendar:datetime_to_gregorian_seconds(UniversalTime),
    LocalSeconds - UniversalSeconds.


sign(N) when N < 0 -> <<"-">>;
sign(_) -> <<"+">>.
