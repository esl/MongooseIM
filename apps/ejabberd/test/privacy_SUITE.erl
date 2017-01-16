%% @doc This suite tests both old ejabberd_commands module, which is slowly getting deprecated,
%% and the new mongoose_commands implementation.
-module(privacy_SUITE).
-compile([export_all]).

-include_lib("exml/include/exml.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("ejabberd/include/ejabberd_commands.hrl").
-include_lib("ejabberd/include/jlib.hrl").

-define(PRT(X, Y), ct:pal("~p: ~p", [X, Y])).

dump(Stanza) ->
    Keys = lists:sort(maps:keys(Stanza)),
    ct:pal("------", []),
    lists:map(fun(K) -> ct:pal("~p = ~p", [K, maps:get(K, Stanza)]) end, Keys).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% suite configuration
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

all() ->
    [
     {group, privacy_check}
    ].

groups() ->
    [
     {privacy_check, [sequence],
      [check_repeatable ]
     }
    ].


init_per_suite(C) ->
    catch application:ensure_all_started(stringprep),
    mnesia:create_schema([node()]),
    mnesia:start(),
    C.

init_per_testcase(T, Config) ->
    application:ensure_all_started(exometer),
    {ok, _HooksServer} = ejabberd_hooks:start_link(),
    ets:new(local_config, [named_table]),
    ejabberd_hooks:add(privacy_check_packet, <<"localhost">>,
        ?MODULE, check_packet, 50),
    Config.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% test methods
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


check_repeatable(_C) ->
    To1 = {jid,<<"bob">>,<<"localhost">>,<<>>,<<"bob">>,<<"localhost">>,<<>>},
    To2 = {jid,<<"geralt">>,<<"localhost">>,<<>>,<<"geralt">>,<<"localhost">>,<<>>},
    From = {jid,<<"alice">>,<<"localhost">>,<<>>,<<"alice">>,<<"localhost">>,<<>>},
    Res = check(From, To1),
    ?assertEqual(allow, mongoose_stanza:get(privacy_check, Res)),
    Res_o = check(From, To2),
    ?assertEqual(deny, mongoose_stanza:get(privacy_check, Res_o)),
    Res_o2 = check(Res, From, To2),
    ?assertEqual(deny, mongoose_stanza:get(privacy_check, Res_o2)),
    Res_o3 = check(Res_o2, From, To2),
    ?assertEqual(deny, mongoose_stanza:get(privacy_check, Res_o3)),
    Res_o4 = check(Res_o3, From, To1),
    ?assertEqual(allow, mongoose_stanza:get(privacy_check, Res_o4)),
    ok.

check(From, To) ->
    Packet = {xmlel,<<"presence">>,[{<<"type">>,<<"unsubscribed">>}],[]},
    Stanza = mongoose_stanza:from_map(#{element=>Packet, from_jid=>From, to_jid=>To,
        type=><<"presence">>}),
    check(Stanza, From, To).

check(Stanza, _From, To) ->
    List = [{listitem,jid,{<<"geralt">>,<<"localhost">>,<<>>},deny,1,true,false,false,false,false}],
    NeedDb = false,
    User = <<"alice">>,
    Server = <<"localhost">>,
    PList = {List, NeedDb},
    mongoose_privacy:privacy_check_packet(
        User,
        Server,
        PList,
        Stanza,
        To,
        out
    ).

check_packet(Acc, _, _, _, {_From, To, _}, _) ->
    Res = check_packet(To#jid.luser),
    mongoose_stanza:put(privacy_check, Res, Acc).

check_packet(<<"bob">>) ->
    allow;
check_packet(<<"geralt">>) ->
    deny.
