-module(rest_helper).
-author("bartekgorny").

%% API
-export([
    assert_inlist/2,
    assert_notinlist/2,
    decode_maplist/1,
    gett/1,
    post/2,
    putt/2,
    delete/1,
    gett/2,
    post/3,
    putt/3,
    delete/2,
    maybe_enable_mam/3,
    maybe_disable_mam/2,
    maybe_skip_mam_test_cases/3,
    fill_archive/2,
    fill_room_archive/2,
    make_timestamp/2
]).

-define(PATHPREFIX, <<"/api">>).


%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

assert_inlist(Pattern, L) when is_map(Pattern) ->
    assert_inmaplist(maps:keys(Pattern), Pattern, L, L);
assert_inlist(Pattern, L) ->
    [H|_] = L,
    Fl = lists:filter(fun(X) -> case X of Pattern -> true; _ -> false end end, L),
    case Fl of
        [] ->
            ct:fail(io_lib:format("Fail: ~p not in [~p...]", [Pattern, H]));
        _ ->
            ok
    end.

assert_notinlist(Pattern, L) when is_map(Pattern) ->
    assert_notinmaplist(maps:keys(Pattern), Pattern, L, L);
assert_notinlist(Pattern, L) ->
    [H|_] = L,
    Fl = lists:filter(fun(X) -> case X of Pattern -> true; _ -> false end end, L),
    case Fl of
        [] ->
            ok;
        _ ->
            ct:fail(io_lib:format("Fail: ~p in [~p...]", [Pattern, H]))
    end.

assert_inmaplist([], Map, L, [H|_]) ->
    case L of
        [] ->
            ct:fail(io_lib:format("Fail: ~p not in [~p...]", [Map, H]));
        _ ->
            ok
    end;
assert_inmaplist([K|Keys], Map, L, Orig) ->
    V = maps:get(K, Map),
    Nl = lists:filter(fun(M) -> maps:get(K, M, niema) =:= V end, L),
    assert_inmaplist(Keys, Map, Nl, Orig).


assert_notinmaplist([], Map, L, [H|_]) ->
    case L of
        [] ->
            ok;
        _ ->
            ct:fail(io_lib:format("Fail: ~p in [~p...]", [Map, H]))
    end;
assert_notinmaplist([K|Keys], Map, L, Orig) ->
    V = maps:get(K, Map),
    Nl = lists:filter(fun(M) -> maps:get(K, M, niema) =:= V end, L),
    assert_notinmaplist(Keys, Map, Nl, Orig).


gett(Path) ->
    make_request(<<"GET">>, Path).

post(Path, Body) ->
    make_request(<<"POST">>, Path, Body).

putt(Path, Body) ->
    make_request(<<"PUT">>, Path, Body).

delete(Path) ->
    make_request(<<"DELETE">>, Path).

-spec gett(Path :: string()|binary(), Cred :: {Username :: binary(), Password :: binary()}) -> term().
gett(Path, Cred) ->
    make_request({<<"GET">>, Cred}, Path).

post(Path, Body, Cred) ->
    make_request({<<"POST">>, Cred}, Path, Body).

putt(Path, Body, Cred) ->
    make_request({<<"PUT">>, Cred}, Path, Body).

delete(Path, Cred) ->
    make_request({<<"DELETE">>, Cred}, Path).


make_request(Method, Path) ->
    make_request(Method, Path, <<"">>).

make_request(Method, Path, ReqBody) when is_map(ReqBody) ->
    make_request(Method, Path, jiffy:encode(ReqBody));
make_request(Method, Path, ReqBody) when not is_binary(Path) ->
    make_request(Method, list_to_binary(Path), ReqBody);
make_request(Method, Path, ReqBody) ->
    CPath = <<?PATHPREFIX/binary, Path/binary>>,
    {Code, RespBody} = case fusco_request(Method, CPath, ReqBody) of
                           {RCode, _, Body, _, _} ->
                               {RCode, Body};
                           {RCode, _, Body, _, _, _} ->
                               {RCode, Body}
                       end,
    {Code, decode(RespBody)}.

decode(<<>>) ->
    <<"">>;
decode(RespBody) ->
    try
        jiffy:decode(RespBody)
    catch
        throw:_ ->
            RespBody
    end.

%% a request specyfying credentials is directed to client http listener
fusco_request({Method, {User, Password}}, Path, Body) ->
    Basic = list_to_binary("basic " ++ base64:encode_to_string(to_list(User) ++ ":"++ to_list(Password))),
    Headers = [{<<"authorization">>, Basic}],
    fusco_request(Method, Path, Body, Headers, 8089, true);
%% without them it is for admin (secure) interface
fusco_request(Method, Path, Body) ->
    fusco_request(Method, Path, Body, [], 8088, false).

fusco_request(Method, Path, Body, HeadersIn, Port, SSL) ->
    {ok, Client} = fusco_cp:start_link({"localhost", Port, SSL}, [], 1),
    Headers = [{<<"Content-Type">>, <<"application/json">>} | HeadersIn],
    {ok, Result} = fusco_cp:request(Client, Path, Method, Headers, Body, 2, 10000),
    fusco_cp:stop(Client),
    Result.

mapfromlist(L) ->
    Nl = lists:keymap(fun(B) -> binary_to_existing_atom(B, utf8) end, 1, L),
    maps:from_list(Nl).

decode_maplist(Ml) ->
    Pl = [C || {C} <- Ml],
    [mapfromlist(L) || L <- Pl].


to_list(V) when is_binary(V) ->
    binary_to_list(V);
to_list(V) when is_list(V) ->
    V.

maybe_enable_mam(odbc, Host, Config) ->
    init_module(Host, mod_mam_odbc_arch, [muc, pm, simple]),
    init_module(Host, mod_mam_odbc_prefs, [muc, pm]),
    init_module(Host, mod_mam_odbc_user, [muc, pm]),
    init_module(Host, mod_mam, []),
    init_module(Host, mod_mam_muc, [{host, "muclight.@HOST@"}]),
    [{mam_backend, odbc} | Config];
maybe_enable_mam(riak, Host,  Config) ->
    init_module(Host, mod_mam_riak_timed_arch_yz, [pm, muc]),
    init_module(Host, mod_mam_mnesia_prefs, [pm, muc]),
    init_module(Host, mod_mam, []),
    init_module(Host, mod_mam_muc, [{host, "muclight.@HOST@"}]),
    [{mam_backend, riak}, {yz_wait, 2500} | Config];
maybe_enable_mam(_, _, C) ->
    [{mam_backend, disabled} | C].

init_module(Host, Mod, Opts) ->
    dynamic_modules:start(Host, Mod, Opts).

maybe_disable_mam(odbc, Host) ->
    stop_module(Host, mod_mam_odbc_arch),
    stop_module(Host, mod_mam_odbc_prefs),
    stop_module(Host, mod_mam_odbc_user),
    stop_module(Host, mod_mam),
    stop_module(Host, mod_mam_muc);
maybe_disable_mam(riak, Host) ->
    stop_module(Host, mod_mam_riak_timed_arch_yz),
    stop_module(Host, mod_mam_mnesia_prefs),
    stop_module(Host, mod_mam),
    stop_module(Host, mod_mam_muc);
maybe_disable_mam(_, _) ->
    ok.

stop_module(Host, Mod) ->
    dynamic_modules:stop(Host, Mod).

maybe_skip_mam_test_cases(CaseName, CasesRequireingMAM, Config) ->
    case lists:member(CaseName, CasesRequireingMAM) of
        false ->
            escalus:init_per_testcase(CaseName, Config);
        _ ->
            skip_if_mam_disabled(CaseName, Config)
    end.

skip_if_mam_disabled(CaseName, Config) ->
    case proplists:get_value(mam_backend, Config) of
        disabled ->
            {skip, mam_not_available};
        _ ->
            escalus:init_per_testcase(CaseName, Config)
    end.

fill_archive(A, B) ->
    % here we generate messages sent one, two and three days ago at 10am
    {TodayDate, _} = calendar:local_time(),
    Today = calendar:date_to_gregorian_days(TodayDate),
    put_msg(A, B, <<"A">>, Today - 3),
    put_msg(B, A, <<"A">>, Today - 3),
    put_msg(A, B, <<"B">>, Today - 2),
    put_msg(B, A, <<"B">>, Today - 2),
    put_msg(A, B, <<"C">>, Today - 1),
    put_msg(B, A, <<"C">>, Today - 1).

put_msg(Aclient, Bclient, Content, Days) ->
    DateTime = {calendar:gregorian_days_to_date(Days), {10, 0, 0}},
    AArcId = make_arc_id(Aclient),
    BArcId = make_arc_id(Bclient),
    Msg = mam_helper:generate_msg_for_date_user(AArcId, BArcId, DateTime, Content),
    put_msg(Msg),
    ok.

put_msg({{MsgIdOwner, MsgIdRemote},
    {_FromBin, FromJID, FromArcID},
    {_ToBin, ToJID, ToArcID},
    {_, Source, _}, Packet}) ->
    Host = ct:get_config({hosts, mim, domain}),
    OutArgs = [Host, MsgIdOwner, FromArcID, FromJID, ToJID, Source, outgoing, Packet],
    ok = mam_helper:rpc_apply(mod_mam, archive_message, OutArgs),
    InArgs = [Host, MsgIdRemote, ToArcID, ToJID, FromJID, Source, incoming, Packet],
    ok = mam_helper:rpc_apply(mod_mam, archive_message, InArgs).

make_arc_id(Client) ->
    User = escalus_client:username(Client),
    Server = escalus_client:server(Client),
    Bin = escalus_client:short_jid(Client),
    Jid = mam_helper:rpc_apply(jid, make, [User, Server, <<"">>]),
    {Bin, Jid, mam_helper:rpc_apply(mod_mam, archive_id, [Server, User])}.

fill_room_archive(RoomID, Users) ->
    {TodayDate, _} = calendar:local_time(),
    Today = calendar:date_to_gregorian_days(TodayDate),
    Days = [Today - I || I <- lists:seq(0, 3)],
    Host = ct:get_config({hosts, mim, domain}),
    MUCLight = <<"muclight.", Host/binary>>,
    RoomJID = mam_helper:rpc_apply(jid, make, [RoomID, MUCLight, <<>>]),
    RoomBinJID = <<RoomID/binary, "@", MUCLight/binary>>,
    RoomArcID = mam_helper:rpc_apply(mod_mam_muc, archive_id_int, [Host, RoomJID]),
    Room = {RoomBinJID, RoomJID, RoomArcID},
    UserArcIDs = [make_room_arc_id(Room, User) || User <- Users],
    [put_room_msgs_in_day(Room, UserArcIDs, Day) || Day <- lists:reverse(Days)].

put_room_msgs_in_day(RoomJID, Users, Day) ->
    [put_room_msg_in_day(RoomJID, User, Day) || User <- Users].

put_room_msg_in_day(RoomArcID, FromArcID, Day) ->
    {_, Time} = calendar:local_time(),
    DateTime = {calendar:gregorian_days_to_date(Day), Time},
    Msg = mam_helper:generate_msg_for_date_user(FromArcID, RoomArcID, DateTime),
    put_room_msg(Msg).

put_room_msg({{_, MsgID},
              {FromJIDBin, FromJID, _},
              {_, ToJID, ToArcID},
              {_, SrcJID, _}, Msg}) ->
    Host = ct:get_config({hosts, mim, domain}),
    ok = mam_helper:rpc_apply(mod_mam_muc, archive_message,
                         [Host, MsgID, ToArcID, ToJID, FromJID, SrcJID,
                          incoming, Msg]),
    {MsgID, FromJIDBin, Msg}.

make_timestamp(Offset, Time) ->
    {TodayDate, _} = calendar:local_time(),
    Today = calendar:date_to_gregorian_days(TodayDate),
    Dt = {calendar:gregorian_days_to_date(Today + Offset), Time},
    (calendar:datetime_to_gregorian_seconds(Dt) - 62167219200) * 1000.

make_room_arc_id({_, RoomJID, _}, Client) ->
    Bin = escalus_client:short_jid(Client),
    JID = mam_helper:rpc_apply(jid, replace_resource, [RoomJID, Bin]),
    JIDBin = mam_helper:rpc_apply(jid, to_binary, [JID]),
    {JIDBin, JID, undefined}.

