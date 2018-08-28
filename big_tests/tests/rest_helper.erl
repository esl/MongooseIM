-module(rest_helper).
-author("bartekgorny").

%% API
-export([
    assert_inlist/2,
    assert_notinlist/2,
    decode_maplist/1,
    gett/2,
    gett/3,
    post/3,
    post/4,
    putt/3,
    putt/4,
    delete/2,
    delete/3,
    delete/4,
    maybe_enable_mam/3,
    maybe_disable_mam/2,
    maybe_skip_mam_test_cases/3,
    fill_archive/2,
    fill_room_archive/2,
    make_timestamp/2,
    change_admin_creds/1,
    make_msg_stanza_with_props/2,
    make_malformed_msg_stanza_with_props/2
]).

-import(distributed_helper, [mim/0,
                             rpc/4]).

-define(PATHPREFIX, <<"/api">>).

-type role() :: admin | client.

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
    Fl = lists:filter(fun(X) -> case X of Pattern -> true; _ -> false end end, L),
    case Fl of
        [] ->
            ok;
        _ ->
            ct:fail(io_lib:format("Fail: ~p in ~p", [Pattern, L]))
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


gett(Role, Path) ->
    make_request(Role, <<"GET">>, Path).

post(Role, Path, Body) ->
    make_request(Role, <<"POST">>, Path, Body).

putt(Role, Path, Body) ->
    make_request(Role, <<"PUT">>, Path, Body).

delete(Role, Path) ->
    make_request(Role, <<"DELETE">>, Path).

-spec gett(role(), Path :: string()|binary(), Cred :: {Username :: binary(), Password :: binary()}) -> term().
gett(Role, Path, Cred) ->
    make_request(Role, {<<"GET">>, Cred}, Path).

post(Role, Path, Body, Cred) ->
    make_request(Role, {<<"POST">>, Cred}, Path, Body).

putt(Role, Path, Body, Cred) ->
    make_request(Role, {<<"PUT">>, Cred}, Path, Body).

delete(Role, Path, Cred) ->
    make_request(Role, {<<"DELETE">>, Cred}, Path).

delete(Role, Path, Cred, Body) ->
    make_request(Role, {<<"DELETE">>, Cred}, Path, Body).



make_request(Role, Method, Path) ->
    make_request(Role, Method, Path, <<"">>).

make_request(Role, Method, Path, ReqBody) when is_map(ReqBody) ->
    make_request(Role, Method, Path, jiffy:encode(ReqBody));
make_request(Role, Method, Path, ReqBody) when not is_binary(Path) ->
    make_request(Role, Method, list_to_binary(Path), ReqBody);
make_request(Role, Method, Path, ReqBody) ->
    CPath = <<?PATHPREFIX/binary, Path/binary>>,
    {Code, RespBody} = case fusco_request(Role, Method, CPath, ReqBody) of
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
fusco_request(Role, {Method, {User, Password}}, Path, Body) ->
    Basic = list_to_binary("Basic " ++ base64:encode_to_string(to_list(User) ++ ":"++ to_list(Password))),
    Headers = [{<<"authorization">>, Basic}],
    fusco_request(Method, Path, Body, Headers, get_port(Role), get_ssl_status(Role));
%% without them it is for admin (secure) interface
fusco_request(Role, Method, Path, Body) ->
    fusco_request(Method, Path, Body, [], get_port(Role), get_ssl_status(Role)).

fusco_request(Method, Path, Body, HeadersIn, Port, SSL) ->
    {ok, Client} = fusco_cp:start_link({"localhost", Port, SSL}, [], 1),
    Headers = [{<<"Content-Type">>, <<"application/json">>} | HeadersIn],
    {ok, Result} = fusco_cp:request(Client, Path, Method, Headers, Body, 2, 10000),
    fusco_cp:stop(Client),
    Result.

-spec get_port(role()) -> Port :: integer().
get_port(Role) -> 
    Listeners = rpc(mim(), ejabberd_config, get_local_option, [listen]),
    [{PortIpNet, ejabberd_cowboy, _Opts}] = lists:filter(fun(Config) -> is_roles_config(Config, Role) end, Listeners),
    case PortIpNet of
        {Port, _Host, _Net} -> Port;
        {Port, _Host} -> Port;
        Port -> Port
    end.

-spec get_ssl_status(role()) -> boolean().
get_ssl_status(Role) ->
    Listeners = rpc(mim(), ejabberd_config, get_local_option, [listen]),
    [{_PortIpNet, _Module, Opts}] = lists:filter(fun (Opts) -> is_roles_config(Opts, Role) end, Listeners),
    lists:keymember(ssl, 1, Opts).

% @doc Changes the control credentials for admin by restarting the listener
% with new options.
-spec change_admin_creds({User :: binary(), Password :: binary()}) -> 'ok' | 'error'.
change_admin_creds(Creds) ->
    stop_admin_listener(),
    {ok, _} =  start_admin_listener(Creds).

-spec stop_admin_listener() -> 'ok' | {'error', 'not_found' | 'restarting' | 'running' | 'simple_one_for_one'}.
stop_admin_listener() ->
    Listeners = rpc(mim(), ejabberd_config, get_local_option, [listen]),
    [{PortIpNet, Module, _Opts}] = lists:filter(fun (Opts) -> is_roles_config(Opts, admin) end, Listeners),
    rpc(mim(), ejabberd_listener, stop_listener, [PortIpNet, Module]).

-spec start_admin_listener(Creds :: {binary(), binary()}) -> {'error', pid()} | {'ok', _}.
start_admin_listener(Creds) ->
    Listeners = rpc(mim(), ejabberd_config, get_local_option, [listen]),
    [{PortIpNet, Module, Opts}] = lists:filter(fun (Opts) -> is_roles_config(Opts, admin) end, Listeners),
    NewOpts = insert_creds(Opts, Creds),
    rpc(mim(), ejabberd_listener, start_listener, [PortIpNet, Module, NewOpts]).

insert_creds(Opts, Creds) ->
    Modules = proplists:get_value(modules, Opts),
    {Host, Path, mongoose_api_admin, PathOpts} = lists:keyfind(mongoose_api_admin, 3, Modules),
    NewPathOpts = inject_creds_to_opts(PathOpts, Creds),
    NewModules = lists:keyreplace(mongoose_api_admin, 3, Modules, {Host, Path, mongoose_api_admin,  NewPathOpts}),
    lists:keyreplace(modules, 1, Opts, {modules, NewModules}).

inject_creds_to_opts(PathOpts, any) ->
    lists:keydelete(auth, 1, PathOpts);
inject_creds_to_opts(PathOpts, Creds) ->
    case lists:keymember(auth, 1, PathOpts) of
        true ->
            lists:keyreplace(auth, 1, PathOpts, {auth, Creds});
        false ->
            lists:append(PathOpts, [{auth, Creds}])
    end.

% @doc Checks whether a config for a port is an admin or client one.
% This is determined based on modules used. If there is any mongoose_api_admin module used,
% it is admin config. If not and there is at least one mongoose_api_client* module used,
% it's clients.
is_roles_config({_PortIpNet, ejabberd_cowboy, Opts}, admin) ->
    {value, {modules, Modules}} = lists:keysearch(modules, 1, Opts),
    lists:any(fun({_, _Path,  Mod, _Args}) -> Mod == mongoose_api_admin; (_) -> false  end, Modules);
is_roles_config({_PortIpNet, ejabberd_cowboy, Opts}, client) ->
    {value, {modules, ModulesConfs}} = lists:keysearch(modules, 1, Opts),
    ModulesTokens = lists:map(fun({_, _Path, Mod, _}) -> string:tokens(atom_to_list(Mod), "_"); (_) -> [] end, ModulesConfs),
    lists:any(fun(["mongoose", "client", "api" | _T]) -> true; (_) -> false end, ModulesTokens);
is_roles_config(_, _) -> false.

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

maybe_enable_mam(rdbms, Host, Config) ->
    init_module(Host, mod_mam_rdbms_arch, [muc, pm, simple]),
    init_module(Host, mod_mam_rdbms_prefs, [muc, pm]),
    init_module(Host, mod_mam_rdbms_user, [muc, pm]),
    init_module(Host, mod_mam, []),
    init_module(Host, mod_mam_muc, [{host, "muclight.@HOST@"}]),
    [{mam_backend, rdbms} | Config];
maybe_enable_mam(riak, Host,  Config) ->
    init_module(Host, mod_mam_riak_timed_arch_yz, [pm, muc]),
    init_module(Host, mod_mam_mnesia_prefs, [pm, muc]),
    init_module(Host, mod_mam, []),
    init_module(Host, mod_mam_muc, [{host, "muclight.@HOST@"}]),
    [{mam_backend, riak}, {archive_wait, 2500} | Config];
maybe_enable_mam(_, _, C) ->
    [{mam_backend, disabled} | C].

init_module(Host, Mod, Opts) ->
    dynamic_modules:start(Host, Mod, Opts).

maybe_disable_mam(rdbms, Host) ->
    stop_module(Host, mod_mam_rdbms_arch),
    stop_module(Host, mod_mam_rdbms_prefs),
    stop_module(Host, mod_mam_rdbms_user),
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

%%Make sample message with property for Smack lib.
make_msg_stanza_with_props(ToJID,MsgID) ->
    escalus_stanza:from_xml(
        <<"<message xml:lang='en' to='",ToJID/binary,"' id='",MsgID/binary,"' type='chat'>
            <body xml:lang='en_US'>Test message with properties</body>
            <properties xmlns='http://www.jivesoftware.com/xmlns/xmpp/properties'>
                <property>
                    <name>some_string</name>
                    <value type='string'>abcdefghijklmnopqrstuvwxyz</value>
                </property>
                <property>
                    <name>some_number</name>
                    <value type='long'>1234567890</value>
                </property>
            </properties>
        </message>">>).

%%Make sample message with general property, malformed i.e. not for Smack lib.
make_malformed_msg_stanza_with_props(ToJID,MsgID) ->
    escalus_stanza:from_xml(
        <<"<message xml:lang='en' to='",ToJID/binary,"' id='",MsgID/binary,"' type='chat'>
            <body xml:lang='en_US'>Test message with malformed properties</body>
            <properties>
                <property1>
                    <name>some_string</name>
                    <value type='string'>abcdefghijklmnopqrstuvwxyz</value>
                </property1>
                <property2>
                    <name>some_number</name>
                    <value type='long'>1234567890</value>
                </property2>
            </properties>
        </message>">>).