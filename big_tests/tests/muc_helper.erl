-module(muc_helper).
-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("exml/include/exml.hrl").
-include_lib("escalus/include/escalus_xmlns.hrl").

-import(distributed_helper, [mim/0,
                             rpc/4]).

-type verify_fun() :: fun((Incoming :: #xmlel{}) -> any()).

-define(MUC_HOST, <<"muc.localhost">>).

-export_type([verify_fun/0]).

-spec foreach_occupant(
        Users :: [escalus:client()], Stanza :: #xmlel{}, VerifyFun :: verify_fun()) -> ok.
foreach_occupant(Users, Stanza, VerifyFun) ->
    lists:foreach(
      fun(Sender) ->
              escalus:send(Sender, Stanza),
              case exml_query:path(Stanza, [{attr, <<"type">>}]) of
                  <<"get">> ->
                      Incoming = escalus:wait_for_stanza(Sender),
                      escalus:assert(is_iq_result, Incoming),
                      VerifyFun(Incoming);
                  _ ->
                      foreach_recipient(Users, VerifyFun),
                      case Stanza of
                          #xmlel{ name = <<"iq">> } ->
                              escalus:assert(is_iq_result, escalus:wait_for_stanza(Sender));
                          _ ->
                              ok
                      end
              end
      end, Users).

-spec foreach_recipient(Users :: [escalus:client()], VerifyFun :: verify_fun()) -> ok.
foreach_recipient(Users, VerifyFun) ->
    lists:foreach(
      fun(Recipient) ->
              VerifyFun(escalus:wait_for_stanza(Recipient))
      end, Users).

load_muc(Host) ->
    %% Stop modules before trying to start them
    unload_muc(),
    %% TODO refactoring. "localhost" should be passed as a parameter
    dynamic_modules:start(<<"localhost">>, mod_muc,
                          [{host, binary_to_list(Host)},
                          %% XXX TODO Uncomment, when mod_muc_db_rdbms is written
                          %{backend, Backend},
                           {hibernate_timeout, 2000},
                           {hibernated_room_check_interval, 1000},
                           {hibernated_room_timeout, 2000},
                           {access, muc},
                           {access_create, muc_create}]),
    dynamic_modules:start(<<"localhost">>, mod_muc_log,
                          [{outdir, "/tmp/muclogs"},
                           {access_log, muc}]).

unload_muc() ->
    dynamic_modules:stop(<<"localhost">>, mod_muc),
    dynamic_modules:stop(<<"localhost">>, mod_muc_log).

muc_host() ->
    ?MUC_HOST.

start_room(Config, User, Room, Nick, Opts) ->
    From = generate_rpc_jid(User),
    create_instant_room(<<"localhost">>, Room, From, Nick, Opts),
    RoomJID = room_address(Room),
    [{nick, Nick}, {room, Room}, {room_jid, RoomJID}, {muc_host, muc_host()} | Config].

start_fresh_room(Config, User, Nick, Opts) ->
    Room = fresh_room_name(),
    start_room(Config, User, Room, Nick, Opts).

enter_room(Config, UsersAndNicks) ->
    [Room, RoomJid] = [proplists:get_value(Key, Config) || Key <- [room, room_jid]],
    lists:foldl(fun({User, Nick}, Acc) ->
                        escalus:send(User, stanza_muc_enter_room(Room, Nick)),
                        wait_for_presence(RoomJid, User, length(Acc)),
                        foreach_recipient([User | Acc],
                                          fun(Stanza) ->
                                                  validate_presence(Stanza,
                                                                    RoomJid,
                                                                    Nick)
                                          end),
                        Subject = escalus:wait_for_stanza(User),
                        validate_subject_message(Subject, RoomJid),
                        [User | Acc]
                end,
                [],UsersAndNicks).

wait_for_presence(_, _, 0) ->
    ok;
wait_for_presence(RoomJid, User, N) ->
    Stanza = escalus:wait_for_stanza(User),
    validate_presence(Stanza, RoomJid),
    wait_for_presence(RoomJid, User, N - 1).

validate_presence(Stanza, RoomJid) ->
    escalus:assert(is_presence, [], Stanza),
    [RoomJid, _] = binary:split(exml_query:attr(Stanza, <<"from">>), <<"/">>).

validate_presence(Stanza, RoomJid, Nick) ->
    [RoomJid, Nick] = validate_presence(Stanza, RoomJid).

validate_subject_message(Stanza, RoomJid) ->
    RoomJid = exml_query:attr(Stanza, <<"from">>),
    #xmlel{} = exml_query:subelement(Stanza, <<"subject">>).

verify_message_received(Config, Users, Nick, TextBody) ->
    RoomJid = proplists:get_value(room_jid, Config),
    foreach_recipient(Users, muc_msg_verify(RoomJid, Nick, TextBody)).

muc_msg_verify(RoomBareJID, NickName, MsgText) ->
    fun(Msg) ->
        escalus:assert(is_groupchat_message, [MsgText], Msg),
        [RoomBareJID, NickName] = binary:split(exml_query:attr(Msg, <<"from">>), <<"/">>)
    end.

send_to_room(RoomCfg, User, TextBody) ->
    RoomJid = proplists:get_value(room_jid, RoomCfg),
    Stanza = escalus_stanza:groupchat_to(RoomJid, TextBody),
    escalus:send(User, Stanza).

generate_rpc_jid({_,User}) ->
    {username, Username} = lists:keyfind(username, 1, User),
    {server, Server} = lists:keyfind(server, 1, User),
    LUsername = escalus_utils:jid_to_lower(Username),
    LServer = escalus_utils:jid_to_lower(Server),
    {jid, Username, Server, <<"rpc">>, LUsername, LServer, <<"rpc">>}.

create_instant_room(Host, Room, From, Nick, Opts) ->
    Room1 = rpc(mim(), jid, nodeprep, [Room]),
    rpc(mim(), mod_muc, create_instant_room,
        [Host, Room1, From, Nick, Opts]).

destroy_room(Config) ->
    destroy_room(?MUC_HOST, ?config(room, Config)).

destroy_room(Host, Room) when is_binary(Host), is_binary(Room) ->
    Room1 = rpc(mim(), jid, nodeprep, [Room]),
    case rpc(mim(), ets, lookup, [muc_online_room, {Room1, Host}]) of
        [{_,_,Pid}|_] -> gen_fsm_compat:send_all_state_event(Pid, destroy);
        _ -> ok
    end.

stanza_muc_enter_room(Room, Nick) ->
    stanza_to_room(
        escalus_stanza:presence(  <<"available">>,
                                [#xmlel{ name = <<"x">>, attrs=[{<<"xmlns">>, <<"http://jabber.org/protocol/muc">>}]}]),
        Room, Nick).

stanza_default_muc_room(Room, Nick) ->
    Form = escalus_stanza:x_data_form(<<"submit">>, []),
    Query = escalus_stanza:query_el(?NS_MUC_OWNER, [Form]),
    IQSet = escalus_stanza:iq(<<"set">>, [Query]),
    stanza_to_room(IQSet, Room, Nick).

stanza_to_room(Stanza, Room) ->
    escalus_stanza:to(Stanza, room_address(Room)).

stanza_to_room(Stanza, Room, Nick) ->
    escalus_stanza:to(Stanza, room_address(Room, Nick)).

room_address(Room) ->
    <<Room/binary, "@", ?MUC_HOST/binary>>.

room_address(Room, Nick) ->
    <<Room/binary, "@", ?MUC_HOST/binary, "/", Nick/binary>>.

given_fresh_room(Config, UserSpec, RoomOpts) ->
    Username = proplists:get_value(username, UserSpec),
    RoomName = fresh_room_name(Username),
    start_room(Config, {user, UserSpec}, RoomName, Username, RoomOpts).

disco_service_story(Config) ->
    escalus:fresh_story(Config, [{alice, 1}], fun(Alice) ->
        Server = escalus_client:server(Alice),
        escalus:send(Alice, escalus_stanza:service_discovery(Server)),
        Stanza = escalus:wait_for_stanza(Alice),
        escalus:assert(has_service, [muc_host()], Stanza),
        escalus:assert(is_stanza_from,
                            [ct:get_config({hosts, mim, domain})], Stanza)
    end).

disco_features_story(Config, Features) ->
    escalus:fresh_story(Config, [{alice, 1}], fun(Alice) ->
        escalus:send(Alice, stanza_get_features()),
        Stanza = escalus:wait_for_stanza(Alice),
        has_features(Stanza, Features),
        escalus:assert(is_stanza_from, [muc_host()], Stanza)
    end).

disco_info_story(Config, Features) ->
    escalus:fresh_story(Config, [{alice, 1}], fun(Alice) ->
        Stanza = escalus:send_iq_and_wait_for_result(
                     Alice, stanza_to_room(escalus_stanza:iq_get(?NS_DISCO_INFO,[]), <<"alicesroom">>)),
        has_features(Stanza, Features)
    end).

fresh_room_name(Username) ->
    escalus_utils:jid_to_lower(<<"room-", Username/binary>>).

fresh_room_name() ->
    fresh_room_name(base16:encode(crypto:strong_rand_bytes(5))).

stanza_get_features() ->
    %% <iq from='hag66@shakespeare.lit/pda'
    %%     id='lx09df27'
    %%     to='chat.shakespeare.lit'
    %%     type='get'>
    %%  <query xmlns='http://jabber.org/protocol/disco#info'/>
    %% </iq>
    escalus_stanza:setattr(escalus_stanza:iq_get(?NS_DISCO_INFO, []), <<"to">>,
                           muc_host()).

has_features(#xmlel{children = [ Query ]}, Features) ->
    %%<iq from='chat.shakespeare.lit'
    %%  id='lx09df27'
    %%  to='hag66@shakespeare.lit/pda'
    %%  type='result'>
    %%  <query xmlns='http://jabber.org/protocol/disco#info'>
    %%    <identity
    %%      category='conference'
    %%      name='Shakespearean Chat Service'
    %%      type='text'/>
    %%      <feature var='http://jabber.org/protocol/muc'/>
    %%  </query>
    %%</iq>

    Identity = exml_query:subelement(Query, <<"identity">>),
    <<"conference">> = exml_query:attr(Identity, <<"category">>),
    Features = exml_query:paths(Query, [{element, <<"feature">>},
                                        {attr, <<"var">>}]).

assert_valid_affiliation(<<"owner">>) -> ok;
assert_valid_affiliation(<<"admin">>) -> ok;
assert_valid_affiliation(<<"member">>) -> ok;
assert_valid_affiliation(<<"outcast">>) -> ok;
assert_valid_affiliation(<<"none">>) -> ok.

assert_valid_role(<<"moderator">>) -> ok;
assert_valid_role(<<"participant">>) -> ok;
assert_valid_role(<<"visitor">>) -> ok;
assert_valid_role(<<"none">>) -> ok.


story_with_room(Config, RoomOpts, [{Owner, _}|_] = UserSpecs, StoryFun) ->
    Config1 = escalus_fresh:create_users(Config, UserSpecs),
    AliceSpec = escalus_users:get_userspec(Config1, Owner),
    Config2 = given_fresh_room(Config1, AliceSpec, RoomOpts),
    try
        StoryFun2 = fun(Args) -> apply(StoryFun, [Config2 | Args]) end,
        escalus_story:story_with_client_list(Config2, UserSpecs, StoryFun2)
    after
        destroy_room(Config2)
    end.
