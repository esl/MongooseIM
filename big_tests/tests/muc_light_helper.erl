-module(muc_light_helper).

-compile([export_all]).

-include("mam_helper.hrl").
-include("muc_light.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("exml/include/exml.hrl").

-import(escalus_ejabberd, [rpc/3]).
-import(distributed_helper, [mim/0,
                             rpc/4]).

-type ct_aff_user() :: {EscalusClient :: escalus:client(), Aff :: atom()}.
-type ct_aff_users() :: [ct_aff_user()].

-spec room_bin_jid(Room :: binary()) -> binary().
room_bin_jid(Room) ->
    <<Room/binary, $@, (muc_host())/binary>>.

muc_host() ->
    Host = ct:get_config({hosts, mim, domain}),
    <<"muclight.", Host/binary>>.

create_room(RoomU, MUCHost, Owner, Members, Config, Version) ->
    DefaultConfig = default_config(),
    RoomUS = {RoomU, MUCHost},
    AffUsers = [{to_lus(Owner, Config), owner}
                | [ {to_lus(Member, Config), member} || Member <- Members ]],
    AffUsersSort = lists:sort(AffUsers),
    {ok, _RoomUS} = rpc(mim(), mod_muc_light_db_backend, create_room,
                        [RoomUS, DefaultConfig, AffUsersSort, Version]).

-spec default_config() -> list().
default_config() -> rpc(mim(), mod_muc_light, default_config, [muc_host()]).

-spec ns_muc_light_affiliations() -> binary().
ns_muc_light_affiliations() ->
    ?NS_MUC_LIGHT_AFFILIATIONS.

given_muc_light_room(Name, Creator, InitOccupants) ->
    CreateStanza = stanza_create_room(Name, [], InitOccupants),
    escalus:send(Creator, CreateStanza),
    Affiliations = [{Creator, owner} | InitOccupants],
    verify_aff_bcast(Affiliations, Affiliations),
    IQResult = escalus:wait_for_stanza(Creator),
    escalus:assert(is_iq_result, IQResult),
    exml_query:attr(IQResult, <<"from">>).

when_muc_light_message_is_sent(Sender, Room, Body, Id) ->
    RoomJid = room_bin_jid(Room),
    Stanza = escalus_stanza:set_id(
               escalus_stanza:groupchat_to(RoomJid, Body), Id),
    escalus:send(Sender, Stanza),
    {Room, Body, Id}.

then_muc_light_message_is_received_by(Users, {Room, Body, Id}) ->
    F = gc_message_verify_fun(Room, Body, Id),
    [ F(escalus:wait_for_stanza(User)) || User <- Users ].

when_muc_light_affiliations_are_set(Sender, Room, Affiliations) ->
    Stanza = stanza_aff_set(Room, Affiliations),
    escalus:send(Sender, Stanza),
    {Room, Affiliations}.

then_muc_light_affiliations_are_received_by(Users, {_Room, Affiliations}) ->
    F = aff_msg_verify_fun(Affiliations),
    [ F(escalus:wait_for_stanza(User)) || User <- Users ].

when_archive_query_is_sent(Sender, RecipientJid, Config) ->
    P = ?config(props, Config),
    Request = case RecipientJid of
                  undefined -> mam_helper:stanza_archive_request(P, <<"q">>);
                  _ -> escalus_stanza:to(mam_helper:stanza_archive_request(P, <<"q">>), RecipientJid)
              end,
    escalus:send(Sender, Request).

-spec user_leave(Room :: binary(), User :: escalus:client(), RemainingOccupants :: ct_aff_users()) -> ok.
user_leave(Room, User, RemainingOccupants) ->
  AffUsersChanges = [{User, none}],
  Stanza = stanza_aff_set(Room, AffUsersChanges),
  escalus:send(User, Stanza),
  % bcast
  verify_aff_bcast(RemainingOccupants, AffUsersChanges),
  escalus:assert(is_iq_result, escalus:wait_for_stanza(User)).

then_archive_response_is(Receiver, Expected, Config) ->
    P = ?config(props, Config),
    Response = mam_helper:wait_archive_respond(P, Receiver),
    Stanzas = mam_helper:respond_messages(mam_helper:assert_respond_size(length(Expected), Response)),
    ParsedStanzas = [ mam_helper:parse_forwarded_message(Stanza) || Stanza <- Stanzas ],
    [ assert_archive_element(Element)
      || Element <- lists:zip(Expected, ParsedStanzas) ].

assert_archive_element({{create, Affiliations}, Stanza}) ->
    mam_helper:verify_archived_muc_light_aff_msg(Stanza, Affiliations, _IsCreate = true);
assert_archive_element({{affiliations, Affiliations}, Stanza}) ->
    mam_helper:verify_archived_muc_light_aff_msg(Stanza, Affiliations, _IsCreate = false);
assert_archive_element({{muc_message, Room, Sender, Body}, Stanza}) ->
    FromJid = escalus_utils:jid_to_lower(muc_light_room_jid(Room, Sender)),
    #forwarded_message{message_body = Body,
                       delay_from = FromJid} = Stanza;
assert_archive_element({{message, Sender, Body}, Stanza}) ->
    FromJid = escalus_utils:jid_to_lower(escalus_utils:get_jid(Sender)),
    #forwarded_message{message_body = Body, delay_from = FromJid} = Stanza.


muc_light_room_jid(Room, User) ->
    RoomJid = room_bin_jid(Room),
    UserJid = escalus_utils:get_short_jid(User),
    <<RoomJid/binary, $/, UserJid/binary>>.

verify_aff_bcast(CurrentOccupants, AffUsersChanges) ->
    verify_aff_bcast(CurrentOccupants, AffUsersChanges, []).

verify_aff_bcast(CurrentOccupants, AffUsersChanges, ExtraNSs) ->
    muc_helper:foreach_recipient(
      [ User || {User, _} <- CurrentOccupants ], aff_msg_verify_fun(AffUsersChanges)),
    lists:foreach(
      fun({Leaver, none}) ->
              Incoming = escalus:wait_for_stanza(Leaver),
              %% This notification must come from the room bare JID
              MUCHost = muc_host(),
              [_, MUCHost] = binary:split(exml_query:attr(Incoming, <<"from">>), <<"@">>),
              {[X], []} = lists:foldl(
                            fun(XEl, {XAcc, NSAcc}) ->
                                    XMLNS = exml_query:attr(XEl, <<"xmlns">>),
                                    case lists:member(XMLNS, NSAcc) of
                                        true -> {XAcc, lists:delete(XMLNS, NSAcc)};
                                        false -> {[XEl | XAcc], NSAcc}
                                    end
                            end, {[], ExtraNSs}, exml_query:subelements(Incoming, <<"x">>)),
              ?NS_MUC_LIGHT_AFFILIATIONS = exml_query:attr(X, <<"xmlns">>),
              [Item] = exml_query:subelements(X, <<"user">>),
              <<"none">> = exml_query:attr(Item, <<"affiliation">>),
              LeaverJIDBin = lbin(escalus_client:short_jid(Leaver)),
              LeaverJIDBin = exml_query:cdata(Item);
         (_) ->
              ignore
      end, AffUsersChanges).

-spec aff_msg_verify_fun(AffUsersChanges :: ct_aff_users()) -> muc_helper:verify_fun().
aff_msg_verify_fun(AffUsersChanges) ->
    BinAffUsersChanges = bin_aff_users(AffUsersChanges),
    fun(Incoming) ->
            [X] = exml_query:subelements(Incoming, <<"x">>),
            ?NS_MUC_LIGHT_AFFILIATIONS = exml_query:attr(X, <<"xmlns">>),
            PrevVersion = exml_query:path(X, [{element, <<"prev-version">>}, cdata]),
            Version = exml_query:path(X, [{element, <<"version">>}, cdata]),
            [Item | RItems] = Items = exml_query:subelements(X, <<"user">>),
            [ToBin | _] = binary:split(exml_query:attr(Incoming, <<"to">>), <<"/">>),
            true = is_binary(Version),
            true = Version =/= PrevVersion,
            case {ToBin == exml_query:cdata(Item), RItems} of
                {true, []} ->
                    {_, ProperAff} = lists:keyfind(ToBin, 1, BinAffUsersChanges),
                    ProperAff = exml_query:attr(Item, <<"affiliation">>);
                _ ->
                    true = is_binary(PrevVersion),
                    verify_aff_users(Items, BinAffUsersChanges)
            end
    end.

-spec lbin(Bin :: binary()) -> binary().
lbin(Bin) -> list_to_binary(string:to_lower(binary_to_list(Bin))).


-spec bin_aff_users(AffUsers :: ct_aff_users()) -> [{LBinJID :: binary(), AffBin :: binary()}].
bin_aff_users(AffUsers) ->
    [ {lbin(escalus_client:short_jid(User)), list_to_binary(atom_to_list(Aff))}
      || {User, Aff} <- AffUsers ].

-spec verify_aff_users(Items :: [exml:element()], BinAffUsers :: [{binary(), binary()}]) -> [].
verify_aff_users(Items, BinAffUsers) ->
    true = (length(Items) == length(BinAffUsers)),
    [] = lists:foldl(
           fun(Item, AffAcc) ->
                   JID = exml_query:cdata(Item),
                   Aff = exml_query:attr(Item, <<"affiliation">>),
                   verify_keytake(lists:keytake(JID, 1, AffAcc), JID, Aff, AffAcc)
           end, BinAffUsers, Items).

-spec verify_keytake(Result :: {value, Item :: tuple(), Acc :: list()}, JID :: binary(),
                     Aff :: binary(), AffAcc :: list()) -> list().
verify_keytake({value, {_, Aff}, NewAffAcc}, _JID, Aff, _AffAcc) -> NewAffAcc.

-spec stanza_create_room(RoomNode :: binary() | undefined, InitConfig :: [{binary(), binary()}],
                         InitOccupants :: ct_aff_users()) -> exml:element().
stanza_create_room(RoomNode, InitConfig, InitOccupants) ->
    Host = muc_host(),
    ToBinJID = case RoomNode of
                     undefined -> Host;
                     _ -> <<RoomNode/binary, $@, Host/binary>>
                 end,
    ConfigItem = #xmlel{ name = <<"configuration">>,
                         children = [ kv_el(K, V) || {K, V} <- InitConfig ] },
    OccupantsItems = [ #xmlel{ name = <<"user">>,
                               attrs = [{<<"affiliation">>, BinAff}],
                               children = [#xmlcdata{ content = BinJID }] }
                       || {BinJID, BinAff} <- bin_aff_users(InitOccupants) ],
    OccupantsItem = #xmlel{ name = <<"occupants">>, children = OccupantsItems },
    escalus_stanza:to(escalus_stanza:iq_set(
                        ?NS_MUC_LIGHT_CREATE, [ConfigItem, OccupantsItem]), ToBinJID).

-spec kv_el(K :: binary(), V :: binary()) -> exml:element().
kv_el(K, V) ->
    #xmlel{ name = K, children = [ #xmlcdata{ content = V } ] }.

-spec to_lus(Config :: list(), UserAtom :: atom()) -> {binary(), binary()}.
to_lus(UserAtom, Config) ->
    {lbin(escalus_users:get_username(Config, UserAtom)),
     lbin(escalus_users:get_server(Config, UserAtom))}.

-spec gc_message_verify_fun(Room :: binary(), MsgText :: binary(), Id :: binary()) -> muc_helper:verify_fun().
gc_message_verify_fun(Room, MsgText, Id) ->
    Host = muc_host(),
    fun(Incoming) ->
            escalus:assert(is_groupchat_message, [MsgText], Incoming),
            [RoomBareJID, FromNick] = binary:split(exml_query:attr(Incoming, <<"from">>), <<"/">>),
            [Room, Host] = binary:split(RoomBareJID, <<"@">>),
            [_] = binary:split(FromNick, <<"/">>), % nick is bare JID
            Id = exml_query:attr(Incoming, <<"id">>)
    end.


-spec stanza_aff_set(Room :: binary(), AffUsers :: ct_aff_users()) -> exml:element().
stanza_aff_set(Room, AffUsers) ->
    Items = [#xmlel{ name = <<"user">>, attrs = [{<<"affiliation">>, AffBin}],
                     children = [#xmlcdata{ content = UserBin }] }
             || {UserBin, AffBin} <- bin_aff_users(AffUsers)],
    escalus_stanza:to(escalus_stanza:iq_set(?NS_MUC_LIGHT_AFFILIATIONS, Items), room_bin_jid(Room)).

clear_db() ->
    rpc(mim(), mod_muc_light_db_backend, force_clear, []).

-spec ver(Int :: integer()) -> binary().
ver(Int) ->
  <<"ver-", (list_to_binary(integer_to_list(Int)))/binary>>.

-spec set_mod_config(K :: atom(), V :: any(), Host :: binary()) -> ok.
set_mod_config(K, V, Host) ->
        true = rpc(gen_mod, set_module_opt_by_subhost, [Host, mod_muc_light, K, V]).
