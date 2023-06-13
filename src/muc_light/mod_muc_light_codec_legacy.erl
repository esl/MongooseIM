%%%----------------------------------------------------------------------
%%% File    : mod_muc_light_codec_legacy.erl
%%% Author  : Piotr Nosek <piotr.nosek@erlang-solutions.com>
%%% Purpose : MUC Light codec for XEP-0045 compatibility
%%% Created : 27 Oct 2015 by Piotr Nosek <piotr.nosek@erlang-solutions.com>
%%%----------------------------------------------------------------------

-module(mod_muc_light_codec_legacy).
-author('piotr.nosek@erlang-solutions.com').

-behaviour(mod_muc_light_codec_backend).

%% API
-export([decode/4, encode/5, encode_error/5]).

-include("mongoose.hrl").
-include("jlib.hrl").
-include("mod_muc_light.hrl").

%%====================================================================
%% API
%%====================================================================

-spec decode(From :: jid:jid(), To :: jid:jid(),
             Stanza :: jlib:iq() | exml:element(),
             Acc :: mongoose_acc:t()) -> mod_muc_light_codec_backend:decode_result().
decode(_From, #jid{ luser = ToU } = _To, #xmlel{ name = <<"presence">> } = Stanza, _Acc)
  when ToU =/= <<>> ->
    case {exml_query:path(Stanza, [{element, <<"x">>}, {attr, <<"xmlns">>}]),
         exml_query:attr(Stanza, <<"type">>)} of
        {?NS_MUC, Available} when Available =:= undefined orelse
                                  Available =:= <<"available">> -> {ok, {set, #create{}}};
        _ -> ignore
    end;
decode(_From, #jid{ lresource = Resource }, _Stanza, _Acc) when Resource =/= <<>> ->
    {error, bad_request};
decode(_From, _To, #xmlel{ name = <<"message">> } = Stanza, _Acc) ->
    decode_message(Stanza);
decode(From, _To, #xmlel{ name = <<"iq">> } = Stanza, _Acc) ->
    decode_iq(From, jlib:iq_query_info(Stanza));
decode(From, _To, #iq{} = IQ, _Acc) ->
    decode_iq(From, IQ);
decode(_, _, _, _Acc) ->
    {error, bad_request}.

-spec encode(Request :: muc_light_encode_request(),
             OriginalSender :: jid:jid(), RoomUS :: jid:jid(),
             HandleFun :: mod_muc_light_codec_backend:encoded_packet_handler(),
             Acc :: mongoose_acc:t()) -> mongoose_acc:t().
encode({#msg{} = Msg, AffUsers}, Sender, RoomBareJid, HandleFun, Acc) ->
    US = jid:to_lus(Sender),
    Aff = get_sender_aff(AffUsers, US),
    FromNick = jid:to_bare_binary(Sender),
    {RoomJID, RoomBin} = jids_from_room_with_resource(RoomBareJid, FromNick),
    Attrs = [
             {<<"id">>, Msg#msg.id},
             {<<"type">>, <<"groupchat">>},
             {<<"from">>, RoomBin}
            ],
    MsgForArch = #xmlel{ name = <<"message">>, attrs = Attrs, children = Msg#msg.children },
    TS = mongoose_acc:timestamp(Acc),
    EventData = #{from_nick => FromNick,
                  from_jid => Sender,
                  room_jid => RoomBareJid,
                  affiliation => Aff,
                  role => mod_muc_light_utils:light_aff_to_muc_role(Aff),
                  timestamp => TS},
    HostType = mod_muc_light_utils:acc_to_host_type(Acc),
    Packet1 = #xmlel{ children = Children }
        = mongoose_hooks:filter_room_packet(HostType, MsgForArch, EventData),
    lists:foreach(
      fun({{U, S}, _}) ->
              send_to_aff_user(RoomJID, U, S, <<"message">>, Attrs, Children, HandleFun)
      end, AffUsers),
    mongoose_acc:update_stanza(#{from_jid => RoomJID,
                                 to_jid => RoomBareJid,
                                 element => Packet1}, Acc);
encode(OtherCase, Sender, RoomBareJid, HandleFun, Acc) ->
    {RoomJID, RoomBin} = jids_from_room_with_resource(RoomBareJid, <<>>),
    case encode_meta(OtherCase, RoomJID, Sender, HandleFun, Acc) of
        {iq_reply, ID} ->
            IQRes = make_iq_result(RoomBin, jid:to_binary(Sender), ID, <<>>, undefined),
            HandleFun(RoomJID, Sender, IQRes);
        {iq_reply, XMLNS, Els, ID} ->
            IQRes = make_iq_result(RoomBin, jid:to_binary(Sender), ID, XMLNS, Els),
            HandleFun(RoomJID, Sender, IQRes);
        noreply ->
            Acc
    end.

-spec encode_error(
        ErrMsg :: tuple(), OrigFrom :: jid:jid(), OrigTo :: jid:jid(),
        OrigPacket :: exml:element(), Acc :: mongoose_acc:t()) ->
    mongoose_acc:t().
encode_error(_, OrigFrom, OrigTo, #xmlel{ name = <<"presence">> } = OrigPacket, Acc) ->
    %% The only error case for valid presence is registration-required for room creation
    X = #xmlel{ name = <<"x">>, attrs = [{<<"xmlns">>, ?NS_MUC}] },
    mod_muc_light_codec_backend:encode_error({error, registration_required}, [X], OrigFrom, OrigTo,
                                     OrigPacket, Acc);
encode_error(ErrMsg, OrigFrom, OrigTo, OrigPacket, Acc) ->
    mod_muc_light_codec_backend:encode_error(ErrMsg, [], OrigFrom, OrigTo, OrigPacket, Acc).

%%====================================================================
%% Message decoding
%%====================================================================

-spec decode_message(Packet :: exml:element()) ->
    {ok, muc_light_packet()} | {error, bad_request} | ignore.
decode_message(#xmlel{ attrs = Attrs, children = Children }) ->
    decode_message_by_type(lists:keyfind(<<"type">>, 1, Attrs),
                           lists:keyfind(<<"id">>, 1, Attrs), Children).

-spec decode_message_by_type(Type :: {binary(), binary()} | false,
                             Id :: {binary(), binary()} | false,
                             Children :: [jlib:xmlch()]) ->
    {ok, msg() | {set, mod_muc_light_room_config:kv()}} | {error, bad_request} | ignore.
decode_message_by_type({_, <<"groupchat">>}, _, [#xmlel{ name = <<"subject">> } = SubjectEl]) ->
    {ok, {set, #config{ raw_config = [{<<"subject">>, exml_query:cdata(SubjectEl)}] }}};
decode_message_by_type({_, <<"groupchat">>}, Id, Children) ->
    {ok, #msg{ children = Children, id = ensure_id(Id) }};
decode_message_by_type({_, <<"error">>}, _, _) ->
    ignore;
decode_message_by_type(_, _, _) ->
    {error, bad_request}.

-spec ensure_id(Id :: {binary(), binary()} | false) -> binary().
ensure_id(false) -> mongoose_bin:gen_from_timestamp();
ensure_id({_, Id}) -> Id.

%%====================================================================
%% IQ decoding
%%====================================================================

-spec decode_iq(From :: jid:jid(), IQ :: jlib:iq()) ->
    {ok, muc_light_packet() | muc_light_disco() | jlib:iq()} | {error, bad_request} | ignore.
decode_iq(_From, #iq{ xmlns = ?NS_MUC_OWNER, type = get, sub_el = _QueryEl, id = ID }) ->
    {ok, {get, #config{ id = ID }}};
decode_iq(From, IQ = #iq{ xmlns = ?NS_MUC_OWNER, type = set, sub_el = QueryEl, id = ID }) ->
    case exml_query:subelement(QueryEl, <<"destroy">>) of
        undefined ->
            case parse_config_form(QueryEl) of
                {ok, RawConfig} ->
                    {ok, {set, #config{ id = ID, raw_config = RawConfig }}};
                {error, Reason} ->
                    ?LOG_WARNING(#{what => muc_parse_config_failed,
                                   from_jid => jid:to_binary(From), iq => IQ, reason => Reason}),
                    {error, bad_request}
            end;
        _ ->
            {ok, {set, #destroy{ id = ID }}}
    end;
decode_iq(_From, #iq{ xmlns = ?NS_MUC_ADMIN, type = get, sub_el = _QueryEl, id = ID }) ->
    {ok, {get, #affiliations{ id = ID }}};
decode_iq(From, IQ = #iq{ xmlns = ?NS_MUC_ADMIN, type = set, sub_el = QueryEl, id = ID }) ->
    try parse_aff_users(exml_query:subelements(QueryEl, <<"item">>)) of
        {ok, AffUsers} ->
            {ok, {set, #affiliations{ id = ID, aff_users = AffUsers }}}
    catch Class:Reason:Stacktrace ->
            ?LOG_WARNING(#{what => muc_parse_aff_users_failed,
                           from_jid => jid:to_binary(From), iq => IQ,
                           class => Class, reason => Reason, stacktrace => Stacktrace}),
            {error, bad_request}
    end;
decode_iq(_From, #iq{ xmlns = ?NS_PRIVACY, type = get, id = ID }) ->
    {ok, {get, #blocking{ id = ID }}};
decode_iq(From, IQ = #iq{ xmlns = ?NS_PRIVACY, type = set,
               sub_el = #xmlel{ children = Lists }, id = ID }) ->
    case lists:keyfind([{<<"name">>, ?NS_MUC_LIGHT}], #xmlel.attrs, Lists) of
        false ->
            ignore;
        List ->
            try parse_blocking_list(exml_query:subelements(List, <<"item">>)) of
                {ok, BlockingList} ->
                    {ok, {set, #blocking{ id = ID, items = BlockingList }}}
            catch Class:Reason:Stacktrace ->
                    ?LOG_WARNING(#{what => muc_parse_blocking_list_failed,
                                   from_jid => jid:to_binary(From), iq => IQ,
                                   class => Class, reason => Reason, stacktrace => Stacktrace})
            end
    end;
decode_iq(_From, #iq{ xmlns = ?NS_DISCO_ITEMS, type = get, id = ID} = IQ) ->
    {ok, {get, #disco_items{ id = ID, rsm = jlib:rsm_decode(IQ) }}};
decode_iq(_From, #iq{ xmlns = ?NS_DISCO_INFO, type = get, id = ID}) ->
    {ok, {get, #disco_info{ id = ID }}};
decode_iq(_From, #iq{ type = error }) ->
    ignore;
decode_iq(_From, #iq{} = IQ) ->
    {ok, IQ}.

%% ------------------ Parsers ------------------

-spec parse_config_form(exml:element()) -> {ok, [{binary(), binary()}]} | {error, binary()}.
parse_config_form(QueryEl) ->
    case mongoose_data_forms:find_and_parse_form(QueryEl) of
        #{type := <<"submit">>, kvs := KVs} ->
            {ok, [{K, V} || {K, [V]} <- maps:to_list(KVs)]};
        #{} ->
            {error, <<"Invalid form type">>};
        {error, Msg} ->
            {error, Msg}
    end.

-spec parse_aff_users(Els :: [jlib:xmlch()]) -> {ok, aff_users()}.
parse_aff_users(Els) ->
    parse_aff_users(Els, []).

-spec parse_aff_users(Els :: [jlib:xmlch()], AffUsersAcc :: aff_users()) -> {ok, aff_users()}.
parse_aff_users([], AffUsersAcc) ->
    {ok, AffUsersAcc};
parse_aff_users([Item | RItemsEls], AffUsersAcc) ->
    AffBin = exml_query:attr(Item, <<"affiliation">>),
    JIDBin = exml_query:attr(Item, <<"jid">>),
    #jid{} = JID = jid:from_binary(JIDBin),
    Aff = mod_muc_light_utils:b2aff(AffBin),
    parse_aff_users(RItemsEls, [{jid:to_lus(JID), Aff} | AffUsersAcc]).

-spec parse_blocking_list(Els :: [jlib:xmlch()]) -> {ok, [blocking_item()]}.
parse_blocking_list(ItemsEls) ->
    parse_blocking_list(ItemsEls, []).

-spec parse_blocking_list(Els :: [jlib:xmlch()], ItemsAcc :: [blocking_item()]) ->
    {ok, [blocking_item()]}.
parse_blocking_list([], ItemsAcc) ->
    {ok, ItemsAcc};
parse_blocking_list([Item | RItemsEls], ItemsAcc) ->
    JIDBin = exml_query:attr(Item, <<"value">>),
    ActionBin = exml_query:attr(Item, <<"action">>),
    #jid{} = JID = jid:from_binary(JIDBin),
    Action = b2action(ActionBin),
    {What, Who} = case {JID#jid.luser =:= <<>>, JID#jid.lresource =:= <<>>} of
                      {false, true} ->
                          {room, JID};
                      {true, false} ->
                          {user, jid:from_binary(JID#jid.lresource)}
                  end,
    parse_blocking_list(RItemsEls, [{What, Action, jid:to_lus(Who)} | ItemsAcc]).

%%====================================================================
%% Encoding
%%====================================================================

-spec encode_meta(Request :: muc_light_encode_request(), RoomJID :: jid:jid(),
                  SenderJID :: jid:jid(),
                  HandleFun :: mod_muc_light_codec_backend:encoded_packet_handler(),
                  Acc :: mongoose_acc:t()) ->
    {iq_reply, ID :: binary()} |
    {iq_reply, XMLNS :: binary(), Els :: [jlib:xmlch()], ID :: binary()} |
    noreply.
encode_meta({get, #disco_info{ id = ID }}, RoomJID, SenderJID, _HandleFun, Acc) ->
    HostType = mod_muc_light_utils:acc_to_host_type(Acc),
    IdentityXML = mongoose_disco:identities_to_xml([identity()]),
    FeatureXML = mongoose_disco:get_muc_features(HostType, SenderJID, RoomJID, <<>>, <<>>,
                                                 [?NS_MUC]),
    DiscoEls = IdentityXML ++ FeatureXML,
    {iq_reply, ?NS_DISCO_INFO, DiscoEls, ID};
encode_meta({get, #disco_items{ rooms = Rooms, id = ID, rsm = RSMOut }},
          _RoomJID, _SenderJID, _HandleFun, _Acc) ->
    DiscoEls = [ #xmlel{ name = <<"item">>,
                         attrs = [{<<"jid">>, <<RoomU/binary, $@, RoomS/binary>>},
                                  {<<"name">>, RoomName}] }
                 || {{RoomU, RoomS}, RoomName, _RoomVersion} <- Rooms ],
    {iq_reply, ?NS_DISCO_ITEMS, jlib:rsm_encode(RSMOut) ++ DiscoEls, ID};
encode_meta({get, #config{} = Config}, _RoomJID, _SenderJID, _HandleFun, _Acc) ->
    Fields = [#{var => K, type => <<"text-single">>, values => [V]}
              || {K, V} <- Config#config.raw_config],
    XEl = mongoose_data_forms:form(#{title => <<"Configuration form for the room">>,
                                     ns => <<"http://jabber.org/protocol/muc#roomconfig">>,
                                     fields => Fields}),
    {iq_reply, ?NS_MUC_OWNER, [XEl], Config#config.id};
encode_meta({get, #affiliations{} = Affs}, _RoomJID, _SenderJID, _HandleFun, _Acc) ->
    AffEls = [ aff_user_to_item(AffUser) || AffUser <- Affs#affiliations.aff_users ],
    {iq_reply, ?NS_MUC_ADMIN, AffEls, Affs#affiliations.id};
encode_meta({set, #affiliations{} = Affs, OldAffUsers, NewAffUsers},
            RoomJID, SenderJID, HandleFun, _Acc) ->
    bcast_aff_messages(RoomJID, OldAffUsers, NewAffUsers, SenderJID,
                       Affs#affiliations.aff_users, HandleFun),
    {iq_reply, Affs#affiliations.id};
encode_meta({get, #blocking{} = Blocking}, RoomJID, _SenderJID, _HandleFun, Acc) ->
    HostType = mod_muc_light_utils:acc_to_host_type(Acc),
    ServerHost = mod_muc_light_utils:room_jid_to_server_host(RoomJID),
    MUCHost = mongoose_subdomain_utils:get_fqdn(mod_muc_light:subdomain_pattern(HostType), ServerHost),
    BlockingEls = [ blocking_to_el(BlockingItem, MUCHost)
                    || BlockingItem <- Blocking#blocking.items ],
    Blocklist = #xmlel{ name = <<"list">>, attrs = [{<<"name">>, ?NS_MUC_LIGHT}],
                        children = BlockingEls },
    {iq_reply, ?NS_PRIVACY, [Blocklist], Blocking#blocking.id};
encode_meta({set, #blocking{ id = ID }}, _RoomJID, _SenderJID, _HandleFun, _Acc) ->
    {iq_reply, ID};
encode_meta({set, #create{} = Create, _UniqueRequested}, RoomJID, _SenderJID, HandleFun, _Acc) ->
    [{{ToU, ToS}, CreatorAff}] = Create#create.aff_users,
    ToBin = jid:to_binary({ToU, ToS, <<>>}),
    {From, FromBin} = jids_from_room_with_resource(RoomJID, ToBin),
    Attrs = [{<<"from">>, FromBin}],
    {AffBin, RoleBin} = case CreatorAff of
                            owner -> {<<"owner">>, <<"moderator">>};
                            member -> {<<"member">>, <<"participant">>}
                        end,
    NotifEls = [ #xmlel{ name = <<"item">>,
                         attrs = [{<<"affiliation">>, AffBin}, {<<"role">>, RoleBin}]},
                 status(<<"110">>), status(<<"201">>) ],
    Children = envelope(?NS_MUC_USER, NotifEls),

    send_to_aff_user(From, ToU, ToS, <<"presence">>, Attrs, Children, HandleFun),
    noreply;
encode_meta({set, #destroy{ id = ID }, AffUsers}, RoomJID, _SenderJID, HandleFun, _Acc) ->
    lists:foreach(
      fun({{U, S}, _}) ->
              FromJID = jid:replace_resource(RoomJID, jid:to_binary({U, S, <<>>})),
              Attrs = [{<<"from">>, jid:to_binary(FromJID)},
                       {<<"type">>, <<"unavailable">>}],
              Children = [ #xmlel{ name = <<"item">>,
                                   attrs = [{<<"affiliation">>, <<"none">>},
                                            {<<"role">>, <<"none">>}] },
                           #xmlel{ name = <<"destroy">> } ],
              send_to_aff_user(FromJID, U, S, <<"presence">>, Attrs,
                               envelope(?NS_MUC_USER, Children), HandleFun)
      end, AffUsers),

    {iq_reply, ID};
encode_meta({set, #config{ raw_config = [{<<"subject">>, Subject}], id = ID }, AffUsers},
          RoomJID, _SenderJID, HandleFun, _Acc) ->
    Attrs = [
             {<<"id">>, ID},
             {<<"type">>, <<"groupchat">>},
             {<<"from">>, jid:to_binary(RoomJID)}
            ],
    SubjectEl = #xmlel{ name = <<"subject">>, children = [ #xmlcdata{ content = Subject } ] },
    lists:foreach(
      fun({{U, S}, _}) ->
              send_to_aff_user(RoomJID, U, S, <<"message">>, Attrs, [SubjectEl], HandleFun)
      end, AffUsers),
    noreply;
encode_meta({set, #config{} = Config, AffUsers}, RoomJID, _SenderJID, HandleFun, _Acc) ->
    Attrs = [{<<"id">>, Config#config.id},
             {<<"from">>, jid:to_binary(RoomJID)},
             {<<"type">>, <<"groupchat">>}],
    ConfigNotif = envelope(?NS_MUC_USER, [status(<<"104">>)]),
    lists:foreach(
      fun({{U, S}, _}) ->
              send_to_aff_user(RoomJID, U, S, <<"message">>, Attrs, ConfigNotif, HandleFun)
      end, AffUsers),

    {iq_reply, Config#config.id}.

%% --------------------------- Helpers ---------------------------

-spec identity() -> mongoose_disco:identity().
identity() ->
    #{category => <<"conference">>, type => <<"text">>, name => <<"MUC Light (legacy)">>}.

-spec aff_user_to_item(aff_user()) -> exml:element().
aff_user_to_item({User, Aff}) ->
    UserBin = jid:to_binary(User),
    {RoleBin, NickEl} = case Aff of
                            owner -> {<<"moderator">>, [{<<"nick">>, UserBin}]};
                            member -> {<<"participant">>, [{<<"nick">>, UserBin}]};
                            none -> {<<"none">>, []}
                        end,
    #xmlel{ name = <<"item">>,
            attrs = [{<<"affiliation">>, mod_muc_light_utils:aff2b(Aff)},
                     {<<"jid">>, UserBin},
                     {<<"role">>, RoleBin} | NickEl] }.

-spec blocking_to_el(BlockingItem :: blocking_item(), Service :: binary()) -> exml:element().
blocking_to_el({What, Action, {WhoU, WhoS}}, Service) ->
    WhoBin = jid:to_binary({WhoU, WhoS, <<>>}),
    Value = case What of
                room -> WhoBin;
                user -> <<Service/binary, $/, WhoBin/binary>>
            end,
    #xmlel{ name = <<"item">>,
            attrs = [
                     {<<"type">>, <<"jid">>},
                     {<<"value">>, Value},
                     {<<"action">>, action2b(Action)},
                     {<<"order">>, <<"1">>}
                    ] }.

-spec envelope(XMLNS :: binary(), Children :: [jlib:xmlch()]) -> [jlib:xmlch()].
envelope(XMLNS, Children) ->
    [ #xmlel{ name = <<"x">>, attrs = [{<<"xmlns">>, XMLNS}], children = Children } ].

-spec bcast_aff_messages(Room :: jid:jid(), OldAffUsers :: aff_users(),
                         NewAffUsers :: aff_users(), SenderJID :: jid:jid(),
                         ChangedAffUsers :: aff_users(),
                         HandleFun :: mod_muc_light_codec_backend:encoded_packet_handler()) -> ok.
bcast_aff_messages(_, [], [], _, _, _) ->
    ok;
bcast_aff_messages(Room, [{User, _} | ROldAffUsers], [], SenderJID, ChangedAffUsers, HandleFun) ->
    msg_to_leaving_user(Room, User, HandleFun),
    bcast_aff_messages(Room, ROldAffUsers, [], SenderJID, ChangedAffUsers, HandleFun);
bcast_aff_messages(Room, [{{ToU, ToS} = User, _} | ROldAffUsers], [{User, _} | RNewAffUsers],
                   SenderJID, ChangedAffUsers, HandleFun) ->
    lists:foreach(
      fun({{ChangedU, ChangedS}, NewAff} = ChangedAffUser) ->
              ChangedUserBin = jid:to_binary({ChangedU, ChangedS, <<>>}),
              {From, FromBin} = jids_from_room_with_resource(Room,
                                                             ChangedUserBin),
              Attrs0 = [{<<"from">>, FromBin}],
              ElToEnvelope0 = aff_user_to_item(ChangedAffUser),
              {Attrs, ElsToEnvelope} = case NewAff of
                                           none -> {[{<<"type">>, <<"unavailable">>} | Attrs0],
                                                    [ElToEnvelope0, status(<<"321">>)]};
                                           _ -> {Attrs0, [ElToEnvelope0]}
                                       end,
              Children = envelope(?NS_MUC_USER, ElsToEnvelope),
              send_to_aff_user(From, ToU, ToS, <<"presence">>, Attrs, Children, HandleFun)
      end, ChangedAffUsers),
    bcast_aff_messages(Room, ROldAffUsers, RNewAffUsers, SenderJID, ChangedAffUsers, HandleFun);
bcast_aff_messages(Room, [{User1, _} | ROldAffUsers], [{User2, _} | _] = NewAffUsers,
                   SenderJID, ChangedAffUsers, HandleFun) when User1 < User2 ->
    msg_to_leaving_user(Room, User1, HandleFun),
    bcast_aff_messages(Room, ROldAffUsers, NewAffUsers, SenderJID, ChangedAffUsers, HandleFun);
bcast_aff_messages(Room, OldAffUsers, [{{ToU, ToS}, _} | RNewAffUsers],
                   SenderJID, ChangedAffUsers, HandleFun) ->
    InviterBin = jid:to_binary({SenderJID#jid.luser, SenderJID#jid.lserver, <<>>}),
    RoomBin = jid:to_binary(jid:to_lower(Room)),
    InviteEl = #xmlel{ name = <<"invite">>,
                       attrs = [{<<"from">>, InviterBin}] },
    NotifForNewcomer = envelope(?NS_MUC_USER, [InviteEl]),
    send_to_aff_user(Room, ToU, ToS, <<"message">>, [{<<"from">>, RoomBin}],
                     NotifForNewcomer, HandleFun),
    bcast_aff_messages(Room, OldAffUsers, RNewAffUsers, SenderJID, ChangedAffUsers, HandleFun).

-spec msg_to_leaving_user(Room :: jid:jid(), User :: jid:simple_bare_jid(),
                          HandleFun :: mod_muc_light_codec_backend:encoded_packet_handler()) -> ok.
msg_to_leaving_user(Room, {ToU, ToS} = User, HandleFun) ->
    UserBin = jid:to_binary({ToU, ToS, <<>>}),
    {From, FromBin} = jids_from_room_with_resource(Room, UserBin),
    Attrs = [{<<"from">>, FromBin},
             {<<"type">>, <<"unavailable">>}],
    NotifForLeaving = envelope(?NS_MUC_USER, [ aff_user_to_item({User, none}), status(<<"321">>) ]),
    send_to_aff_user(From, ToU, ToS, <<"presence">>, Attrs, NotifForLeaving, HandleFun).

-spec send_to_aff_user(From :: jid:jid(), ToU :: jid:luser(), ToS :: jid:lserver(),
                       Name :: binary(), Attrs :: [{binary(), binary()}],
                       Children :: [jlib:xmlch()],
                       HandleFun :: mod_muc_light_codec_backend:encoded_packet_handler()) -> ok.
send_to_aff_user(From, ToU, ToS, Name, Attrs, Children, HandleFun) ->
    To = jid:make_noprep(ToU, ToS, <<>>),
    ToBin = jid:to_binary({ToU, ToS, <<>>}),
    Packet = #xmlel{ name = Name, attrs = [{<<"to">>, ToBin} | Attrs],
                     children = Children },
    HandleFun(From, To, Packet).

-spec jids_from_room_with_resource(Room :: jid:jid(), binary()) ->
    {jid:jid(), binary()}.
jids_from_room_with_resource(RoomJID, Resource) ->
    From = jid:replace_resource(RoomJID, Resource),
    FromBin = jid:to_binary(jid:to_lower(From)),
    {From, FromBin}.

-spec make_iq_result(FromBin :: binary(), ToBin :: binary(), ID :: binary(),
                     XMLNS :: binary(), Els :: [jlib:xmlch()] | undefined) -> exml:element().
make_iq_result(FromBin, ToBin, ID, XMLNS, Els) ->
    Attrs = [
             {<<"from">>, FromBin},
             {<<"to">>, ToBin},
             {<<"id">>, ID},
             {<<"type">>, <<"result">>}
            ],
    Query = make_query_el(XMLNS, Els),
    #xmlel{ name = <<"iq">>, attrs = Attrs, children = Query }.

-spec make_query_el(binary(), [jlib:xmlch()] | undefined) -> [exml:element()].
make_query_el(_, undefined) ->
    [];
make_query_el(XMLNS, Els) ->
    [#xmlel{ name = <<"query">>, attrs = [{<<"xmlns">>, XMLNS}], children = Els }].

-spec status(Code :: binary()) -> exml:element().
status(Code) -> #xmlel{ name = <<"status">>, attrs = [{<<"code">>, Code}] }.

%%====================================================================
%% Common helpers and internal functions
%%====================================================================

-spec b2action(ActionBin :: binary()) -> atom().
b2action(<<"allow">>) -> allow;
b2action(<<"deny">>) -> deny.

-spec action2b(Action :: atom()) -> binary().
action2b(allow) -> <<"allow">>;
action2b(deny) -> <<"deny">>.

get_sender_aff(Users, US) ->
    case lists:keyfind(US, 1, Users) of
        {US, Aff} -> Aff;
        _ -> undefined
    end.
