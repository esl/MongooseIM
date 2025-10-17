%%%----------------------------------------------------------------------
%%% File    : mod_muc_light_codec_modern.erl
%%% Author  : Piotr Nosek <piotr.nosek@erlang-solutions.com>
%%% Purpose : MUC Light codec for modern syntax
%%% Created : 29 Sep 2015 by Piotr Nosek <piotr.nosek@erlang-solutions.com>
%%%----------------------------------------------------------------------

-module(mod_muc_light_codec_modern).
-author('piotr.nosek@erlang-solutions.com').

-behaviour(mod_muc_light_codec_backend).

%% API
-export([decode/4, encode/5, encode_error/5]).

-include("mongoose.hrl").
-include("jlib.hrl").
-include("mod_muc_light.hrl").

-define(HANDLE_PARSE_ERROR(Function, From, IQ),
        {error, Reason} ->
            ?LOG_WARNING(#{what => muc_parse_failed, parser => Function,
                           iq => IQ, from_jid => jid:to_binary(From),
                           reason => Reason}),
            {error, bad_request}
        catch Class:Reason:Stacktrace ->
            ?LOG_WARNING(#{what => muc_parse_failed, parser => Function,
                           iq => IQ, from_jid => jid:to_binary(From),
                           class => Class, reason => Reason, stacktrace => Stacktrace}),
            {error, bad_request}).

-type bad_request() :: bad_request | {bad_request, binary()}.

%%====================================================================
%% API
%%====================================================================

-spec decode(From :: jid:jid(), To :: jid:jid(), Stanza :: exml:element(),
             Acc :: mongoose_acc:t()) ->
    mod_muc_light_codec_backend:decode_result().
decode(_From, #jid{ lresource = Resource }, _Stanza, _Acc) when Resource =/= <<>> ->
    {error, {bad_request, <<"Resource expected to be empty">>}};
decode(_From, _To, #xmlel{ name = <<"message">> } = Stanza, _Acc) ->
    decode_message(Stanza);
decode(From, _To, #xmlel{ name = <<"iq">> } = Stanza, _Acc) ->
    decode_iq(From, jlib:iq_query_info(Stanza));
decode(_, _, _, _) ->
    {error, {bad_request, <<"Failed to decode unknown format">>}}.

-spec encode(Request :: muc_light_encode_request(), OriginalSender :: jid:jid(),
             RoomJID :: jid:jid(),
             HandleFun :: mod_muc_light_codec_backend:encoded_packet_handler(),
             Acc :: mongoose_acc:t()) -> mongoose_acc:t().
encode({#msg{} = Msg, AffUsers}, Sender, RoomBareJid, HandleFun, Acc) ->
    US = jid:to_lus(Sender),
    FromNick = jid:to_binary(US),
    Aff = get_sender_aff(AffUsers, US),
    {RoomJID, RoomBin} = jids_from_room_with_resource(RoomBareJid, FromNick),
    Attrs = #{ <<"id">> => Msg#msg.id,
               <<"type">> => <<"groupchat">>,
               <<"from">> => RoomBin },
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
              msg_to_aff_user_online(RoomJID, U, S, Attrs, Children, HandleFun)
      end, AffUsers),
    mongoose_acc:update_stanza(#{from_jid => RoomJID,
                                 to_jid => RoomBareJid,
                                 element => Packet1}, Acc);
encode(OtherCase, Sender, RoomBareJid, HandleFun, Acc) ->
    {RoomJID, RoomBin} = jids_from_room_with_resource(RoomBareJid, <<>>),
    case encode_iq(OtherCase, Sender, RoomJID, RoomBin, HandleFun, Acc) of
        {reply, ID} ->
            IQRes = make_iq_result(RoomBin, jid:to_binary(Sender), ID, <<>>, undefined),
            HandleFun(RoomJID, Sender, IQRes);
        {reply, XMLNS, Els, ID} ->
            IQRes = make_iq_result(RoomBin, jid:to_binary(Sender), ID, XMLNS, Els),
            HandleFun(RoomJID, Sender, IQRes);
        {reply, FromJID, FromBin, XMLNS, Els, ID} ->
            IQRes = make_iq_result(FromBin, jid:to_binary(Sender), ID, XMLNS, Els),
            HandleFun(FromJID, Sender, IQRes)
    end.

get_sender_aff(Users, US) ->
    case lists:keyfind(US, 1, Users) of
        {US, Aff} -> Aff;
        _ -> undefined
    end.

-spec encode_error(
        ErrMsg :: tuple(), OrigFrom :: jid:jid(), OrigTo :: jid:jid(),
        OrigPacket :: exml:element(), Acc :: mongoose_acc:t()) ->
    mongoose_acc:t().
encode_error(ErrMsg, OrigFrom, OrigTo, OrigPacket, Acc) ->
    mod_muc_light_codec_backend:encode_error(ErrMsg, [], OrigFrom, OrigTo, OrigPacket, Acc).

%%====================================================================
%% Message decoding
%%====================================================================

-spec decode_message(Packet :: exml:element()) ->
    {ok, muc_light_packet()} | {error, bad_request} | ignore.
decode_message(#xmlel{ attrs = Attrs, children = Children }) ->
    decode_message_by_type(Attrs, Children).

-spec decode_message_by_type(exml:attrs(), Children :: [exml:child()]) ->
    {ok, msg()} | {error, bad_request} | ignore.
decode_message_by_type(#{<<"type">> := <<"groupchat">>} = Attrs, Children) ->
    {ok, #msg{ children = Children, id = ensure_id(Attrs) }};
decode_message_by_type(#{<<"type">> := <<"error">>}, _) ->
    ignore;
decode_message_by_type(_, _) ->
    {error, bad_request}.

-spec ensure_id(exml:attrs()) -> binary().
ensure_id(#{<<"id">> := Id}) -> Id;
ensure_id(_) -> mongoose_bin:gen_from_timestamp().

%%====================================================================
%% IQ decoding
%%====================================================================

-spec decode_iq(From :: jid:jid(), IQ :: jlib:iq()) ->
    {ok, muc_light_packet() | muc_light_disco() | jlib:iq()} | {error, bad_request()} | ignore.
decode_iq(_From, #iq{ xmlns = ?NS_MUC_LIGHT_CONFIGURATION, type = get,
                      sub_el = QueryEl, id = ID }) ->
    Version = exml_query:path(QueryEl, [{element, <<"version">>}, cdata], <<>>),
    {ok, {get, #config{ id = ID, prev_version = Version }}};
decode_iq(From, IQ = #iq{ xmlns = ?NS_MUC_LIGHT_CONFIGURATION, type = set,
               sub_el = #xmlel{ children = QueryEls }, id = ID }) ->
    try parse_config(QueryEls) of
        {ok, RawConfig} ->
            {ok, {set, #config{ id = ID, raw_config = RawConfig }}};
        ?HANDLE_PARSE_ERROR(parse_config, From, IQ)
    end;
decode_iq(_From, #iq{ xmlns = ?NS_MUC_LIGHT_AFFILIATIONS, type = get,
                      sub_el = QueryEl, id = ID }) ->
    Version = exml_query:path(QueryEl, [{element, <<"version">>}, cdata], <<>>),
    {ok, {get, #affiliations{ id = ID, prev_version = Version }}};
decode_iq(From, IQ = #iq{ xmlns = ?NS_MUC_LIGHT_AFFILIATIONS, type = set,
               sub_el = #xmlel{ children = QueryEls }, id = ID }) ->
    try parse_aff_users(QueryEls) of
        {ok, AffUsers} ->
            {ok, {set, #affiliations{ id = ID, aff_users = AffUsers }}};
        ?HANDLE_PARSE_ERROR(parse_aff_users, From, IQ)
    end;
decode_iq(_From, #iq{ xmlns = ?NS_MUC_LIGHT_INFO, type = get, sub_el = QueryEl, id = ID }) ->
    Version = exml_query:path(QueryEl, [{element, <<"version">>}, cdata], <<>>),
    {ok, {get, #info{
                  id = ID,
                  prev_version = Version
                 }}};
decode_iq(_From, #iq{ xmlns = ?NS_MUC_LIGHT_BLOCKING, type = get, id = ID }) ->
    {ok, {get, #blocking{ id = ID }}};
decode_iq(From, IQ = #iq{ xmlns = ?NS_MUC_LIGHT_BLOCKING, type = set,
               sub_el = #xmlel{ children = QueryEls }, id = ID }) ->
    try parse_blocking_list(QueryEls) of
        {ok, BlockingList} ->
            {ok, {set, #blocking{ id = ID, items = BlockingList }}};
        ?HANDLE_PARSE_ERROR(parse_blocking_list, From, IQ)
    end;
decode_iq(From, IQ = #iq{ xmlns = ?NS_MUC_LIGHT_CREATE, type = set, sub_el = QueryEl, id = ID }) ->
    ConfigEl = exml_query:path(QueryEl, [{element, <<"configuration">>}]),
    OccupantsEl = exml_query:path(QueryEl, [{element, <<"occupants">>}]),
    try parse_config(safe_get_children(ConfigEl)) of
        {ok, RawConfig} ->
            try parse_aff_users(safe_get_children(OccupantsEl)) of
                {ok, AffUsers} ->
                   {ok, {set, #create{ id = ID, raw_config = RawConfig, aff_users = AffUsers }}};
                ?HANDLE_PARSE_ERROR(parse_aff_users, From, IQ)
            end;
        ?HANDLE_PARSE_ERROR(parse_config, From, IQ)
    end;
decode_iq(_From, #iq{ xmlns = ?NS_MUC_LIGHT_DESTROY, type = set, id = ID }) ->
    {ok, {set, #destroy{ id = ID }}};
decode_iq(_From, #iq{ xmlns = ?NS_DISCO_ITEMS, type = get, id = ID} = IQ) ->
    {ok, {get, #disco_items{ id = ID, rsm = jlib:rsm_decode(IQ) }}};
decode_iq(_From, #iq{ xmlns = ?NS_DISCO_INFO, type = get, id = ID}) ->
    {ok, {get, #disco_info{ id = ID }}};
decode_iq(_From, #iq{ type = error }) ->
    ignore;
decode_iq(_From, #iq{} = IQ) ->
    {ok, IQ};
decode_iq(_, _) ->
    {error, {bad_request, <<"Unknown IQ format">>}}.

%% ------------------ Parsers ------------------

-spec parse_config(Els :: [exml:child()]) -> {ok, mod_muc_light_room_config:binary_kv()}
                                             | {error, bad_request()}.
parse_config(Els) ->
    parse_config(Els, []).

-spec parse_config(Els :: [exml:child()], ConfigAcc :: mod_muc_light_room_config:binary_kv()) ->
    {ok, mod_muc_light_room_config:binary_kv()} | {error, bad_request()}.
parse_config([], ConfigAcc) ->
    {ok, ConfigAcc};
parse_config([#xmlel{ name = <<"version">> } | _], _) ->
    {error, {bad_request, <<"Version element not allowed">>}};
parse_config([#xmlel{ name = Key, children = [ #xmlcdata{ content = Value } ] } | REls],
             ConfigAcc) ->
    parse_config(REls, [{Key, Value} | ConfigAcc]);
parse_config([_ | REls], ConfigAcc) ->
    parse_config(REls, ConfigAcc).

-spec parse_aff_users(Els :: [exml:child()]) ->
    {ok, aff_users()} | {error, bad_request()}.
parse_aff_users(Els) ->
    parse_aff_users(Els, []).

-spec parse_aff_users(Els :: [exml:child()], AffUsersAcc :: aff_users()) ->
    {ok, aff_users()} | {error, bad_request()}.
parse_aff_users([], AffUsersAcc) ->
    {ok, AffUsersAcc};
parse_aff_users([#xmlcdata{} | RItemsEls], AffUsersAcc) ->
    parse_aff_users(RItemsEls, AffUsersAcc);
parse_aff_users([#xmlel{ name = <<"user">>, attrs = #{<<"affiliation">> := AffBin},
                                  children = [ #xmlcdata{ content = JIDBin } ] } | RItemsEls],
                          AffUsersAcc) ->
    #jid{} = JID = jid:from_binary(JIDBin),
    Aff = mod_muc_light_utils:b2aff(AffBin),
    parse_aff_users(RItemsEls, [{jid:to_lus(JID), Aff} | AffUsersAcc]);
parse_aff_users(_, _) ->
    {error, {bad_request, <<"Failed to parse affiliations">>}}.

-spec parse_blocking_list(Els :: [exml:child()]) -> {ok, [blocking_item()]} | {error, bad_request}.
parse_blocking_list(ItemsEls) ->
    parse_blocking_list(ItemsEls, []).

-spec parse_blocking_list(Els :: [exml:child()], ItemsAcc :: [blocking_item()]) ->
    {ok, [blocking_item()]} | {error, bad_request}.
parse_blocking_list([], ItemsAcc) ->
    {ok, ItemsAcc};
parse_blocking_list([#xmlel{ name = WhatBin, attrs = #{<<"action">> := ActionBin},
                             children = [ #xmlcdata{ content = JIDBin } ] } | RItemsEls],
                          ItemsAcc) ->
    #jid{} = JID = jid:from_binary(JIDBin),
    Action = b2action(ActionBin),
    What = b2what(WhatBin),
    parse_blocking_list(RItemsEls, [{What, Action, jid:to_lus(JID)} | ItemsAcc]);
parse_blocking_list(_, _) ->
    {error, bad_request}.

%%====================================================================
%% Encoding
%%====================================================================

encode_iq({get, #disco_info{ id = ID }}, Sender, RoomJID, _RoomBin, _HandleFun, Acc) ->
    HostType = mod_muc_light_utils:acc_to_host_type(Acc),
    IdentityXML = mongoose_disco:identities_to_xml([identity()]),
    FeatureXML = mongoose_disco:get_muc_features(HostType, Sender, RoomJID, <<>>, <<>>,
                                                 [?NS_MUC_LIGHT]),
    DiscoEls = IdentityXML ++ FeatureXML,
    {reply, ?NS_DISCO_INFO, DiscoEls, ID};
encode_iq({get, #disco_items{ rooms = Rooms, id = ID, rsm = RSMOut }},
          _Sender, _RoomJID, _RoomBin, _HandleFun, _Acc) ->
    DiscoEls = [ #xmlel{ name = <<"item">>,
                         attrs = #{<<"jid">> => <<RoomU/binary, $@, RoomS/binary>>,
                                   <<"name">> => RoomName,
                                   <<"version">> => RoomVersion} }
                 || {{RoomU, RoomS}, RoomName, RoomVersion} <- Rooms ],
    {reply, ?NS_DISCO_ITEMS, jlib:rsm_encode(RSMOut) ++ DiscoEls, ID};
encode_iq({get, #config{ prev_version = SameVersion, version = SameVersion, id = ID }},
          _Sender, _RoomJID, _RoomBin, _HandleFun, _Acc) ->
    {reply, ID};
encode_iq({get, #config{} = Config},
          _Sender, _RoomJID, _RoomBin, _HandleFun, _Acc) ->
    ConfigEls = [ kv_to_el(Field) || Field <- [{<<"version">>, Config#config.version}
                                                         | Config#config.raw_config] ],
    {reply, ?NS_MUC_LIGHT_CONFIGURATION, ConfigEls, Config#config.id};
encode_iq({get, #affiliations{ prev_version = SameVersion, version = SameVersion, id = ID }},
          _Sender, _RoomJID, _RoomBin, _HandleFun, _Acc) ->
    {reply, ID};
encode_iq({get, #affiliations{ version = Version } = Affs},
          _Sender, _RoomJID, _RoomBin, _HandleFun, _Acc) ->
    AffEls = [ aff_user_to_el(AffUser) || AffUser <- Affs#affiliations.aff_users ],
    {reply, ?NS_MUC_LIGHT_AFFILIATIONS, [kv_to_el(<<"version">>, Version) | AffEls],
     Affs#affiliations.id};
encode_iq({get, #info{ prev_version = SameVersion, version = SameVersion, id = ID }},
          _Sender, _RoomJID, _RoomBin, _HandleFun, _Acc) ->
    {reply, ID};
encode_iq({get, #info{ version = Version } = Info},
          _Sender, _RoomJID, _RoomBin, _HandleFun, _Acc) ->
    ConfigEls = [ kv_to_el(Field) || Field <- Info#info.raw_config ],
    AffEls = [ aff_user_to_el(AffUser) || AffUser <- Info#info.aff_users ],
    InfoEls = [
               kv_to_el(<<"version">>, Version),
               #xmlel{ name = <<"configuration">>, children = ConfigEls },
               #xmlel{ name = <<"occupants">>, children = AffEls }
              ],
    {reply, ?NS_MUC_LIGHT_INFO, InfoEls, Info#info.id};
encode_iq({set, #affiliations{} = Affs, OldAffUsers, NewAffUsers},
          _Sender, RoomJID, RoomBin, HandleFun, Acc) ->
    Attrs = #{<<"id">> => Affs#affiliations.id,
              <<"type">> => <<"groupchat">>,
              <<"from">> => RoomBin},

    AllAffsEls = [ aff_user_to_el(AffUser) || AffUser <- Affs#affiliations.aff_users ],
    VersionEl = kv_to_el(<<"version">>, Affs#affiliations.version),
    NotifForCurrentNoPrevVersion = [ VersionEl | AllAffsEls ],
    MsgForArch = #xmlel{ name = <<"message">>, attrs = Attrs,
                         children = msg_envelope(?NS_MUC_LIGHT_AFFILIATIONS,
                                                 NotifForCurrentNoPrevVersion) },
    EventData = room_event(Acc, RoomJID),
    HostType = mod_muc_light_utils:acc_to_host_type(Acc),
    #xmlel{children = FinalChildrenForCurrentNoPrevVersion}
        = mongoose_hooks:filter_room_packet(HostType, MsgForArch, EventData),
    FinalChildrenForCurrent = inject_prev_version(FinalChildrenForCurrentNoPrevVersion,
                                                  Affs#affiliations.prev_version),
    bcast_aff_messages(RoomJID, OldAffUsers, NewAffUsers, Attrs, VersionEl,
                       FinalChildrenForCurrent, HandleFun),

    {reply, Affs#affiliations.id};
encode_iq({get, #blocking{} = Blocking},
          _Sender, _RoomJID, _RoomBin, _HandleFun, _Acc) ->
    BlockingEls = [ blocking_to_el(BlockingItem) || BlockingItem <- Blocking#blocking.items ],
    {reply, ?NS_MUC_LIGHT_BLOCKING, BlockingEls, Blocking#blocking.id};
encode_iq({set, #blocking{ id = ID }},
          _Sender, _RoomJID, _RoomBin, _HandleFun, _Acc) ->
    {reply, ID};
encode_iq({set, #create{} = Create, UniqueRequested},
          _Sender, RoomJID, RoomBin, HandleFun, Acc) ->
    Attrs = #{<<"id">> => Create#create.id,
              <<"type">> => <<"groupchat">>,
              <<"from">> => RoomBin},

    VersionEl = kv_to_el(<<"version">>, Create#create.version),
    bcast_aff_messages(RoomJID, [], Create#create.aff_users, Attrs, VersionEl, [], HandleFun),

    AllAffsEls = [ aff_user_to_el(AffUser) || AffUser <- Create#create.aff_users ],
    MsgForArch = #xmlel{ name = <<"message">>, attrs = Attrs,
                         children = msg_envelope(?NS_MUC_LIGHT_AFFILIATIONS, AllAffsEls) },
    EventData = room_event(Acc, RoomJID),
    HostType = mod_muc_light_utils:acc_to_host_type(Acc),
    mongoose_hooks:filter_room_packet(HostType, MsgForArch, EventData),

    %% IQ reply "from"
    %% Sent from service JID when unique room was requested
    {ResFromJID, ResFromBin} = case UniqueRequested of
                                   true -> {#jid{lserver = RoomJID#jid.lserver},
                                            RoomJID#jid.lserver};
                                   false -> {RoomJID, RoomBin}
                               end,
    {reply, ResFromJID, ResFromBin, <<>>, undefined, Create#create.id};
encode_iq({set, #destroy{ id = ID }, AffUsers},
          _Sender, RoomJID, RoomBin, HandleFun, _Acc) ->
    Attrs = #{<<"id">> => ID,
              <<"type">> => <<"groupchat">>,
              <<"from">> => RoomBin},

    lists:foreach(
      fun({{U, S}, _}) ->
              NoneAffEnveloped = msg_envelope(?NS_MUC_LIGHT_AFFILIATIONS,
                                              [aff_user_to_el({{U, S}, none})]),
              DestroyEnveloped = [ #xmlel{ name = <<"x">>,
                                           attrs = #{<<"xmlns">> => ?NS_MUC_LIGHT_DESTROY} }
                                   | NoneAffEnveloped ],
              msg_to_aff_user(RoomJID, U, S, Attrs, DestroyEnveloped, HandleFun)
      end, AffUsers),

    {reply, ID};
encode_iq({set, #config{} = Config, AffUsers},
          _Sender, RoomJID, RoomBin, HandleFun, Acc) ->
    MsgForArch = encode_set_config(Config, RoomBin),
    EventData = room_event(Acc, RoomJID),
    HostType = mod_muc_light_utils:acc_to_host_type(Acc),
    #xmlel{ children = FinalConfigNotif }
        = mongoose_hooks:filter_room_packet(HostType, MsgForArch, EventData),

    lists:foreach(
      fun({{U, S}, _}) ->
              msg_to_aff_user(RoomJID, U, S, MsgForArch#xmlel.attrs, FinalConfigNotif, HandleFun)
      end, AffUsers),

    {reply, Config#config.id}.

encode_set_config(Config, RoomBin) ->
    Attrs = #{<<"id">> => Config#config.id,
              <<"type">> => <<"groupchat">>,
              <<"from">> => RoomBin},
    ConfigEls = [ kv_to_el(ConfigField) || ConfigField <- Config#config.raw_config ],
    ConfigNotif = [ kv_to_el(<<"prev-version">>, Config#config.prev_version),
                    kv_to_el(<<"version">>, Config#config.version)
                    | ConfigEls ],
    #xmlel{name = <<"message">>, attrs = Attrs,
           children = msg_envelope(?NS_MUC_LIGHT_CONFIGURATION, ConfigNotif) }.

%% --------------------------- Helpers ---------------------------

-spec identity() -> mongoose_disco:identity().
identity() ->
    #{category => <<"conference">>, type => <<"text">>, name => <<"MUC Light (modern)">>}.

-spec aff_user_to_el(aff_user()) -> exml:element().
aff_user_to_el({User, Aff}) ->
    #xmlel{ name = <<"user">>,
            attrs = #{<<"affiliation">> => mod_muc_light_utils:aff2b(Aff)},
            children = [#xmlcdata{ content = jid:to_binary(User) }] }.

-spec blocking_to_el(blocking_item()) -> exml:element().
blocking_to_el({What, Action, Who}) ->
    #xmlel{ name = what2b(What),
            attrs = #{<<"action">> => action2b(Action)},
            children = [#xmlcdata{ content = jid:to_binary(Who) }] }.

-spec kv_to_el({binary(), binary()}) -> exml:element().
kv_to_el({Key, Value}) ->
    kv_to_el(Key, Value).

-spec kv_to_el(binary(), binary()) -> exml:element().
kv_to_el(Key, Value) ->
    #xmlel{ name = Key, children = [#xmlcdata{ content = Value }] }.

-spec msg_envelope(XMLNS :: binary(), Children :: [exml:child()]) -> [exml:element()].
msg_envelope(XMLNS, Children) ->
    [ #xmlel{ name = <<"x">>, attrs = #{<<"xmlns">> => XMLNS}, children = Children },
      #xmlel{ name = <<"body">> } ].

-spec inject_prev_version(IQChildren :: [exml:child()], PrevVersion :: binary()) -> [exml:child()].
inject_prev_version([#xmlel{ name = <<"x">>, attrs = #{<<"xmlns">> := ?NS_MUC_LIGHT_AFFILIATIONS},
                             children = Items} = XEl | REls], PrevVersion) ->
    [XEl#xmlel{ children = [kv_to_el(<<"prev-version">>, PrevVersion) | Items] } | REls];
inject_prev_version([El | REls], PrevVersion) ->
    [El | inject_prev_version(REls, PrevVersion)].

-spec bcast_aff_messages(From :: jid:jid(), OldAffUsers :: aff_users(),
                         NewAffUsers :: aff_users(), Attrs :: exml:attrs(),
                         VersionEl :: exml:element(), Children :: [exml:child()],
                         HandleFun :: mod_muc_light_codec_backend:encoded_packet_handler()) -> ok.
bcast_aff_messages(_, [], [], _, _, _, _) ->
    ok;
bcast_aff_messages(From, [{User, _} | ROldAffUsers], [], Attrs, VersionEl, Children, HandleFun) ->
    msg_to_leaving_user(From, User, Attrs, HandleFun),
    bcast_aff_messages(From, ROldAffUsers, [], Attrs, VersionEl, Children, HandleFun);
bcast_aff_messages(From, [{{ToU, ToS} = User, _} | ROldAffUsers], [{User, _} | RNewAffUsers],
                   Attrs, VersionEl, Children, HandleFun) ->
    msg_to_aff_user(From, ToU, ToS, Attrs, Children, HandleFun),
    bcast_aff_messages(From, ROldAffUsers, RNewAffUsers, Attrs, VersionEl, Children, HandleFun);
bcast_aff_messages(From, [{User1, _} | ROldAffUsers], [{User2, _} | _] = NewAffUsers,
                   Attrs, VersionEl, Children, HandleFun) when User1 < User2 ->
    msg_to_leaving_user(From, User1, Attrs, HandleFun),
    bcast_aff_messages(From, ROldAffUsers, NewAffUsers, Attrs, VersionEl, Children, HandleFun);
bcast_aff_messages(From, OldAffUsers, [{{ToU, ToS}, _} = AffUser | RNewAffUsers],
                   Attrs, VersionEl, Children, HandleFun) ->
    NotifForNewcomer = msg_envelope(?NS_MUC_LIGHT_AFFILIATIONS,
                                    [ VersionEl, aff_user_to_el(AffUser) ]),
    msg_to_aff_user(From, ToU, ToS, Attrs, NotifForNewcomer, HandleFun),
    bcast_aff_messages(From, OldAffUsers, RNewAffUsers, Attrs, VersionEl, Children, HandleFun).

-spec msg_to_leaving_user(From :: jid:jid(), User :: jid:simple_bare_jid(),
                          Attrs :: exml:attrs(),
                          HandleFun :: mod_muc_light_codec_backend:encoded_packet_handler()) -> ok.
msg_to_leaving_user(From, {ToU, ToS} = User, Attrs, HandleFun) ->
    NotifForLeaving = msg_envelope(?NS_MUC_LIGHT_AFFILIATIONS, [ aff_user_to_el({User, none}) ]),
    msg_to_aff_user(From, ToU, ToS, Attrs, NotifForLeaving, HandleFun).

-spec msg_to_aff_user(From :: jid:jid(), ToU :: jid:luser(), ToS :: jid:lserver(),
                      Attrs :: exml:attrs(), Children :: [exml:child()],
                      HandleFun :: mod_muc_light_codec_backend:encoded_packet_handler()) -> ok.
msg_to_aff_user(From, ToU, ToS, Attrs, Children, HandleFun) ->
    To = jid:make_noprep(ToU, ToS, <<>>),
    ToBin = jid:to_binary({ToU, ToS, <<>>}),
    Packet = #xmlel{ name = <<"message">>, attrs = Attrs#{<<"to">> => ToBin},
                     children = Children },
    HandleFun(From, To, Packet).

-spec jids_from_room_with_resource(jid:jid(), binary()) ->
    {jid:jid(), binary()}.
jids_from_room_with_resource(RoomJID, Resource) ->
    From = jid:replace_resource(RoomJID, Resource),
    FromBin = jid:to_binary(jid:to_lower(From)),
    {From, FromBin}.

-spec make_iq_result(FromBin :: binary(), ToBin :: binary(), ID :: binary(),
                     XMLNS :: binary(), Els :: [exml:child()] | undefined) -> exml:element().
make_iq_result(FromBin, ToBin, ID, XMLNS, Els) ->
    Attrs = #{<<"from">> => FromBin,
              <<"to">> => ToBin,
              <<"id">> => ID,
              <<"type">> => <<"result">>},
    Query = make_query_el(XMLNS, Els),
    #xmlel{ name = <<"iq">>, attrs = Attrs, children = Query }.

-spec make_query_el(binary(), [exml:child()] | undefined) -> [exml:element()].
make_query_el(_, undefined) ->
    [];
make_query_el(XMLNS, Els) ->
    [#xmlel{ name = <<"query">>, attrs = #{<<"xmlns">> => XMLNS}, children = Els }].

%%====================================================================
%% Common helpers and internal functions
%%====================================================================

-spec safe_get_children(exml:element() | term()) -> [exml:element() | exml:cdata()].
safe_get_children(#xmlel{ children = Ch }) -> Ch;
safe_get_children(_) -> [].

-spec b2action(ActionBin :: binary()) -> atom().
b2action(<<"allow">>) -> allow;
b2action(<<"deny">>) -> deny.

-spec action2b(Action :: atom()) -> binary().
action2b(allow) -> <<"allow">>;
action2b(deny) -> <<"deny">>.

-spec b2what(WhatBin :: binary()) -> atom().
b2what(<<"user">>) -> user;
b2what(<<"room">>) -> room.

-spec what2b(What :: atom()) -> binary().
what2b(user) -> <<"user">>;
what2b(room) -> <<"room">>.

-spec room_event(mongoose_acc:t(), jid:jid()) -> mod_muc:room_event_data().
room_event(Acc, RoomJID) ->
    TS = mongoose_acc:timestamp(Acc),
    #{from_nick => <<>>,
      from_jid => RoomJID,
      room_jid => RoomJID,
      affiliation => owner,
      role => moderator,
      timestamp => TS}.

-spec msg_to_aff_user_online(From :: jid:jid(), ToU :: jid:luser(), ToS :: jid:lserver(),
                             Attrs :: exml:attrs(), Children :: [exml:child()],
                             HandleFun :: mod_muc_light_codec_backend:encoded_packet_handler()) -> ok.
msg_to_aff_user_online(RoomJID, U, S, Attrs, Children, HandleFun) ->
    case ejabberd_sm_backend:get_sessions(U, S) of
        [_ | _] ->
            msg_to_aff_user(RoomJID, U, S, Attrs, Children, HandleFun);
        _ ->
            ok
    end.
