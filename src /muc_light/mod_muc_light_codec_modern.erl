%%%----------------------------------------------------------------------
%%% File    : mod_muc_light_codec_modern.erl
%%% Author  : Piotr Nosek <piotr.nosek@erlang-solutions.com>
%%% Purpose : MUC Light codec for modern syntax
%%% Created : 29 Sep 2015 by Piotr Nosek <piotr.nosek@erlang-solutions.com>
%%%----------------------------------------------------------------------

-module(mod_muc_light_codec_modern).
-author('piotr.nosek@erlang-solutions.com').

-behaviour(mod_muc_light_codec).

%% API
-export([decode/3, encode/4, encode_error/5]).

-include("mongoose.hrl").
-include("jlib.hrl").
-include("mod_muc_light.hrl").

%%====================================================================
%% API
%%====================================================================

-spec decode(From :: jid:jid(), To :: jid:jid(), Stanza :: exml:element()) ->
    mod_muc_light_codec:decode_result().
decode(_From, #jid{ lresource = Resource }, _Stanza) when Resource =/= <<>> ->
    {error, bad_request};
decode(_From, _To, #xmlel{ name = <<"message">> } = Stanza) ->
    decode_message(Stanza);
decode(From, _To, #xmlel{ name = <<"iq">> } = Stanza) ->
    decode_iq(From, jlib:iq_query_info(Stanza));
decode(_, _, _) ->
    {error, bad_request}.

-spec encode(Request :: muc_light_encode_request(), OriginalSender :: jid:jid(),
             RoomUS :: jid:simple_bare_jid(),
             HandleFun :: mod_muc_light_codec:encoded_packet_handler()) -> any().
encode({#msg{} = Msg, AffUsers}, Sender, {RoomU, RoomS} = RoomUS, HandleFun) ->
    US = jid:to_lus(Sender),
    FromNick = jid:to_binary(US),
    Aff = get_sender_aff(AffUsers, US),
    {RoomJID, RoomBin} = jids_from_room_with_resource(RoomUS, FromNick),
    Attrs = [
             {<<"id">>, Msg#msg.id},
             {<<"type">>, <<"groupchat">>},
             {<<"from">>, RoomBin}
            ],
    MsgForArch = #xmlel{ name = <<"message">>, attrs = Attrs, children = Msg#msg.children },
    EventData = [{from_nick, FromNick},
                 {from_jid, Sender},
                 {room_jid, jid:make_noprep({RoomU, RoomS, <<>>})},
                 {affiliation, Aff},
                 {role, Aff}
                ],
    FilteredPacket = #xmlel{ children = Children }
                     = ejabberd_hooks:run_fold(filter_room_packet, RoomS, MsgForArch, [EventData]),
    ejabberd_hooks:run(room_send_packet, RoomS, [FilteredPacket, EventData]),
    lists:foreach(
      fun({{U, S}, _}) ->
              msg_to_aff_user(RoomJID, U, S, Attrs, Children, HandleFun)
      end, AffUsers);
encode(OtherCase, Sender, RoomUS, HandleFun) ->
    {RoomJID, RoomBin} = jids_from_room_with_resource(RoomUS, <<>>),
    case encode_iq(OtherCase, RoomJID, RoomBin, HandleFun) of
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
        OrigPacket :: exml:element(), HandleFun :: mod_muc_light_codec:encoded_packet_handler()) ->
    any().
encode_error(ErrMsg, OrigFrom, OrigTo, OrigPacket, HandleFun) ->
    mod_muc_light_codec:encode_error(ErrMsg, [], OrigFrom, OrigTo, OrigPacket, HandleFun).

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
    {ok, msg()} | {error, bad_request} | ignore.
decode_message_by_type({_, <<"groupchat">>}, Id, Children) ->
    {ok, #msg{ children = Children, id = ensure_id(Id) }};
decode_message_by_type({_, <<"error">>}, _, _) ->
    ignore;
decode_message_by_type(_, _, _) ->
    {error, bad_request}.

-spec ensure_id(Id :: {binary(), binary()} | false) -> binary().
ensure_id(false) -> mod_muc_light_utils:bin_ts();
ensure_id({_, Id}) -> Id.

%%====================================================================
%% IQ decoding
%%====================================================================

-spec decode_iq(From :: jid:jid(), IQ :: jlib:iq()) ->
    {ok, muc_light_packet() | muc_light_disco() | jlib:iq()} | {error, bad_request} | ignore.
decode_iq(_From, #iq{ xmlns = ?NS_MUC_LIGHT_CONFIGURATION, type = get,
                      sub_el = QueryEl, id = ID }) ->
    Version = exml_query:path(QueryEl, [{element, <<"version">>}, cdata], <<>>),
    {ok, {get, #config{
            id = ID,
            prev_version = Version
           }}};
decode_iq(_From, #iq{ xmlns = ?NS_MUC_LIGHT_CONFIGURATION, type = set,
               sub_el = #xmlel{ children = QueryEls }, id = ID }) ->
    case catch parse_config(QueryEls) of
        {ok, RawConfig} ->
            {ok, {set, #config{
                    id = ID,
                    raw_config = RawConfig
                   }}};
        Error ->
            ?WARNING_MSG("query_els=~p, error=~p", [QueryEls, Error]),
            {error, bad_request}
    end;
decode_iq(_From, #iq{ xmlns = ?NS_MUC_LIGHT_AFFILIATIONS, type = get,
                      sub_el = QueryEl, id = ID }) ->
    Version = exml_query:path(QueryEl, [{element, <<"version">>}, cdata], <<>>),
    {ok, {get, #affiliations{
            id = ID,
            prev_version = Version
           }}};
decode_iq(_From, #iq{ xmlns = ?NS_MUC_LIGHT_AFFILIATIONS, type = set,
               sub_el = #xmlel{ children = QueryEls }, id = ID }) ->
    case catch parse_aff_users(QueryEls) of
        {ok, AffUsers} ->
            {ok, {set, #affiliations{
                    id = ID,
                    aff_users = AffUsers
                   }}};
        Error ->
            ?WARNING_MSG("query_els=~p, error=~p", [QueryEls, Error]),
            {error, bad_request}
    end;
decode_iq(_From, #iq{ xmlns = ?NS_MUC_LIGHT_INFO, type = get, sub_el = QueryEl, id = ID }) ->
    Version = exml_query:path(QueryEl, [{element, <<"version">>}, cdata], <<>>),
    {ok, {get, #info{
                  id = ID,
                  prev_version = Version
                 }}};
decode_iq(_From, #iq{ xmlns = ?NS_MUC_LIGHT_BLOCKING, type = get, id = ID }) ->
    {ok, {get, #blocking{ id = ID }}};
decode_iq(_From, #iq{ xmlns = ?NS_MUC_LIGHT_BLOCKING, type = set,
               sub_el = #xmlel{ children = QueryEls }, id = ID }) ->
    case catch parse_blocking_list(QueryEls) of
        {ok, BlockingList} ->
            {ok, {set, #blocking{
                    id = ID,
                    items = BlockingList
                   }}};
        Error ->
            ?WARNING_MSG("query_els=~p, error=~p", [QueryEls, Error]),
            {error, bad_request}
    end;
decode_iq(_From, #iq{ xmlns = ?NS_MUC_LIGHT_CREATE, type = set, sub_el = QueryEl, id = ID }) ->
    ConfigEl = exml_query:path(QueryEl, [{element, <<"configuration">>}]),
    OccupantsEl = exml_query:path(QueryEl, [{element, <<"occupants">>}]),
    case {catch parse_config(safe_get_children(ConfigEl)),
          catch parse_aff_users(safe_get_children(OccupantsEl))} of
        {{ok, RawConfig}, {ok, AffUsers}} ->
            {ok, {set, #create{
                          id = ID,
                          raw_config = RawConfig,
                          aff_users = AffUsers
                         }}};
        Error ->
            ?WARNING_MSG("query_els=~p, error=~p", [QueryEl#xmlel.children, Error]),
            {error, bad_request}
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
    {error, bad_request}.

%% ------------------ Parsers ------------------

-spec parse_config(Els :: [jlib:xmlch()]) -> {ok, raw_config()} | {error, bad_request}.
parse_config(Els) ->
    parse_config(Els, []).

-spec parse_config(Els :: [jlib:xmlch()], ConfigAcc :: raw_config()) ->
    {ok, raw_config()} | {error, bad_request}.
parse_config([], ConfigAcc) ->
    {ok, ConfigAcc};
parse_config([#xmlel{ name = <<"version">> } | _], _) ->
    {error, bad_request};
parse_config([#xmlel{ name = Key, children = [ #xmlcdata{ content = Value } ] } | REls],
             ConfigAcc) ->
    parse_config(REls, [{Key, Value} | ConfigAcc]);
parse_config([_ | REls], ConfigAcc) ->
    parse_config(REls, ConfigAcc).

-spec parse_aff_users(Els :: [jlib:xmlch()]) ->
    {ok, aff_users()} | {error, bad_request}.
parse_aff_users(Els) ->
    parse_aff_users(Els, []).

-spec parse_aff_users(Els :: [jlib:xmlch()], AffUsersAcc :: aff_users()) ->
    {ok, aff_users()} | {error, bad_request}.
parse_aff_users([], AffUsersAcc) ->
    {ok, AffUsersAcc};
parse_aff_users([#xmlcdata{} | RItemsEls], AffUsersAcc) ->
    parse_aff_users(RItemsEls, AffUsersAcc);
parse_aff_users([#xmlel{ name = <<"user">>, attrs = [{<<"affiliation">>, AffBin}],
                                  children = [ #xmlcdata{ content = JIDBin } ] } | RItemsEls],
                          AffUsersAcc) ->
    #jid{} = JID = jid:from_binary(JIDBin),
    Aff = mod_muc_light_utils:b2aff(AffBin),
    parse_aff_users(RItemsEls, [{jid:to_lus(JID), Aff} | AffUsersAcc]);
parse_aff_users(_, _) ->
    {error, bad_request}.

-spec parse_blocking_list(Els :: [jlib:xmlch()]) -> {ok, [blocking_item()]} | {error, bad_request}.
parse_blocking_list(ItemsEls) ->
    parse_blocking_list(ItemsEls, []).

-spec parse_blocking_list(Els :: [jlib:xmlch()], ItemsAcc :: [blocking_item()]) ->
    {ok, [blocking_item()]} | {error, bad_request}.
parse_blocking_list([], ItemsAcc) ->
    {ok, ItemsAcc};
parse_blocking_list([#xmlel{ name = WhatBin, attrs = [{<<"action">>, ActionBin}],
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

encode_iq({get, #disco_info{ id = ID }}, _RoomJID, _RoomBin, _HandleFun) ->
    DiscoEls = [#xmlel{name = <<"identity">>,
                       attrs = [{<<"category">>, <<"conference">>},
                                {<<"type">>, <<"text">>},
                                {<<"name">>, <<"MUC Light">>}]},
                #xmlel{name = <<"feature">>, attrs = [{<<"var">>, ?NS_MUC_LIGHT}]}],
    {reply, ?NS_DISCO_INFO, DiscoEls, ID};
encode_iq({get, #disco_items{ rooms = Rooms, id = ID, rsm = RSMOut }},
          _RoomJID, _RoomBin, _HandleFun) ->
    DiscoEls = [ #xmlel{ name = <<"item">>,
                         attrs = [{<<"jid">>, <<RoomU/binary, $@, RoomS/binary>>},
                                  {<<"name">>, RoomName},
                                  {<<"version">>, RoomVersion}] }
                 || {{RoomU, RoomS}, RoomName, RoomVersion} <- Rooms ],
    {reply, ?NS_DISCO_ITEMS, jlib:rsm_encode(RSMOut) ++ DiscoEls, ID};
encode_iq({get, #config{ prev_version = SameVersion, version = SameVersion, id = ID }},
          _RoomJID, _RoomBin, _HandleFun) ->
    {reply, ID};
encode_iq({get, #config{} = Config}, _RoomJID, _RoomBin, _HandleFun) ->
    ConfigEls = [ kv_to_el(Field) || Field <- [{<<"version">>, Config#config.version}
                                                         | Config#config.raw_config] ],
    {reply, ?NS_MUC_LIGHT_CONFIGURATION, ConfigEls, Config#config.id};
encode_iq({get, #affiliations{ prev_version = SameVersion, version = SameVersion, id = ID }},
          _RoomJID, _RoomBin, _HandleFun) ->
    {reply, ID};
encode_iq({get, #affiliations{ version = Version } = Affs}, _RoomJID, _RoomBin, _HandleFun) ->
    AffEls = [ aff_user_to_el(AffUser) || AffUser <- Affs#affiliations.aff_users ],
    {reply, ?NS_MUC_LIGHT_AFFILIATIONS, [kv_to_el(<<"version">>, Version) | AffEls],
     Affs#affiliations.id};
encode_iq({get, #info{ prev_version = SameVersion, version = SameVersion, id = ID }},
          _RoomJID, _RoomBin, _HandleFun) ->
    {reply, ID};
encode_iq({get, #info{ version = Version } = Info}, _RoomJID, _RoomBin, _HandleFun) ->
    ConfigEls = [ kv_to_el(Field) || Field <- Info#info.raw_config ],
    AffEls = [ aff_user_to_el(AffUser) || AffUser <- Info#info.aff_users ],
    InfoEls = [
               kv_to_el(<<"version">>, Version),
               #xmlel{ name = <<"configuration">>, children = ConfigEls },
               #xmlel{ name = <<"occupants">>, children = AffEls }
              ],
    {reply, ?NS_MUC_LIGHT_INFO, InfoEls, Info#info.id};
encode_iq({set, #affiliations{} = Affs, OldAffUsers, NewAffUsers}, RoomJID, RoomBin, HandleFun) ->
    Attrs = [
             {<<"id">>, Affs#affiliations.id},
             {<<"type">>, <<"groupchat">>},
             {<<"from">>, RoomBin}
            ],

    AllAffsEls = [ aff_user_to_el(AffUser) || AffUser <- Affs#affiliations.aff_users ],
    VersionEl = kv_to_el(<<"version">>, Affs#affiliations.version),
    NotifForCurrentNoPrevVersion = [ VersionEl | AllAffsEls ],
    MsgForArch = #xmlel{ name = <<"message">>, attrs = Attrs,
                         children = msg_envelope(?NS_MUC_LIGHT_AFFILIATIONS,
                                                 NotifForCurrentNoPrevVersion) },
    EventData = room_event(RoomJID),
    FilteredPacket = #xmlel{ children = FinalChildrenForCurrentNoPrevVersion }
    = ejabberd_hooks:run_fold(filter_room_packet, RoomJID#jid.lserver, MsgForArch,
                              [EventData]),
    ejabberd_hooks:run(room_send_packet, RoomJID#jid.lserver, [FilteredPacket, EventData]),
    FinalChildrenForCurrent = inject_prev_version(FinalChildrenForCurrentNoPrevVersion,
                                                  Affs#affiliations.prev_version),
    bcast_aff_messages(RoomJID, OldAffUsers, NewAffUsers, Attrs, VersionEl,
                       FinalChildrenForCurrent, HandleFun),

    {reply, Affs#affiliations.id};
encode_iq({get, #blocking{} = Blocking}, _RoomJID, _RoomBin, _HandleFun) ->
    BlockingEls = [ blocking_to_el(BlockingItem) || BlockingItem <- Blocking#blocking.items ],
    {reply, ?NS_MUC_LIGHT_BLOCKING, BlockingEls, Blocking#blocking.id};
encode_iq({set, #blocking{ id = ID }}, _RoomJID, _RoomBin, _HandleFun) ->
    {reply, ID};
encode_iq({set, #create{} = Create, UniqueRequested}, RoomJID, RoomBin, HandleFun) ->
    Attrs = [
             {<<"id">>, Create#create.id},
             {<<"type">>, <<"groupchat">>},
             {<<"from">>, RoomBin}
            ],

    VersionEl = kv_to_el(<<"version">>, Create#create.version),
    bcast_aff_messages(RoomJID, [], Create#create.aff_users, Attrs, VersionEl, [], HandleFun),

    AllAffsEls = [ aff_user_to_el(AffUser) || AffUser <- Create#create.aff_users ],
    MsgForArch = #xmlel{ name = <<"message">>, attrs = Attrs,
                         children = msg_envelope(?NS_MUC_LIGHT_AFFILIATIONS, AllAffsEls) },
    EventData = room_event(RoomJID),
    FilteredPacket = ejabberd_hooks:run_fold(filter_room_packet, RoomJID#jid.lserver, MsgForArch,
                                             [EventData]),
    ejabberd_hooks:run(room_send_packet, RoomJID#jid.lserver, [FilteredPacket, EventData]),

    %% IQ reply "from"
    %% Sent from service JID when unique room was requested
    {ResFromJID, ResFromBin} = case UniqueRequested of
                                   true -> {#jid{ server = RoomJID#jid.lserver,
                                                  lserver = RoomJID#jid.lserver },
                                            RoomJID#jid.lserver};
                                   false -> {RoomJID, RoomBin}
                               end,
    {reply, ResFromJID, ResFromBin, <<>>, undefined, Create#create.id};
encode_iq({set, #destroy{ id = ID }, AffUsers}, RoomJID, RoomBin, HandleFun) ->
    Attrs = [
             {<<"id">>, ID},
             {<<"type">>, <<"groupchat">>},
             {<<"from">>, RoomBin}
            ],

    lists:foreach(
      fun({{U, S}, _}) ->
              NoneAffEnveloped = msg_envelope(?NS_MUC_LIGHT_AFFILIATIONS,
                                              [aff_user_to_el({{U, S}, none})]),
              DestroyEnveloped = [ #xmlel{ name = <<"x">>,
                                           attrs = [{<<"xmlns">>, ?NS_MUC_LIGHT_DESTROY}] }
                                   | NoneAffEnveloped ],
              msg_to_aff_user(RoomJID, U, S, Attrs, DestroyEnveloped, HandleFun)
      end, AffUsers),

    {reply, ID};
encode_iq({set, #config{} = Config, AffUsers}, RoomJID, RoomBin, HandleFun) ->
    Attrs = [
             {<<"id">>, Config#config.id},
             {<<"type">>, <<"groupchat">>},
             {<<"from">>, RoomBin}
            ],
    ConfigEls = [ kv_to_el(ConfigField) || ConfigField <- Config#config.raw_config ],
    ConfigNotif = [ kv_to_el(<<"prev-version">>, Config#config.prev_version),
                    kv_to_el(<<"version">>, Config#config.version)
                    | ConfigEls ],
    MsgForArch = #xmlel{ name = <<"message">>, attrs = Attrs,
                         children = msg_envelope(?NS_MUC_LIGHT_CONFIGURATION, ConfigNotif) },
    EventData = room_event(RoomJID),
    FilteredPacket = #xmlel{ children = FinalConfigNotif }
    = ejabberd_hooks:run_fold(filter_room_packet, RoomJID#jid.lserver, MsgForArch,
                              [EventData]),
    ejabberd_hooks:run(room_send_packet, RoomJID#jid.lserver, [FilteredPacket, EventData]),

    lists:foreach(
      fun({{U, S}, _}) ->
              msg_to_aff_user(RoomJID, U, S, Attrs, FinalConfigNotif, HandleFun)
      end, AffUsers),

    {reply, Config#config.id}.

%% --------------------------- Helpers ---------------------------

-spec aff_user_to_el(aff_user()) -> exml:element().
aff_user_to_el({User, Aff}) ->
    #xmlel{ name = <<"user">>,
            attrs = [{<<"affiliation">>, mod_muc_light_utils:aff2b(Aff)}],
            children = [#xmlcdata{ content = jid:to_binary(User) }] }.

-spec blocking_to_el(blocking_item()) -> exml:element().
blocking_to_el({What, Action, Who}) ->
    #xmlel{ name = what2b(What),
            attrs = [{<<"action">>, action2b(Action)}],
            children = [#xmlcdata{ content = jid:to_binary(Who) }] }.

-spec kv_to_el({binary(), binary()}) -> exml:element().
kv_to_el({Key, Value}) ->
    kv_to_el(Key, Value).

-spec kv_to_el(binary(), binary()) -> exml:element().
kv_to_el(Key, Value) ->
    #xmlel{ name = Key, children = [#xmlcdata{ content = Value }] }.

-spec msg_envelope(XMLNS :: binary(), Children :: [jlib:xmlch()]) -> [exml:element()].
msg_envelope(XMLNS, Children) ->
    [ #xmlel{ name = <<"x">>, attrs = [{<<"xmlns">>, XMLNS}], children = Children },
      #xmlel{ name = <<"body">> } ].

-spec inject_prev_version(IQChildren :: [jlib:xmlch()], PrevVersion :: binary()) -> [jlib:xmlch()].
inject_prev_version([#xmlel{ name = <<"x">>, attrs = [{<<"xmlns">>, ?NS_MUC_LIGHT_AFFILIATIONS}],
                             children = Items} = XEl | REls], PrevVersion) ->
    [XEl#xmlel{ children = [kv_to_el(<<"prev-version">>, PrevVersion) | Items] } | REls];
inject_prev_version([El | REls], PrevVersion) ->
    [El | inject_prev_version(REls, PrevVersion)].

-spec bcast_aff_messages(From :: jid:jid(), OldAffUsers :: aff_users(),
                         NewAffUsers :: aff_users(), Attrs :: [{binary(), binary()}],
                         VersionEl :: exml:element(), Children :: [jlib:xmlch()],
                         HandleFun :: mod_muc_light_codec:encoded_packet_handler()) -> ok.
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
                          Attrs :: [{binary(), binary()}],
                          HandleFun :: mod_muc_light_codec:encoded_packet_handler()) -> ok.
msg_to_leaving_user(From, {ToU, ToS} = User, Attrs, HandleFun) ->
    NotifForLeaving = msg_envelope(?NS_MUC_LIGHT_AFFILIATIONS, [ aff_user_to_el({User, none}) ]),
    msg_to_aff_user(From, ToU, ToS, Attrs, NotifForLeaving, HandleFun).

-spec msg_to_aff_user(From :: jid:jid(), ToU :: jid:luser(), ToS :: jid:lserver(),
                      Attrs :: [{binary(), binary()}], Children :: [jlib:xmlch()],
                      HandleFun :: mod_muc_light_codec:encoded_packet_handler()) -> ok.
msg_to_aff_user(From, ToU, ToS, Attrs, Children, HandleFun) ->
    To = jid:make_noprep({ToU, ToS, <<>>}),
    ToBin = jid:to_binary({ToU, ToS, <<>>}),
    Packet = #xmlel{ name = <<"message">>, attrs = [{<<"to">>, ToBin} | Attrs],
                     children = Children },
    HandleFun(From, To, Packet).

-spec jids_from_room_with_resource(RoomUS :: jid:simple_bare_jid(), binary()) ->
    {jid:jid(), binary()}.
jids_from_room_with_resource({RoomU, RoomS}, Resource) ->
    FromBin = jid:to_binary({RoomU, RoomS, Resource}),
    From = jid:make_noprep({RoomU, RoomS, Resource}),
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

room_event(RoomJID) ->
    [{from_nick, <<>>},
     {from_jid, RoomJID},
     {room_jid, RoomJID},
     {role, owner},
     {affiliation, owner}].
