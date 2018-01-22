%%%----------------------------------------------------------------------
%%% File    : mod_muc_light.erl
%%% Author  : Piotr Nosek <piotr.nosek@erlang-solutions.com>
%%% Purpose : MUC light support
%%% Created : 8 Sep 2014 by Piotr Nosek <piotr.nosek@erlang-solutions.com>
%%%
%%% Configuration options:
%%% * backend (mnesia) - DB backend
%%% * equal_occupants (false) - When enabled, everyone in the room will have 'member' status,
%%%                             including creator
%%% * legacy_mode (false) - Enables XEP-0045 compatibility mode
%%% * rooms_per_user (infinity) - Default maximum count of rooms the user can occupy
%%% * blocking (true) - Blocking feature enabled/disabled
%%% * all_can_configure (false) - Every room occupant can change room configuration
%%% * all_can_invite (false) - Every room occupant can invite a user to the room
%%% * max_occupants (infinity) - Maximal occupant count per room
%%% * rooms_per_page (10) - Maximal room count per result page in room disco
%%% * rooms_in_rosters (false) - If enabled, rooms that user occupies will be included in
%%%                              user's roster
%%% * config_schema (["roomname", "subject"]) - Custom list of configuration options for a room;
%%%                               WARNING! Lack of `roomname` field will cause room names in
%%%                               Disco results and Roster items be set to room username.
%%% * default_config ([{"roomname, "Untitled"}, {"subject", ""}]) -
%%%                                Custom default room configuration; must be a subset of
%%%                                config schema. It's a list of KV tuples with string keys
%%%                                and values of appriopriate type. String values will be
%%%                                converted to binary automatically.
%%%        Example: [{"roomname", "The room"}, {"subject", "Chit-chat"}, {"security", 10}]
%%%
%%% Allowed `config_schema` list items (may be mixed):
%%% * Just field name: "field" - will be expanded to "field" of type 'binary'
%%% * Field name and type: {"field", integer}
%%% * Field name, atom and the type: {"field", field, float} - useful only for debugging
%%%                 or unusual applications
%%% Sample valid list: `["roomname", {"subject", binary}, {"priority", priority, integer}]`
%%%
%%% Valid config field types:
%%% * binary
%%% * integer
%%% * float
%%%
%%%----------------------------------------------------------------------

-module(mod_muc_light).
-author('piotr.nosek@erlang-solutions.com').

-include("mongoose.hrl").
-include("jlib.hrl").
-include("mod_muc_light.hrl").
-include("mod_roster.hrl").
-include("mongoose_rsm.hrl").

-behaviour(gen_mod).
-behaviour(mongoose_packet_handler).

%% API
-export([standard_config_schema/0, standard_default_config/0, default_host/0]).
-export([config_schema/1, default_config/1]).

%% For Administration API
-export([try_to_create_room/3, delete_room/1]).

%% gen_mod callbacks
-export([start/2, stop/1]).

%% Packet handler export
-export([process_packet/5]).

%% Hook handlers
-export([prevent_service_unavailable/4,
         get_muc_service/5,
         remove_user/3,
         add_rooms_to_roster/2,
         process_iq_get/5,
         process_iq_set/4,
         is_room_owner/3,
         can_access_room/3,
         can_access_identity/3,
         muc_room_pid/2]).

%% For propEr
-export([apply_rsm/3]).

%%====================================================================
%% API
%%====================================================================

-spec standard_config_schema() -> [Field :: string()].
standard_config_schema() -> ["roomname", "subject"].

-spec standard_default_config() -> [{K :: string(), V :: string()}].
standard_default_config() -> [{"roomname", "Untitled"}, {"subject", ""}].

-spec default_host() -> binary().
default_host() ->
    <<"muclight.@HOST@">>.

-spec default_config(MUCServer :: jid:lserver()) -> config().
default_config(MUCServer) ->
    gen_mod:get_module_opt_by_subhost(MUCServer, ?MODULE, default_config, []).

-spec config_schema(MUCServer :: jid:lserver()) -> config_schema().
config_schema(MUCServer) ->
    gen_mod:get_module_opt_by_subhost(MUCServer, ?MODULE, config_schema, undefined).

%%====================================================================
%% Administration API
%%====================================================================

-spec try_to_create_room(CreatorUS :: jid:simple_bare_jid(), RoomJID :: jid:jid(),
                         CreationCfg :: create_req_props()) ->
    {ok, jid:simple_bare_jid(), create_req_props()}
    | {error, validation_error() | bad_request | exists}.
try_to_create_room(CreatorUS, RoomJID, #create{raw_config = RawConfig} = CreationCfg) ->
    {_RoomU, RoomS} = RoomUS = jid:to_lus(RoomJID),
    InitialAffUsers = mod_muc_light_utils:filter_out_prevented(
                        CreatorUS, RoomUS, CreationCfg#create.aff_users),
    MaxOccupants = gen_mod:get_module_opt_by_subhost(
                     RoomJID#jid.lserver, ?MODULE, max_occupants, ?DEFAULT_MAX_OCCUPANTS),
    case {mod_muc_light_utils:process_raw_config(
            RawConfig, default_config(RoomS), config_schema(RoomS)),
          process_create_aff_users_if_valid(RoomS, CreatorUS, InitialAffUsers)} of
        {{ok, Config0}, {ok, FinalAffUsers}} when length(FinalAffUsers) =< MaxOccupants ->
            Version = mod_muc_light_utils:bin_ts(),
            case mod_muc_light_db_backend:create_room(
                   RoomUS, lists:sort(Config0), FinalAffUsers, Version) of
                {ok, FinalRoomUS} ->
                    {ok, FinalRoomUS, CreationCfg#create{
                                        aff_users = FinalAffUsers, version = Version}};
                Other ->
                    Other
            end;
        {{error, _} = Error, _} ->
            Error;
        _ ->
            {error, bad_request}
    end.

-spec delete_room(RoomUS :: jid:simple_bare_jid()) -> ok | {error, not_exists}.
delete_room(RoomUS) ->
    mod_muc_light_db_backend:destroy_room(RoomUS).

%%====================================================================
%% gen_mod callbacks
%%====================================================================

-spec start(Host :: jid:server(), Opts :: list()) -> ok.
start(Host, Opts) ->
    %% Prevent sending service-unavailable on groupchat messages

    MUCHost = gen_mod:get_opt_subhost(Host, Opts, default_host()),
    ejabberd_hooks:add(hooks(Host, MUCHost)),

    case gen_mod:get_opt(rooms_in_rosters, Opts, ?DEFAULT_ROOMS_IN_ROSTERS) of
        false -> ignore;
        true -> ejabberd_hooks:add(roster_get, Host, ?MODULE, add_rooms_to_roster, 50)
    end,

    TrackedDBFuns = [create_room, destroy_room, room_exists, get_user_rooms, remove_user,
                     get_config, set_config, get_blocking, set_blocking,
                     get_aff_users, modify_aff_users],
    gen_mod:start_backend_module(mod_muc_light_db, Opts, TrackedDBFuns),
    Codec = case gen_mod:get_opt(legacy_mode, Opts, ?DEFAULT_LEGACY_MODE) of
                false ->
                    modern;
                true ->
                    ejabberd_hooks:add(privacy_iq_get, Host, ?MODULE, process_iq_get, 1),
                    ejabberd_hooks:add(privacy_iq_set, Host, ?MODULE, process_iq_set, 1),
                    legacy
            end,
    gen_mod:start_backend_module(mod_muc_light_codec, [{backend, Codec}], []),

    mod_muc_light_db_backend:start(Host, MUCHost),
    mongoose_subhosts:register(Host, MUCHost),
    ejabberd_router:register_route(MUCHost, mongoose_packet_handler:new(?MODULE)),

    %% Prepare config schema
    ConfigSchema = mod_muc_light_utils:make_config_schema(
                     gen_mod:get_opt(config_schema, Opts, standard_config_schema())),
    gen_mod:set_module_opt(Host, ?MODULE, config_schema, ConfigSchema),

    %% Prepare default config
    DefaultConfig = mod_muc_light_utils:make_default_config(
                      gen_mod:get_opt(default_config, Opts, standard_default_config()),
                      ConfigSchema),
    gen_mod:set_module_opt(Host, ?MODULE, default_config, DefaultConfig),

    ok.

-spec stop(Host :: jid:server()) -> ok.
stop(Host) ->
    MUCHost = gen_mod:get_module_opt_subhost(Host, ?MODULE, default_host()),
    ejabberd_router:unregister_route(MUCHost),
    mongoose_subhosts:unregister(MUCHost),

    mod_muc_light_db_backend:stop(Host, MUCHost),

    ejabberd_hooks:delete(hooks(Host, MUCHost)),

    %% Hook for room in roster
    ejabberd_hooks:delete(roster_get, Host, ?MODULE, add_rooms_to_roster, 50),
    %% Hooks for legacy mode
    ejabberd_hooks:delete(privacy_iq_get, Host, ?MODULE, process_iq_get, 1),
    ejabberd_hooks:delete(privacy_iq_set, Host, ?MODULE, process_iq_set, 1),

    ok.

hooks(Host, MUCHost) ->
    [{is_muc_room_owner, MUCHost, ?MODULE, is_room_owner, 50},
     {muc_room_pid, MUCHost, ?MODULE, muc_room_pid, 50},
     {can_access_room, MUCHost, ?MODULE, can_access_room, 50},
     {can_access_identity, MUCHost, ?MODULE, can_access_identity, 50},

     {offline_groupchat_message_hook, Host, ?MODULE, prevent_service_unavailable, 90},
     {remove_user, Host, ?MODULE, remove_user, 50},
     {disco_local_items, Host, ?MODULE, get_muc_service, 50}].

%%====================================================================
%% Routing
%%====================================================================

-spec process_packet(Acc :: mongoose_acc:t(), From ::jid:jid(), To ::jid:jid(),
                     El :: exml:element(), Extra :: any()) -> any().
process_packet(Acc, From, To, El, _Extra) ->
    process_decoded_packet(From, To, mod_muc_light_codec_backend:decode(From, To, El), Acc, El).

-spec process_decoded_packet(From :: jid:jid(), To :: jid:jid(),
                     DecodedPacket :: mod_muc_light_codec:decode_result(),
                     Acc :: mongoose_acc:t(),
                     OrigPacket :: exml:element()) -> any().
process_decoded_packet(From, To, {ok, {set, #create{} = Create}}, _Acc, OrigPacket) ->
    FromUS = jid:to_lus(From),
    case not mod_muc_light_utils:room_limit_reached(FromUS, To#jid.lserver) of
        true ->
            create_room(From, FromUS, To, Create, OrigPacket);
        false ->
            mod_muc_light_codec_backend:encode_error(
              {error, bad_request}, From, To, OrigPacket, fun ejabberd_router:route/3)
    end;
process_decoded_packet(From, To, {ok, {get, #disco_info{} = DI}}, _Acc, _OrigPacket) ->
    handle_disco_info_get(From, To, DI);
process_decoded_packet(From, To, {ok, {get, #disco_items{} = DI}}, _Acc, OrigPacket) ->
    handle_disco_items_get(From, To, DI, OrigPacket);
process_decoded_packet(From, To, {ok, {_, #blocking{}} = Blocking}, _Acc, OrigPacket) ->
    RouteFun = fun ejabberd_router:route/3,
    case gen_mod:get_module_opt_by_subhost(To#jid.lserver, ?MODULE, blocking, ?DEFAULT_BLOCKING) of
        true ->
            Res = handle_blocking(From, To, Blocking),
            case (mongoose_acc:is_acc(Res) or (Res == ok)) of
                true ->
                    ok;
                false ->
                    mod_muc_light_codec_backend:encode_error(Res, From, To, OrigPacket, RouteFun)
            end;
        false -> mod_muc_light_codec_backend:encode_error(
                   {error, bad_request}, From, To, OrigPacket, fun ejabberd_router:route/3)
    end;
process_decoded_packet(From, To, {ok, #iq{} = IQ}, Acc, OrigPacket) ->
    case mod_muc_iq:process_iq(To#jid.lserver, From, To, Acc, IQ) of
        ignore -> ok;
        error ->
            mod_muc_light_codec_backend:encode_error(
              {error, feature_not_implemented}, From, To, OrigPacket, fun ejabberd_router:route/3)
    end;
process_decoded_packet(From, #jid{ luser = RoomU } = To, {ok, RequestToRoom}, _Acc, OrigPacket)
  when RoomU =/= <<>> ->
    case mod_muc_light_db_backend:room_exists(jid:to_lus(To)) of
        true -> mod_muc_light_room:handle_request(From, To, OrigPacket, RequestToRoom);
        false -> mod_muc_light_codec_backend:encode_error(
                   {error, item_not_found}, From, To, OrigPacket, fun ejabberd_router:route/3)
    end;
process_decoded_packet(From, To, {error, _} = Err, _Acc, OrigPacket) ->
    mod_muc_light_codec_backend:encode_error(
      Err, From, To, OrigPacket, fun ejabberd_router:route/3);
process_decoded_packet(_From, _To, ignore, _Acc, _OrigPacket) ->
     ok;
process_decoded_packet(From, To, _InvalidReq, _Acc, OrigPacket) ->
    mod_muc_light_codec_backend:encode_error(
      {error, bad_request}, From, To, OrigPacket, fun ejabberd_router:route/3).

%%====================================================================
%% Hook handlers
%%====================================================================

-spec prevent_service_unavailable(Acc :: map(), From ::jid:jid(), To ::jid:jid(),
                                  Packet :: exml:element()) -> map() | {stop, map()}.
prevent_service_unavailable(Acc, _From, _To, Packet) ->
    case xml:get_tag_attr_s(<<"type">>, Packet) of
        <<"groupchat">> -> {stop, Acc};
        _Type -> Acc
    end.

-spec get_muc_service(Acc :: {result, [exml:element()]}, From :: jid:jid(), To :: jid:jid(),
                      NS :: binary(), ejabberd:lang()) -> {result, [exml:element()]}.
get_muc_service({result, Nodes}, _From, #jid{lserver = LServer} = _To, <<"">>, _Lang) ->
    XMLNS = case gen_mod:get_module_opt_by_subhost(
                   LServer, ?MODULE, legacy_mode, ?DEFAULT_LEGACY_MODE) of
                true -> ?NS_MUC;
                false -> ?NS_MUC_LIGHT
            end,
    SubHost = gen_mod:get_module_opt_subhost(LServer, ?MODULE, default_host()),
    Item = [#xmlel{name = <<"item">>,
                   attrs = [{<<"jid">>, SubHost},
                            {<<"node">>, XMLNS}]}],
    {result, [Item | Nodes]};
get_muc_service(Acc, _From, _To, _Node, _Lang) ->
    Acc.

-spec remove_user(Acc :: mongoose_acc:t(), User :: binary(), Server :: binary()) ->
    mongoose_acc:t().
remove_user(Acc, User, Server) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Server),
    UserUS = {LUser, LServer},
    Version = mod_muc_light_utils:bin_ts(),
    case mod_muc_light_db_backend:remove_user(UserUS, Version) of
        {error, _} = Err ->
            ?ERROR_MSG("hook=remove_user, error=~p", [Err]);
        AffectedRooms ->
            bcast_removed_user(UserUS, AffectedRooms, Version),
            maybe_forget_rooms(AffectedRooms),
            Acc
    end.

-spec add_rooms_to_roster(Acc :: mongoose_acc:t(), UserUS :: jid:simple_bare_jid()) ->
    mongoose_acc:t().
add_rooms_to_roster(Acc, UserUS) ->
    Items = mongoose_acc:get(roster, Acc, []),
    RoomList = mod_muc_light_db_backend:get_user_rooms(UserUS, undefined),
    Info = get_rooms_info(lists:sort(RoomList)),
    NewItems = lists:foldl(
      fun({{RoomU, RoomS}, RoomName, RoomVersion}, Items0) ->
              Item = #roster{
                        jid = jid:make_noprep(RoomU, RoomS, <<>>),
                        name = RoomName,
                        subscription = to,
                        groups = [?NS_MUC_LIGHT],
                        xs = [#xmlel{ name = <<"version">>,
                                      children = [#xmlcdata{ content = RoomVersion }] }]
                       },
              [Item | Items0]
      end, Items, Info),
    mongoose_acc:put(roster, NewItems, Acc).

-spec process_iq_get(Acc :: mongoose_acc:t(), From :: jid:jid(), To :: jid:jid(),
                     IQ :: jlib:iq(), ActiveList :: binary()) ->
    {stop, mongoose_acc:t()} | mongoose_acc:t().
process_iq_get(Acc, #jid{ lserver = FromS } = From, To, #iq{} = IQ, _ActiveList) ->
    MUCHost = gen_mod:get_module_opt_subhost(FromS, ?MODULE, default_host()),
    case {mod_muc_light_codec_backend:decode(From, To, IQ),
          gen_mod:get_module_opt_by_subhost(MUCHost, ?MODULE, blocking, ?DEFAULT_BLOCKING)} of
        {{ok, {get, #blocking{} = Blocking}}, true} ->
            Items = mod_muc_light_db_backend:get_blocking(jid:to_lus(From), MUCHost),
            mod_muc_light_codec_backend:encode(
              {get, Blocking#blocking{ items = Items }}, From, jid:to_lus(To),
              fun(_, _, Packet) -> put(encode_res, Packet) end),
            #xmlel{ children = ResponseChildren } = erase(encode_res),
            Result = {result, ResponseChildren},
            {stop, mongoose_acc:put(iq_result, Result, Acc)};
        {{ok, {get, #blocking{}}}, false} ->
            Result = {error, mongoose_xmpp_errors:bad_request()},
            {stop, mongoose_acc:put(iq_result, Result, Acc)};
        _ ->
            Result = {error, mongoose_xmpp_errors:bad_request()},
            mongoose_acc:put(iq_result, Result, Acc)
    end.

-spec process_iq_set(Acc :: mongoose_acc:t(), From :: jid:jid(),
                     To :: jid:jid(), IQ :: jlib:iq()) ->
    {stop, mongoose_acc:t()} | mongoose_acc:t().
process_iq_set(Acc, #jid{ lserver = FromS } = From, To, #iq{} = IQ) ->
    MUCHost = gen_mod:get_module_opt_subhost(FromS, ?MODULE, default_host()),
    case {mod_muc_light_codec_backend:decode(From, To, IQ),
          gen_mod:get_module_opt_by_subhost(MUCHost, ?MODULE, blocking, ?DEFAULT_BLOCKING)} of
        {{ok, {set, #blocking{ items = Items }} = Blocking}, true} ->
            RouteFun = fun(_, _, Packet) -> put(encode_res, Packet) end,
            ConditionFun = fun({_, _, {WhoU, WhoS}}) -> WhoU =:= <<>> orelse WhoS =:= <<>> end,
            case lists:any(ConditionFun, Items) of
                true ->
                    {stop, mongoose_acc:put(iq_result, {error, mongoose_xmpp_errors:bad_request()}, Acc)};
                false ->
                    ok = mod_muc_light_db_backend:set_blocking(jid:to_lus(From), MUCHost, Items),
                    mod_muc_light_codec_backend:encode(Blocking, From, jid:to_lus(To), RouteFun),
                    #xmlel{ children = ResponseChildren } = erase(encode_res),
                    {stop, mongoose_acc:put(iq_result, {result, ResponseChildren}, Acc)}
            end;
        {{ok, {set, #blocking{}}}, false} ->
            {stop, mongoose_acc:put(iq_result, {error, mongoose_xmpp_errors:bad_request()}, Acc)};
        _ ->
            mongoose_acc:put(iq_result, {error, mongoose_xmpp_errors:bad_request()}, Acc)
    end.

-spec is_room_owner(Acc :: boolean(), Room :: jid:jid(), User :: jid:jid()) -> boolean().
is_room_owner(_, Room, User) ->
    owner == get_affiliation(Room, User).

-spec muc_room_pid(Acc :: any(), Room :: jid:jid()) -> {ok, processless}.
muc_room_pid(_, _) ->
    {ok, processless}.

-spec can_access_room(Acc :: boolean(), Room :: jid:jid(), User :: jid:jid()) ->
    boolean().
can_access_room(_, Room, User) ->
    none =/= get_affiliation(Room, User).

-spec can_access_identity(Acc :: boolean(), Room :: jid:jid(), User :: jid:jid()) ->
    boolean().
can_access_identity(_Acc, _Room, _User) ->
    %% User JIDs are explicit in MUC Light but this hook is about appending
    %% 0045 MUC element with user identity and we don't want it
    false.

%%====================================================================
%% Internal functions
%%====================================================================

get_affiliation(Room, User) ->
    case mod_muc_light_db_backend:get_aff_users(jid:to_lus(Room)) of
        {ok, AffUsers, _} ->
            case lists:keyfind(jid:to_lus(User), 1, AffUsers) of
                {_, Aff} -> Aff;
                _ -> none
            end;
        _ ->
            none
    end.

-spec create_room(From :: jid:jid(), FromUS :: jid:simple_bare_jid(),
                  To :: jid:jid(), Create :: create_req_props(), OrigPacket :: exml:element()) ->
    exml:element().
create_room(From, FromUS, To, Create0, OrigPacket) ->
    case try_to_create_room(FromUS, To, Create0) of
        {ok, FinalRoomUS, Details} ->
            mod_muc_light_codec_backend:encode({set, Details, To#jid.luser == <<>>}, From,
                                               FinalRoomUS, fun ejabberd_router:route/3);
        {error, exists} ->
            mod_muc_light_codec_backend:encode_error({error, conflict}, From, To, OrigPacket,
                                                     fun ejabberd_router:route/3);
        {error, bad_request} ->
            mod_muc_light_codec_backend:encode_error({error, bad_request}, From, To, OrigPacket,
                                                     fun ejabberd_router:route/3);
        {error, Error} ->
            ErrorText = io_lib:format("~s:~p", tuple_to_list(Error)),
            mod_muc_light_codec_backend:encode_error(
              {error, bad_request, ErrorText}, From, To, OrigPacket, fun ejabberd_router:route/3)
    end.

-spec process_create_aff_users_if_valid(MUCServer :: jid:lserver(),
                                        Creator :: jid:simple_bare_jid(),
                                        AffUsers :: aff_users()) ->
    {ok, aff_users()} | {error, bad_request}.
process_create_aff_users_if_valid(MUCServer, Creator, AffUsers) ->
    case lists:any(fun ({User, _}) when User =:= Creator -> true;
                       ({_, Aff}) -> Aff =:= none end, AffUsers) of
        false ->
            process_create_aff_users(
              Creator, AffUsers, gen_mod:get_module_opt_by_subhost(
                                   MUCServer, ?MODULE, equal_occupants, ?DEFAULT_EQUAL_OCCUPANTS));
        true ->
            {error, bad_request}
    end.

-spec process_create_aff_users(Creator :: jid:simple_bare_jid(), AffUsers :: aff_users(),
                               EqualOccupants :: boolean()) ->
    {ok, aff_users()} | {error, bad_request}.
process_create_aff_users(Creator, AffUsers, EqualOccupants) ->
    case mod_muc_light_utils:change_aff_users([{Creator, creator_aff(EqualOccupants)}], AffUsers) of
        {ok, FinalAffUsers, _ChangedAffUsers, _JoiningUsers, _LeavingUsers} -> {ok, FinalAffUsers};
        Error -> Error
    end.

-spec creator_aff(EqualOccupants :: boolean()) -> owner | member.
creator_aff(true) -> member;
creator_aff(false) -> owner.

-spec handle_disco_info_get(From ::jid:jid(), To ::jid:jid(), DiscoInfo :: disco_info_req_props()) -> ok.
handle_disco_info_get(From, To, DiscoInfo) ->
    mod_muc_light_codec_backend:encode({get, DiscoInfo}, From, jid:to_lus(To),
                                       fun ejabberd_router:route/3).

-spec handle_disco_items_get(From ::jid:jid(), To ::jid:jid(), DiscoItems :: disco_items_req_props(),
                             OrigPacket :: exml:element()) -> ok.
handle_disco_items_get(From, To, DiscoItems0, OrigPacket) ->
    case catch mod_muc_light_db_backend:get_user_rooms(jid:to_lus(From), To#jid.lserver) of
        {error, Error} ->
            ?ERROR_MSG("Couldn't get room list for user ~p: ~p", [From, Error]),
            mod_muc_light_codec_backend:encode_error(
              {error, internal_server_error}, From, To, OrigPacket, fun ejabberd_router:route/3);
        Rooms ->
            RoomsInfo = get_rooms_info(lists:sort(Rooms)),
            RouteFun = fun ejabberd_router:route/3,
            RoomsPerPage = gen_mod:get_module_opt_by_subhost(
                             To#jid.lserver, ?MODULE, rooms_per_page, ?DEFAULT_ROOMS_PER_PAGE),
            case apply_rsm(RoomsInfo, length(RoomsInfo),
                           page_service_limit(DiscoItems0#disco_items.rsm, RoomsPerPage)) of
                {ok, RoomsInfoSlice, RSMOut} ->
                    DiscoItems = DiscoItems0#disco_items{ rooms = RoomsInfoSlice, rsm = RSMOut },
                    mod_muc_light_codec_backend:encode({get, DiscoItems}, From, jid:to_lus(To),
                                                       RouteFun);
                {error, item_not_found} ->
                    mod_muc_light_codec_backend:encode_error({error, item_not_found},
                                                             From, To, OrigPacket, RouteFun)
            end
    end.

-spec get_rooms_info(Rooms :: [jid:simple_bare_jid()]) -> [disco_room_info()].
get_rooms_info([]) ->
    [];
get_rooms_info([{RoomU, _} = RoomUS | RRooms]) ->
    {ok, Config, Version} = mod_muc_light_db_backend:get_config(RoomUS),
    RoomName = case lists:keyfind(roomname, 1, Config) of
                   false -> RoomU;
                   {_, RoomName0} -> RoomName0
               end,
    [{RoomUS, RoomName, Version} | get_rooms_info(RRooms)].

-spec apply_rsm(RoomsInfo :: [disco_room_info()], RoomsInfoLen :: non_neg_integer(),
                RSMIn :: jlib:rsm_in()) ->
    {ok, RoomsInfoSlice :: [disco_room_info()], RSMOut :: jlib:rsm_out()} | {error, item_not_found}.
apply_rsm(RoomsInfo, _RoomsInfoLen, none) ->
    {ok, RoomsInfo, none};
apply_rsm(_RoomsInfo, _RoomsInfoLen, #rsm_in{ max = Max }) when Max < 0 ->
    {error, item_not_found};
apply_rsm(_RoomsInfo, RoomsInfoLen, #rsm_in{ max = 0 }) ->
    {ok, [], #rsm_out{ count = RoomsInfoLen }};
apply_rsm([], 0, #rsm_in{ direction = undefined, id = undefined, index = undefined } = _RSMIn) ->
    {ok, [], none};
apply_rsm(RoomsInfo, RoomsInfoLen, #rsm_in{ direction = undefined, id = undefined,
                                            index = undefined } = RSMIn) ->
    apply_rsm(RoomsInfo, RoomsInfoLen, RSMIn#rsm_in{ index = 0 });
apply_rsm(RoomsInfo, RoomsInfoLen, #rsm_in{ direction = before, id = <<>>, max = Max }) ->
    apply_rsm(RoomsInfo, RoomsInfoLen, #rsm_in{ max = Max, index = RoomsInfoLen - Max });
apply_rsm(RoomsInfo, RoomsInfoLen, #rsm_in{ index = undefined, direction = Direction,
                                            id = RoomUSBin, max = Max }) ->
    case find_room_pos(RoomUSBin, RoomsInfo) of
        {error, item_not_found} ->
            {error, item_not_found};
        RoomPos ->
            FirstPos = case {Direction, RoomPos - Max} of
                           {aft, _} -> RoomPos + 1;
                           {before, TooLow} when TooLow < 1 -> 1;
                           {before, FirstPos0} -> FirstPos0
                       end,
            [{FirstRoomUS, _, _} | _] = RoomsInfoSlice = lists:sublist(RoomsInfo, FirstPos, Max),
            {LastRoomUS, _, _} = lists:last(RoomsInfoSlice),
            {ok, RoomsInfoSlice, #rsm_out{ count = RoomsInfoLen,
                                           index = FirstPos - 1,
                                           first = jid:to_binary(FirstRoomUS),
                                           last = jid:to_binary(LastRoomUS) }}
    end;
apply_rsm(RoomsInfo, RoomsInfoLen, #rsm_in{ max = Max, index = Index}) when Index < RoomsInfoLen ->
    [{FirstRoomUS, _, _} | _] = RoomsInfoSlice = lists:sublist(RoomsInfo, Index + 1, Max),
    {LastRoomUS, _, _} = lists:last(RoomsInfoSlice),
    {ok, RoomsInfoSlice, #rsm_out{ count = RoomsInfoLen,
                                   index = Index,
                                   first = jid:to_binary(FirstRoomUS),
                                   last = jid:to_binary(LastRoomUS) }};
apply_rsm(_RoomsInfo, _RoomsInfoLen, _RSMIn) ->
    {error, item_not_found}.

-spec page_service_limit(RSMIn :: jlib:rsm_in() | undefined, ServiceMax :: integer()) ->
    jlib:rsm_in() | none.
page_service_limit(none, infinity) -> none;
page_service_limit(none, ServiceMax) -> #rsm_in{ max = ServiceMax };
page_service_limit(#rsm_in{ max = Max } = RSMIn, ServiceMax) when Max =< ServiceMax -> RSMIn;
page_service_limit(RSMIn, ServiceMax) -> RSMIn#rsm_in{ max = ServiceMax }.

-spec find_room_pos(RoomUSBin :: binary(), RoomsInfo :: [disco_room_info()]) ->
    pos_integer() | {error, item_not_found}.
find_room_pos(RoomUSBin, RoomsInfo) ->
    case jid:from_binary(RoomUSBin) of
        error -> {error, item_not_found};
        #jid{ luser = RoomU, lserver = RoomS } -> find_room_pos({RoomU, RoomS}, RoomsInfo, 1)
    end.

-spec find_room_pos(RoomUS :: jid:simple_bare_jid(), RoomsInfo :: [disco_room_info()],
                    Pos :: pos_integer()) -> pos_integer() | {error, item_not_found}.
find_room_pos(RoomUS, [{RoomUS, _, _} | _], Pos) -> Pos;
find_room_pos(RoomUS, [_ | RRooms], Pos) -> find_room_pos(RoomUS, RRooms, Pos + 1);
find_room_pos(_, [], _) -> {error, item_not_found}.

-spec handle_blocking(From :: jid:jid(), To :: jid:jid(),
                      BlockingReq :: {get | set, blocking_req_props()}) ->
    {error, bad_request} | ok.
handle_blocking(From, To, {get, #blocking{} = Blocking}) ->
    BlockingItems = mod_muc_light_db_backend:get_blocking(jid:to_lus(From), To#jid.lserver),
    mod_muc_light_codec_backend:encode({get, Blocking#blocking{ items = BlockingItems }},
                                       From, jid:to_lus(To), fun ejabberd_router:route/3);
handle_blocking(From, To, {set, #blocking{ items = Items }} = BlockingReq) ->
    case lists:any(fun({_, _, {WhoU, WhoS}}) -> WhoU =:= <<>> orelse WhoS =:= <<>> end, Items) of
        true ->
            {error, bad_request};
        false ->
            ok = mod_muc_light_db_backend:set_blocking(jid:to_lus(From), To#jid.lserver, Items),
            mod_muc_light_codec_backend:encode(
              BlockingReq, From, jid:to_lus(To), fun ejabberd_router:route/3),
            ok
    end.

-spec bcast_removed_user(UserUS :: jid:simple_bare_jid(),
                         AffectedRooms :: mod_muc_light_db:remove_user_return(),
                         Version :: binary()) -> ok.
bcast_removed_user({UserU, UserS}, AffectedRooms, Version) ->
    bcast_removed_user(jid:make_noprep(UserU, UserS, <<>>), AffectedRooms,
                       Version, mod_muc_light_utils:bin_ts()).

-spec bcast_removed_user(UserJID :: jid:jid(),
                         AffectedRooms :: mod_muc_light_db:remove_user_return(),
                         Version :: binary(),
                         PacketID :: binary()) -> ok.
bcast_removed_user(_UserJID, [], _Version, _ID) ->
    ok;
bcast_removed_user(UserJID,
                   [{RoomUS, {ok, OldAffUsers, NewAffUsers, AffUsersChanged, PrevVersion}}
                    | RAffected], Version, ID) ->
    Affiliations = #affiliations{
                      id = ID,
                      prev_version = PrevVersion,
                      version = Version,
                      aff_users = AffUsersChanged
                     },
    mod_muc_light_codec_backend:encode({set, Affiliations, OldAffUsers, NewAffUsers},
                                       UserJID, RoomUS, fun ejabberd_router:route/3),
    bcast_removed_user(UserJID, RAffected, Version, ID);
bcast_removed_user(UserJID, [{RoomUS, Error} | RAffected], Version, ID) ->
    ?ERROR_MSG("user=~p, room=~p, remove_user_error=~p", [UserJID, RoomUS, Error]),
    bcast_removed_user(UserJID, RAffected, Version, ID).

-spec maybe_forget_rooms(AffectedRooms :: mod_muc_light_db:remove_user_return()) -> ok.
maybe_forget_rooms([]) ->
    ok;
maybe_forget_rooms([{RoomUS, {ok, _, NewAffUsers, _, _}} | RAffectedRooms]) ->
    mod_muc_light_room:maybe_forget(RoomUS, NewAffUsers),
    maybe_forget_rooms(RAffectedRooms).

