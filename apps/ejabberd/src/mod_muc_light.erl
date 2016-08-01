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
%%%
%%%----------------------------------------------------------------------

-module(mod_muc_light).
-author('piotr.nosek@erlang-solutions.com').

-behaviour(gen_mod).

%% API
-export([default_config/0]).
-export([get_opt/3, set_opt/3]).

%% gen_mod callbacks
-export([start/2, stop/1]).

%% Router export
-export([route/3]).

%% Hook handlers
-export([prevent_service_unavailable/3,
         get_muc_service/5,
         remove_user/2,
         add_rooms_to_roster/2,
         process_iq_get/5,
         process_iq_set/4,
         is_room_owner/3,
         can_access_room/3,
         muc_room_pid/2]).

%% Administration
-export([create_room_just_with_validation/3]).

%% For propEr
-export([apply_rsm/3]).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("mod_muc_light.hrl").
-include("mod_roster.hrl").

-define(CONFIG_TAB, muc_light_config).

%%====================================================================
%% API
%%====================================================================

-spec default_config() -> config().
default_config() ->
    [
     {roomname, <<"Untitled">>},
     {subject, <<>>}
    ].

-spec get_opt(MUCServer :: ejabberd:lserver(), OptName :: atom(), Default :: any()) -> any().
get_opt(MUCServer, OptName, Default) ->
    [{_, Opts}] = ets:lookup(?CONFIG_TAB, MUCServer),
    case lists:keyfind(OptName, 1, Opts) of
        false -> Default;
        {_, Val} -> Val
    end.

-spec set_opt(MUCServer :: ejabberd:lserver(), OptName :: atom(), Value :: any()) -> any().
set_opt(MUCServer, OptName, Value) ->
    [{_, Opts}] = ets:lookup(?CONFIG_TAB, MUCServer),
    ets:insert(?CONFIG_TAB, {MUCServer, lists:keystore(OptName, 1, Opts, {OptName, Value})}).

%%====================================================================
%% gen_mod callbacks
%%====================================================================

-spec start(Host :: ejabberd:server(), Opts :: list()) -> ok.
start(Host, Opts) ->
    %% Prevent sending service-unavailable on groupchat messages
    ejabberd_hooks:add(offline_groupchat_message_hook, Host,
                       ?MODULE, prevent_service_unavailable, 90),
    ejabberd_hooks:add(remove_user, Host, ?MODULE, remove_user, 50),
    ejabberd_hooks:add(disco_local_items, Host, ?MODULE, get_muc_service, 50),
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

    MyDomain = gen_mod:get_opt_host(Host, Opts, ?DEFAULT_HOST),
    ?BACKEND:start(Host, MyDomain),
    ejabberd_router:register_route(MyDomain, {apply, ?MODULE, route}),

    ejabberd_hooks:add(is_muc_room_owner, MyDomain, ?MODULE, is_room_owner, 50),
    ejabberd_hooks:add(muc_room_pid, MyDomain, ?MODULE, muc_room_pid, 50),
    ejabberd_hooks:add(can_access_room, MyDomain, ?MODULE, can_access_room, 50),

    EjdSupPid = whereis(ejabberd_sup),
    HeirOpt = case self() =:= EjdSupPid of
                  true -> [];
                  false -> [{heir, EjdSupPid, testing}] % for dynamic start from tests
              end,
    catch ets:new(?CONFIG_TAB, [set, public, named_table, {read_concurrency, true} | HeirOpt]),
    ets:insert(?CONFIG_TAB, {MyDomain, Opts}),

    ok.

-spec stop(Host :: ejabberd:server()) -> ok.
stop(Host) ->
    MyDomain = gen_mod:get_module_opt_host(Host, ?MODULE, ?DEFAULT_HOST),
    ejabberd_router:unregister_route(MyDomain),

    ets:delete(?CONFIG_TAB, MyDomain),

    ?BACKEND:stop(Host, MyDomain),

    ejabberd_hooks:delete(is_muc_room_owner, MyDomain, ?MODULE, is_room_owner, 50),
    ejabberd_hooks:delete(muc_room_pid, MyDomain, ?MODULE, muc_room_pid, 50),
    ejabberd_hooks:delete(can_access_room, MyDomain, ?MODULE, can_access_room, 50),

    ejabberd_hooks:delete(roster_get, Host, ?MODULE, add_rooms_to_roster, 50),
    ejabberd_hooks:delete(privacy_iq_get, Host, ?MODULE, process_iq_get, 1),
    ejabberd_hooks:delete(privacy_iq_set, Host, ?MODULE, process_iq_set, 1),
    ejabberd_hooks:delete(offline_groupchat_message_hook, Host,
                          ?MODULE, prevent_service_unavailable, 90),
    ejabberd_hooks:delete(remove_user, Host, ?MODULE, remove_user, 50),
    ejabberd_hooks:delete(disco_local_items, Host, ?MODULE, get_muc_service, 50),
    ok.

%%====================================================================
%% Routing
%%====================================================================

-spec route(From :: ejabberd:jid(), To :: ejabberd:jid(), Packet :: jlib:xmlel()) -> any().
route(From, To, Packet) ->
    process_packet(From, To, ?CODEC:decode(From, To, Packet), Packet).

-spec process_packet(From :: ejabberd:jid(), To :: ejabberd:jid(),
                     DecodedPacket :: mod_muc_light_codec:decode_result(),
                     OrigPacket :: jlib:xmlel()) -> any().
process_packet(From, To, {ok, {set, #create{} = Create}}, OrigPacket) ->
    RoomsPerUser = get_opt(To#jid.lserver, rooms_per_user, ?DEFAULT_ROOMS_PER_USER),
    FromUS = jid:to_lus(From),
    case RoomsPerUser == infinity orelse length(?BACKEND:get_user_rooms(FromUS)) < RoomsPerUser of
        true ->
            create_room(From, FromUS, To, Create, OrigPacket);
        false ->
            ?CODEC:encode_error(
              {error, bad_request}, From, To, OrigPacket, fun ejabberd_router:route/3)
    end;
process_packet(From, To, {ok, {get, #disco_info{} = DI}}, _OrigPacket) ->
    handle_disco_info_get(From, To, DI);
process_packet(From, To, {ok, {get, #disco_items{} = DI}}, OrigPacket) ->
    handle_disco_items_get(From, To, DI, OrigPacket);
process_packet(From, To, {ok, {_, #blocking{}} = Blocking}, OrigPacket) ->
    case get_opt(To#jid.lserver, blocking, ?DEFAULT_BLOCKING) of
        true ->
            case handle_blocking(From, To, Blocking) of
                ok ->
                    ok;
                Error ->
                    ?CODEC:encode_error(Error, From, To, OrigPacket,
                                                     fun ejabberd_router:route/3)
            end;
        false -> ?CODEC:encode_error(
                   {error, bad_request}, From, To, OrigPacket, fun ejabberd_router:route/3)
    end;
process_packet(From, To, {ok, #iq{} = IQ}, OrigPacket) ->
    case mod_muc_iq:process_iq(To#jid.lserver, From, To, IQ) of
        ignore -> ok;
        error ->
            ?CODEC:encode_error(
              {error, feature_not_implemented}, From, To, OrigPacket, fun ejabberd_router:route/3);
        ResIQ ->
            ejabberd_router:route(To, From, jlib:iq_to_xml(ResIQ))
    end;
process_packet(From, #jid{ luser = RoomU } = To, {ok, RequestToRoom}, OrigPacket)
  when RoomU =/= <<>> ->
    case ?BACKEND:room_exists(jid:to_lus(To)) of
        true -> mod_muc_light_room:handle_request(From, To, OrigPacket, RequestToRoom);
        false -> ?CODEC:encode_error({error, item_not_found}, From, To, OrigPacket,
                                                  fun ejabberd_router:route/3)
    end;
process_packet(From, To, {error, _} = Err, OrigPacket) ->
    ?CODEC:encode_error(Err, From, To, OrigPacket, fun ejabberd_router:route/3);
process_packet(From, To, _InvalidReq, OrigPacket) ->
    ?CODEC:encode_error(
      {error, bad_request}, From, To, OrigPacket, fun ejabberd_router:route/3).

%%====================================================================
%% Hook handlers
%%====================================================================

-spec prevent_service_unavailable(From :: jid(), To :: jid(), Packet :: #xmlel{}) -> ok | stop.
prevent_service_unavailable(_From, _To, Packet) ->
    case xml:get_tag_attr_s(<<"type">>, Packet) of
        <<"groupchat">> -> stop;
        _Type -> ok
    end.

-spec get_muc_service(Acc :: {result, [jlib:xmlel()]}, From :: ejabberd:jid(), To :: ejabberd:jid(),
                      NS :: binary(), ejabberd:lang()) -> {result, [jlib:xmlel()]}.
get_muc_service({result, Nodes}, _From, #jid{lserver = LServer} = _To, <<"">>, _Lang) ->
    XMLNS = case get_opt(LServer, legacy_mode, ?DEFAULT_LEGACY_MODE) of
                true -> ?NS_MUC;
                false -> ?NS_MUC_LIGHT
            end,
    Host = gen_mod:get_module_opt_host(LServer, ?MODULE, ?DEFAULT_HOST),
    Item = [#xmlel{name = <<"item">>,
                   attrs = [{<<"jid">>, Host},
                            {<<"node">>, XMLNS}]}],
    {result, [Item | Nodes]};
get_muc_service(Acc, _From, _To, _Node, _Lang) ->
    Acc.

-spec remove_user(User :: binary(), Server :: binary()) -> ok.
remove_user(User, Server) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Server),
    UserUS = {LUser, LServer},
    Version = mod_muc_light_utils:bin_ts(),
    case ?BACKEND:remove_user(UserUS, Version) of
        {error, _} = Err -> ?ERROR_MSG("hook=remove_user,error=~p", [Err]);
        AffectedRooms -> bcast_removed_user(UserUS, AffectedRooms, Version)
    end.

-spec add_rooms_to_roster(Acc :: [#roster{}], UserUS :: ejabberd:simple_bare_jid()) -> [#roster{}].
add_rooms_to_roster(Acc, UserUS) ->
    lists:foldl(
      fun({{RoomU, RoomS}, RoomName, RoomVersion}, Acc0) ->
              Item = #roster{
                        jid = jid:make_noprep(RoomU, RoomS, <<>>),
                        name = RoomName,
                        subscription = to,
                        groups = [?NS_MUC_LIGHT],
                        xs = [#xmlel{ name = <<"version">>,
                                      children = [#xmlcdata{ content = RoomVersion }] }]
                       },
              [Item | Acc0]
      end, Acc, get_rooms_info(lists:sort(?BACKEND:get_user_rooms(UserUS)))).

-spec process_iq_get(Acc :: any(), From :: #jid{}, To :: #jid{},
                     IQ :: #iq{}, ActiveList :: binary()) ->
    {stop, {result, [jlib:xmlel()]}} | {error, #xmlel{}}.
process_iq_get(_Acc, #jid{ lserver = FromS } = From, To, #iq{} = IQ, _ActiveList) ->
    MUCHost = gen_mod:get_module_opt_host(FromS, ?MODULE, ?DEFAULT_HOST),
    case {?CODEC:decode(From, To, IQ), get_opt(MUCHost, blocking, ?DEFAULT_BLOCKING)} of
        {{ok, {get, #blocking{} = Blocking}}, true} ->
            Items = ?BACKEND:get_blocking(jid:to_lus(From)),
            ?CODEC:encode({get, Blocking#blocking{ items = Items }}, From, jid:to_lus(To),
                          fun(_, _, Packet) -> put(encode_res, Packet) end),
            #xmlel{ children = ResponseChildren } = erase(encode_res),
            {stop, {result, ResponseChildren}};
        {{ok, {get, #blocking{}}}, false} ->
            {stop, {error, ?ERR_BAD_REQUEST}};
        _ ->
            {error, ?ERR_BAD_REQUEST}
    end.

-spec process_iq_set(Acc :: any(), From :: #jid{}, To :: #jid{}, IQ :: #iq{}) ->
    {stop, {result, [jlib:xmlel()]}} | {error, #xmlel{}}.
process_iq_set(_Acc, #jid{ lserver = FromS } = From, To, #iq{} = IQ) ->
    MUCHost = gen_mod:get_module_opt_host(FromS, ?MODULE, ?DEFAULT_HOST),
    case {?CODEC:decode(From, To, IQ), get_opt(MUCHost, blocking, ?DEFAULT_BLOCKING)} of
        {{ok, {set, #blocking{ items = Items }} = Blocking}, true} ->
            case lists:any(fun({_, _, {WhoU, WhoS}}) ->
                                   WhoU =:= <<>> orelse WhoS =:= <<>>
                           end, Items) of
                true ->
                    {stop, {error, ?ERR_BAD_REQUEST}};
                false ->
                    ok = ?BACKEND:set_blocking(jid:to_lus(From), Items),
                    ?CODEC:encode(Blocking, From, jid:to_lus(To),
                                  fun(_, _, Packet) -> put(encode_res, Packet) end),
                    #xmlel{ children = ResponseChildren } = erase(encode_res),
                    {stop, {result, ResponseChildren}}
            end;
        {{ok, {set, #blocking{}}}, false} ->
            {stop, {error, ?ERR_BAD_REQUEST}};
        _ ->
            {error, ?ERR_BAD_REQUEST}
    end.

-spec is_room_owner(Acc :: boolean(), Room :: ejabberd:jid(), User :: ejabberd:jid()) -> boolean().
is_room_owner(_, Room, User) ->
    owner == get_affiliation(Room, User).

-spec muc_room_pid(Acc :: any(), Room :: ejabberd:jid()) -> {ok, processless}.
muc_room_pid(_, _) ->
    {ok, processless}.

-spec can_access_room(Acc :: boolean(), Room :: ejabberd:jid(), User :: ejabberd:jid()) ->
    boolean().
can_access_room(_, User, Room) ->
    none =/= get_affiliation(Room, User).

%%====================================================================
%% Internal functions
%%====================================================================

get_affiliation(Room, User) ->
    case ?BACKEND:get_aff_users(jid:to_lus(Room)) of
        {ok, AffUsers, _} ->
            case lists:keyfind(jid:to_lus(User), 1, AffUsers) of
                {_, Aff} -> Aff;
                _ -> none
            end;
        _ ->
            none
    end.


-spec create_room(From :: ejabberd:jid(), FromUS :: ejabberd:simple_bare_jid(),
                  To :: ejabberd:jid(), Create :: #create{}, OrigPacket :: jlib:xmlel()) -> ok.
create_room(From, FromUS, To, #create{ raw_config = RawConfig } = Create0, OrigPacket) ->
    {RoomU, _} = RoomUS = jid:to_lus(To), % might be service JID for room autogeneration
    InitialAffUsers = mod_muc_light_utils:filter_out_prevented(
                        FromUS, RoomUS, Create0#create.aff_users),
    MaxOccupants = get_opt(To#jid.lserver, max_occupants, ?DEFAULT_MAX_OCCUPANTS),
    case {mod_muc_light_utils:process_raw_config(RawConfig, default_config()),
          process_create_aff_users_if_valid(To#jid.lserver, FromUS, InitialAffUsers)} of
        {{ok, Config0}, {ok, FinalAffUsers}} when length(FinalAffUsers) =< MaxOccupants ->
            Version = mod_muc_light_utils:bin_ts(),
            case ?BACKEND:create_room(RoomUS, lists:sort(Config0), FinalAffUsers, Version) of
                {ok, FinalRoomUS} ->
                    Create = Create0#create{ version = Version, aff_users = FinalAffUsers },
                    ?CODEC:encode({set, Create, RoomU == <<>>}, From,
                                  FinalRoomUS, fun ejabberd_router:route/3);
                {error, exists} ->
                    ?CODEC:encode_error({error, conflict}, From, To, OrigPacket,
                                                     fun ejabberd_router:route/3)
            end;
        {{error, Error}, _} ->
            ErrorText = io_lib:format("~s:~p", tuple_to_list(Error)),
            ?CODEC:encode_error({error, bad_request, ErrorText}, From, To, OrigPacket,
                                             fun ejabberd_router:route/3);
        {_, _} ->
            ?CODEC:encode_error({error, bad_request}, From, To, OrigPacket,
                                             fun ejabberd_router:route/3)
    end.

-spec create_room_just_with_validation(FromUS :: ejabberd:simple_bare_jid(),
                  To :: ejabberd:jid(), Create :: #create{}) -> ok.
create_room_just_with_validation(FromUS, To, #create{ raw_config = RawConfig } = Create0) ->
    RoomUS = jid:to_lus(To), % might be service JID for room autogeneration
    InitialAffUsers = mod_muc_light_utils:filter_out_prevented(
                        FromUS, RoomUS, Create0#create.aff_users),
    MaxOccupants = get_opt(To#jid.lserver, max_occupants, ?DEFAULT_MAX_OCCUPANTS),
    case {mod_muc_light_utils:process_raw_config(RawConfig, default_config()),
          process_create_aff_users_if_valid(To#jid.lserver, FromUS, InitialAffUsers)} of
        {{ok, Config0}, {ok, FinalAffUsers}} when length(FinalAffUsers) =< MaxOccupants ->
            Version = mod_muc_light_utils:bin_ts(),
            case ?BACKEND:create_room(RoomUS, lists:sort(Config0), FinalAffUsers, Version) of
                {ok, FinalRoomUS} ->
                    FinalRoomUS;
                {error, exists} = E ->
                    E
            end;
        {_, _} ->
            {error, erlang:get_stacktrace()}
    end.

-spec process_create_aff_users_if_valid(MUCServer :: ejabberd:lserver(),
                                        Creator :: ejabberd:simple_bare_jid(),
                                        AffUsers :: aff_users()) ->
    {ok, aff_users()} | {error, bad_request}.
process_create_aff_users_if_valid(MUCServer, Creator, AffUsers) ->
    case lists:any(fun ({User, _}) when User =:= Creator -> true;
                       ({_, Aff}) -> Aff =:= none end, AffUsers) of
        false ->
            process_create_aff_users(
              Creator, AffUsers, get_opt(MUCServer, equal_occupants, ?DEFAULT_EQUAL_OCCUPANTS));
        true ->
            {error, bad_request}
    end.

-spec process_create_aff_users(Creator :: ejabberd:simple_bare_jid(), AffUsers :: aff_users(),
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

-spec handle_disco_info_get(From :: jid(), To :: jid(), DiscoInfo :: #disco_info{}) -> ok.
handle_disco_info_get(From, To, DiscoInfo) ->
    ?CODEC:encode({get, DiscoInfo}, From, jid:to_lus(To), fun ejabberd_router:route/3).

-spec handle_disco_items_get(From :: jid(), To :: jid(), DiscoItems :: #disco_items{},
                            OrigPacket :: jlib:xmlel()) -> ok.
handle_disco_items_get(From, To, DiscoItems0, OrigPacket) ->
    case catch ?BACKEND:get_user_rooms(jid:to_lus(From)) of
        {error, Error} ->
            ?ERROR_MSG("Couldn't get room list for user ~p: ~p", [From, Error]),
            ?CODEC:encode_error({error, internal_server_error}, From, To, OrigPacket,
                                fun ejabberd_router:route/3);
        Rooms ->
            RoomsInfo = get_rooms_info(lists:sort(Rooms)),
            RoomsPerPage = get_opt(To#jid.lserver, rooms_per_page, ?DEFAULT_ROOMS_PER_PAGE),
            case apply_rsm(RoomsInfo, length(RoomsInfo),
                           page_service_limit(DiscoItems0#disco_items.rsm, RoomsPerPage)) of
                {ok, RoomsInfoSlice, RSMOut} ->
                    DiscoItems = DiscoItems0#disco_items{ rooms = RoomsInfoSlice, rsm = RSMOut },
                    ?CODEC:encode({get, DiscoItems}, From, jid:to_lus(To),
                                  fun ejabberd_router:route/3);
                {error, item_not_found} ->
                    ?CODEC:encode_error({error, item_not_found}, From, To, OrigPacket,
                                        fun ejabberd_router:route/3)

            end
    end.

-spec get_rooms_info(Rooms :: [ejabberd:simple_bare_jid()]) -> [disco_room_info()].
get_rooms_info([]) ->
    [];
get_rooms_info([RoomUS | RRooms]) ->
    {ok, RoomName, Version} = ?BACKEND:get_config(RoomUS, roomname),
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

-spec find_room_pos(RoomUS :: ejabberd:simple_bare_jid(), RoomsInfo :: [disco_room_info()],
                    Pos :: pos_integer()) -> pos_integer() | {error, item_not_found}.
find_room_pos(RoomUS, [{RoomUS, _, _} | _], Pos) -> Pos;
find_room_pos(RoomUS, [_ | RRooms], Pos) -> find_room_pos(RoomUS, RRooms, Pos + 1);
find_room_pos(_, [], _) -> {error, item_not_found}.

-spec handle_blocking(From :: ejabberd:jid(), To :: ejabberd:jid(),
                      BlockingReq :: {get | set, #blocking{}}) ->
    {error, bad_request} | ok.
handle_blocking(From, To, {get, #blocking{} = Blocking}) ->
    ?CODEC:encode({get, Blocking#blocking{ items = ?BACKEND:get_blocking(jid:to_lus(From)) }},
                  From, jid:to_lus(To), fun ejabberd_router:route/3);
handle_blocking(From, To, {set, #blocking{ items = Items }} = BlockingReq) ->
    case lists:any(fun({_, _, {WhoU, WhoS}}) -> WhoU =:= <<>> orelse WhoS =:= <<>> end, Items) of
        true ->
            {error, bad_request};
        false ->
            ok = ?BACKEND:set_blocking(jid:to_lus(From), Items),
            ?CODEC:encode(BlockingReq, From, jid:to_lus(To), fun ejabberd_router:route/3),
            ok
    end.

-spec bcast_removed_user(UserUS :: ejabberd:simple_bare_jid(),
                         AffectedRooms :: mod_muc_light_db:remove_user_return(),
                         Version :: binary()) -> ok.
bcast_removed_user({UserU, UserS}, AffectedRooms, Version) ->
    bcast_removed_user(jid:make_noprep(UserU, UserS, <<>>), AffectedRooms,
                       Version, mod_muc_light_utils:bin_ts()).

-spec bcast_removed_user(UserJID :: ejabberd:jid(),
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
    ?CODEC:encode({set, Affiliations, OldAffUsers, NewAffUsers},
                  UserJID, RoomUS, fun ejabberd_router:route/3),
    bcast_removed_user(UserJID, RAffected, Version, ID);
bcast_removed_user(UserJID, [{RoomUS, Error} | RAffected], Version, ID) ->
    ?ERROR_MSG("user=~p, room=~p, remove_user_error=~p", [UserJID, RoomUS, Error]),
    bcast_removed_user(UserJID, RAffected, Version, ID).

