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
%%% !!NOT IMPLEMENTED YET!! * simple_aff_changes_model (false) - If enabled, only these aff changes requests are allowed
%%%                                      (simpler, more efficient algorithm can be used):
%%%     * owner leaves (and optionally picks the successor)
%%%     * owner adds and removes member (no change in membership)
%%%     * member adds users
%%%     * member leaves
%%%
%%%----------------------------------------------------------------------

-module(mod_muc_light).
-author('piotr.nosek@erlang-solutions.com').

-behaviour(gen_mod).

%% API
-export([default_config/0]).
-export([get_service_opt/2]).

%% gen_mod callbacks
-export([start/2, stop/1]).

%% Router export
-export([route/3]).

%% Hook handlers
-export([prevent_service_unavailable/3,
         get_muc_service/5,
         remove_user/2]).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("mod_muc_light.hrl").

-define(DEFAULT_HOST, <<"muclight.@HOST@">>).

%%====================================================================
%% API
%%====================================================================

-spec default_config() -> config().
default_config() ->
    [
     {roomname, <<"Untitled">>},
     {subject, <<>>}
    ].

-spec get_service_opt(OptName :: atom(), Default :: any()) -> any().
get_service_opt(OptName, Default) ->
    gen_mod:get_module_opt(global, ?MODULE, OptName, Default).

%%====================================================================
%% gen_mod callbacks
%%====================================================================

-spec start(Host :: ejabberd:server(), Opts :: list()) -> ok.
start(Host, Opts) ->
    %% Prevent sending service-unavailable on groupchat messages
    ejabberd_hooks:add(offline_message_hook, Host,
                       ?MODULE, prevent_service_unavailable, 90),
    ejabberd_hooks:add(remove_user, Host, ?MODULE, remove_user, 50),
    ejabberd_hooks:add(disco_local_items, Host, ?MODULE, get_muc_service, 50),

    MyDomain = gen_mod:get_opt_host(Host, Opts, ?DEFAULT_HOST),
    ejabberd_router:register_route(MyDomain, {apply, ?MODULE, route}),

    code:load_file(mod_muc_light_utils), % ensures loading atoms from utils
    code:soft_purge(mod_muc_light_utils),
    
    TrackedDBFuns = [create_room, destroy_room, room_exists, get_user_rooms, remove_user,
                     get_config, set_config, get_blocking, set_blocking,
                     get_aff_users, modify_aff_users],
    gen_mod:start_backend_module(mod_muc_light_db, Opts, TrackedDBFuns),
    Codec = case gen_mod:get_opt(legacy_mode, Opts, ?DEFAULT_LEGACY_MODE) of
        false -> modern;
        true -> legacy
    end,
    gen_mod:start_backend_module(mod_muc_light_codec, [{backend, Codec}], []),
    ?BACKEND:start(Host, MyDomain),
    ok.

-spec stop(Host :: ejabberd:server()) -> ok.
stop(Host) ->
    MyDomain = gen_mod:get_module_opt_host(Host, ?MODULE, ?DEFAULT_HOST),
    ejabberd_router:unregister_route(MyDomain),

    ?BACKEND:stop(Host, MyDomain),

    ejabberd_hooks:delete(offline_message_hook, Host,
                          ?MODULE, prevent_service_unavailable, 90),
    ejabberd_hooks:delete(remove_user, Host, ?MODULE, remove_user, 50),
    ejabberd_hooks:delete(disco_local_items, Host, ?MODULE, get_muc_service, 50),
    ok.

%%====================================================================
%% Routing
%%====================================================================

-spec route(From :: jlib:jid(), To :: jlib:jid(), Packet :: jlib:xmlel()) -> any().
route(From, To, Packet) ->
    process_packet(From, To, ?CODEC:decode(From, To, Packet), Packet).

-spec process_packet(From :: jlib:jid(), To :: jlib:jid(),
                     DecodedPacket :: mod_muc_light_codec:decode_result(),
                     OrigPacket :: jlib:xmlel()) -> any().
process_packet(From, To, {ok, {set, #create{} = Create}}, OrigPacket) ->
    create_room(From, To, Create, OrigPacket);
process_packet(From, To, {ok, {get, #disco_info{} = DI}}, _OrigPacket) ->
    handle_disco_info_get(From, To, DI);
process_packet(From, To, {ok, {get, #disco_items{} = DI}}, OrigPacket) ->
    handle_disco_items_get(From, To, DI, OrigPacket);
process_packet(From, To, {ok, {_, #blocking{}} = Blocking}, OrigPacket) ->
    handle_blocking(From, To, Blocking, OrigPacket);
process_packet(From, #jid{ luser = RoomU } = To, {ok, RequestToRoom}, OrigPacket)
  when RoomU =/= <<>> ->
    case ?BACKEND:room_exists(jlib:jid_to_lus(To)) of
        true -> mod_muc_light_room:handle_request(From, To, OrigPacket, RequestToRoom);
        false -> mod_muc_light_codec:encode_error({error, item_not_found}, From, To, OrigPacket,
                                                  fun ejabberd_router:route/3)
    end;
process_packet(From, To, {error, _} = Err, OrigPacket) ->
    mod_muc_light_codec:encode_error(Err, From, To, OrigPacket, fun ejabberd_router:route/3);
process_packet(From, To, _InvalidReq, OrigPacket) ->
    mod_muc_light_codec:encode_error(
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
    XMLNS = case get_service_opt(legacy_mode, ?DEFAULT_LEGACY_MODE) of
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
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    UserUS = {LUser, LServer},
    Version = mod_muc_light_utils:bin_ts(),
    case ?BACKEND:remove_user(UserUS, Version) of
        {error, _} = Err -> ?ERROR_MSG("hook=remove_user,error=~p", [Err]);
        AffectedRooms -> bcast_removed_user(UserUS, AffectedRooms, Version)
    end.

%%====================================================================
%% Internal functions
%%====================================================================

-spec create_room(From :: jlib:jid(), To :: jlib:jid(), Create :: #create{},
                  OrigPacket :: jlib:xmlel()) -> ok.
create_room(From, To, #create{ raw_config = RawConfig } = Create0, OrigPacket) ->
    FromUS = jlib:jid_to_lus(From),
    RoomUS = jlib:jid_to_lus(To), % might be service JID for room autogeneration
    InitialAffUsers = mod_muc_light_utils:filter_out_prevented(
                        FromUS, RoomUS, Create0#create.aff_users),
    case {mod_muc_light_utils:process_raw_config(RawConfig, default_config()),
          process_create_aff_users(FromUS, InitialAffUsers)} of
        {{ok, Config0}, {ok, FinalAffUsers}} ->
            Version = mod_muc_light_utils:bin_ts(),
            case ?BACKEND:create_room(RoomUS, lists:sort(Config0), FinalAffUsers, Version) of
                {ok, FinalRoomUS} ->
                    Create = Create0#create{ version = Version, aff_users = FinalAffUsers },
                    ?CODEC:encode({set, Create}, From, FinalRoomUS, fun ejabberd_router:route/3);
                {error, exists} ->
                    mod_muc_light_codec:encode_error({error, conflict}, From, To, OrigPacket,
                                                     fun ejabberd_router:route/3)
            end;
        {{error, Error}, _} ->
            ErrorText = io_lib:format("~s:~p", tuple_to_list(Error)),
            mod_muc_light_codec:encode_error({error, bad_request, ErrorText}, From, To, OrigPacket,
                                             fun ejabberd_router:route/3);
        {_, {error, bad_request} = Error} ->
            mod_muc_light_codec:encode_error(Error, From, To, OrigPacket,
                                             fun ejabberd_router:route/3)
    end.

-spec process_create_aff_users(Creator :: ejabberd:simple_bare_jid(), AffUsers :: aff_users()) ->
    {ok, aff_users()} | {error, bad_request}.
process_create_aff_users(Creator, AffUsers) ->
    case lists:any(fun ({User, _}) when User =:= Creator -> true;
                       ({_, Aff}) -> Aff =:= none end, AffUsers) of
        false ->
            process_create_aff_users(
              Creator, AffUsers, get_service_opt(equal_occupants, ?DEFAULT_EQUAL_OCCUPANTS));
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
    ?CODEC:encode({get, DiscoInfo}, From, jlib:jid_to_lus(To), fun ejabberd_router:route/3).

-spec handle_disco_items_get(From :: jid(), To :: jid(), DiscoItems :: #disco_items{},
                            OrigPacket :: jlib:xmlel()) -> ok.
handle_disco_items_get(From, To, DiscoItems0, OrigPacket) ->
    case ?BACKEND:get_user_rooms(jlib:jid_to_lus(From)) of
        {ok, Rooms} ->
            RoomsInfo = get_rooms_info(Rooms),
            DiscoItems = DiscoItems0#disco_items{ rooms = RoomsInfo },
            ?CODEC:encode({get, DiscoItems}, From, jlib:jid_to_lus(To),
                          fun ejabberd_router:route/3);
        {error, Error} ->
            ?ERROR_MSG("Couldn't get room list for user ~p: ~p", [From, Error]),
            mod_muc_light_codec:encode_error({error, internal_server_error}, From, To, OrigPacket,
                                             fun ejabberd_router:route/3)
    end.

-spec get_rooms_info(Rooms :: [ejabberd:simple_bare_jid()]) ->
    [{RoomUS :: ejabberd:simple_bare_jid(), RoomName :: binary(), RoomVersion :: binary()}].
get_rooms_info([]) ->
    [];
get_rooms_info([RoomUS | RRooms]) ->
    {ok, RoomName, Version} = ?BACKEND:get_config(RoomUS, roomname),
    [{RoomUS, RoomName, Version} | get_rooms_info(RRooms)].

-spec handle_blocking(From :: jlib:jid(), To :: jlib:jid(),
                      BlockingReq :: {get | set, #blocking{}},
                      OrigPacket :: jlib:xmlel()) -> any().
handle_blocking(From, To, {get, #blocking{} = Blocking}, _OrigPacket) ->
    ?CODEC:encode({get, Blocking#blocking{ items = ?BACKEND:get_blocking(jlib:jid_to_lus(From)) }},
                  From, jlib:jid_to_lus(To), fun ejabberd_router:route/3);
handle_blocking(From, To, {set, #blocking{ items = Items }} = BlockingReq, OrigPacket) ->
    case lists:any(fun({_, _, {WhoU, WhoS}}) -> WhoU =:= <<>> orelse WhoS =:= <<>> end, Items) of
        true ->
            mod_muc_light_codec:encode_error({error, bad_request}, From, To, OrigPacket,
                                             fun ejabberd_router:route/3);
        false ->
            ok = ?BACKEND:set_blocking(jlib:jid_to_lus(From), Items),
            ?CODEC:encode(BlockingReq, From, jlib:jid_to_lus(To), fun ejabberd_router:route/3)
    end.

-spec bcast_removed_user(UserUS :: ejabberd:simple_bare_jid(),
                         AffectedRooms :: mod_muc_light_db:remove_user_return(),
                         Version :: binary()) -> ok.
bcast_removed_user({UserU, UserS}, AffectedRooms, Version) ->
    bcast_removed_user(jlib:make_jid_noprep(UserU, UserS, <<>>), AffectedRooms,
                       Version, mod_muc_light_utils:bin_ts()).

-spec bcast_removed_user(UserJID :: jlib:jid(),
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

