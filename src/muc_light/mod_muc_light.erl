%%%----------------------------------------------------------------------
%%% File    : mod_muc_light.erl
%%% Author  : Piotr Nosek <piotr.nosek@erlang-solutions.com>
%%% Purpose : MUC light support
%%% Created : 8 Sep 2014 by Piotr Nosek <piotr.nosek@erlang-solutions.com>
%%%
%%% Looking for documentation excerpt that was present here for 5 years?
%%% Now everything is moved to doc/modules/mod_muc_light.md
%%%
%%%----------------------------------------------------------------------

-module(mod_muc_light).
-author('piotr.nosek@erlang-solutions.com').

-include("mongoose.hrl").
-include("jlib.hrl").
-include("mod_muc_light.hrl").
-include("mod_roster.hrl").
-include("mongoose_rsm.hrl").
-include("mongoose_config_spec.hrl").

-behaviour(gen_mod).
-behaviour(mongoose_packet_handler).
-behaviour(mongoose_module_metrics).

%% API
-export([default_schema_definition/0, default_host/0]).
-export([config_schema/1, default_config/1]).

%% For Administration API
-export([try_to_create_room/3,
         change_room_config/5,
         delete_room/1]).

%% gen_mod callbacks
-export([start/2, stop/1, config_spec/0]).

%% config processing callback
-export([process_config_schema/1]).

%% Packet handler export
-export([process_packet/5]).

%% Hook handlers
-export([prevent_service_unavailable/4,
         disco_local_items/5,
         remove_user/3,
         remove_domain/3,
         add_rooms_to_roster/2,
         process_iq_get/5,
         process_iq_set/4,
         is_muc_room_owner/4,
         can_access_room/4,
         can_access_identity/4]).

%% For propEr
-export([apply_rsm/3]).

-export([config_metrics/1]).

-type muc_server() :: jid:lserver().
-type host_type() :: mongooseim:host_type().

%%====================================================================
%% API
%%====================================================================

-spec default_schema_definition() -> mod_muc_light_room_config:user_defined_schema().
default_schema_definition() ->
    [{"roomname", "Untitled"},
     {"subject", ""}].

-spec default_host() -> mongoose_subdomain_utils:subdomain_pattern().
default_host() ->
    mongoose_subdomain_utils:make_subdomain_pattern(<<"muclight.@HOST@">>).

-spec default_config(MUCServer :: muc_server()) -> mod_muc_light_room_config:kv().
default_config(MUCServer) ->
    HostType = mod_muc_light_utils:muc_host_to_host_type(MUCServer),
    default_config_for_host_type(HostType).

-spec config_schema(MUCServer :: muc_server()) -> mod_muc_light_room_config:schema().
config_schema(MUCServer) ->
    HostType = mod_muc_light_utils:muc_host_to_host_type(MUCServer),
    config_schema_for_host_type(HostType).

%% Internals
-spec default_config_for_host_type(host_type()) -> mod_muc_light_room_config:kv().
default_config_for_host_type(HostType) ->
    gen_mod:get_module_opt(HostType, ?MODULE, default_config, []).

-spec config_schema_for_host_type(host_type()) -> mod_muc_light_room_config:schema().
config_schema_for_host_type(HostType) ->
    gen_mod:get_module_opt(HostType, ?MODULE, config_schema, undefined).

%%====================================================================
%% Administration API
%%====================================================================

-spec try_to_create_room(CreatorUS :: jid:simple_bare_jid(), RoomJID :: jid:jid(),
                         CreationCfg :: create_req_props()) ->
    {ok, jid:simple_bare_jid(), create_req_props()}
    | {error, validation_error() | bad_request | exists | max_occupants_reached}.
try_to_create_room(CreatorUS, RoomJID, #create{raw_config = RawConfig} = CreationCfg) ->
    RoomUS = jid:to_lus(RoomJID),
    HostType = mod_muc_light_utils:room_jid_to_host_type(RoomJID),
    CfgRes = prepare_config(HostType, RawConfig),
    AffRes = prepare_affs(HostType, CreatorUS, RoomUS, CreationCfg),
    case {CfgRes, AffRes} of
        {{ok, Config0}, {ok, FinalAffUsers}} ->
            Version = mongoose_bin:gen_from_timestamp(),
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
        {_, {error, _} = Error} ->
            Error
    end.

-spec change_room_config(UserJid :: jid:jid(), RoomID :: jid:resource(),
                         MUCLightDomain :: jid:server(),
                         ConfigReq :: config_req_props(),
                         Acc :: mongoose_acc:t()) ->
    {ok, jid:jid(), config_req_props()}
    | {error, validation_error() | bad_request | not_allowed}.
change_room_config(UserJid, RoomID, MUCLightDomain, ConfigReq, Acc) ->
    R = {RoomID, MUCLightDomain},
    RoomJID = jid:make(RoomID, MUCLightDomain, <<>>),
    RoomUS = jid:to_lus(RoomJID),
    AffUsersRes = mod_muc_light_db_backend:get_aff_users(RoomUS),

    case mod_muc_light_room:process_request(UserJid, R, {set, ConfigReq}, AffUsersRes, Acc) of
        {set, ConfigResp, _} ->
            {ok, RoomJID, ConfigResp};
        {error, _Reason} = E ->
            E
    end.

-spec delete_room(RoomUS :: jid:simple_bare_jid()) -> ok | {error, not_exists}.
delete_room(RoomUS) ->
    mod_muc_light_db_backend:destroy_room(RoomUS).

%%====================================================================
%% gen_mod callbacks
%%====================================================================

-spec start(HostType :: host_type(), Opts :: list()) -> ok.
start(HostType, Opts) ->
    set_dynamic_opts(HostType, Opts),
    Codec = host_type_to_codec(HostType),
    gen_mod:start_backend_module(mod_muc_light_db, Opts, tracked_db_funs()),
    gen_mod:start_backend_module(mod_muc_light_codec, [{backend, Codec}], []),
    mod_muc_light_db_backend:start(HostType),
    ejabberd_hooks:add(hooks(HostType)),
    %% Handler
    SubdomainPattern = subdomain_pattern(HostType),
    PacketHandler = mongoose_packet_handler:new(?MODULE),
    mongoose_domain_api:register_subdomain(HostType, SubdomainPattern, PacketHandler),
    ok.

-spec stop(HostType :: host_type()) -> ok.
stop(HostType) ->
    SubdomainPattern = subdomain_pattern(HostType),
    mongoose_domain_api:unregister_subdomain(HostType, SubdomainPattern),
    mod_muc_light_db_backend:stop(HostType),
    ejabberd_hooks:delete(hooks(HostType)),
    ok.

%% Init helpers
subdomain_pattern(HostType) ->
    gen_mod:get_module_opt(HostType, ?MODULE, host, default_host()).

server_host_to_muc_host(HostType, ServerHost) ->
    mongoose_subdomain_utils:get_fqdn(subdomain_pattern(HostType), ServerHost).

host_type_to_codec(HostType) ->
    case gen_mod:get_module_opt(HostType, legacy_mode, ?MODULE, ?DEFAULT_LEGACY_MODE) of
        false ->
            modern;
        true ->
            legacy
    end.

tracked_db_funs() ->
    [create_room, destroy_room, room_exists, get_user_rooms,
     remove_user, remove_domain,
     get_config, set_config, get_blocking, set_blocking,
     get_aff_users, modify_aff_users].

set_dynamic_opts(HostType, Opts) ->
    %% Prepare config schema
    Def = gen_mod:get_opt(config_schema, Opts, default_schema_definition()),
    ConfigSchema = mod_muc_light_room_config:schema_from_definition(Def),
    gen_mod:set_module_opt(HostType, ?MODULE, config_schema, ConfigSchema),
    %% Prepare default config
    DefaultConfig = mod_muc_light_room_config:default_from_schema(ConfigSchema),
    gen_mod:set_module_opt(HostType, ?MODULE, default_config, DefaultConfig).

%% Config callbacks
-spec config_spec() -> mongoose_config_spec:config_section().
config_spec() ->
    #section{
       items = #{<<"backend">> => #option{type = atom,
                                          validate = {module, mod_muc_light_db}},
                 <<"host">> => #option{type = string,
                                       validate = subdomain_template,
                                       process = fun mongoose_subdomain_utils:make_subdomain_pattern/1},
                 <<"equal_occupants">> => #option{type = boolean},
                 <<"legacy_mode">> => #option{type = boolean},
                 <<"rooms_per_user">> => #option{type = int_or_infinity,
                                                 validate = positive},
                 <<"blocking">> => #option{type = boolean},
                 <<"all_can_configure">> => #option{type = boolean},
                 <<"all_can_invite">> => #option{type = boolean},
                 <<"max_occupants">> => #option{type = int_or_infinity,
                                                validate = positive},
                 <<"rooms_per_page">> => #option{type = int_or_infinity,
                                                 validate = positive},
                 <<"rooms_in_rosters">> => #option{type = boolean},
                 <<"config_schema">> => #list{items = config_schema_spec()}
                }
      }.

config_schema_spec() ->
    #section{
       items = #{<<"field">> => #option{type = string,
                                        validate = non_empty},
                 <<"string_value">> => #option{type = binary},
                 <<"integer_value">> => #option{type = integer},
                 <<"float_value">> => #option{type = float},
                 <<"internal_key">> => #option{type = atom,
                                               validate = non_empty}
                },
       required = [<<"field">>],
       process = fun ?MODULE:process_config_schema/1
      }.

process_config_schema(KVs) ->
    {[[{field, FieldName}], InternalKeyOpts], ValueOpts} =
        proplists:split(KVs, [field, internal_key]),
    {Value, Type} = process_config_schema_value(ValueOpts),
    InternalKey = proplists:get_value(internal_key, InternalKeyOpts, list_to_atom(FieldName)),
    {FieldName, Value, InternalKey, Type}.

process_config_schema_value([{string_value, Val}]) -> {Val, binary};
process_config_schema_value([{integer_value, Val}]) -> {Val, integer};
process_config_schema_value([{float_value, Val}]) -> {Val, float}.

hooks(HostType) ->
    Codec = host_type_to_codec(HostType),
    Roster = gen_mod:get_module_opt(HostType, rooms_in_rosters, ?MODULE, ?DEFAULT_ROOMS_IN_ROSTERS),
    [{is_muc_room_owner, HostType, ?MODULE, is_muc_room_owner, 50},
     {can_access_room, HostType, ?MODULE, can_access_room, 50},
     {can_access_identity, HostType, ?MODULE, can_access_identity, 50},
      %% Prevent sending service-unavailable on groupchat messages
     {offline_groupchat_message_hook, HostType, ?MODULE, prevent_service_unavailable, 90},
     {remove_user, HostType, ?MODULE, remove_user, 50},
     {remove_domain, HostType, ?MODULE, remove_domain, 50},
     {disco_local_items, HostType, ?MODULE, disco_local_items, 50}] ++
    case Codec of
        legacy ->
            [{privacy_iq_get, HostType, ?MODULE, process_iq_get, 1},
             {privacy_iq_set, HostType, ?MODULE, process_iq_set, 1}];
        _ ->
            []
    end ++
    case Roster of
        false -> [];
        true -> [{roster_get, HostType, ?MODULE, add_rooms_to_roster, 50}]
    end.

%%====================================================================
%% Routing
%%====================================================================

-spec process_packet(Acc :: mongoose_acc:t(), From ::jid:jid(), To ::jid:jid(),
                     El :: exml:element(), Extra :: map()) -> any().
process_packet(Acc, From, To, El, _Extra) ->
    HostType = mod_muc_light_utils:acc_to_host_type(Acc),
    DecodedPacket = mod_muc_light_codec_backend:decode(From, To, El, Acc),
    process_decoded_packet(HostType, From, To, Acc, El, DecodedPacket).

-spec process_decoded_packet(
                     HostType :: host_type(),
                     From :: jid:jid(), To :: jid:jid(),
                     Acc :: mongoose_acc:t(),
                     OrigPacket :: exml:element(),
                     DecodedPacket :: mod_muc_light_codec:decode_result()) -> any().
process_decoded_packet(HostType, From, To, Acc, El,
                       {ok, {set, #create{} = Create}}) ->
    FromUS = jid:to_lus(From),
    case not mod_muc_light_utils:room_limit_reached(FromUS, HostType) of
        true -> create_room(Acc, From, FromUS, To, Create, El);
        false -> make_err(From, To, El, Acc, {error, room_limit_reached})
    end;
process_decoded_packet(_HostType, From, To, Acc, _El,
                       {ok, {get, #disco_info{} = DI}}) ->
    handle_disco_info_get(From, To, DI, Acc);
process_decoded_packet(HostType, From, To, Acc, El,
                       {ok, {get, #disco_items{} = DI}}) ->
    handle_disco_items_get(HostType, Acc, From, To, DI, El);
process_decoded_packet(HostType, From, To, Acc, El,
                       {ok, {_, #blocking{}} = Blocking}) ->
    case gen_mod:get_module_opt(HostType, ?MODULE, blocking, ?DEFAULT_BLOCKING) of
        true ->
            case handle_blocking(Acc, From, To, Blocking) of
                {error, _} = Res -> make_err(From, To, El, Acc, Res);
                _ -> ok
            end;
        false -> make_err(From, To, El, Acc, {error, blocking_disabled})
    end;
process_decoded_packet(HostType, From, To, Acc, El,
                       {ok, #iq{} = IQ}) ->
    case mod_muc_iq:process_iq(HostType, From, To, Acc, IQ) of
        {Acc1, error} ->
            make_err(From, To, El, Acc1, {error, feature_not_implemented});
        _ -> ok
    end;
process_decoded_packet(_HostType, From, To, Acc, El,
                       {ok, RequestToRoom})
  when To#jid.luser =/= <<>> ->
    case mod_muc_light_db_backend:room_exists(jid:to_lus(To)) of
        true -> mod_muc_light_room:handle_request(From, To, El, RequestToRoom, Acc);
        false -> make_err(From, To, El, Acc, {error, item_not_found})
    end;
process_decoded_packet(_HostType, From, To, Acc, El,
                       {error, _} = Err) ->
    make_err(From, To, El, Acc, Err);
process_decoded_packet(_HostType, _From, _To, _Acc, _El, ignore) ->
     ok;
process_decoded_packet(_HostType, From, To, Acc, El, InvalidReq) ->
    ?LOG_WARNING(#{what => muc_light_invalid_request,
                   acc => Acc, reason => InvalidReq}),
    make_err(From, To, El, Acc, {error, bad_request}).

make_err(From, To, El, Acc, Reason) ->
    mod_muc_light_codec_backend:encode_error(Reason, From, To, El,
                                             make_handler_fun(Acc)).

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

%% To is the host name, not subdomain.
-spec disco_local_items(Acc :: {result, [exml:element()]} | empty | {error, any()},
                      From :: jid:jid(), To :: jid:jid(),
                      NS :: binary(), ejabberd:lang())
                     -> {result, [exml:element()]} | empty | {error, any()}.
disco_local_items({result, Nodes}, _From, #jid{lserver = ServerHost} = To, <<"">>, _Lang) ->
    HostType = mod_muc_light_utils:room_jid_to_host_type(To),
    XMLNS = case legacy_mode(HostType) of
                true -> ?NS_MUC;
                false -> ?NS_MUC_LIGHT
            end,
    MUCHost = server_host_to_muc_host(HostType, ServerHost),
    Item = [#xmlel{name = <<"item">>,
                   attrs = [{<<"jid">>, MUCHost},
                            {<<"node">>, XMLNS}]}],
    {result, [Item | Nodes]};
disco_local_items(Acc, _From, _To, _Node, _Lang) ->
    Acc.

legacy_mode(HostType) ->
    gen_mod:get_module_opt(HostType, ?MODULE, legacy_mode, ?DEFAULT_LEGACY_MODE).

-spec remove_user(Acc :: mongoose_acc:t(), User :: binary(), Server :: binary()) ->
    mongoose_acc:t().
remove_user(Acc, User, Server) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Server),
    UserUS = {LUser, LServer},
    Version = mongoose_bin:gen_from_timestamp(),
    case mod_muc_light_db_backend:remove_user(UserUS, Version) of
        {error, Reason} ->
            ?LOG_ERROR(#{what => muc_remove_user_failed,
                         reason => Reason, acc => Acc}),
            Acc;
        AffectedRooms ->
            bcast_removed_user(Acc, UserUS, AffectedRooms, Version),
            maybe_forget_rooms(Acc, AffectedRooms),
            Acc
    end.

-spec remove_domain(mongoose_hooks:simple_acc(),
                    mongooseim:host_type(), jid:lserver()) ->
    mongoose_hooks:simple_acc().
remove_domain(Acc, HostType, Domain) ->
    MUCHost = server_host_to_muc_host(HostType, Domain),
    mod_muc_light_db_backend:remove_domain(HostType, MUCHost, Domain),
    Acc.

-spec add_rooms_to_roster(Acc :: mongoose_acc:t(), UserJID :: jid:jid()) -> mongoose_acc:t().
add_rooms_to_roster(Acc, UserJID) ->
    UserUS = jid:to_lus(UserJID),
    Items = mongoose_acc:get(roster, items, [], Acc),
    RoomList = mod_muc_light_db_backend:get_user_rooms(UserUS, undefined),
    Info = get_rooms_info(lists:sort(RoomList)),
    NewItems = [make_roster_item(Item) || Item <- Info] ++ Items,
    mongoose_acc:set(roster, items, NewItems, Acc).

make_roster_item({{RoomU, RoomS}, RoomName, RoomVersion}) ->
    JID = jid:make_noprep(RoomU, RoomS, <<>>),
    VerEl = #xmlel{ name = <<"version">>,
                    children = [#xmlcdata{ content = RoomVersion }] },
    #roster{usj = {RoomU, RoomS, jid:to_lower(JID)},
            us = {RoomU, RoomS},
            jid = jid:to_lower(JID),
            name = RoomName,
            subscription = to,
            groups = [?NS_MUC_LIGHT],
            xs = [VerEl] }.

-spec process_iq_get(Acc :: mongoose_acc:t(), From :: jid:jid(), To :: jid:jid(),
                     IQ :: jlib:iq(), ActiveList :: binary()) ->
    {stop, mongoose_acc:t()} | mongoose_acc:t().
process_iq_get(Acc, #jid{ lserver = FromS } = From, To, #iq{} = IQ, _ActiveList) ->
    HostType = mod_muc_light_utils:acc_to_host_type(Acc),
    MUCHost = gen_mod:get_module_opt_subhost(FromS, ?MODULE, default_host()),
    case {mod_muc_light_codec_backend:decode(From, To, IQ, Acc),
          gen_mod:get_module_opt(HostType, ?MODULE, blocking, ?DEFAULT_BLOCKING)} of
        {{ok, {get, #blocking{} = Blocking}}, true} ->
            Items = mod_muc_light_db_backend:get_blocking(jid:to_lus(From), MUCHost),
            mod_muc_light_codec_backend:encode(
              {get, Blocking#blocking{ items = Items }}, From, jid:to_lus(To),
              fun(_, _, Packet) -> put(encode_res, Packet) end,
              Acc),
            #xmlel{ children = ResponseChildren } = erase(encode_res),
            Result = {result, ResponseChildren},
            {stop, mongoose_acc:set(hook, result, Result, Acc)};
        {{ok, {get, #blocking{}}}, false} ->
            Result = {error, mongoose_xmpp_errors:bad_request()},
            {stop, mongoose_acc:set(hook, result, Result, Acc)};
        _ ->
            Result = {error, mongoose_xmpp_errors:bad_request()},
            mongoose_acc:set(hook, result, Result, Acc)
    end.

%% Blocking is done using your local domain
-spec process_iq_set(Acc :: mongoose_acc:t(), From :: jid:jid(),
                     To :: jid:jid(), IQ :: jlib:iq()) ->
    {stop, mongoose_acc:t()} | mongoose_acc:t().
process_iq_set(Acc, #jid{ lserver = FromS } = From, To, #iq{} = IQ) ->
    HostType = mod_muc_light_utils:acc_to_host_type(Acc),
    MUCHost = server_host_to_muc_host(HostType, FromS),
    case {mod_muc_light_codec_backend:decode(From, To, IQ, Acc),
          gen_mod:get_module_opt(HostType, ?MODULE, blocking, ?DEFAULT_BLOCKING)} of
        {{ok, {set, #blocking{ items = Items }} = Blocking}, true} ->
            RouteFun = fun(_, _, Packet) -> put(encode_res, Packet) end,
            ConditionFun = fun({_, _, {WhoU, WhoS}}) -> WhoU =:= <<>> orelse WhoS =:= <<>> end,
            case lists:any(ConditionFun, Items) of
                true ->
                    {stop, mongoose_acc:set(hook, result,
                                            {error, mongoose_xmpp_errors:bad_request()}, Acc)};
                false ->
                    ok = mod_muc_light_db_backend:set_blocking(jid:to_lus(From), MUCHost, Items),
                    mod_muc_light_codec_backend:encode(Blocking, From, jid:to_lus(To), RouteFun, Acc),
                    #xmlel{ children = ResponseChildren } = erase(encode_res),
                    {stop, mongoose_acc:set(hook, result, {result, ResponseChildren}, Acc)}
            end;
        {{ok, {set, #blocking{}}}, false} ->
            {stop, mongoose_acc:set(hook, result,
                                    {error, mongoose_xmpp_errors:bad_request()}, Acc)};
        _ ->
            mongoose_acc:set(hook, result, {error, mongoose_xmpp_errors:bad_request()}, Acc)
    end.

-spec is_muc_room_owner(Acc :: boolean(), HostType :: mongooseim:host_type(),
                        Room :: jid:jid(), User :: jid:jid()) -> boolean().
is_muc_room_owner(_, _HostType, Room, User) ->
    owner == get_affiliation(Room, User).

-spec can_access_room(Acc :: boolean(), HostType :: mongooseim:host_type(),
                      Room :: jid:jid(), User :: jid:jid()) ->
    boolean().
can_access_room(_, _HostType, Room, User) ->
    none =/= get_affiliation(Room, User).

-spec can_access_identity(Acc :: boolean(), HostType :: mongooseim:host_type(),
                          Room :: jid:jid(), User :: jid:jid()) ->
    boolean().
can_access_identity(_Acc, _HostType, _Room, _User) ->
    %% User JIDs are explicit in MUC Light but this hook is about appending
    %% 0045 MUC element with user identity and we don't want it
    false.

%%====================================================================
%% Internal functions
%%====================================================================

prepare_config(HostType, RawConfig) ->
    DefConfig = default_config_for_host_type(HostType),
    Schema = config_schema_for_host_type(HostType),
    mod_muc_light_room_config:apply_binary_kv(RawConfig, DefConfig, Schema).

prepare_affs(HostType, CreatorUS, RoomUS, #create{aff_users = AffUsers}) ->
    InitialAffUsers = mod_muc_light_utils:filter_out_prevented(HostType,
                        CreatorUS, RoomUS, AffUsers),
    Res = process_create_aff_users_if_valid(HostType, CreatorUS, InitialAffUsers),
    MaxOccupants = max_occupants(HostType),
    case Res of
        {ok, FinalAffUsers} when length(FinalAffUsers) > MaxOccupants ->
            {error, max_occupants_reached};
        _ ->
            Res
    end.

max_occupants(HostType) ->
    gen_mod:get_module_opt(HostType, ?MODULE, max_occupants, ?DEFAULT_MAX_OCCUPANTS).

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

-spec create_room(mongoose_acc:t(), From :: jid:jid(), FromUS :: jid:simple_bare_jid(),
                  To :: jid:jid(), Create :: create_req_props(), OrigPacket :: exml:element()) ->
    exml:element().
create_room(Acc, From, FromUS, To, Create0, OrigPacket) ->
    case try_to_create_room(FromUS, To, Create0) of
        {ok, FinalRoomUS, Details} ->
            mod_muc_light_codec_backend:encode({set, Details, To#jid.luser == <<>>}, From,
                                               FinalRoomUS, fun ejabberd_router:route/3, Acc);
        {error, exists} ->
            mod_muc_light_codec_backend:encode_error({error, {conflict, <<"Room already exists">>}},
                                                      From, To, OrigPacket,
                                                     make_handler_fun(Acc));
        {error, bad_request} ->
            mod_muc_light_codec_backend:encode_error({error, bad_request}, From, To, OrigPacket,
                                                     make_handler_fun(Acc));
        {error, Error} ->
            ErrorText = io_lib:format("~s:~p", tuple_to_list(Error)),
            mod_muc_light_codec_backend:encode_error(
              {error, bad_request, ErrorText}, From, To, OrigPacket, make_handler_fun(Acc))
    end.

-spec process_create_aff_users_if_valid(HostType :: host_type(),
                                        Creator :: jid:simple_bare_jid(),
                                        AffUsers :: aff_users()) ->
    {ok, aff_users()} | {error, bad_request}.
process_create_aff_users_if_valid(HostType, Creator, AffUsers) ->
    case lists:any(fun ({User, _}) when User =:= Creator -> true;
                       ({_, Aff}) -> Aff =:= none end, AffUsers) of
        false ->
            process_create_aff_users(Creator, AffUsers, equal_occupants(HostType));
        true ->
            {error, bad_request}
    end.

equal_occupants(HostType) ->
    gen_mod:get_module_opt(HostType, ?MODULE,
                           equal_occupants, ?DEFAULT_EQUAL_OCCUPANTS).

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

-spec handle_disco_info_get(From ::jid:jid(), To ::jid:jid(),
                            DiscoInfo :: disco_info_req_props(),
                            Acc :: mongoose_acc:t()) -> ok.
handle_disco_info_get(From, To, DiscoInfo, Acc) ->
    mod_muc_light_codec_backend:encode({get, DiscoInfo}, From, jid:to_lus(To),
                                       fun ejabberd_router:route/3, Acc).

-spec handle_disco_items_get(HostType :: host_type(),
                             Acc :: mongoose_acc:t(),
                             From ::jid:jid(), To ::jid:jid(),
                             DiscoItems :: disco_items_req_props(),
                             OrigPacket :: exml:element()) -> ok.
handle_disco_items_get(HostType, Acc, From, To, DiscoItems0, OrigPacket) ->
    case catch mod_muc_light_db_backend:get_user_rooms(jid:to_lus(From), To#jid.lserver) of
        {error, Error} ->
            ?LOG_ERROR(#{what => muc_get_user_rooms_failed,
                         text => <<"Couldn't get room list for user">>,
                         from_jid => From, reason => Error}),
            mod_muc_light_codec_backend:encode_error(
              {error, internal_server_error}, From, To, OrigPacket, fun ejabberd_router:route/3);
        Rooms ->
            RoomsInfo = get_rooms_info(lists:sort(Rooms)),
            RouteFun = fun ejabberd_router:route/3,
            RoomsPerPage = gen_mod:get_module_opt(HostType, ?MODULE, rooms_per_page, ?DEFAULT_ROOMS_PER_PAGE),
            case apply_rsm(RoomsInfo, length(RoomsInfo),
                           page_service_limit(DiscoItems0#disco_items.rsm, RoomsPerPage)) of
                {ok, RoomsInfoSlice, RSMOut} ->
                    DiscoItems = DiscoItems0#disco_items{ rooms = RoomsInfoSlice, rsm = RSMOut },
                    mod_muc_light_codec_backend:encode({get, DiscoItems},
                                                       From, jid:to_lus(To),
                                                       RouteFun, Acc);
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

-spec handle_blocking(Acc :: mongoose_acc:t(), From :: jid:jid(), To :: jid:jid(),
                      BlockingReq :: {get | set, blocking_req_props()}) ->
    {error, bad_request} | ok.
handle_blocking(Acc, From, To, {get, #blocking{} = Blocking}) ->
    BlockingItems = mod_muc_light_db_backend:get_blocking(jid:to_lus(From), To#jid.lserver),
    mod_muc_light_codec_backend:encode({get, Blocking#blocking{ items = BlockingItems }},
                                       From, jid:to_lus(To), fun ejabberd_router:route/3, Acc);
handle_blocking(Acc, From, To, {set, #blocking{ items = Items }} = BlockingReq) ->
    case lists:any(fun({_, _, {WhoU, WhoS}}) -> WhoU =:= <<>> orelse WhoS =:= <<>> end, Items) of
        true ->
            {error, bad_request};
        false ->
            ok = mod_muc_light_db_backend:set_blocking(jid:to_lus(From), To#jid.lserver, Items),
            mod_muc_light_codec_backend:encode(
              BlockingReq, From, jid:to_lus(To), fun ejabberd_router:route/3, Acc),
            ok
    end.

-spec bcast_removed_user(Acc :: mongoose_acc:t(), UserUS :: jid:simple_bare_jid(),
                         AffectedRooms :: mod_muc_light_db:remove_user_return(),
                         Version :: binary()) -> ok.
bcast_removed_user(Acc, {UserU, UserS}, AffectedRooms, Version) ->
    bcast_removed_user(Acc, jid:make_noprep(UserU, UserS, <<>>), AffectedRooms,
                       Version, mongoose_bin:gen_from_timestamp()).

-spec bcast_removed_user(Acc :: mongoose_acc:t(), UserJID :: jid:jid(),
                         AffectedRooms :: mod_muc_light_db:remove_user_return(),
                         Version :: binary(),
                         PacketID :: binary()) -> ok.
bcast_removed_user(_Acc, _UserJID, [], _Version, _ID) ->
    ok;
bcast_removed_user(Acc, UserJID,
                   [{RoomUS, {ok, OldAffUsers, NewAffUsers, AffUsersChanged, PrevVersion}}
                    | RAffected], Version, ID) ->
    Affiliations = #affiliations{
                      id = ID,
                      prev_version = PrevVersion,
                      version = Version,
                      aff_users = AffUsersChanged
                     },
    Cmd = {set, Affiliations, OldAffUsers, NewAffUsers},
    mod_muc_light_codec_backend:encode(Cmd, UserJID, RoomUS, fun ejabberd_router:route/3, Acc),
    bcast_removed_user(Acc, UserJID, RAffected, Version, ID);
bcast_removed_user(Acc, UserJID, [{{RoomU, RoomS} = _RoomUS, Error} | RAffected], Version, ID) ->
    ?LOG_ERROR(#{what => muc_remove_user_failed,
                 user_jid => jid:to_binary(UserJID), room => RoomU, sub_host => RoomS,
                 reason => Error}),
    bcast_removed_user(Acc, UserJID, RAffected, Version, ID).

-spec maybe_forget_rooms(Acc :: mongoose_acc:t(),
                         AffectedRooms :: mod_muc_light_db:remove_user_return()) -> ok.
maybe_forget_rooms(_Acc, []) ->
    ok;
maybe_forget_rooms(Acc, [{RoomUS, {ok, _, NewAffUsers, _, _}} | RAffectedRooms]) ->
    mod_muc_light_room:maybe_forget(Acc, RoomUS, NewAffUsers),
    maybe_forget_rooms(Acc, RAffectedRooms).

make_handler_fun(Acc) ->
    fun(From, To, Packet) -> ejabberd_router:route(From, To, Acc, Packet) end.

config_metrics(Host) ->
    OptsToReport = [{backend, mnesia}], %list of tuples {option, defualt_value}
    mongoose_module_metrics:opts_for_module(Host, ?MODULE, OptsToReport).
