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
-export([server_host_to_muc_host/2]).
-export([config_schema/1]).

%% For Administration API
-export([try_to_create_room/3,
         change_room_config/5,
         delete_room/1]).

%% gen_mod callbacks
-export([start/2, stop/1, hooks/1, config_spec/0, supported_features/0, deps/2]).

%% config processing callback
-export([process_config_schema/1]).

%% Packet handler export
-export([process_packet/5]).

%% Hook handlers
-export([prevent_service_unavailable/3,
         disco_local_items/3,
         remove_user/3,
         remove_domain/3,
         add_rooms_to_roster/3,
         process_iq_get/3,
         process_iq_set/3,
         is_muc_room_owner/3,
         can_access_room/3,
         acc_room_affiliations/3,
         room_exists/3,
         can_access_identity/3]).

-export([get_acc_room_affiliations/2]).

%% For propEr
-export([apply_rsm/3]).

-export([config_metrics/1]).
%% for mod_muc_light_codec_legacy
-export([subdomain_pattern/1]).

-export([get_room_affs_from_acc/2, set_room_affs_from_acc/3]).

%% For tests
-export([default_schema/0,
         force_clear_from_ct/1]).

-export_type([aff_users/0]).

-ignore_xref([
    apply_rsm/3, default_schema/0, force_clear_from_ct/1, server_host_to_muc_host/2
]).

-type muc_server() :: jid:lserver().
-type host_type() :: mongooseim:host_type().
-type versioned_affs() :: {ok, aff_users(), binary()}.

%%====================================================================
%% API
%%====================================================================

-spec supported_features() -> [atom()].
supported_features() ->
    [dynamic_domains].

-spec default_schema() -> mod_muc_light_room_config:schema().
default_schema() ->
    % This list needs to be sorted
    [{<<"roomname">>, <<"Untitled">>, roomname, binary},
     {<<"subject">>, <<>>, subject, binary}].

-spec default_host() -> mongoose_subdomain_utils:subdomain_pattern().
default_host() ->
    mongoose_subdomain_utils:make_subdomain_pattern(<<"muclight.@HOST@">>).

-spec config_schema(MUCServer :: muc_server()) -> mod_muc_light_room_config:schema().
config_schema(MUCServer) ->
    HostType = mod_muc_light_utils:muc_host_to_host_type(MUCServer),
    config_schema_for_host_type(HostType).

%% Internals

-spec config_schema_for_host_type(host_type()) -> mod_muc_light_room_config:schema().
config_schema_for_host_type(HostType) ->
    gen_mod:get_module_opt(HostType, ?MODULE, config_schema).

force_clear_from_ct(HostType) ->
    catch mod_muc_light_cache:force_clear(HostType),
    mod_muc_light_db_backend:force_clear(HostType).

%%====================================================================
%% Administration API
%%====================================================================

-spec try_to_create_room(CreatorJid :: jid:jid(), RoomJID :: jid:jid(),
                         CreationCfg :: create_req_props()) ->
    {ok, jid:jid(), create_req_props()} | validation_error()
    | {error, bad_request | exists | max_occupants_reached}.
try_to_create_room(CreatorJid, RoomJID, #create{raw_config = RawConfig} = CreationCfg) ->
    RoomUS = jid:to_lus(RoomJID),
    HostType = mod_muc_light_utils:room_jid_to_host_type(RoomJID),
    CfgRes = prepare_config(HostType, RawConfig),
    AffRes = prepare_affs(HostType, CreatorJid, RoomUS, CreationCfg),
    case {CfgRes, AffRes} of
        {{ok, Config0}, {ok, FinalAffUsers}} ->
            Version = mongoose_bin:gen_from_timestamp(),
            case mod_muc_light_db_backend:create_room(
                   HostType, RoomUS, lists:sort(Config0), FinalAffUsers, Version) of
                {ok, {FinalU, FinalS}} ->
                    {ok, jid:make_noprep(FinalU, FinalS, <<>>), CreationCfg#create{
                                        aff_users = FinalAffUsers, version = Version}};
                Other ->
                    Other
            end;
        {{error, _} = Error, _} ->
            Error;
        {_, {error, _} = Error} ->
            Error
    end.

-spec change_room_config(UserJid :: jid:jid(), RoomID :: jid:user(),
                         MUCLightDomain :: jid:server(),
                         ConfigReq :: config_req_props(),
                         Acc :: mongoose_acc:t()) ->
    {ok, jid:jid(), config_req_props()}
    | {error, validation_error() | bad_request | not_allowed | not_exists | item_not_found}.
change_room_config(UserJid, RoomID, MUCLightDomain, ConfigReq, Acc1) ->
    RoomJID = jid:make_bare(RoomID, MUCLightDomain),
    {Acc2, AffUsersRes} = get_acc_room_affiliations(Acc1, RoomJID),
    case mod_muc_light_room:process_request(UserJid, RoomJID, {set, ConfigReq},
                                            AffUsersRes, Acc2) of
        {set, ConfigResp, _} ->
            {ok, RoomJID, ConfigResp};
        {error, _Reason} = E ->
            E
    end.

-spec delete_room(RoomUS :: jid:simple_bare_jid()) -> ok | {error, not_exists}.
delete_room({_, RoomS} = RoomUS) ->
    HostType = mod_muc_light_utils:muc_host_to_host_type(RoomS),
    mod_muc_light_db_backend:destroy_room(HostType, RoomUS).

%%====================================================================
%% gen_mod callbacks
%%====================================================================

-spec start(host_type(), gen_mod:module_opts()) -> ok.
start(HostType, Opts) ->
    Codec = host_type_to_codec(HostType),
    mod_muc_light_db_backend:start(HostType, Opts),
    mod_muc_light_codec_backend:start(HostType, #{backend => Codec}),
    %% Handler
    SubdomainPattern = subdomain_pattern(HostType),
    PacketHandler = mongoose_packet_handler:new(?MODULE),
    mongoose_domain_api:register_subdomain(HostType, SubdomainPattern, PacketHandler),
    ok.

-spec stop(host_type()) -> ok.
stop(HostType) ->
    SubdomainPattern = subdomain_pattern(HostType),
    mongoose_domain_api:unregister_subdomain(HostType, SubdomainPattern),
    mod_muc_light_codec_backend:stop(HostType),
    mod_muc_light_db_backend:stop(HostType),
    ok.

-spec deps(mongooseim:host_type(), gen_mod:module_opts()) -> gen_mod_deps:deps().
deps(_HostType, #{cache_affs := CacheOpts}) ->
    [{mod_muc_light_cache, CacheOpts, hard}];
deps(_HostType, #{}) ->
    [].

%% Init helpers
subdomain_pattern(HostType) ->
    gen_mod:get_module_opt(HostType, ?MODULE, host).

server_host_to_muc_host(HostType, ServerHost) ->
    mongoose_subdomain_utils:get_fqdn(subdomain_pattern(HostType), ServerHost).

host_type_to_codec(HostType) ->
    case gen_mod:get_module_opt(HostType, ?MODULE, legacy_mode) of
        true ->
            legacy;
        false ->
            modern
    end.

%% Config callbacks
-spec config_spec() -> mongoose_config_spec:config_section().
config_spec() ->
    #section{
       items = #{<<"backend">> => #option{type = atom,
                                          validate = {module, mod_muc_light_db}},
                 <<"cache_affs">> => mod_muc_light_cache:config_spec(),
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
                 <<"config_schema">> => #list{items = config_schema_spec(),
                                              process = fun ?MODULE:process_config_schema/1}
                },
       defaults = #{<<"backend">> => mnesia,
                    <<"host">> => default_host(),
                    <<"equal_occupants">> => ?DEFAULT_EQUAL_OCCUPANTS,
                    <<"legacy_mode">> => ?DEFAULT_LEGACY_MODE,
                    <<"rooms_per_user">> => ?DEFAULT_ROOMS_PER_USER,
                    <<"blocking">> => ?DEFAULT_BLOCKING,
                    <<"all_can_configure">> => ?DEFAULT_ALL_CAN_CONFIGURE,
                    <<"all_can_invite">> => ?DEFAULT_ALL_CAN_INVITE,
                    <<"max_occupants">> => ?DEFAULT_MAX_OCCUPANTS,
                    <<"rooms_per_page">> => ?DEFAULT_ROOMS_PER_PAGE,
                    <<"rooms_in_rosters">> => ?DEFAULT_ROOMS_IN_ROSTERS,
                    <<"config_schema">> => default_schema()}
      }.

config_schema_spec() ->
    #section{
       items = #{<<"field">> => #option{type = binary,
                                        validate = non_empty},
                 <<"string_value">> => #option{type = binary},
                 <<"integer_value">> => #option{type = integer},
                 <<"float_value">> => #option{type = float},
                 <<"internal_key">> => #option{type = atom,
                                               validate = non_empty}
                },
       required = [<<"field">>]
      }.

-spec process_config_schema([map()]) -> mod_muc_light_room_config:schema().
process_config_schema(Items) ->
    lists:ukeysort(1, lists:map(fun process_config_schema_item/1, Items)).

process_config_schema_item(#{field := FieldName} = FieldSpec) ->
    InternalKey = maps:get(internal_key, FieldSpec, binary_to_atom(FieldName)),
    FieldTypes = schema_field_types(),
    case [K || K <- maps:keys(FieldTypes), maps:is_key(K, FieldSpec)] of
        [Key] ->
            {FieldName, maps:get(Key, FieldSpec), InternalKey, maps:get(Key, FieldTypes)};
        _ ->
            error(#{what => invalid_schema_field_specification, field_spec => FieldSpec})
    end.

schema_field_types() ->
    #{string_value => binary, integer_value => integer, float_value => float}.

-spec hooks(mongooseim:host_type()) -> gen_hook:hook_list().
hooks(HostType) ->
    Codec = host_type_to_codec(HostType),
    Roster = gen_mod:get_module_opt(HostType, ?MODULE, rooms_in_rosters),
    [{is_muc_room_owner, HostType, fun ?MODULE:is_muc_room_owner/3, #{}, 50},
     {can_access_room, HostType, fun ?MODULE:can_access_room/3, #{}, 50},
     {acc_room_affiliations, HostType, fun ?MODULE:acc_room_affiliations/3, #{}, 50},
     {room_exists, HostType, fun ?MODULE:room_exists/3, #{}, 50},
     {can_access_identity, HostType, fun ?MODULE:can_access_identity/3, #{}, 50},
      %% Prevent sending service-unavailable on groupchat messages
     {offline_groupchat_message_hook, HostType, fun ?MODULE:prevent_service_unavailable/3, #{}, 90},
     {remove_user, HostType, fun ?MODULE:remove_user/3, #{}, 50},
     {remove_domain, HostType, fun ?MODULE:remove_domain/3, #{}, 50},
     {disco_local_items, HostType, fun ?MODULE:disco_local_items/3, #{}, 50}] ++
    case Codec of
        legacy ->
            [{privacy_iq_get, HostType, fun ?MODULE:process_iq_get/3, #{}, 1},
             {privacy_iq_set, HostType, fun ?MODULE:process_iq_set/3, #{}, 1}];
        _ ->
            []
    end ++
    case Roster of
        false -> [];
        true -> [{roster_get, HostType, fun ?MODULE:add_rooms_to_roster/3, #{}, 50}]
    end.

%%====================================================================
%% Routing
%%====================================================================

-spec process_packet(Acc :: mongoose_acc:t(), From ::jid:jid(), To ::jid:jid(),
                     El :: exml:element(), Extra :: gen_hook:extra()) -> mongoose_acc:t().
process_packet(Acc, From, To, El, _Extra) ->
    HostType = mod_muc_light_utils:acc_to_host_type(Acc),
    DecodedPacket = mod_muc_light_codec_backend:decode(From, To, El, Acc),
    process_decoded_packet(HostType, From, To, Acc, El, DecodedPacket).

-spec process_decoded_packet(
                     HostType :: host_type(),
                     From :: jid:jid(), To :: jid:jid(),
                     Acc :: mongoose_acc:t(),
                     OrigPacket :: exml:element(),
                     DecodedPacket :: mod_muc_light_codec_backend:decode_result()) ->
    mongoose_acc:t().
process_decoded_packet(HostType, From, To, Acc, El,
                       {ok, {set, #create{} = Create}}) ->
    case not mod_muc_light_utils:room_limit_reached(From, HostType) of
        true -> create_room(Acc, From, To, Create, El);
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
    case gen_mod:get_module_opt(HostType, ?MODULE, blocking) of
        true ->
            case handle_blocking(Acc, From, To, Blocking) of
                {error, _} = Res -> make_err(From, To, El, Acc, Res);
                _ -> Acc
            end;
        false -> make_err(From, To, El, Acc, {error, blocking_disabled})
    end;
process_decoded_packet(_HostType, From, To, Acc, El,
                       {ok, #iq{} = IQ}) ->
    case mod_muc_iq:process_iq(To#jid.lserver, From, To, Acc, IQ) of
        {Acc1, error} ->
            E = {error, {feature_not_implemented, <<"mod_muc_iq returns error">>}},
            make_err(From, To, El, Acc1, E);
        _ -> Acc
    end;
process_decoded_packet(HostType, From, To, Acc, El,
                       {ok, RequestToRoom})
  when To#jid.luser =/= <<>> ->
    case mongoose_hooks:room_exists(HostType, To) of
        true -> mod_muc_light_room:handle_request(From, To, El, RequestToRoom, Acc);
        false -> make_err(From, To, El, Acc, {error, item_not_found})
    end;
process_decoded_packet(_HostType, From, To, Acc, El,
                       {error, _} = Err) ->
    make_err(From, To, El, Acc, Err);
process_decoded_packet(_HostType, _From, _To, Acc, _El, ignore) ->
     Acc;
process_decoded_packet(_HostType, From, To, Acc, El, InvalidReq) ->
    ?LOG_WARNING(#{what => muc_light_invalid_request,
                   acc => Acc, reason => InvalidReq}),
    make_err(From, To, El, Acc, {error, bad_request}).

make_err(From, To, El, Acc, Reason) ->
    mod_muc_light_codec_backend:encode_error(Reason, From, To, El, Acc).

%%====================================================================
%% Hook handlers
%%====================================================================

-spec prevent_service_unavailable(Acc, Params, Extra) -> {ok | stop, Acc} when
    Acc :: mongoose_acc:t(),
    Params :: map(),
    Extra :: gen_hook:extra().
prevent_service_unavailable(Acc, #{packet := Packet}, _Extra) ->
    case xml:get_tag_attr_s(<<"type">>, Packet) of
        <<"groupchat">> -> {stop, Acc};
        _Type -> {ok, Acc}
    end.

-spec disco_local_items(Acc, Params, Extra) -> {ok, Acc} when
    Acc :: mongoose_acc:t(),
    Params :: map(),
    Extra :: gen_hook:extra().
disco_local_items(Acc = #{host_type := HostType,
                          to_jid := #jid{lserver = ServerHost},
                          node := <<>>},
                  _Params,
                  _Extra) ->
    XMLNS = case legacy_mode(HostType) of
                true -> ?NS_MUC;
                false -> ?NS_MUC_LIGHT
            end,
    MUCHost = server_host_to_muc_host(HostType, ServerHost),
    Items = [#{jid => MUCHost, node => XMLNS}],
    {ok, mongoose_disco:add_items(Items, Acc)};
disco_local_items(Acc, _Params, _Extra) ->
    {ok, Acc}.

legacy_mode(HostType) ->
    gen_mod:get_module_opt(HostType, ?MODULE, legacy_mode).

-spec remove_user(Acc, Params, Extra) -> {ok, Acc} when
    Acc :: mongoose_acc:t(),
    Params :: map(),
    Extra :: gen_hook:extra().
remove_user(Acc, #{jid := UserJid}, #{host_type := HostType}) ->
    Version = mongoose_bin:gen_from_timestamp(),
    case mod_muc_light_db_backend:remove_user(HostType, jid:to_lus(UserJid), Version) of
        {error, Reason} ->
            ?LOG_ERROR(#{what => muc_remove_user_failed,
                         reason => Reason, acc => Acc}),
            {ok, Acc};
        AffectedRooms ->
            bcast_removed_user(Acc, UserJid, AffectedRooms, Version),
            maybe_forget_rooms(Acc, AffectedRooms, Version),
            {ok, Acc}
    end.

-spec remove_domain(Acc, Params, Extra) -> {ok | stop, Acc} when
    Acc :: mongoose_domain_api:remove_domain_acc(),
    Params :: map(),
    Extra :: gen_hook:extra().
remove_domain(Acc, #{domain := Domain}, #{host_type := HostType}) ->
    F = fun() ->
            MUCHost = server_host_to_muc_host(HostType, Domain),
            mod_muc_light_db_backend:remove_domain(HostType, MUCHost, Domain),
            Acc
        end,
    mongoose_domain_api:remove_domain_wrapper(Acc, F, ?MODULE).

-spec add_rooms_to_roster(Acc, Params, Extra) -> {ok, Acc} when
    Acc :: [mod_roster:roster()],
    Params :: #{jid := jid:jid()},
    Extra :: gen_hook:extra().
add_rooms_to_roster(Items, #{jid := UserJID}, #{host_type := HostType}) ->
    RoomList = mod_muc_light_db_backend:get_user_rooms(HostType, jid:to_lus(UserJID), undefined),
    Info = get_rooms_info(HostType, lists:sort(RoomList)),
    NewItems = [make_roster_item(Item) || Item <- Info],
    {ok, NewItems ++ Items}.

make_roster_item({{RoomU, RoomS}, RoomName, RoomVersion}) ->
    JID = jid:make_noprep(RoomU, RoomS, <<>>),
    VerEl = #xmlel{ name = <<"version">>,
                    children = [#xmlcdata{ content = RoomVersion }] },
    #roster{usj = {jid:to_lus(JID), jid:to_lower(JID)},
            us = jid:to_lus(JID),
            jid = jid:to_lower(JID),
            name = RoomName,
            subscription = to,
            groups = [?NS_MUC_LIGHT],
            xs = [VerEl] }.

-spec process_iq_get(Acc, Params, Extra) -> {ok | stop, Acc} when
    Acc :: mongoose_acc:t(),
    Params :: map(),
    Extra :: gen_hook:extra().
process_iq_get(Acc, #{from := #jid{lserver = FromS} = From, to := To, iq := IQ}, #{host_type := HostType}) ->
    MUCHost = server_host_to_muc_host(HostType, FromS),
    case {mod_muc_light_codec_backend:decode(From, To, IQ, Acc),
          gen_mod:get_module_opt(HostType, ?MODULE, blocking)} of
        {{ok, {get, #blocking{} = Blocking}}, true} ->
            Items = mod_muc_light_db_backend:get_blocking(HostType, jid:to_lus(From), MUCHost),
            mod_muc_light_codec_backend:encode(
              {get, Blocking#blocking{ items = Items }}, From, To,
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
            {ok, mongoose_acc:set(hook, result, Result, Acc)}
    end.

%% Blocking is done using your local domain
-spec process_iq_set(Acc, Params, Extra) -> {ok | stop, Acc} when
    Acc :: mongoose_acc:t(),
    Params :: map(),
    Extra :: gen_hook:extra().
process_iq_set(Acc, #{from := #jid{ lserver = FromS } = From, to := To, iq := IQ}, _Extra) ->
    HostType = mod_muc_light_utils:acc_to_host_type(Acc),
    MUCHost = server_host_to_muc_host(HostType, FromS),
    case {mod_muc_light_codec_backend:decode(From, To, IQ, Acc),
          gen_mod:get_module_opt(HostType, ?MODULE, blocking)} of
        {{ok, {set, #blocking{ items = Items }} = Blocking}, true} ->
            RouteFun = fun(_, _, Packet) -> put(encode_res, Packet) end,
            ConditionFun = fun({_, _, {WhoU, WhoS}}) -> WhoU =:= <<>> orelse WhoS =:= <<>> end,
            case lists:any(ConditionFun, Items) of
                true ->
                    {stop, mongoose_acc:set(hook, result,
                                            {error, mongoose_xmpp_errors:bad_request()}, Acc)};
                false ->
                    ok = mod_muc_light_db_backend:set_blocking(HostType, jid:to_lus(From), MUCHost, Items),
                    mod_muc_light_codec_backend:encode(Blocking, From, To, RouteFun, Acc),
                    #xmlel{ children = ResponseChildren } = erase(encode_res),
                    {stop, mongoose_acc:set(hook, result, {result, ResponseChildren}, Acc)}
            end;
        {{ok, {set, #blocking{}}}, false} ->
            {stop, mongoose_acc:set(hook, result,
                                    {error, mongoose_xmpp_errors:bad_request()}, Acc)};
        _ ->
            {ok, mongoose_acc:set(hook, result, {error, mongoose_xmpp_errors:bad_request()}, Acc)}
    end.

-spec is_muc_room_owner(Acc, Params, Extra) -> {ok, Acc} when
    Acc :: boolean(),
    Params :: map(),
    Extra :: gen_hook:extra().
is_muc_room_owner(true, _Params, _Extra) ->
    {ok, true};
is_muc_room_owner(_, #{acc := Acc, room := Room, user := User}, _Extra) ->
    {ok, owner == get_affiliation(Acc, Room, User)}.

-spec can_access_room(Acc, Params, Extra) -> {ok, Acc} when
    Acc :: boolean(),
    Params :: map(),
    Extra :: gen_hook:extra().
can_access_room(true, _Params, _Extra) ->
    {ok, true};
can_access_room(_, #{acc := Acc, room := Room, user := User}, _Extra) ->
    {ok, none =/= get_affiliation(Acc, Room, User)}.

-spec acc_room_affiliations(Acc, Params, Extra) -> {ok, Acc} when
    Acc :: mongoose_acc:t(),
    Params :: map(),
    Extra :: gen_hook:extra().
acc_room_affiliations(Acc1, #{room := RoomJid}, _Extra) ->
    case get_room_affs_from_acc(Acc1, RoomJid) of
        {error, _} ->
            HostType = mongoose_acc:host_type(Acc1),
            case mod_muc_light_db_backend:get_aff_users(HostType, jid:to_lus(RoomJid)) of
                {error, not_exists} ->
                    {ok, Acc1};
                {ok, _Affs, _Version} = Res ->
                    {ok, set_room_affs_from_acc(Acc1, RoomJid, Res)}
            end;
        _Affs ->
            {ok, Acc1}
    end.

-spec room_exists(Acc, Params, Extra) -> {ok, Acc} when
    Acc :: boolean(),
    Params :: map(),
    Extra :: gen_hook:extra().
room_exists(_, #{room := RoomJid}, #{host_type := HostType}) ->
    {ok, mod_muc_light_db_backend:room_exists(HostType, jid:to_lus(RoomJid))}.

-spec get_acc_room_affiliations(mongoose_acc:t(), jid:jid()) ->
    {mongoose_acc:t(), versioned_affs() | {error, not_exists}}.
get_acc_room_affiliations(Acc1, RoomJid) ->
    case get_room_affs_from_acc(Acc1, RoomJid) of
        {error, not_exists} ->
            Acc2 = mongoose_hooks:acc_room_affiliations(Acc1, RoomJid),
            {Acc2, get_room_affs_from_acc(Acc2, RoomJid)};
        Res ->
            {Acc1, Res}
    end.

-spec get_room_affs_from_acc(mongoose_acc:t(), jid:jid()) -> versioned_affs() | {error, not_exists}.
get_room_affs_from_acc(Acc, RoomJid) ->
    mongoose_acc:get(?MODULE, {affiliations, RoomJid}, {error, not_exists}, Acc).

-spec set_room_affs_from_acc(mongoose_acc:t(), jid:jid(), versioned_affs()) -> mongoose_acc:t().
set_room_affs_from_acc(Acc, RoomJid, Affs) ->
    mongoose_acc:set(?MODULE, {affiliations, RoomJid}, Affs, Acc).

-spec can_access_identity(Acc, Params, Extra) -> {ok, Acc} when
    Acc :: boolean(),
    Params :: map(),
    Extra :: gen_hook:extra().
can_access_identity(true, _Params, _Extra) ->
    {ok, true};
can_access_identity(_Acc, _Params, _Extra) ->
    %% User JIDs are explicit in MUC Light but this hook is about appending
    %% 0045 MUC element with user identity and we don't want it
    {ok, false}.

%%====================================================================
%% Internal functions
%%====================================================================

prepare_config(HostType, RawConfig) ->
    Schema = config_schema_for_host_type(HostType),
    mod_muc_light_room_config:from_binary_kv(RawConfig, Schema).

prepare_affs(HostType, CreatorJid, RoomUS, #create{aff_users = AffUsers}) ->
    CreatorUS = jid:to_lus(CreatorJid),
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
    gen_mod:get_module_opt(HostType, ?MODULE, max_occupants).

get_affiliation(Acc, Room, User) ->
    case get_acc_room_affiliations(Acc, Room) of
        {_, {ok, AffUsers, _}} ->
            case lists:keyfind(jid:to_lus(User), 1, AffUsers) of
                {_, Aff} -> Aff;
                _ -> none
            end;
        _ ->
            none
    end.

-spec create_room(mongoose_acc:t(),
                  jid:jid(),
                  jid:jid(),
                  create_req_props(),
                  exml:element()) ->
    mongoose_acc:t().
create_room(Acc, From, To, Create0, OrigPacket) ->
    case try_to_create_room(From, To, Create0) of
        {ok, FinalRoomJid, Details} ->
            mod_muc_light_codec_backend:encode({set, Details, To#jid.luser == <<>>}, From,
                                               FinalRoomJid, make_handler_fun(Acc), Acc);
        {error, exists} ->
            mod_muc_light_codec_backend:encode_error({error, {conflict, <<"Room already exists">>}},
                                                     From, To, OrigPacket,
                                                     Acc);
        {error, bad_request} ->
            mod_muc_light_codec_backend:encode_error({error, bad_request}, From, To, OrigPacket,
                                                     Acc);
        {error, {_, _} = Error} ->
            ErrorText = io_lib:format("~s:~p", tuple_to_list(Error)),
            mod_muc_light_codec_backend:encode_error(
              {error, bad_request, ErrorText}, From, To, OrigPacket, Acc);
        {error, Error} ->
            ErrorText = io_lib:format("~p", [Error]),
            mod_muc_light_codec_backend:encode_error(
              {error, bad_request, ErrorText}, From, To, OrigPacket, Acc)
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
    gen_mod:get_module_opt(HostType, ?MODULE, equal_occupants).

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

-spec handle_disco_info_get(From ::jid:jid(),
                            To :: jid:jid(),
                            DiscoInfo :: disco_info_req_props(),
                            Acc :: mongoose_acc:t()) ->
    mongoose_acc:t().
handle_disco_info_get(From, To, DiscoInfo, Acc) ->
    mod_muc_light_codec_backend:encode({get, DiscoInfo}, From, To,
                                       make_handler_fun(Acc), Acc).

-spec handle_disco_items_get(HostType :: host_type(),
                             Acc :: mongoose_acc:t(),
                             From ::jid:jid(), To ::jid:jid(),
                             DiscoItems :: disco_items_req_props(),
                             OrigPacket :: exml:element()) ->
    mongoose_acc:t().
handle_disco_items_get(HostType, Acc, From, To, DiscoItems0, OrigPacket) ->
    case catch mod_muc_light_db_backend:get_user_rooms(HostType, jid:to_lus(From), To#jid.lserver) of
        {error, Error} ->
            ?LOG_ERROR(#{what => muc_get_user_rooms_failed,
                         text => <<"Couldn't get room list for user">>,
                         from_jid => From, reason => Error}),
            mod_muc_light_codec_backend:encode_error(
              {error, internal_server_error}, From, To, OrigPacket, Acc);
        Rooms ->
            RoomsInfo = get_rooms_info(HostType, lists:sort(Rooms)),
            RouteFun = make_handler_fun(Acc),
            RoomsPerPage = gen_mod:get_module_opt(HostType, ?MODULE, rooms_per_page),
            case apply_rsm(RoomsInfo, length(RoomsInfo),
                           page_service_limit(DiscoItems0#disco_items.rsm, RoomsPerPage)) of
                {ok, RoomsInfoSlice, RSMOut} ->
                    DiscoItems = DiscoItems0#disco_items{ rooms = RoomsInfoSlice, rsm = RSMOut },
                    mod_muc_light_codec_backend:encode({get, DiscoItems},
                                                       From, To,
                                                       RouteFun, Acc);
                {error, item_not_found} ->
                    mod_muc_light_codec_backend:encode_error({error, item_not_found},
                                                             From, To, OrigPacket, Acc)
            end
    end.

-spec get_rooms_info(HostType :: mongooseim:host_type(),
                     Rooms :: [jid:simple_bare_jid()]) -> [disco_room_info()].
get_rooms_info(_HostType, []) ->
    [];
get_rooms_info(HostType, [{RoomU, _} = RoomUS | RRooms]) ->
    {ok, Config, Version} = mod_muc_light_db_backend:get_config(HostType, RoomUS),
    RoomName = case lists:keyfind(roomname, 1, Config) of
                   false -> RoomU;
                   {_, RoomName0} -> RoomName0
               end,
    [{RoomUS, RoomName, Version} | get_rooms_info(HostType, RRooms)].

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
    HostType = mongoose_acc:host_type(Acc),
    BlockingItems = mod_muc_light_db_backend:get_blocking(HostType, jid:to_lus(From), To#jid.lserver),
    mod_muc_light_codec_backend:encode({get, Blocking#blocking{ items = BlockingItems }},
                                       From, To, make_handler_fun(Acc), Acc);
handle_blocking(Acc, From, To, {set, #blocking{ items = Items }} = BlockingReq) ->
    case lists:any(fun({_, _, {WhoU, WhoS}}) -> WhoU =:= <<>> orelse WhoS =:= <<>> end, Items) of
        true ->
            {error, bad_request};
        false ->
            HostType = mongoose_acc:host_type(Acc),
            ok = mod_muc_light_db_backend:set_blocking(HostType, jid:to_lus(From), To#jid.lserver, Items),
            mod_muc_light_codec_backend:encode(
              BlockingReq, From, To, make_handler_fun(Acc), Acc),
            ok
    end.

-spec bcast_removed_user(Acc :: mongoose_acc:t(), UserJid :: jid:jid(),
                         AffectedRooms :: mod_muc_light_db_backend:remove_user_return(),
                         Version :: binary()) -> ok.
bcast_removed_user(Acc, UserJid, AffectedRooms, Version) ->
    bcast_removed_user(Acc, UserJid, AffectedRooms,
                       Version, mongoose_bin:gen_from_timestamp()).

-spec bcast_removed_user(Acc :: mongoose_acc:t(), UserJID :: jid:jid(),
                         AffectedRooms :: mod_muc_light_db_backend:remove_user_return(),
                         Version :: binary(),
                         PacketID :: binary()) -> ok.
bcast_removed_user(_Acc, _UserJID, [], _Version, _ID) ->
    ok;
bcast_removed_user(Acc, UserJID,
                   [{{RoomU, RoomS}, {ok, OldAffUsers, NewAffUsers, AffUsersChanged, PrevVersion}}
                    | RAffected], Version, ID) ->
    Affiliations = #affiliations{
                      id = ID,
                      prev_version = PrevVersion,
                      version = Version,
                      aff_users = AffUsersChanged
                     },
    Cmd = {set, Affiliations, OldAffUsers, NewAffUsers},
    RoomJid = jid:make_noprep(RoomU, RoomS, <<>>),
    mod_muc_light_codec_backend:encode(Cmd, UserJID, RoomJid, make_handler_fun(Acc), Acc),
    bcast_removed_user(Acc, UserJID, RAffected, Version, ID);
bcast_removed_user(Acc, UserJID, [{{RoomU, RoomS} = _RoomUS, Error} | RAffected], Version, ID) ->
    ?LOG_ERROR(#{what => muc_remove_user_failed,
                 user_jid => jid:to_binary(UserJID), room => RoomU, sub_host => RoomS,
                 reason => Error}),
    bcast_removed_user(Acc, UserJID, RAffected, Version, ID).

-spec maybe_forget_rooms(Acc :: mongoose_acc:t(),
                         AffectedRooms :: mod_muc_light_db_backend:remove_user_return(),
                         Version :: binary()) -> ok.
maybe_forget_rooms(_Acc, [], _) ->
    ok;
maybe_forget_rooms(Acc, [{RoomUS, {ok, _, NewAffUsers, _, _}} | RAffectedRooms], Version) ->
    mod_muc_light_room:maybe_forget(Acc, RoomUS, NewAffUsers, Version),
    maybe_forget_rooms(Acc, RAffectedRooms, Version).

make_handler_fun(Acc) ->
    fun(From, To, Packet) -> ejabberd_router:route(From, To, Acc, Packet) end.

-spec config_metrics(mongooseim:host_type()) -> [{gen_mod:opt_key(), gen_mod:opt_value()}].
config_metrics(HostType) ->
    mongoose_module_metrics:opts_for_module(HostType, ?MODULE, [backend]).
