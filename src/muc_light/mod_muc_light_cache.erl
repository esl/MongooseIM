-module(mod_muc_light_cache).

-include("mongoose_config_spec.hrl").

-behaviour(gen_mod).

%% gen_mod callbacks
-export([start/2, stop/1, hooks/1, config_spec/0, supported_features/0]).

%% Hook handlers
-export([pre_acc_room_affiliations/3, post_acc_room_affiliations/3,
         pre_room_exists/3, post_room_exists/3,
         forget_room/3, remove_domain/3, room_new_affiliations/3]).

%% For tests
-export([force_clear/1]).

-spec start(mongooseim:host_type(), gen_mod:module_opts()) -> ok.
start(HostType, Opts) ->
    start_cache(HostType, Opts),
    ok.

-spec stop(mongooseim:host_type()) -> ok.
stop(HostType) ->
    stop_cache(HostType),
    ok.

-spec config_spec() -> mongoose_config_spec:config_section().
config_spec() ->
    Section = #section{defaults = Defaults} = mongoose_user_cache:config_spec(),
    Section#section{defaults = Defaults#{<<"time_to_live">> := 2}}.

-spec supported_features() -> [atom()].
supported_features() ->
    [dynamic_domains].

-spec hooks(mongooseim:host_type()) -> gen_hook:hook_list().
hooks(HostType) ->
    [
     {acc_room_affiliations, HostType, fun ?MODULE:pre_acc_room_affiliations/3, #{}, 40},
     {acc_room_affiliations, HostType, fun ?MODULE:post_acc_room_affiliations/3, #{}, 60},
     {room_exists, HostType, fun ?MODULE:pre_room_exists/3, #{}, 40},
     {room_exists, HostType, fun ?MODULE:post_room_exists/3, #{}, 60},
     {forget_room, HostType, fun ?MODULE:forget_room/3, #{}, 80},
     {remove_domain, HostType, fun ?MODULE:remove_domain/3, #{}, 20},
     {room_new_affiliations, HostType, fun ?MODULE:room_new_affiliations/3, #{}, 50}
    ].

-spec pre_acc_room_affiliations(Acc, Params, Extra) -> {ok | stop, Acc} when
    Acc :: mongoose_acc:t(),
    Params :: map(),
    Extra :: gen_hook:extra().
pre_acc_room_affiliations(Acc, #{room := RoomJid}, #{host_type := HostType}) ->
    case mod_muc_light:get_room_affs_from_acc(Acc, RoomJid) of
        {error, _} ->
            case mongoose_user_cache:get_entry(HostType, ?MODULE, RoomJid) of
                #{affs := Res} ->
                    {ok, mod_muc_light:set_room_affs_from_acc(Acc, RoomJid, Res)};
                _ ->
                    {ok, Acc}
            end;
        _Res ->
            {stop, Acc}
    end.

-spec post_acc_room_affiliations(Acc, Params, Extra) -> {ok, Acc} when
    Acc :: mongoose_acc:t(),
    Params :: map(),
    Extra :: gen_hook:extra().
post_acc_room_affiliations(Acc, #{room := RoomJid}, #{host_type := HostType}) ->
    case mod_muc_light:get_room_affs_from_acc(Acc, RoomJid) of
        {error, _} ->
            {ok, Acc};
        Res ->
            mongoose_user_cache:merge_entry(HostType, ?MODULE, RoomJid, #{affs => Res}),
            {ok, Acc}
    end.

-spec pre_room_exists(Acc, Params, Extra) -> {ok | stop, Acc} when
    Acc :: boolean(),
    Params :: map(),
    Extra :: gen_hook:extra().
pre_room_exists(false, #{room := RoomJid}, #{host_type := HostType}) ->
    case mongoose_user_cache:is_member(HostType, ?MODULE, RoomJid) of
        true -> {stop, true};
        false -> {ok, false}
    end;
pre_room_exists(Status, _, _) ->
    {ok, Status}.

-spec post_room_exists(Acc, Params, Extra) -> {ok, Acc} when
    Acc :: boolean(),
    Params :: map(),
    Extra :: gen_hook:extra().
post_room_exists(true, #{room := RoomJid}, #{host_Type := HostType}) ->
    mongoose_user_cache:merge_entry(HostType, ?MODULE, RoomJid, #{}),
    {ok, true};
post_room_exists(Status, _, _) ->
    {ok, Status}.

-spec forget_room(Acc, Params, Extra) -> {ok, Acc} when
    Acc :: mongoose_acc:t(),
    Params :: #{muc_host := jid:server(), room := jid:luser()},
    Extra :: gen_hook:extra().
forget_room(Acc, #{muc_host := RoomS, room := RoomU}, #{host_type := HostType}) ->
    mongoose_user_cache:delete_user(HostType, ?MODULE, jid:make_noprep(RoomU, RoomS, <<>>)),
    {ok, Acc}.

-spec remove_domain(Acc, Params, Extra) -> {ok, Acc} when
    Acc :: mongoose_domain_api:remove_domain_acc(),
    Params :: map(),
    Extra :: gen_hook:extra().
remove_domain(Acc, #{domain := Domain}, #{host_type := HostType}) ->
    MUCHost = mod_muc_light:server_host_to_muc_host(HostType, Domain),
    mongoose_user_cache:delete_domain(HostType, ?MODULE, MUCHost),
    {ok, Acc}.

-spec room_new_affiliations(Acc, Params, Extra) -> {ok, Acc} when
    Acc :: mongoose_acc:t(),
    Params :: map(),
    Extra :: gen_hook:extra().
room_new_affiliations(Acc, #{room := RoomJid, new_affs := NewAffs, version := NewVersion}, _Extra) ->
    HostType = mod_muc_light_utils:acc_to_host_type(Acc),
    % make sure other nodes forget about stale values
    mongoose_user_cache:delete_user(HostType, ?MODULE, RoomJid),
    mongoose_user_cache:merge_entry(HostType, ?MODULE, RoomJid, #{affs => {ok, NewAffs, NewVersion}}),
    {ok, Acc}.

-spec force_clear(mongooseim:host_type()) -> ok.
force_clear(HostType) ->
    CacheName = gen_mod:get_module_proc(HostType, ?MODULE),
    segmented_cache:delete_pattern(CacheName, {'_', '_'}),
    ok.

%%====================================================================
%% internal
%%====================================================================
-spec start_cache(mongooseim:host_type(), gen_mod:module_opts()) -> any().
start_cache(HostType, Opts) ->
    mongoose_user_cache:start_new_cache(HostType, ?MODULE, Opts).

-spec stop_cache(mongooseim:host_type()) -> any().
stop_cache(HostType) ->
    mongoose_user_cache:stop_cache(HostType, ?MODULE).
