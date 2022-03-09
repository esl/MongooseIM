-module(mod_muc_light_cache).

-include("mongoose_config_spec.hrl").

-behaviour(gen_mod).
-define(FRONTEND, mod_muc_light).

%% gen_mod callbacks
-export([start/2, stop/1, config_spec/0, supported_features/0]).

%% Hook handlers
-export([pre_acc_room_affiliations/2, post_acc_room_affiliations/2,
         pre_room_exists/3, post_room_exists/3,
         forget_room/4, remove_domain/3, room_new_affiliations/4]).
-ignore_xref([pre_acc_room_affiliations/2, post_acc_room_affiliations/2,
              pre_room_exists/3, post_room_exists/3,
              forget_room/4, remove_domain/3, room_new_affiliations/4]).

%% For tests
-export([force_clear/1]).

-spec start(mongooseim:host_type(), gen_mod:module_opts()) -> ok.
start(HostType, Opts) ->
    start_cache(HostType, Opts),
    ejabberd_hooks:add(hooks(HostType)),
    ok.

-spec stop(mongooseim:host_type()) -> ok.
stop(HostType) ->
    ejabberd_hooks:delete(hooks(HostType)),
    stop_cache(HostType),
    ok.

-spec config_spec() -> mongoose_config_spec:config_section().
config_spec() ->
    Section = #section{defaults = Defaults} = mongoose_user_cache:config_spec(),
    Section#section{defaults = Defaults#{<<"time_to_live">> := 2}}.

-spec supported_features() -> [atom()].
supported_features() ->
    [dynamic_domains].

-spec hooks(mongooseim:host_type()) -> [ejabberd_hooks:hook()].
hooks(HostType) ->
    [
     {acc_room_affiliations, HostType, ?MODULE, pre_acc_room_affiliations, 40},
     {acc_room_affiliations, HostType, ?MODULE, post_acc_room_affiliations, 60},
     {room_exists, HostType, ?MODULE, pre_room_exists, 40},
     {room_exists, HostType, ?MODULE, post_room_exists, 60},
     {forget_room, HostType, ?MODULE, forget_room, 80},
     {remove_domain, HostType, ?MODULE, remove_domain, 20},
     {room_new_affiliations, HostType, ?MODULE, room_new_affiliations, 50}
    ].

-spec pre_acc_room_affiliations(mongoose_acc:t(), jid:jid()) ->
    mongoose_acc:t() | {stop, mongoose_acc:t()}.
pre_acc_room_affiliations(Acc, RoomJid) ->
    case mongoose_acc:get(?FRONTEND, affiliations, {error, not_exists}, Acc) of
        {error, _} ->
            HostType = mongoose_acc:host_type(Acc),
            case mongoose_user_cache:get_entry(HostType, ?MODULE, RoomJid) of
                #{affs := Res} ->
                    mongoose_acc:set(?FRONTEND, affiliations, Res, Acc);
                _ ->
                    Acc
            end;
        _Res ->
            {stop, Acc}
    end.

-spec post_acc_room_affiliations(mongoose_acc:t(), jid:jid()) -> mongoose_acc:t().
post_acc_room_affiliations(Acc, RoomJid) ->
    case mongoose_acc:get(?FRONTEND, affiliations, {error, not_exists}, Acc) of
        {error, _} ->
            Acc;
        Res ->
            HostType = mongoose_acc:host_type(Acc),
            mongoose_user_cache:merge_entry(HostType, ?MODULE, RoomJid, #{affs => Res}),
            Acc
    end.

-spec pre_room_exists(boolean(), mongooseim:host_type(), jid:jid()) ->
    boolean() | {stop, true}.
pre_room_exists(false, HostType, RoomJid) ->
    case mongoose_user_cache:is_member(HostType, ?MODULE, RoomJid) of
        true -> {stop, true};
        false -> false
    end;
pre_room_exists(Status, _, _) ->
    Status.

-spec post_room_exists(boolean(), mongooseim:host_type(), jid:jid()) ->
    boolean().
post_room_exists(true, HostType, RoomJid) ->
    mongoose_user_cache:merge_entry(HostType, ?MODULE, RoomJid, #{}),
    true;
post_room_exists(Status, _, _) ->
    Status.

-spec forget_room(mongoose_hooks:simple_acc(), mongooseim:host_type(), jid:lserver(), binary()) ->
    mongoose_hooks:simple_acc().
forget_room(Acc, HostType, RoomS, RoomU) ->
    mongoose_user_cache:delete_user(HostType, ?MODULE, jid:make_noprep(RoomU, RoomS, <<>>)),
    Acc.

-spec remove_domain(mongoose_hooks:simple_acc(), mongooseim:host_type(), jid:lserver()) ->
    mongoose_hooks:simple_acc().
remove_domain(Acc, HostType, Domain) ->
    MUCHost = mod_muc_light:server_host_to_muc_host(HostType, Domain),
    mongoose_user_cache:delete_domain(HostType, ?MODULE, MUCHost),
    Acc.

-spec room_new_affiliations(mongoose_acc:t(), jid:jid(), mod_muc_light:aff_users(), binary()) ->
    mongoose_acc:t().
room_new_affiliations(Acc, RoomJid, NewAffs, NewVersion) ->
    HostType = mod_muc_light_utils:acc_to_host_type(Acc),
    % make sure other nodes forget about stale values
    mongoose_user_cache:delete_user(HostType, ?MODULE, RoomJid),
    mongoose_user_cache:merge_entry(HostType, ?MODULE, RoomJid, #{affs => {ok, NewAffs, NewVersion}}),
    Acc.

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
