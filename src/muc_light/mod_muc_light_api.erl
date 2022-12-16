%% @doc Provide an interface for frontends (like graphql or ctl) to manage MUC Light rooms.
-module(mod_muc_light_api).

-export([create_room/3,
         create_room/4,
         invite_to_room/3,
         change_room_config/3,
         change_affiliation/4,
         send_message/3,
         send_message/4,
         delete_room/2,
         delete_room/1,
         get_room_messages/3,
         get_room_messages/4,
         get_room_messages/5,
         get_user_rooms/1,
         get_room_info/1,
         get_room_info/2,
         get_room_aff/1,
         get_room_aff/2,
         get_blocking_list/1,
         set_blocking/2
        ]).

-include("mod_muc_light.hrl").
-include("mongoose.hrl").
-include("jlib.hrl").
-include("mongoose_rsm.hrl").

-type room() :: #{jid := jid:jid(),
                  aff_users := aff_users(),
                  options := map()}.

-export_type([room/0]).

-define(ROOM_DELETED_SUCC_RESULT, {ok, "Room deleted successfully"}).
-define(USER_NOT_ROOM_MEMBER_RESULT, {not_room_member, "Given user does not occupy this room"}).
-define(ROOM_NOT_FOUND_RESULT, {room_not_found, "Room not found"}).
-define(MUC_SERVER_NOT_FOUND_RESULT, {muc_server_not_found, "MUC Light server not found"}).
-define(VALIDATION_ERROR_RESULT(Key, Reason),
        {validation_error, io_lib:format("Validation failed for key: ~ts with reason ~p",
                                         [Key, Reason])}).

-spec create_room(jid:lserver(), jid:jid(), map()) ->
          {ok, room()} | {user_not_found | muc_server_not_found |
                          max_occupants_reached | validation_error, iolist()}.
create_room(MUCLightDomain, CreatorJID, Config) ->
    M = #{user => CreatorJID, room => jid:make_bare(<<>>, MUCLightDomain), options => Config},
    fold(M, [fun check_user/1, fun check_muc_domain/1, fun create_room_raw/1]).

-spec create_room(jid:lserver(), jid:luser(), jid:jid(), map()) ->
          {ok, room()} | {user_not_found | muc_server_not_found | already_exists |
                          max_occupants_reached | validation_error, iolist()}.
create_room(MUCLightDomain, RoomID, CreatorJID, Config) ->
    M = #{user => CreatorJID, room => jid:make_bare(RoomID, MUCLightDomain), options => Config},
    fold(M, [fun check_user/1, fun check_muc_domain/1, fun create_room_raw/1]).

-spec invite_to_room(jid:jid(), jid:jid(), jid:jid()) ->
    {ok | user_not_found | muc_server_not_found | room_not_found | not_room_member, iolist()}.
invite_to_room(RoomJID, SenderJID, RecipientJID) ->
    M = #{user => SenderJID, room => RoomJID, recipient => RecipientJID},
    fold(M, [fun check_user/1, fun check_muc_domain/1, fun get_user_aff/1,
             fun do_invite_to_room/1]).

-spec change_room_config(jid:jid(), jid:jid(), map()) ->
    {ok, room()} | {user_not_found | muc_server_not_found | room_not_found | not_room_member |
                    not_allowed | validation_error, iolist()}.
change_room_config(RoomJID, UserJID, Config) ->
    M = #{user => UserJID, room => RoomJID, config => Config},
    fold(M, [fun check_user/1, fun check_muc_domain/1, fun do_change_room_config/1]).

-spec change_affiliation(jid:jid(), jid:jid(), jid:jid(), add | remove) ->
          {ok | user_not_found | muc_server_not_found | room_not_found | not_room_member |
           not_allowed, iolist()}.
change_affiliation(RoomJID, SenderJID, RecipientJID, Op) ->
    M = #{user => SenderJID, room => RoomJID, recipient => RecipientJID, op => Op},
    fold(M, [fun check_user/1, fun check_muc_domain/1, fun get_user_aff/1,
             fun check_aff_permission/1, fun do_change_affiliation/1]).

-spec send_message(jid:jid(), jid:jid(), binary()) ->
    {ok | user_not_found | muc_server_not_found | room_not_found | not_room_member, iolist()}.
send_message(RoomJID, SenderJID, Text) when is_binary(Text) ->
    Body = #xmlel{name = <<"body">>, children = [#xmlcdata{content = Text}]},
    send_message(RoomJID, SenderJID, [Body], []).

-spec send_message(jid:jid(), jid:jid(), [exml:element()], [exml:attr()]) ->
    {ok | user_not_found | muc_server_not_found | room_not_found | not_room_member, iolist()}.
send_message(RoomJID, SenderJID, Children, ExtraAttrs) ->
    M = #{user => SenderJID, room => RoomJID, children => Children, attrs => ExtraAttrs},
    fold(M, [fun check_user/1, fun check_muc_domain/1, fun get_user_aff/1, fun do_send_message/1]).

-spec delete_room(jid:jid(), jid:jid()) ->
    {ok | not_allowed | room_not_found | not_room_member | muc_server_not_found , iolist()}.
delete_room(RoomJID, UserJID) ->
    M = #{user => UserJID, room => RoomJID},
    fold(M, [fun check_user/1, fun check_muc_domain/1, fun get_user_aff/1,
             fun check_delete_permission/1, fun do_delete_room/1]).

-spec delete_room(jid:jid()) -> {ok | muc_server_not_found | room_not_found, iolist()}.
delete_room(RoomJID) ->
    M = #{room => RoomJID},
    fold(M, [fun check_muc_domain/1, fun do_delete_room/1]).

-spec get_room_messages(jid:jid(), jid:jid(), integer() | undefined,
                        mod_mam:unix_timestamp() | undefined) ->
    {ok, list()} | {user_not_found | muc_server_not_found | room_not_found | not_room_member |
                    internal, iolist()}.
get_room_messages(RoomJID, UserJID, PageSize, Before) ->
    M = #{user => UserJID, room => RoomJID, page_size => PageSize, before => Before},
    fold(M, [fun check_user/1, fun check_muc_domain/1, fun get_user_aff/1,
             fun do_get_room_messages/1]).

-spec get_room_messages(jid:jid(), integer() | undefined,
                        mod_mam:unix_timestamp() | undefined) ->
    {ok, [mod_mam:message_row()]} | {muc_server_not_found | room_not_found | internal, iolist()}.
get_room_messages(RoomJID, PageSize, Before) ->
    M = #{user => undefined, room => RoomJID, page_size => PageSize, before => Before},
    fold(M, [fun check_muc_domain/1, fun check_room/1, fun do_get_room_messages/1]).

-spec get_room_info(jid:jid(), jid:jid()) ->
    {ok, room()} | {user_not_found | muc_server_not_found | room_not_found | not_room_member,
                    iolist()}.
get_room_info(RoomJID, UserJID) ->
    M = #{user => UserJID, room => RoomJID},
    fold(M, [fun check_user/1, fun check_muc_domain/1, fun do_get_room_info/1,
             fun check_room_member/1, fun return_info/1]).

-spec get_room_info(jid:jid()) -> {ok, room()} | {muc_server_not_found | room_not_found, iolist()}.
get_room_info(RoomJID) ->
    M = #{room => RoomJID},
    fold(M, [fun check_muc_domain/1, fun do_get_room_info/1, fun return_info/1]).

-spec get_room_aff(jid:jid(), jid:jid()) ->
    {ok, aff_users()} | {user_not_found | muc_server_not_found | room_not_found | not_room_member,
                         iolist()}.
get_room_aff(RoomJID, UserJID) ->
    M = #{user => UserJID, room => RoomJID},
    fold(M, [fun check_user/1, fun check_muc_domain/1, fun do_get_room_aff/1,
             fun check_room_member/1, fun return_aff/1]).

-spec get_room_aff(jid:jid()) ->
          {ok, aff_users()} | {muc_server_not_found | room_not_found, iolist()}.
get_room_aff(RoomJID) ->
    M = #{room => RoomJID},
    fold(M, [fun check_muc_domain/1, fun do_get_room_aff/1, fun return_aff/1]).

-spec get_user_rooms(jid:jid()) ->
          {ok, [RoomUS :: jid:simple_bare_jid()]} | {user_not_found, iolist()}.
get_user_rooms(UserJID) ->
    fold(#{user => UserJID}, [fun check_user/1, fun do_get_user_rooms/1]).

-spec get_blocking_list(jid:jid()) -> {ok, [blocking_item()]} | {user_not_found, iolist()}.
get_blocking_list(UserJID) ->
    fold(#{user => UserJID}, [fun check_user/1, fun do_get_blocking_list/1]).

-spec set_blocking(jid:jid(), [blocking_item()]) -> {ok | user_not_found, iolist()}.
set_blocking(UserJID, Items) ->
    fold(#{user => UserJID, items => Items}, [fun check_user/1, fun do_set_blocking_list/1]).

%% Internal: steps used in fold/2

check_user(M = #{user := UserJID = #jid{lserver = LServer}}) ->
    case mongoose_domain_api:get_domain_host_type(LServer) of
        {ok, HostType} ->
            case ejabberd_auth:does_user_exist(HostType, UserJID, stored) of
                true -> M#{user_host_type => HostType};
                false -> {user_not_found, "Given user does not exist"}
            end;
        {error, not_found} ->
            {user_not_found, "User's domain does not exist"}
    end.

check_muc_domain(M = #{room := #jid{lserver = LServer}}) ->
    case mongoose_domain_api:get_subdomain_host_type(LServer) of
        {ok, HostType} ->
            M#{muc_host_type => HostType};
        {error, not_found} ->
            ?MUC_SERVER_NOT_FOUND_RESULT
    end.

check_room_member(M = #{user := UserJID, aff_users := AffUsers}) ->
    case get_aff(jid:to_lus(UserJID), AffUsers) of
        none ->
            ?USER_NOT_ROOM_MEMBER_RESULT;
        _ ->
            M
    end.

create_room_raw(#{room := InRoomJID, user := CreatorJID, options := Options}) ->
    Config = make_room_config(Options),
    case mod_muc_light:try_to_create_room(CreatorJID, InRoomJID, Config) of
        {ok, RoomJID, #create{aff_users = AffUsers, raw_config = Conf}} ->
            {ok, make_room(RoomJID, Conf, AffUsers)};
        {error, exists} ->
            {already_exists, "Room already exists"};
        {error, max_occupants_reached} ->
            {max_occupants_reached, "Max occupants number reached"};
        {error, {Key, Reason}} ->
            ?VALIDATION_ERROR_RESULT(Key, Reason)
    end.

do_invite_to_room(#{user := SenderJID, room := RoomJID, recipient := RecipientJID}) ->
    S = jid:to_bare(SenderJID),
    R = jid:to_bare(RoomJID),
    RecipientBin = jid:to_binary(jid:to_bare(RecipientJID)),
    Changes = query(?NS_MUC_LIGHT_AFFILIATIONS, [affiliate(RecipientBin, <<"member">>)]),
    ejabberd_router:route(S, R, iq(jid:to_binary(S), jid:to_binary(R), <<"set">>, [Changes])),
    {ok, "User invited successfully"}.

do_change_room_config(#{user := UserJID, room := RoomJID, config := Config,
                        muc_host_type := HostType}) ->
    UserUS = jid:to_bare(UserJID),
    ConfigReq = #config{ raw_config = maps:to_list(Config) },
    #jid{lserver = LServer} = UserJID,
    #jid{luser = RoomID, lserver = MUCServer} = RoomJID,
    Acc = mongoose_acc:new(#{location => ?LOCATION, lserver => LServer, host_type => HostType}),
    case mod_muc_light:change_room_config(UserUS, RoomID, MUCServer, ConfigReq, Acc) of
        {ok, RoomJID, KV}  ->
            {ok, make_room(RoomJID, KV, [])};
        {error, item_not_found} ->
            ?USER_NOT_ROOM_MEMBER_RESULT;
        {error, not_allowed} ->
            {not_allowed, "Given user does not have permission to change config"};
        {error, not_exists} ->
            ?ROOM_NOT_FOUND_RESULT;
        {error, {Key, Reason}} ->
            ?VALIDATION_ERROR_RESULT(Key, Reason)
    end.

check_aff_permission(M = #{user := UserJID, recipient := RecipientJID, aff := Aff, op := Op}) ->
    case {Aff, Op} of
        {member, remove} when RecipientJID =:= UserJID ->
            M;
        {owner, _} ->
            M;
        _ -> {not_allowed, "Given user does not have permission to change affiliations"}
    end.

check_delete_permission(M = #{aff := owner}) -> M;
check_delete_permission(#{}) -> {not_allowed, "Given user cannot delete this room"}.

get_user_aff(M = #{muc_host_type := HostType, user := UserJID, room := RoomJID}) ->
    case get_room_user_aff(HostType, RoomJID, UserJID) of
        {ok, owner} ->
            M#{aff => owner};
        {ok, member} ->
            M#{aff => member};
        {ok, none} ->
            ?USER_NOT_ROOM_MEMBER_RESULT;
        {error, room_not_found} ->
            ?ROOM_NOT_FOUND_RESULT
    end.

do_change_affiliation(#{user := SenderJID, room := RoomJID, recipient := RecipientJID, op := Op}) ->
    RecipientBare = jid:to_bare(RecipientJID),
    S = jid:to_bare(SenderJID),
    Changes = query(?NS_MUC_LIGHT_AFFILIATIONS,
                    [affiliate(jid:to_binary(RecipientBare), op_to_aff(Op))]),
    ejabberd_router:route(S, RoomJID, iq(jid:to_binary(S), jid:to_binary(RoomJID),
                                         <<"set">>, [Changes])),
    {ok, "Affiliation change request sent successfully"}.

do_send_message(#{user := SenderJID, room := RoomJID, children := Children, attrs := ExtraAttrs}) ->
    SenderBare = jid:to_bare(SenderJID),
    RoomBare = jid:to_bare(RoomJID),
    Stanza = #xmlel{name = <<"message">>,
                    attrs = [{<<"type">>, <<"groupchat">>} | ExtraAttrs],
                    children = Children},
    ejabberd_router:route(SenderBare, RoomBare, Stanza),
    {ok, "Message sent successfully"}.

do_delete_room(#{room := RoomJID}) ->
    case mod_muc_light:delete_room(jid:to_lus(RoomJID)) of
        ok ->
            ?ROOM_DELETED_SUCC_RESULT;
        {error, not_exists} ->
            ?ROOM_NOT_FOUND_RESULT
    end.

do_get_room_messages(#{user := CallerJID, room := RoomJID, page_size := PageSize, before := Before,
                       muc_host_type := HostType}) ->
   get_room_messages(HostType, RoomJID, CallerJID, PageSize, Before).

%% Exported for mod_muc_api
get_room_messages(HostType, RoomJID, CallerJID, PageSize, Before) ->
    ArchiveID = mod_mam_muc:archive_id_int(HostType, RoomJID),
    Now = os:system_time(microsecond),
    End = maybe_before(Before, Now),
    RSM = #rsm_in{direction = before, id = undefined},
    Params = #{archive_id => ArchiveID,
               owner_jid => RoomJID,
               rsm => RSM,
               borders => undefined,
               start_ts => undefined,
               end_ts => End,
               now => Now,
               with_jid => undefined,
               search_text => undefined,
               page_size => PageSize,
               limit_passed => true,
               max_result_limit => 50,
               is_simple => true},
    case mod_mam_muc:lookup_messages(HostType, maybe_caller_jid(CallerJID, Params)) of
        {ok, {_, _, Messages}} ->
            {ok, Messages};
        {error, Term} ->
            {internal, io_lib:format("Internal error occured ~p", [Term])}
    end.

do_get_room_info(M = #{room := RoomJID, muc_host_type := HostType}) ->
    case mod_muc_light_db_backend:get_info(HostType, jid:to_lus(RoomJID)) of
        {ok, Config, AffUsers, _Version} ->
            M#{aff_users => AffUsers, options => Config};
        {error, not_exists} ->
            ?ROOM_NOT_FOUND_RESULT
    end.

return_info(#{room := RoomJID, aff_users := AffUsers, options := Config}) ->
    {ok, make_room(jid:to_binary(RoomJID), Config, AffUsers)}.

do_get_room_aff(M = #{room := RoomJID, muc_host_type := HostType}) ->
    case mod_muc_light_db_backend:get_aff_users(HostType, jid:to_lus(RoomJID)) of
        {ok, AffUsers, _Version} ->
            M#{aff_users => AffUsers};
        {error, not_exists} ->
            ?ROOM_NOT_FOUND_RESULT
    end.

return_aff(#{aff_users := AffUsers}) ->
    {ok, AffUsers}.

check_room(M = #{room := RoomJID, muc_host_type := HostType}) ->
    case mod_muc_light_db_backend:room_exists(HostType, jid:to_lus(RoomJID)) of
        true ->
            M;
        false ->
            ?ROOM_NOT_FOUND_RESULT
    end.

do_get_user_rooms(#{user := UserJID, user_host_type := HostType}) ->
    MUCServer = mod_muc_light_utils:server_host_to_muc_host(HostType, UserJID#jid.lserver),
    {ok, mod_muc_light_db_backend:get_user_rooms(HostType, jid:to_lus(UserJID), MUCServer)}.

do_get_blocking_list(#{user := UserJID, user_host_type := HostType}) ->
    MUCServer = mod_muc_light_utils:server_host_to_muc_host(HostType, UserJID#jid.lserver),
    {ok, mod_muc_light_db_backend:get_blocking(HostType, jid:to_lus(UserJID), MUCServer)}.

do_set_blocking_list(#{user := UserJID, user_host_type := HostType, items := Items}) ->
    MUCServer = mod_muc_light_utils:server_host_to_muc_host(HostType, UserJID#jid.lserver),
    Q = query(?NS_MUC_LIGHT_BLOCKING, [blocking_item(I) || I <- Items]),
    Iq = iq(jid:to_binary(UserJID), MUCServer, <<"set">>, [Q]),
    ejabberd_router:route(UserJID, jid:from_binary(MUCServer), Iq),
    {ok, "User blocking list updated successfully"}.

%% Internal: helpers

-spec blocking_item(blocking_item()) -> exml:element().
blocking_item({What, Action, Who}) ->
    #xmlel{name = atom_to_binary(What),
           attrs = [{<<"action">>, atom_to_binary(Action)}],
           children = [#xmlcdata{ content = jid:to_binary(Who)}]
          }.

-spec make_room_config(map()) -> create_req_props().
make_room_config(Options) ->
    #create{raw_config = maps:to_list(Options)}.

-spec get_room_user_aff(mongooseim:host_type(), jid:jid(), jid:jid()) ->
    {ok, aff()} | {error, room_not_found}.
get_room_user_aff(HostType, RoomJID, UserJID) ->
    RoomUS = jid:to_lus(RoomJID),
    UserUS = jid:to_lus(UserJID),
    case mod_muc_light_db_backend:get_aff_users(HostType, RoomUS) of
        {ok, Affs, _Version} ->
            {ok, get_aff(UserUS, Affs)};
        {error, not_exists} ->
            {error, room_not_found}
    end.

-spec get_aff(jid:simple_bare_jid(), aff_users()) -> aff().
get_aff(UserUS, Affs) ->
    case lists:keyfind(UserUS, 1, Affs) of
        {_, Aff} -> Aff;
        false -> none
    end.

make_room(JID, #config{ raw_config = Options}, AffUsers) ->
    make_room(JID, Options, AffUsers);
make_room(JID, Options, AffUsers) when is_list(Options) ->
    make_room(JID, maps:from_list(ensure_keys_are_binaries(Options)), AffUsers);
make_room(JID, Options, AffUsers) when is_map(Options) ->
    #{jid => JID, aff_users => AffUsers, options => Options}.

ensure_keys_are_binaries([{K, _}|_] = Conf) when is_binary(K) ->
    Conf;
ensure_keys_are_binaries(Conf) ->
    [{atom_to_binary(K), V} || {K, V} <- Conf].

iq(To, From, Type, Children) ->
    UUID = uuid:uuid_to_string(uuid:get_v4(), binary_standard),
    #xmlel{name = <<"iq">>,
           attrs = [{<<"from">>, From},
                    {<<"to">>, To},
                    {<<"type">>, Type},
                    {<<"id">>, UUID}],
           children = Children
          }.

query(NS, Children) when is_binary(NS), is_list(Children) ->
    #xmlel{name = <<"query">>,
           attrs = [{<<"xmlns">>, NS}],
           children = Children
          }.

affiliate(JID, Kind) when is_binary(JID), is_binary(Kind) ->
    #xmlel{name = <<"user">>,
           attrs = [{<<"affiliation">>, Kind}],
           children = [ #xmlcdata{ content = JID } ]
          }.

maybe_before(undefined, Now) ->
    Now;
maybe_before(Timestamp, _) ->
   Timestamp.

maybe_caller_jid(undefined, Params) ->
    Params;
maybe_caller_jid(CallerJID, Params) ->
    Params#{caller_jid => CallerJID}.

op_to_aff(add) -> <<"member">>;
op_to_aff(remove) -> <<"none">>.

fold({_, _} = Result, _) ->
    Result;
fold(M, [Step | Rest]) when is_map(M) ->
    fold(Step(M), Rest).
