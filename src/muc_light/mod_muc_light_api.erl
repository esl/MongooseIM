%% @doc Provide an interface for frontends (like graphql or ctl) to manage MUC Light rooms.
-module(mod_muc_light_api).

-export([create_room/4,
         create_room/5,
         invite_to_room/3,
         change_room_config/3,
         change_affiliation/4,
         remove_user_from_room/3,
         send_message/3,
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

-type create_room_result() :: {ok, room()} | {already_exists | max_occupants_reached |
                                              validation_error | muc_server_not_found, iolist()}.

-type room() :: #{jid := jid:jid(),
                  name := binary(),
                  subject := binary(),
                  aff_users := aff_users()}.

-export_type([room/0]).

-define(ROOM_DELETED_SUCC_RESULT, {ok, "Room deleted successfully"}).
-define(USER_NOT_ROOM_MEMBER_RESULT, {not_room_member, "Given user does not occupy this room"}).
-define(ROOM_NOT_FOUND_RESULT, {room_not_found, "Room not found"}).
-define(DELETE_NOT_EXISTING_ROOM_RESULT, {room_not_found, "Cannot remove not existing room"}).
-define(MUC_SERVER_NOT_FOUND_RESULT, {muc_server_not_found, "MUC Light server not found"}).
-define(VALIDATION_ERROR_RESULT(Key, Reason),
        {validation_error, io_lib:format("Validation failed for key: ~p with reason ~p",
                                         [Key, Reason])}).

-spec create_room(jid:lserver(), jid:jid(), binary(), binary()) -> create_room_result().
create_room(MUCLightDomain, CreatorJID, RoomTitle, Subject) ->
    create_room(MUCLightDomain, <<>>, CreatorJID, RoomTitle, Subject).

-spec create_room(jid:lserver(), jid:luser(), jid:jid(), binary(), binary()) ->
    create_room_result().
create_room(MUCLightDomain, RoomID, CreatorJID, RoomTitle, Subject) ->
    RoomJID = jid:make_bare(RoomID, MUCLightDomain),
    Options = #{<<"roomname">> => RoomTitle, <<"subject">> => Subject},
    create_room_raw(RoomJID, CreatorJID, Options).

-spec invite_to_room(jid:jid(), jid:jid(), jid:jid()) ->
    {ok | not_room_member | muc_server_not_found, iolist()}.
invite_to_room(#jid{lserver = MUCServer} = RoomJID, SenderJID, RecipientJID) ->
    case mongoose_domain_api:get_subdomain_host_type(MUCServer) of
        {ok, HostType} ->
            RecipientBin = jid:to_binary(jid:to_bare(RecipientJID)),
            case is_user_room_member(HostType, jid:to_lus(SenderJID), jid:to_lus(RoomJID)) of
                true ->
                    S = jid:to_bare(SenderJID),
                    R = jid:to_bare(RoomJID),
                    Changes = query(?NS_MUC_LIGHT_AFFILIATIONS,
                                    [affiliate(RecipientBin, <<"member">>)]),
                    ejabberd_router:route(S, R, iq(jid:to_binary(S), jid:to_binary(R),
                                                   <<"set">>, [Changes])),
                    {ok, "User invited successfully"};
                false ->
                   ?USER_NOT_ROOM_MEMBER_RESULT
            end;
        {error, not_found} ->
            ?MUC_SERVER_NOT_FOUND_RESULT
    end.


-spec change_room_config(jid:jid(), jid:jid(), map()) ->
    {ok, room()} | {not_room_member | not_allowed | room_not_found |
                    validation_error | muc_server_not_found, iolist()}.
change_room_config(#jid{luser = RoomID, lserver = MUCServer} = RoomJID,
                   UserJID, Config) ->
    case mongoose_domain_api:get_subdomain_info(MUCServer) of
        {ok, #{host_type := HostType, parent_domain := LServer}} ->
            UserUS = jid:to_bare(UserJID),
            ConfigReq = #config{ raw_config = maps:to_list(Config) },
            Acc = mongoose_acc:new(#{location => ?LOCATION, lserver => LServer,
                                     host_type => HostType}),
            case mod_muc_light:change_room_config(UserUS, RoomID, MUCServer, ConfigReq, Acc) of
                {ok, RoomJID, KV}  ->
                    {ok, make_room(RoomJID, KV, [])};
                {error, item_not_found} ->
                    ?USER_NOT_ROOM_MEMBER_RESULT;
                {error, not_allowed} ->
                    {not_allowed, "Given user does not have permission to change config"};
                {error, not_exists} ->
                    ?ROOM_NOT_FOUND_RESULT;
                {error, {error, {Key, Reason}}} ->
                    ?VALIDATION_ERROR_RESULT(Key, Reason)
            end;
        {error, not_found} ->
            ?MUC_SERVER_NOT_FOUND_RESULT
    end.

-spec change_affiliation(jid:jid(), jid:jid(), jid:jid(), binary()) -> ok.
change_affiliation(RoomJID, SenderJID, RecipientJID, Affiliation) ->
    RecipientBare = jid:to_bare(RecipientJID),
    S = jid:to_bare(SenderJID),
    Changes = query(?NS_MUC_LIGHT_AFFILIATIONS,
                    [affiliate(jid:to_binary(RecipientBare), Affiliation)]),
    ejabberd_router:route(S, RoomJID, iq(jid:to_binary(S), jid:to_binary(RoomJID),
                                         <<"set">>, [Changes])),
    ok.

-spec remove_user_from_room(jid:jid(), jid:jid(), jid:jid()) ->
    {ok, iolist()}.
remove_user_from_room(RoomJID, SenderJID, RecipientJID) ->
    ok = change_affiliation(RoomJID, SenderJID, RecipientJID, <<"none">>),
    {ok, io_lib:format("Stanza kicking user ~s sent successfully", [jid:to_binary(RecipientJID)])}.

-spec send_message(jid:jid(), jid:jid(), binary()) ->
    {ok | not_room_member | muc_server_not_found, iolist()}.
send_message(#jid{lserver = MUCServer} = RoomJID, SenderJID, Message) ->
    case mongoose_domain_api:get_subdomain_host_type(MUCServer) of
        {ok, HostType} ->
            Body = #xmlel{name = <<"body">>,
                          children = [ #xmlcdata{ content = Message } ]
                         },
            Stanza = #xmlel{name = <<"message">>,
                            attrs = [{<<"type">>, <<"groupchat">>}],
                            children = [ Body ]
                           },
            SenderBare = jid:to_bare(SenderJID),
            case is_user_room_member(HostType, jid:to_lus(SenderBare), jid:to_lus(RoomJID)) of
                true ->
                    RoomBare = jid:to_bare(RoomJID),
                    ejabberd_router:route(SenderBare, RoomBare, Stanza),
                    {ok, "Message sent successfully"};
                false ->
                    ?USER_NOT_ROOM_MEMBER_RESULT
            end;
        {error, not_found}->
            ?MUC_SERVER_NOT_FOUND_RESULT
    end.

-spec delete_room(jid:jid(), jid:jid()) ->
    {ok | not_allowed | room_not_found | muc_server_not_found , iolist()}.
delete_room(#jid{lserver = MUCServer} = RoomJID, UserJID) ->
    case mongoose_domain_api:get_subdomain_host_type(MUCServer) of
        {ok, HostType} ->
            case get_room_user_aff(HostType, RoomJID, UserJID) of
                {ok, owner} ->
                    ok = mod_muc_light_db_backend:destroy_room(HostType, jid:to_lus(RoomJID)),
                    ?ROOM_DELETED_SUCC_RESULT;
                {ok, none} ->
                    ?USER_NOT_ROOM_MEMBER_RESULT;
                {ok, member} ->
                    {not_allowed, "Given user cannot delete this room"};
                {error, room_not_found} ->
                    ?DELETE_NOT_EXISTING_ROOM_RESULT
            end;
        {error, not_found}->
            ?MUC_SERVER_NOT_FOUND_RESULT
    end.

-spec delete_room(jid:jid()) -> {ok | room_not_found | muc_server_not_found, iolist()}.
delete_room(RoomJID) ->
    try mod_muc_light:delete_room(jid:to_lus(RoomJID)) of
        ok ->
            ?ROOM_DELETED_SUCC_RESULT;
        {error, not_exists} ->
            ?DELETE_NOT_EXISTING_ROOM_RESULT
    catch
        error:{muc_host_to_host_type_failed, _, _} ->
            ?MUC_SERVER_NOT_FOUND_RESULT
    end.
-spec get_room_messages(jid:jid(), jid:jid(), integer() | undefined,
                        mod_mam:unix_timestamp() | undefined) ->
    {ok, list()} | {muc_server_not_found | internal | not_room_member, iolist()}.
get_room_messages(RoomJID, UserJID, PageSize, Before) ->
    case mongoose_domain_api:get_subdomain_host_type(RoomJID#jid.lserver) of
        {ok, HostType} ->
            case is_user_room_member(HostType, jid:to_lus(UserJID), jid:to_lus(RoomJID)) of
                true ->
                    get_room_messages(HostType, RoomJID, UserJID, PageSize, Before);
                false ->
                    ?USER_NOT_ROOM_MEMBER_RESULT
            end;
        {error, not_found} ->
            ?MUC_SERVER_NOT_FOUND_RESULT
    end.

-spec get_room_messages(jid:jid(), integer() | undefined,
                        mod_mam:unix_timestamp() | undefined) ->
    {ok, [mod_mam:message_row()]} | {muc_server_not_found | internal, iolist()}.
get_room_messages(RoomJID, PageSize, Before) ->
    case mongoose_domain_api:get_subdomain_host_type(RoomJID#jid.lserver) of
        {ok, HostType} ->
            get_room_messages(HostType, RoomJID, undefined, PageSize, Before);
        {error, not_found} ->
            ?MUC_SERVER_NOT_FOUND_RESULT
    end.

-spec get_room_messages(mongooseim:host_type(), jid:jid(), jid:jid() | undefined,
                        integer() | undefined, mod_mam:unix_timestamp() | undefined) ->
    {ok, list()} | {internal, iolist()}.
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

-spec get_room_info(jid:jid(), jid:jid()) ->
    {ok, room()} | {muc_server_not_found | room_not_found | not_room_member, iolist()}.
get_room_info(RoomJID, UserJID) ->
    case get_room_info(RoomJID) of
        {ok, #{aff_users := Affs} = Room} ->
            case get_aff(jid:to_lus(UserJID), Affs) of
                none ->
                    ?USER_NOT_ROOM_MEMBER_RESULT;
                _ ->
                    {ok, Room}
            end;
        Error ->
            Error
    end.

-spec get_room_info(jid:jid()) -> {ok, room()} | {muc_server_not_found | room_not_found, iolist()}.
get_room_info(#jid{lserver = MUCServer} = RoomJID) ->
    case mongoose_domain_api:get_subdomain_host_type(MUCServer) of
        {ok, HostType} ->
            case mod_muc_light_db_backend:get_info(HostType, jid:to_lus(RoomJID)) of
                {ok, Conf, AffUsers, _Version} ->
                    {ok, make_room(jid:to_binary(RoomJID), Conf, AffUsers)};
                {error, not_exists} ->
                    ?ROOM_NOT_FOUND_RESULT
            end;
        {error, not_found}->
            ?MUC_SERVER_NOT_FOUND_RESULT
    end.

-spec get_room_aff(jid:jid(), jid:jid()) ->
    {ok, aff_users()} | {muc_server_not_found | room_not_found, iolist()}.
get_room_aff(RoomJID, UserJID) ->
    case get_room_aff(RoomJID) of
        {ok, Affs} ->
            case get_aff(jid:to_lus(UserJID), Affs) of
                none ->
                    ?USER_NOT_ROOM_MEMBER_RESULT;
                _ ->
                    {ok, Affs}
            end;
        Error ->
            Error
    end.

-spec get_room_aff(jid:jid()) ->
    {ok, aff_users()} | {muc_server_not_found | room_not_found, iolist()}.
get_room_aff(#jid{lserver = MUCServer} = RoomJID) ->
    case mongoose_domain_api:get_subdomain_host_type(MUCServer) of
        {ok, HostType} ->
            case mod_muc_light_db_backend:get_aff_users(HostType, jid:to_lus(RoomJID)) of
                {ok, AffUsers, _Version} ->
                    {ok, AffUsers};
                {error, not_exists} ->
                    ?ROOM_NOT_FOUND_RESULT
            end;
        {error, not_found}->
            ?MUC_SERVER_NOT_FOUND_RESULT
    end.

-spec get_user_rooms(jid:jid()) -> {ok, [RoomUS :: jid:simple_bare_jid()]} |
                                   {muc_server_not_found, iolist()}.
get_user_rooms(#jid{lserver = LServer} = UserJID) ->
    case mongoose_domain_api:get_domain_host_type(LServer) of
        {ok, HostType} ->
            UserUS = jid:to_lus(UserJID),
            MUCServer = mod_muc_light_utils:server_host_to_muc_host(HostType, LServer),
            {ok, mod_muc_light_db_backend:get_user_rooms(HostType, UserUS, MUCServer)};
        {error, not_found} ->
            ?MUC_SERVER_NOT_FOUND_RESULT
    end.

-spec get_blocking_list(jid:jid()) -> {ok, [blocking_item()]} | {muc_server_not_found, iolist()}.
get_blocking_list(#jid{lserver = LServer} = User) ->
    case mongoose_domain_api:get_domain_host_type(LServer) of
        {ok, HostType} ->
            MUCServer = mod_muc_light_utils:server_host_to_muc_host(HostType, LServer),
            {ok, mod_muc_light_db_backend:get_blocking(HostType, jid:to_lus(User), MUCServer)};
        {error, not_found} ->
            ?MUC_SERVER_NOT_FOUND_RESULT
    end.

-spec set_blocking(jid:jid(), [blocking_item()]) -> {ok | muc_server_not_found, iolist()}.
set_blocking(#jid{lserver = LServer} = User, Items) ->
     case mongoose_domain_api:get_domain_host_type(LServer) of
        {ok, HostType} ->
            MUCServer = mod_muc_light_utils:server_host_to_muc_host(HostType, LServer),
            Q = query(?NS_MUC_LIGHT_BLOCKING, [blocking_item(I) || I <- Items]),
            Iq = iq(jid:to_binary(User), MUCServer, <<"set">>, [Q]),
            ejabberd_router:route(User, jid:from_binary(MUCServer), Iq),
            {ok, "User blocking list updated successfully"};
        {error, not_found} ->
            ?MUC_SERVER_NOT_FOUND_RESULT
    end.

 %% Internal

-spec create_room_raw(jid:jid(), jid:jid(), map()) -> create_room_result().
create_room_raw(InRoomJID, CreatorJID, Options) ->
    Config = make_room_config(Options),
    try mod_muc_light:try_to_create_room(CreatorJID, InRoomJID, Config) of
        {ok, RoomJID, #create{aff_users = AffUsers, raw_config = Conf}} ->
            {ok, make_room(RoomJID, Conf, AffUsers)};
        {error, exists} ->
            {already_exist, "Room already exists"};
        {error, max_occupants_reached} ->
            {max_occupants_reached, "Max occupants number reached"};
        {error, {Key, Reason}} ->
            ?VALIDATION_ERROR_RESULT(Key, Reason)
    catch
        error:{muc_host_to_host_type_failed, _, _} ->
            ?MUC_SERVER_NOT_FOUND_RESULT
    end.

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

-spec is_user_room_member(mongooseim:host_type(), jid:simple_bare_jid(),
                          jid:simple_bare_jid()) -> boolean().
is_user_room_member(HostType, UserUS, {_, MUCServer} = RoomLUS) ->
    case mod_muc_light_db_backend:get_user_rooms(HostType, UserUS, MUCServer) of
        [] ->
            false;
        RoomJIDs when is_list(RoomJIDs) ->
            lists:any(fun(LUS) -> LUS =:= RoomLUS end, RoomJIDs)
    end.


make_room(JID, #config{ raw_config = Options}, AffUsers) ->
    make_room(JID, Options, AffUsers);
make_room(JID, Options, AffUsers) when is_list(Options) ->
    make_room(JID, maps:from_list(ensure_keys_are_binaries(Options)), AffUsers);
make_room(JID, Options, AffUsers) when is_map(Options) ->
    #{<<"roomname">> := Name, <<"subject">> := Subject} = Options,
    #{jid => JID, name => Name, subject => Subject, aff_users => AffUsers, options => Options}.

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
