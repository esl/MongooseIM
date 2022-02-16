%% @doc Provide an interface for frontends (like graphql or ctl) to manage MUC Light rooms.
-module(mod_muc_light_api).

-export([create_room/5,
         invite_to_room/4,
         change_room_config/5,
         change_affiliation/5,
         remove_user_from_room/4,
         send_message/4,
         delete_room/3,
         delete_room/2,
         get_room_messages/4,
         get_user_rooms/1,
         get_room_info/2,
         get_room_aff/2
        ]).

-include("mod_muc_light.hrl").
-include("mongoose.hrl").
-include("jlib.hrl").
-include("mongoose_rsm.hrl").

-type create_room_result() :: {ok, room()} | {exist |
                                              max_occupants_reached |
                                              validation_error, iolist()}.

-type change_room_config_result() :: {ok, room()} | {wrong_user |
                                                     not_allowed |
                                                     not_exists |
                                                     validation_error, iolist()}.

-type get_room_messages_result() :: {ok, []} | {domain_not_found | internal, iolist()}.

-type invite_to_room_result() :: {ok | user_without_room | not_found, iolist()}.

-type get_room_info_result() :: {ok, map()} | {domain_not_found | not_exists, iolist()}.

-type get_room_aff_result() :: {ok, [aff_user()]} | {domain_not_found | not_exists, iolist()}.

-type room() :: #{jid := jid:jid(),
                 name := binary(),
                 subject := binary(),
                 aff_users := aff_users()
                }.

-export_type([room/0, create_room_result/0]).

-define(ROOM_NOT_EXIST_RESULT, {not_exists, "Room does not exist"}).
-define(VALIDATION_ERROR_RESULT(Key, Reason),
        {validation_error, io_lib:format("Validation failed for key: ~p with reason ~p",
                                         [Key, Reason])}).
-define(USER_WITHOUT_ROOM_RESULT, {user_without_room, "Given user does not occupy any room"}).

-spec create_room(jid:lserver(), binary(), binary(), jid:jid(), binary()) -> create_room_result().
create_room(Domain, RoomId, RoomTitle, CreatorJID, Subject) ->
    case get_muc_hosts(Domain) of
        {ok, _HostType, MUCLightDomain} ->
            MUCServiceJID = jid:make_bare(RoomId, MUCLightDomain),
            Config = make_room_config(RoomTitle, Subject),
            case mod_muc_light:try_to_create_room(CreatorJID, MUCServiceJID, Config) of
                {ok, RoomJID, #create{aff_users = AffUsers}} ->
                    {ok, make_room(RoomJID, RoomTitle, Subject, AffUsers)};
                {error, exists} ->
                    {exist, "Room already exists"};
                {error, max_occupants_reached} ->
                    {max_occupants_reached, "Max occupants number reached"};
                {error, {Key, Reason}} ->
                    ?VALIDATION_ERROR_RESULT(Key, Reason)
            end;
        Error ->
            Error
    end.

-spec invite_to_room(jid:lserver(), binary(), jid:jid(), jid:jid()) -> invite_to_room_result().
invite_to_room(Domain, RoomName, SenderJID, RecipientJID) ->
    case get_muc_hosts(Domain) of
        {ok, HostType, MUCServer} ->
            RecipientBin = jid:to_binary(jid:to_bare(RecipientJID)),
            case muc_light_room_name_to_jid_and_aff(HostType, SenderJID, RoomName, MUCServer) of
                {ok, R, _Aff} ->
                    S = jid:to_bare(SenderJID),
                    Changes = query(?NS_MUC_LIGHT_AFFILIATIONS,
                                    [affiliate(RecipientBin, <<"member">>)]),
                    ejabberd_router:route(S, R, iq(jid:to_binary(S), jid:to_binary(R),
                                                   <<"set">>, [Changes])),
                    {ok, "User invited successfully"};
                {error, given_user_does_not_occupy_any_room} ->
                    ?USER_WITHOUT_ROOM_RESULT;
                {error, not_exists} ->
                    ?ROOM_NOT_EXIST_RESULT
            end;
        Error ->
            Error
    end.

-spec change_room_config(jid:lserver(), binary(), binary(), jid:jid(), binary()) ->
    change_room_config_result().
change_room_config(Domain, RoomID, RoomName, UserJID, Subject) ->
    case get_muc_hosts(Domain) of
        {ok, HostType, MUCLightDomain} ->
            LServer = jid:nameprep(Domain),
            UserUS = jid:to_bare(UserJID),
            ConfigReq = #config{ raw_config =
                                 [{<<"roomname">>, RoomName}, {<<"subject">>, Subject}]},
            Acc = mongoose_acc:new(#{location => ?LOCATION, lserver => LServer,
                                     host_type => HostType}),
            case mod_muc_light:change_room_config(UserUS, RoomID, MUCLightDomain, ConfigReq, Acc) of
                {ok, RoomJID, _}  ->
                    {ok, make_room(RoomJID, RoomName, Subject, [])};
                {error, item_not_found} ->
                    {wrong_user, "The given user is not room participant"};
                {error, not_allowed} ->
                    {not_allowed, "The given user has not permission to change config"};
                {error, not_exists} ->
                    ?ROOM_NOT_EXIST_RESULT;
                {error, {error, {Key, Reason}}} ->
                    ?VALIDATION_ERROR_RESULT(Key, Reason)
            end;
        Error ->
            Error
    end.

-spec change_affiliation(jid:lserver(), binary(), jid:jid(), jid:jid(), binary()) ->
    ok | {domain_not_found, iolist()}.
change_affiliation(Domain, RoomID, SenderJID, RecipientJID, Affiliation) ->
    case get_muc_hosts(Domain) of
        {ok, _HostType, MUCLightDomain} ->
            RecipientBare = jid:to_bare(RecipientJID),
            R = jid:make_bare(RoomID, MUCLightDomain),
            S = jid:to_bare(SenderJID),
            Changes = query(?NS_MUC_LIGHT_AFFILIATIONS,
                            [affiliate(jid:to_binary(RecipientBare), Affiliation)]),
            ejabberd_router:route(S, R, iq(jid:to_binary(S), jid:to_binary(R),
                                           <<"set">>, [Changes])),
            ok;
        Error ->
            Error
    end.

-spec remove_user_from_room(jid:lserver(), binary(), jid:jid(), jid:jid()) ->
    {ok | domain_not_found, iolist()}.
remove_user_from_room(Domain, RoomID, SenderJID, RecipientJID) ->
    case change_affiliation(Domain, RoomID, SenderJID, RecipientJID, <<"none">>) of
        ok ->
            {ok, io_lib:format("User ~s kicked successfully", [jid:to_binary(RecipientJID)])};
        Error ->
            Error
    end.

-spec send_message(jid:lserver(), binary(), jid:jid(), binary()) ->
    {ok | domain_not_found | room_not_found | user_without_room, iolist()}.
send_message(Domain, RoomName, SenderJID, Message) ->
    case get_muc_hosts(Domain) of
        {ok, HostType, MUCServer} ->
            Body = #xmlel{name = <<"body">>,
                          children = [ #xmlcdata{ content = Message } ]
                         },
            Stanza = #xmlel{name = <<"message">>,
                            attrs = [{<<"type">>, <<"groupchat">>}],
                            children = [ Body ]
                           },
            SenderBare = jid:to_bare(SenderJID),
            SenderUS = jid:to_lus(SenderBare),
            case mod_muc_light_db_backend:get_user_rooms(HostType, SenderUS, MUCServer) of
                [] ->
                    ?USER_WITHOUT_ROOM_RESULT;
                RoomJIDs when is_list(RoomJIDs) ->
                    FindFun = find_room_and_user_aff_by_room_name(HostType, RoomName, SenderUS),
                    case lists:foldl(FindFun, none, RoomJIDs) of
                        {ok, {RU, MUCServer}, _Aff} ->
                            R = jid:make_bare(RU, MUCServer),
                            ejabberd_router:route(SenderBare, R, Stanza),
                            {ok, "Message send successfully"};
                        none ->
                            {room_not_found, "Room does not found"}
                    end
            end;
        Error ->
            Error
    end.

-spec delete_room(jid:lserver(), binary(), jid:jid()) ->
    { ok | domain_not_found | not_exists | user_without_room, iolist()}.
delete_room(Domain, RoomName, OwnerJID) ->
    OwnerBare = jid:to_bare(OwnerJID),
    case get_muc_hosts(Domain) of
        {ok, HostType, MUCServer} ->
            Res = case muc_light_room_name_to_jid_and_aff(HostType, OwnerBare,
                                                          RoomName, MUCServer) of
                      {ok, RoomJID, owner} ->
                          mod_muc_light:delete_room(jid:to_lus(RoomJID));
                      {ok, _, _} ->
                          {error, not_allowed};
                      {error, _} = Err ->
                          Err
                  end,
            format_delete_error_message(Res);
        Error ->
            Error
    end.

-spec delete_room(jid:lserver(), binary()) -> { ok | domain_not_found | not_exists, iolist()}.
delete_room(Domain, RoomID) ->
    case get_muc_hosts(Domain) of
        {ok, _HostType, MUCLightDomain} ->
            Res = mod_muc_light:delete_room({RoomID, MUCLightDomain}),
            format_delete_error_message(Res);
        Error ->
            Error
    end.

-spec get_room_messages(jid:lserver(), binary(), integer() | undefined,
                        mod_mam:unix_timestamp() | undefined) -> get_room_messages_result().
get_room_messages(Domain, RoomID, PageSize, Before) ->
    case get_muc_hosts(Domain) of
        {ok, HostType, MUCLightDomain} ->
            RoomJID = jid:make_bare(RoomID, MUCLightDomain),
            Now = os:system_time(microsecond),
            ArchiveID = mod_mam_muc:archive_id_int(HostType, RoomJID),
            End = maybe_before(Before, Now),
            RSM = #rsm_in{direction = before, id = undefined},
            R = mod_mam_muc:lookup_messages(HostType,
                                            #{archive_id => ArchiveID,
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
                                              is_simple => true}),
            case R of
                {ok, {_, _, Messages}} ->
                    {ok, Messages};
                {error, Term} ->
                    {internal, io_lib:format("Internal error occured ~p", [Term])}
            end;
        Error ->
            Error
    end.

-spec get_room_info(jid:lserver(), binary()) -> get_room_info_result().
get_room_info(Domain, RoomID) ->
    case get_muc_hosts(Domain) of
        {ok, HostType, MUCServer} ->
            case mod_muc_light_db_backend:get_info(HostType, {RoomID, MUCServer}) of
                {ok, [{roomname, Name}, {subject, Subject}], AffUsers, _Version} ->
                    {ok, make_room(jid:make_bare(RoomID, MUCServer), Name, Subject, AffUsers)};
                {error, not_exists} ->
                    ?ROOM_NOT_EXIST_RESULT
            end;
        Error ->
            Error
    end.

-spec get_room_aff(jid:lserver(), binary()) -> get_room_aff_result().
get_room_aff(Domain, RoomID) ->
    case get_room_info(Domain, RoomID) of
        {ok, #{aff_users := AffUsers}} ->
            {ok, AffUsers};
        Error ->
            Error
    end.

-spec get_user_rooms(jid:jid()) -> {ok, [RoomUS :: jid:simple_bare_jid()]} |
                                   {domain_not_found, iolist()}.
get_user_rooms(#jid{lserver = LServer} = UserJID) ->
    case get_muc_hosts(LServer) of
        {ok, HostType, MUCServer} ->
            UserUS = jid:to_lus(UserJID),
            {ok, mod_muc_light_db_backend:get_user_rooms(HostType, UserUS, MUCServer)};
        Error ->
            Error
    end.

 %% Internal

-spec get_muc_hosts(jid:lserver()) -> {ok, mongooseim:host_type(), jid:lserver()} |
                                      {domain_not_found, iolist()}.
get_muc_hosts(LServer) ->
    case mongoose_domain_api:get_domain_host_type(LServer) of
        {ok, HostType} ->
            {ok, HostType, mod_muc_light_utils:server_host_to_muc_host(HostType, LServer)};
        {error, not_found} ->
            {domain_not_found, io_lib:format("Domain ~s does not exist", [LServer])}
    end.

make_room(JID, Name, Subject, AffUsers) ->
    #{jid => JID, name => Name, subject => Subject, aff_users => AffUsers}.

format_delete_error_message(ok) ->
    {ok, "Room deleted successfully!"};
format_delete_error_message({error, not_allowed}) ->
    {not_allowed, "You cannot delete this room"};
format_delete_error_message({error, not_exists}) ->
    {not_exists, "Cannot remove not existing room"};
format_delete_error_message({error, given_user_does_not_occupy_any_room}) ->
    ?USER_WITHOUT_ROOM_RESULT.

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

-spec make_room_config(binary(), binary()) -> create_req_props().
make_room_config(Name, Subject) ->
    #create{raw_config = [{<<"roomname">>, Name},
                          {<<"subject">>, Subject}]
           }.

-spec muc_light_room_name_to_jid_and_aff(HostType :: mongooseim:host_type(),
                                         UserJID :: jid:jid(),
                                         RoomName :: binary(),
                                         Domain :: jid:lserver()) ->
    {ok, jid:jid(), aff()} | {error, given_user_does_not_occupy_any_room} | {error, not_exists}.
muc_light_room_name_to_jid_and_aff(HostType, UserJID, RoomName, MUCServer) ->
    UserUS = jid:to_lus(UserJID),
    case mod_muc_light_db_backend:get_user_rooms(HostType, UserUS, MUCServer) of
        [] ->
            {error, given_user_does_not_occupy_any_room};
        RoomUSs when is_list(RoomUSs) ->
            FindFun = find_room_and_user_aff_by_room_name(HostType, RoomName, UserUS),
            case lists:foldl(FindFun, none, RoomUSs) of
                {ok, {RU, MUCServer}, UserAff} ->
                    {ok, jid:make_bare(RU, MUCServer), UserAff};
                none ->
                    {error, not_exists}
            end
    end.

-spec get_room_name_and_user_aff(mongooseim:host_type(), RoomUS :: jid:simple_bare_jid(),
                                 UserUS :: jid:simple_bare_jid()) ->
    {ok, RoomName :: binary(), UserAff :: aff()} | {error, not_exists}.
get_room_name_and_user_aff(HostType, RoomUS, UserUS) ->
    case mod_muc_light_db_backend:get_info(HostType, RoomUS) of
        {ok, Cfg, Affs, _} ->
            {roomname, RoomName} = lists:keyfind(roomname, 1, Cfg),
            {_, UserAff} = lists:keyfind(UserUS, 1, Affs),
            {ok, RoomName, UserAff};
        Error ->
            Error
    end.

-type find_room_acc() :: {ok, RoomUS :: jid:simple_bare_jid(), UserAff :: aff()} | none.

-spec find_room_and_user_aff_by_room_name(mongooseim:host_type(), RoomName :: binary(),
                                          UserUS :: jid:simple_bare_jid()) ->
    fun((RoomUS :: jid:simple_bare_jid(), find_room_acc()) -> find_room_acc()).
find_room_and_user_aff_by_room_name(HostType, RoomName, UserUS) ->
    fun (RoomUS, none) ->
            case get_room_name_and_user_aff(HostType, RoomUS, UserUS) of
                {ok, RoomName, UserAff} ->
                    {ok, RoomUS, UserAff};
                _ ->
                    none
            end;
        (_, Acc) when Acc =/= none ->
            Acc
    end.

maybe_before(undefined, Now) ->
    Now;
maybe_before(Timestamp, _) ->
   Timestamp.
