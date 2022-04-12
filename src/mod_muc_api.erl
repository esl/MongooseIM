%% @doc Provide an interface for frontends (like graphql or ctl) to manage MUC rooms.
-module(mod_muc_api).

-export([get_rooms/4,
         get_room_config/1,
         get_room_config/2,
         get_room_messages/3,
         get_room_messages/4,
         modify_room_config/2,
         modify_room_config/3,
         get_room_users/1,
         get_room_users/2,
         get_room_affiliation/2,
         get_room_affiliation/3,
         create_instant_room/4,
         invite_to_room/4,
         send_message_to_room/3,
         send_private_message/4,
         kick_user_from_room/3,
         set_affiliation/3,
         set_affiliation/4,
         enter_room/3,
         exit_room/2,
         set_role/4,
         set_role/3,
         delete_room/2,
         delete_room/3]).

-ignore_xref([get_rooms/4, get_rooms/5, delete_room/3,
              get_room_config/2, get_room_users/2]).

-include("jlib.hrl").
-include("mongoose_rsm.hrl").
-include("mod_muc_room.hrl").

-type short_room_desc() :: #{jid := jid:jid(),
                             title := binary(),
                             private => boolean(),
                             users_number => non_neg_integer()}.

-type get_rooms_result() :: {[short_room_desc()], jlib:rsm_out()}.

-type user_map() ::
    #{jid := jid:jid() | null,
      role := mod_muc:role(),
      nick := mod_muc:nick()}.

-type room_conf_mod_fun() :: fun((mod_muc_room:config()) -> mod_muc_room:config()).

-type aff_item() :: {jid:simple_jid(), mod_muc:affiliation()}.

-export_type([user_map/0, short_room_desc/0]).

-define(ROOM_DELETED_SUCC_RESULT, {ok, "Room deleted successfully"}).
-define(USER_CANNOT_ACCESS_ROOM_RESULT,
        {not_allowed, "Given user does not have permission to read this room"}).
-define(ROOM_NOT_FOUND_RESULT, {room_not_found, "Room not found"}).
-define(DELETE_NONEXISTENT_ROOM_RESULT, {room_not_found, "Cannot remove non-existent room"}).
-define(USER_NOT_FOUND_RESULT, {user_not_found, "Given user not found"}).
-define(MUC_SERVER_NOT_FOUND_RESULT, {muc_server_not_found, "MUC server not found"}).

-spec get_rooms(jid:lserver(), jid:jid(), non_neg_integer() | undefined,
                non_neg_integer()) -> get_rooms_result().
get_rooms(MUCServer, From, Limit, Index) ->
    {Rooms, RSM} = mod_muc:get_vh_rooms(MUCServer, #rsm_in{max = Limit, index = Index}),
    Rooms2 = lists:filtermap(fun(R) -> room_to_item(R, MUCServer, From) end, Rooms),
    {Rooms2, RSM}.

-spec get_room_config(jid:jid(), jid:jid()) ->
    {ok, mod_muc_room:config()} | {not_allowed | room_not_found, iolist()}.
get_room_config(RoomJID, UserJID) ->
    case mod_muc:room_jid_to_pid(RoomJID) of
        {ok, Pid} ->
            case gen_fsm_compat:sync_send_all_state_event(Pid, {is_room_owner, UserJID}) of
                {ok, true} ->
                    {ok, Config} = gen_fsm_compat:sync_send_all_state_event(Pid, get_config),
                    {ok, Config};
                {ok, false} ->
                    {not_allowed, "Given user does not have permission to read config"}
            end;
        {error, not_found} ->
            ?ROOM_NOT_FOUND_RESULT
    end.

-spec get_room_config(jid:jid()) -> {ok, mod_muc_room:config()} | {room_not_found, iolist()}.
get_room_config(RoomJID) ->
    case mod_muc:room_jid_to_pid(RoomJID) of
        {ok, Pid} ->
            {ok, Config} = gen_fsm_compat:sync_send_all_state_event(Pid, get_config),
            {ok, Config};
        {error, not_found} ->
            ?ROOM_NOT_FOUND_RESULT
    end.


-spec create_instant_room(jid:lserver(), binary(), jid:jid(), binary()) ->
    {ok, short_room_desc()} | {internal | not_found, iolist()}.
create_instant_room(MUCDomain, Name, OwnerJID, Nick) ->
    %% Because these stanzas are sent on the owner's behalf through
    %% the HTTP API, they will certainly receive stanzas as a
    %% consequence, even if their client(s) did not initiate this.
    case ejabberd_auth:does_user_exist(OwnerJID) of
        true ->
            BareRoomJID = jid:make_bare(Name, MUCDomain),
            UserRoomJID = jid:make(Name, MUCDomain, Nick),
            %% Send presence to create a room.
            ejabberd_router:route(OwnerJID, UserRoomJID,
                                  presence(OwnerJID, UserRoomJID, undefined)),
            %% Send IQ set to unlock the room.
            ejabberd_router:route(OwnerJID, BareRoomJID,
                                  declination(OwnerJID, BareRoomJID)),
            case verify_room(BareRoomJID, OwnerJID) of
                ok ->
                    {ok, #{jid => BareRoomJID, title => Name, private => false, users_number => 0}};
                Error ->
                    Error
            end;
        false ->
            ?USER_NOT_FOUND_RESULT
    end.

-spec modify_room_config(jid:jid(), jid:jid(), room_conf_mod_fun()) ->
    {ok, mod_muc_room:config()} | {room_not_found, iolist()}.
modify_room_config(RoomJID, UserJID, Fun) ->
    case mod_muc:room_jid_to_pid(RoomJID) of
        {ok, Pid} ->
            case gen_fsm_compat:sync_send_all_state_event(Pid, {is_room_owner, UserJID}) of
                {ok, true} ->
                    modify_room_config_raw(Pid, Fun);
                {ok, false} ->
                    {not_allowed, "Given user does not have permission to change the config"}
            end;
        {error, not_found} ->
            ?ROOM_NOT_FOUND_RESULT
    end.

-spec modify_room_config(jid:jid(), room_conf_mod_fun()) ->
    {ok, mod_muc_room:config()} | {room_not_found, iolist()}.
modify_room_config(RoomJID, Fun) ->
    case mod_muc:room_jid_to_pid(RoomJID) of
        {ok, Pid} ->
            modify_room_config_raw(Pid, Fun);
        {error, not_found} ->
            ?ROOM_NOT_FOUND_RESULT
    end.

-spec invite_to_room(jid:jid(), jid:jid(), jid:jid(), binary()) -> {ok, iolist()}.
invite_to_room(RoomJID, SenderJID, RecipientJID, Reason) ->
    case verify_room(RoomJID, SenderJID) of
        ok ->
            Attrs = case get_room_config(RoomJID) of
                        {ok, #config{password_protected = true, password = Pass}} ->
                            [{<<"password">>, Pass}];
                        _ ->
                            []
                       end,
            %% Direct invitation: i.e. not mediated by MUC room. See XEP 0249.
            X = #xmlel{name = <<"x">>,
                       attrs = [{<<"xmlns">>, ?NS_CONFERENCE},
                                {<<"jid">>, jid:to_binary(RoomJID)},
                                {<<"reason">>, Reason} | Attrs]
            },
            Invite = message(SenderJID, RecipientJID, <<>>, [X]),
            ejabberd_router:route(SenderJID, RecipientJID, Invite),
            {ok, "Invitation sent successfully"};
        Error ->
            Error
    end.

-spec send_message_to_room(jid:jid(), jid:jid(), binary()) -> {ok, iolist()}.
send_message_to_room(RoomJID, SenderJID, Message) ->
    Body = #xmlel{name = <<"body">>,
                  children = [#xmlcdata{content = Message}]},
    Stanza = message(SenderJID, RoomJID, <<"groupchat">>, [Body]),
    ejabberd_router:route(SenderJID, RoomJID, Stanza),
    {ok, "Message sent successfully"}.

-spec send_private_message(jid:jid(), jid:jid(), binary(), binary()) -> {ok, iolist()}.
send_private_message(RoomJID, SenderJID, ToNick, Message) ->
    RoomJIDRes = jid:replace_resource(RoomJID, ToNick),
    Body = #xmlel{name = <<"body">>,
                  children = [#xmlcdata{content = Message}]},
    X = #xmlel{name = <<"x">>, attrs = [{<<"xmlns">>, ?NS_MUC}]},
    Stanza = message(SenderJID, RoomJID, <<"chat">>, [Body, X]),
    ejabberd_router:route(SenderJID, RoomJIDRes, Stanza),
    {ok, "Message sent successfully"}.

-spec kick_user_from_room(jid:jid(), binary(), binary()) -> {ok, iolist()}.
kick_user_from_room(RoomJID, Nick, ReasonIn) ->
    %% All the machinery which is already deeply embedded in the MUC
    %% modules will perform the neccessary checking.
    SenderJID = room_moderator(RoomJID),
    Reason = #xmlel{name = <<"reason">>,
                    children = [#xmlcdata{content = ReasonIn}]
                   },
    Item = #xmlel{name = <<"item">>,
                  attrs = [{<<"nick">>, Nick},
                           {<<"role">>, <<"none">>}],
                  children = [ Reason ]
                 },
    IQ = iq(<<"set">>, SenderJID, RoomJID, [ query(?NS_MUC_ADMIN, [ Item ]) ]),
    ejabberd_router:route(SenderJID, RoomJID, IQ),
    {ok, "Kick message sent successfully"}.

-spec delete_room(jid:jid(), binary()) -> {ok | room_not_found, iolist()}.
delete_room(RoomJID, Reason) ->
    case mod_muc:room_jid_to_pid(RoomJID) of
        {ok, Pid} ->
            gen_fsm_compat:send_all_state_event(Pid, {destroy, Reason}),
            ?ROOM_DELETED_SUCC_RESULT;
        {error, not_found} ->
            ?DELETE_NONEXISTENT_ROOM_RESULT
    end.

-spec delete_room(jid:jid(), jid:jid(), binary()) ->
    {ok | room_not_found, iolist()}.
delete_room(RoomJID, OwnerJID, Reason) ->
    case mod_muc:room_jid_to_pid(RoomJID) of
        {ok, Pid} ->
            case gen_fsm_compat:sync_send_all_state_event(Pid, {is_room_owner, OwnerJID}) of
                {ok, true} ->
                    gen_fsm_compat:send_all_state_event(Pid, {destroy, Reason}),
                    ?ROOM_DELETED_SUCC_RESULT;
                {ok, false} ->
                    {not_allowed, "Given user does not have permission to delete this room"}
            end;
        {error, not_found} ->
            ?DELETE_NONEXISTENT_ROOM_RESULT
    end.
-spec get_room_users(jid:jid()) -> {ok, [user_map()]} | {room_not_found, iolist()}.
get_room_users(RoomJID) ->
    case mod_muc_room:get_room_users(RoomJID) of
        {ok, Users} ->
            {ok, [to_user_map(U, true) || U <- Users]};
        {error, not_found} ->
           ?ROOM_NOT_FOUND_RESULT
    end.

-spec get_room_users(jid:jid(), jid:jid()) -> {ok, [user_map()]} | {room_not_found, iolist()}.
get_room_users(RoomJID, UserJID) ->
    case mod_muc:room_jid_to_pid(RoomJID) of
        {ok, Pid} ->
            case gen_fsm_compat:sync_send_all_state_event(Pid, {can_access_room, UserJID}) of
                {ok, true} ->
                    {ok, Users} = gen_fsm_compat:sync_send_all_state_event(Pid, get_room_users),
                    WithJID = can_access_identity(Pid, UserJID),
                    {ok, [to_user_map(U, WithJID) || U <- Users]};
                {ok, false} ->
                    ?USER_CANNOT_ACCESS_ROOM_RESULT
            end;
        {error, not_found} ->
            ?ROOM_NOT_FOUND_RESULT
    end.

-spec get_room_messages(jid:jid(), integer() | undefined,
                        mod_mam:unix_timestamp() | undefined) ->
    {ok, [mod_mam:message_row()]} | {muc_server_not_found | internal, iolist()}.
get_room_messages(RoomJID, PageSize, Before) ->
    case mongoose_domain_api:get_subdomain_host_type(RoomJID#jid.lserver) of
        {ok, HostType} ->
            mod_muc_light_api:get_room_messages(HostType, RoomJID, undefined, PageSize, Before);
        {error, not_found} ->
            ?MUC_SERVER_NOT_FOUND_RESULT
    end.

-spec get_room_messages(jid:jid(), jid:jid(), integer() | undefined,
                        mod_mam:unix_timestamp() | undefined) ->
    {ok, list()} | {muc_server_not_found | room_not_found | internal | not_room_member, iolist()}.
get_room_messages(RoomJID, UserJID, PageSize, Before) ->
    case mongoose_domain_api:get_subdomain_host_type(RoomJID#jid.lserver) of
        {ok, HostType} ->
            case mod_muc_room:can_access_room(RoomJID, UserJID) of
                {ok, true} ->
                    mod_muc_light_api:get_room_messages(HostType, RoomJID, UserJID,
                                                        PageSize, Before);
                {ok, false} ->
                    ?USER_CANNOT_ACCESS_ROOM_RESULT;
                {error, not_found} ->
                    ?ROOM_NOT_FOUND_RESULT
            end;
        {error, not_found} ->
            ?MUC_SERVER_NOT_FOUND_RESULT
    end.

-spec get_room_affiliation(jid:jid(), jid:jid(), mod_muc:affiliation()  | undefined) ->
    {ok, [aff_item()]} | {not_allowed | room_not_found, iolist()}.
get_room_affiliation(RoomJID, UserJID, AffType) ->
    case mod_muc_room:can_access_room(RoomJID, UserJID) of
        {ok, true} ->
            get_room_affiliation(RoomJID, AffType);
        {ok, false} ->
            ?USER_CANNOT_ACCESS_ROOM_RESULT;
        {error, not_found} ->
            ?ROOM_NOT_FOUND_RESULT
    end.

-spec get_room_affiliation(jid:jid(), mod_muc:affiliation()  | undefined) ->
    {ok, [aff_item()]} | {room_not_found, iolist()}.
get_room_affiliation(RoomJID, AffType) ->
    case room_users_aff(RoomJID) of
        {ok, Affiliations} ->
            Res = filter_affs_by_type(AffType, Affiliations),
            {ok, Res};
        {error, not_found} ->
            ?ROOM_NOT_FOUND_RESULT
    end.

-spec set_affiliation(jid:jid(), jid:jid(), mod_muc:affiliation()) ->
    {ok | room_not_found | not_allowed, iolist()}.
set_affiliation(RoomJID, UserJID, Affiliation) ->
    case room_users_aff(RoomJID) of
        {ok, Affs} ->
            {OwnerJID, owner} = hd(filter_affs_by_type(owner, Affs)),
                set_affiliation(RoomJID, OwnerJID, UserJID, Affiliation);
        {error, not_found} ->
            ?ROOM_NOT_FOUND_RESULT
    end.

-spec set_affiliation(jid:jid(), jid:jid(), jid:jid(), mod_muc:affiliation()) ->
    {ok | room_not_found | not_allowed, iolist()}.
set_affiliation(RoomJID, FromJID, UserJID, Affiliation) ->
    case mod_muc:room_jid_to_pid(RoomJID) of
        {ok, Pid} ->
            AffItem = affiliation_item(UserJID, Affiliation),
            case gen_fsm_compat:sync_send_event(Pid, {set_admin_items, FromJID, [AffItem]}) of
                ok ->
                    {ok, "Affiliation set successfully"};
                {error, Error} ->
                    format_xml_error(Error, Affiliation, <<"affiliation">>)
            end;
        {error, not_found} ->
            ?ROOM_NOT_FOUND_RESULT
    end.

-spec set_role(jid:jid(), binary(), mod_muc:role()) ->
    {ok | room_not_found | not_allowed, iolist()}.
set_role(RoomJID, Nick, Role) ->
    case mod_muc:room_jid_to_pid(RoomJID) of
        {ok, Pid} ->
            ModJID = room_moderator(RoomJID),
            set_role(Pid, ModJID, Nick, Role);
        {error, not_found} ->
            ?ROOM_NOT_FOUND_RESULT
    end.

-spec set_role(jid:jid() | pid(), jid:jid(), binary(), mod_muc:role()) ->
    {ok | room_not_found | not_allowed, iolist()}.
set_role(#jid{} = RoomJID, ModJID, Nick, Role) ->
    case mod_muc:room_jid_to_pid(RoomJID) of
        {ok, Pid} ->
            set_role(Pid, ModJID, Nick, Role);
        {error, not_found} ->
            ?ROOM_NOT_FOUND_RESULT
    end;
set_role(Pid, ModJID, Nick, Role) when is_pid(Pid) ->
    RoleItem = role_item(Nick, Role),
    ModJIDRes = try_add_role_res(Pid, ModJID, moderator),
    case gen_fsm_compat:sync_send_event(Pid, {set_admin_items, ModJIDRes, [RoleItem]}) of
        ok ->
            {ok, "Role set successfully"};
        {error, Error} ->
            format_xml_error(Error, Role, <<"role">>)
    end.

-spec enter_room(jid:jid(), jid:jid(), binary() | undefined) -> {ok, iolist()}.
enter_room(RoomJID, UserJID, Password) ->
    Presence = presence(UserJID, RoomJID, Password),
    ejabberd_router:route(UserJID, RoomJID, Presence),
    {ok, "Entering room message sent successfully"}.

-spec exit_room(jid:jid(), jid:jid()) -> {ok, iolist()}.
exit_room(RoomJID, UserJID) ->
    Presence = exit_room_presence(UserJID, RoomJID),
    ejabberd_router:route(UserJID, RoomJID, Presence),
    {ok, "Exiting room message sent successfully"}.

%% Internal

-spec try_add_role_res(jid:jid() | pid(), jid:jid(), mod_muc:role()) -> jid:jid().
try_add_role_res(Room, InUserJID, Role) ->
    Res = [UserJID || #user{jid = UserJID, role = Role2} <- room_users(Room),
                      jid:are_bare_equal(InUserJID, UserJID), Role =:= Role2],
    case Res of
        [UserJID | _] ->
            UserJID;
        _ ->
            InUserJID
    end.

filter_affs_by_type(undefined, Affs) -> Affs;
filter_affs_by_type(Type, Affs) -> [Aff || Aff = {_, T} <- Affs, T =:= Type].

format_xml_error(#xmlel{name = <<"error">>}, Aff, Op) ->
    Msg = io_lib:format("Given user does not have permission to set the ~p ~s", [Aff, Op]),
    {not_allowed, Msg}.

-spec can_access_identity(pid(), jid:jid()) -> boolean().
can_access_identity(Pid, UserJID) ->
    {ok, WithJID} = gen_fsm_compat:sync_send_all_state_event(Pid, {can_access_identity, UserJID}),
    WithJID.

-spec modify_room_config_raw(pid(), room_conf_mod_fun()) -> {ok, mod_muc_room:config()}.
modify_room_config_raw(Pid, Fun) ->
    {ok, Config} = gen_fsm_compat:sync_send_all_state_event(Pid, get_config),
    NewConfig = Fun(Config),
    {ok, NewConfig2} =
        gen_fsm_compat:sync_send_all_state_event(Pid, {change_config, NewConfig}),
    {ok, NewConfig2}.

-spec to_user_map(mod_muc_room:user(), boolean()) -> user_map().
to_user_map(#user{role = Role, nick = Nick}, false = _WithJID) ->
    #{jid => null, role => Role, nick => Nick};
to_user_map(#user{jid = JID, role = Role, nick = Nick}, true) ->
    #{jid => JID, role => Role, nick => Nick}.

-spec room_to_item(tuple(), jid:lserver(), jid:jid()) -> {true, short_room_desc()} | false.
room_to_item({{Name, _}, Pid}, MUCServer, From) ->
     case catch gen_fsm_compat:sync_send_all_state_event(
                  Pid, {get_disco_item, From, <<"en">>}, 100) of
         {item, Desc} ->
             Map = room_desc_to_map(Desc),
             {true, Map#{jid => jid:to_binary({Name, MUCServer, <<>>})}};
         _ ->
             false
     end.

-spec room_desc_to_map(binary()) -> #{title := binary(), private => boolean(),
                                      users_number => non_neg_integer()}.
room_desc_to_map(Desc) ->
    MP = "(\\S+)( \\((private, )?(\\d+)\\))?",
    {match, [TitlePos, _, PrivatePos, NumberPos]} = re:run(Desc, MP, [{capture, [1, 2, 3, 4]}]),
    Title = binary:part(Desc, TitlePos),
    case NumberPos of
        {-1, 0} ->
            #{title => Title};
        _ ->
            Private = {-1, 0} =/= PrivatePos,
            Number = binary_to_integer(binary:part(Desc, NumberPos)),
            #{title => Title, private => Private, users_number => Number}
    end.

-spec verify_room(jid:jid(), jid:jid()) -> ok | {internal | not_found, term()}.
verify_room(BareRoomJID, OwnerJID) ->
    case mod_muc_room:can_access_room(BareRoomJID, OwnerJID) of
        {ok, true} ->
            ok;
        {ok, false} ->
            {internal, "Room is locked"};
        {error, not_found} ->
            ?ROOM_NOT_FOUND_RESULT
    end.

role_item(Nick, Role) ->
    #xmlel{name = <<"item">>, attrs = [{<<"nick">>, Nick}, {<<"role">>, atom_to_binary(Role)}]}.

affiliation_item(JID, Aff) ->
    #xmlel{name = <<"item">>, attrs = [{<<"jid">>, jid:to_binary(JID)},
                                       {<<"affiliation">>, atom_to_binary(Aff)}]}.

iq(Type, Sender, Recipient, Children) when is_binary(Type), is_list(Children) ->
    Addresses = address_attributes(Sender, Recipient),
    #xmlel{name = <<"iq">>,
           attrs = Addresses ++ [{<<"type">>, Type}],
           children = Children
          }.

message(Sender, Recipient, Type, Contents) when is_binary(Type), is_list(Contents) ->
    Addresses = address_attributes(Sender, Recipient),
    Attributes = case Type of
                     <<>> -> Addresses;
                     _ -> [{<<"type">>, Type} | Addresses]
                 end,
    #xmlel{name = <<"message">>,
           attrs = Attributes,
           children = Contents}.

query(XMLNameSpace, Children)
  when is_binary(XMLNameSpace), is_list(Children) ->
    #xmlel{name = <<"query">>,
           attrs = [{<<"xmlns">>, XMLNameSpace}],
           children = Children}.

presence(Sender, Recipient, Password) ->
    Children = case Password of
                    undefined -> [];
                    _ -> [#xmlel{name = <<"password">>,
                                 children = [#xmlcdata{content = Password}]}]
                end,
    #xmlel{name = <<"presence">>,
           attrs = address_attributes(Sender, Recipient),
           children = [#xmlel{name = <<"x">>,
                              attrs = [{<<"xmlns">>, ?NS_MUC}],
                              children = Children}]}.

exit_room_presence(Sender, Recipient) ->
    #xmlel{name = <<"presence">>,
           attrs = [{<<"type">>, <<"unavailable">>} | address_attributes(Sender, Recipient)],
           children = [#xmlel{name = <<"x">>, attrs = [{<<"xmlns">>, ?NS_MUC}]}]}.

declination(Sender, Recipient) ->
    iq(<<"set">>, Sender, Recipient, [data_submission()]).

data_submission() ->
    query(?NS_MUC_OWNER, [#xmlel{name = <<"x">>,
                                 attrs = [{<<"xmlns">>, ?NS_XDATA},
                                          {<<"type">>, <<"submit">>}]}]).

address_attributes(Sender, Recipient) ->
    [{<<"from">>, jid:to_binary(jid:to_lower(Sender))},
     {<<"to">>, jid:to_binary(jid:to_lower(Recipient))}].

room_moderator(RoomJID) ->
    [JIDStruct|_] =
        [UserJID || #user{jid = UserJID,
                          role = moderator} <- room_users(RoomJID)],
    JIDStruct.

room_users(#jid{} = RoomJID) ->
    {ok, Affiliations} = mod_muc_room:get_room_users(RoomJID),
    Affiliations;
room_users(RoomPID) when is_pid(RoomPID)->
    {ok, Affiliations} = gen_fsm_compat:sync_send_all_state_event(RoomPID, get_room_users),
    Affiliations.

room_users_aff(RoomJID) ->
    case mod_muc_room:get_room_affiliations(RoomJID) of
        {ok, Affs} ->
            {ok, format_affs(Affs)};
        {error, Error} ->
            {error, Error}
    end.

format_affs(AffsMap) ->
    lists:map(fun
                  ({K, {V, <<>>}}) -> {K, V};
                  ({K, V}) -> {K, V}
              end, maps:to_list(AffsMap)).
