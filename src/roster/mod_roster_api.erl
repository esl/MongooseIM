%% @doc Provide an interface for frontends (like graphql or ctl) to manage roster.
-module(mod_roster_api).

-export([list_contacts/1,
         get_contact/2,
         add_contact/4,
         delete_contact/2,
         subscription/3,
         subscribe_both/2,
         set_mutual_subscription/3]).

-include("mongoose.hrl").
-include("jlib.hrl").
-include("mod_roster.hrl").

-type sub_mutual_action() :: connect | disconnect.

-define(UNKNOWN_DOMAIN_RESULT, {unknown_domain, "Domain not found"}).
-define(INTERNAL_ERROR_RESULT(Error, Operation),
        {internal, io_lib:format("Cannot ~p a contact because: ~p", [Operation, Error])}).

-spec add_contact(jid:jid(), jid:jid(), binary(), [binary()]) ->
    {ok | internal | user_not_exist | unknown_domain, iolist()}.
add_contact(#jid{lserver = LServer} = CallerJID, ContactJID, Name, Groups) ->
    case mongoose_domain_api:get_domain_host_type(LServer) of
        {ok, HostType} ->
            case does_users_exist(CallerJID, ContactJID) of
                ok ->
                    case mod_roster:set_roster_entry(HostType, CallerJID, ContactJID,
                                                     Name, Groups) of
                        ok ->
                            {ok, "Contact added successfully"};
                        {error, Error} ->
                            ?INTERNAL_ERROR_RESULT(Error, create)
                    end;
                Error ->
                    Error
            end;
        {error, not_found} ->
            ?UNKNOWN_DOMAIN_RESULT
    end.

-spec list_contacts(jid:jid()) -> {ok, [mod_roster:roster()]} | {unknown_domain, iolist()}.
list_contacts(#jid{lserver = LServer} = CallerJID) ->
    case mongoose_domain_api:get_domain_host_type(LServer) of
        {ok, HostType} ->
            case ejabberd_auth:does_user_exist(CallerJID) of
                true ->
                    Acc0 = mongoose_acc:new(#{ location => ?LOCATION,
                                               host_type => HostType,
                                               lserver => LServer,
                                               element => undefined }),
                    Roster = mongoose_hooks:roster_get(Acc0, CallerJID, true),
                    {ok, Roster};
                false ->
                    {user_not_exist, io_lib:format("The user ~s does not exist",
                                                   [jid:to_binary(CallerJID)])}
            end;
        {error, not_found} ->
            ?UNKNOWN_DOMAIN_RESULT
    end.

-spec get_contact(jid:jid(), jid:jid()) ->
    {ok, mod_roster:roster()} |
    {contact_not_found | internal | unknown_domain | user_not_exist, iolist()}.
get_contact(#jid{lserver = LServer} = UserJID, ContactJID) ->
    case mongoose_domain_api:get_domain_host_type(LServer) of
        {ok, HostType} ->
            ContactL = jid:to_lower(ContactJID),
            case mod_roster:get_roster_entry(HostType, UserJID, ContactL, full) of
                #roster{} = R ->
                    {ok, R};
                does_not_exist ->
                    {contact_not_found, "Given contact does not exist"};
                error ->
                    ?INTERNAL_ERROR_RESULT(error, get)
            end;
        {error, not_found} ->
            ?UNKNOWN_DOMAIN_RESULT
    end.

-spec delete_contact(jid:jid(), jid:jid()) ->
    {ok | contact_not_found | internal | unknown_domain, iolist()}.
delete_contact(#jid{lserver = LServer} = CallerJID, ContactJID) ->
    case mongoose_domain_api:get_domain_host_type(LServer) of
        {ok, HostType} ->
            case mod_roster:remove_from_roster(HostType, CallerJID, ContactJID) of
                ok ->
                    {ok, io_lib:format("Contact ~s deleted successfully",
                                       [jid:to_binary(ContactJID)])};
                {error, does_not_exist} ->
                    ErrMsg = io_lib:format("Cannot remove ~s contact that does not exist",
                                           [jid:to_binary(ContactJID)]),
                    {contact_not_found, ErrMsg};
                {error, Error} ->
                    ?INTERNAL_ERROR_RESULT(Error, delete)
            end;
        {error, not_found} ->
            ?UNKNOWN_DOMAIN_RESULT
    end.

-spec subscription(jid:jid(), jid:jid(), mod_roster:sub_presence()) ->
    {ok | unknown_domain, iolist()}.
subscription(#jid{lserver = LServer} = CallerJID, ContactJID, Type) ->
    StanzaType = atom_to_binary(Type, latin1),
    El = #xmlel{name = <<"presence">>, attrs = [{<<"type">>, StanzaType}]},
    case mongoose_domain_api:get_domain_host_type(LServer) of
        {ok, HostType} ->
            Acc1 = mongoose_acc:new(#{ location => ?LOCATION,
                                       from_jid => CallerJID,
                                       to_jid => ContactJID,
                                       host_type => HostType,
                                       lserver => LServer,
                                       element => El }),
            Acc2 = mongoose_hooks:roster_out_subscription(Acc1, CallerJID, ContactJID, Type),
            ejabberd_router:route(CallerJID, jid:to_bare(ContactJID), Acc2),
            {ok, io_lib:format("Subscription stanza with type ~s sent successfully", [StanzaType])};
        {error, not_found} ->
            ?UNKNOWN_DOMAIN_RESULT
    end.

-spec set_mutual_subscription(jid:jid(), jid:jid(), sub_mutual_action()) ->
          {ok | contact_not_found | internal | unknown_domain | user_not_exist, iolist()}.
set_mutual_subscription(UserA, UserB, connect) ->
    subscribe_both({UserA, <<>>, []}, {UserB, <<>>, []});
set_mutual_subscription(UserA, UserB, disconnect) ->
    Seq = [fun() -> delete_contact(UserA, UserB) end,
           fun() -> delete_contact(UserB, UserA) end],
    case run_seq(Seq, ok) of
        ok ->
            {ok, "Mutual subscription removed successfully"};
        Error ->
            Error
    end.

-spec subscribe_both({jid:jid(), binary(), [binary()]}, {jid:jid(), binary(), [binary()]}) ->
    {ok | internal | unknown_domain | user_not_exist, iolist()}.
subscribe_both({UserA, NameA, GroupsA}, {UserB, NameB, GroupsB}) ->
    Seq = [fun() -> add_contact(UserA, UserB, NameB, GroupsB) end,
           fun() -> add_contact(UserB, UserA, NameA, GroupsA) end,
           fun() -> subscription(UserA, UserB, subscribe) end,
           fun() -> subscription(UserB, UserA, subscribe) end,
           fun() -> subscription(UserA, UserB, subscribed) end,
           fun() -> subscription(UserB, UserA, subscribed) end],
    case run_seq(Seq, ok) of
        ok ->
            {ok, io_lib:format("Subscription between users ~s and ~s created successfully",
                               [jid:to_binary(UserA), jid:to_binary(UserB)])};
        Error ->
            Error
    end.

%% Internal

-spec does_users_exist(jid:jid(), jid:jid()) -> ok | {user_not_exist, iolist()}.
does_users_exist(User, Contact) ->
    case lists:filter(fun(U) -> not ejabberd_auth:does_user_exist(U) end, [User, Contact]) of
        [] ->
            ok;
        [NotExist | _]->
            {user_not_exist, io_lib:format("The user ~s does not exist",
                                           [jid:to_binary(NotExist)])}
    end.

-spec run_seq([fun(() -> any())], term()) -> ok | {atom(), iolist()}.
run_seq([Cmd | Seq], ok) ->
    run_seq(Seq, Cmd());
run_seq([Cmd | Seq], {ok, _}) ->
    run_seq(Seq, Cmd());
run_seq([], _) -> ok;
run_seq(_, {_, _} = Error) ->
    Error.
