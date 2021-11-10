-module(mod_commands).
-author('bartlomiej.gorny@erlang-solutions.com').

-behaviour(gen_mod).
-behaviour(mongoose_module_metrics).

-export([start/0, stop/0, supported_features/0,
         start/2, stop/1,
         register/3,
         unregister/2,
         registered_commands/0,
         registered_users/1,
         change_user_password/3,
         list_sessions/1,
         list_contacts/1,
         add_contact/2,
         add_contact/3,
         add_contact/4,
         delete_contacts/2,
         delete_contact/2,
         subscription/3,
         set_subscription/3,
         kick_session/3,
         get_recent_messages/3,
         get_recent_messages/4,
         send_message/3,
         send_stanza/1
        ]).

-export([parse_from_to/2]).

-ignore_xref([add_contact/2, add_contact/3, add_contact/4, change_user_password/3,
              delete_contact/2, delete_contacts/2, get_recent_messages/3,
              get_recent_messages/4, kick_session/3, list_contacts/1,
              list_sessions/1, register/3, registered_commands/0, registered_users/1,
              send_message/3, send_stanza/1, set_subscription/3, start/0, stop/0,
              subscription/3, unregister/2]).

-include("mongoose.hrl").
-include("jlib.hrl").
-include("mongoose_rsm.hrl").
-include("session.hrl").

start() ->
    mongoose_commands:register(commands()).

stop() ->
    mongoose_commands:unregister(commands()).

start(_, _) -> start().
stop(_) -> stop().

-spec supported_features() -> [atom()].
supported_features() ->
    [dynamic_domains].

%%%
%%% mongoose commands
%%%

commands() ->
    [
     [
      {name, list_methods},
      {category, <<"commands">>},
      {desc, <<"List commands">>},
      {module, ?MODULE},
      {function, registered_commands},
      {action, read},
      {args, []},
      {result, []}
     ],
     [
      {name, list_users},
      {category, <<"users">>},
      {desc, <<"List registered users on this host">>},
      {module, ?MODULE},
      {function, registered_users},
      {action, read},
      {args, [{host, binary}]},
      {result, []}
     ],
     [
      {name, register_user},
      {category, <<"users">>},
      {desc, <<"Register a user">>},
      {module, ?MODULE},
      {function, register},
      {action, create},
      {args, [{host, binary}, {username, binary}, {password, binary}]},
      {result, {msg, binary}}
     ],
     [
      {name, unregister_user},
      {category, <<"users">>},
      {desc, <<"UnRegister a user">>},
      {module, ?MODULE},
      {function, unregister},
      {action, delete},
      {args, [{host, binary}, {user, binary}]},
      {result, {msg, binary}}
     ],
     [
      {name, list_sessions},
      {category, <<"sessions">>},
      {desc, <<"Get session list">>},
      {module, ?MODULE},
      {function, list_sessions},
      {action, read},
      {args, [{host, binary}]},
      {result, []}
     ],
     [
      {name, kick_user},
      {category, <<"sessions">>},
      {desc, <<"Terminate user connection">>},
      {module, ?MODULE},
      {function, kick_session},
      {action, delete},
      {args, [{host, binary}, {user, binary}, {res, binary}]},
      {result, {msg, binary}}
     ],
     [
      {name, list_contacts},
      {category, <<"contacts">>},
      {desc, <<"Get roster">>},
      {module, ?MODULE},
      {function, list_contacts},
      {action, read},
      {security_policy, [user]},
      {args, [{caller, binary}]},
      {result, []}
     ],
     [
      {name, add_contact},
      {category, <<"contacts">>},
      {desc, <<"Add a contact to roster">>},
      {module, ?MODULE},
      {function, add_contact},
      {action, create},
      {security_policy, [user]},
      {args, [{caller, binary}, {jid, binary}]},
      {result, ok}
     ],
     [
      {name, subscription},
      {category, <<"contacts">>},
      {desc, <<"Send out a subscription request">>},
      {module, ?MODULE},
      {function, subscription},
      {action, update},
      {security_policy, [user]},
      {identifiers, [caller, jid]},
      % caller has to be in identifiers, otherwise it breaks admin rest api
      {args, [{caller, binary}, {jid, binary}, {action, binary}]},
      {result, ok}
     ],
     [
      {name, set_subscription},
      {category, <<"contacts">>},
      {subcategory, <<"manage">>},
      {desc, <<"Set / unset mutual subscription">>},
      {module, ?MODULE},
      {function, set_subscription},
      {action, update},
      {identifiers, [caller, jid]},
      {args, [{caller, binary}, {jid, binary}, {action, binary}]},
      {result, ok}
     ],
     [
      {name, delete_contact},
      {category, <<"contacts">>},
      {desc, <<"Remove a contact from roster">>},
      {module, ?MODULE},
      {function, delete_contact},
      {action, delete},
      {security_policy, [user]},
      {args, [{caller, binary}, {jid, binary}]},
      {result, ok}
     ],
     [
      {name, delete_contacts},
      {category, <<"contacts">>},
      {subcategory, <<"multiple">>},
      {desc, <<"Remove provided contacts from roster">>},
      {module, ?MODULE},
      {function, delete_contacts},
      {action, delete},
      {security_policy, [user]},
      {args, [{caller, binary}, {jids, [binary]}]},
      {result,  []}
     ],
     [
      {name, send_message},
      {category, <<"messages">>},
      {desc, <<"Send chat message from to">>},
      {module, ?MODULE},
      {function, send_message},
      {action, create},
      {security_policy, [user]},
      {args, [{caller, binary}, {to, binary}, {body, binary}]},
      {result, ok}
     ],
     [
      {name, send_stanza},
      {category, <<"stanzas">>},
      {desc, <<"Send an arbitrary stanza">>},
      {module, ?MODULE},
      {function, send_stanza},
      {action, create},
      {args, [{stanza, binary}]},
      {result, ok}
     ],
     [
      {name, get_last_messages_with_everybody},
      {category, <<"messages">>},
      {desc, <<"Get n last messages from archive, optionally before a certain date (unixtime)">>},
      {module, ?MODULE},
      {function, get_recent_messages},
      {action, read},
      {security_policy, [user]},
      {args, [{caller, binary}]},
      {optargs, [{before, integer, 0}, {limit, integer, 100}]},
      {result, []}
     ],
     [
      {name, get_last_messages},
      {category, <<"messages">>},
      {desc, <<"Get n last messages to/from given contact, with limit and date">>},
      {module, ?MODULE},
      {function, get_recent_messages},
      {action, read},
      {security_policy, [user]},
      {args, [{caller, binary}, {with, binary}]},
      {optargs, [{before, integer, 0}, {limit, integer, 100}]},
      {result, []}
     ],
     [
      {name, change_password},
      {category, <<"users">>},
      {desc, <<"Change user password">>},
      {module, ?MODULE},
      {function, change_user_password},
      {action, update},
      {security_policy, [user]},
      {identifiers, [host, user]},
      {args, [{host, binary}, {user, binary}, {newpass, binary}]},
      {result, ok}
     ]
    ].

kick_session(Host, User, Resource) ->
    J = jid:make(User, Host, Resource),
    case ejabberd_c2s:terminate_session(J, <<"kicked">>) of
        no_session -> {error, not_found, <<"no active session">> };
        {exit, <<"kicked">>} -> <<"kicked">>
    end.

list_sessions(Host) ->
    Lst = ejabberd_sm:get_vh_session_list(Host),
    [jid:to_binary(USR) || #session{usr = USR} <- Lst].

registered_users(Host) ->
    Users = ejabberd_auth:get_vh_registered_users(Host),
    SUsers = lists:sort(Users),
    [jid:to_binary(US) || US <- SUsers].

register(Host, User, Password) ->
    JID = jid:make(User, Host, <<>>),
    case ejabberd_auth:try_register(JID, Password) of
        {error, exists} ->
            String = io_lib:format("User ~s already registered at node ~p",
                                   [jid:to_binary(JID), node()]),
            {error, denied, String};
        {error, invalid_jid} ->
            String = io_lib:format("Invalid jid: ~s@~s",
                                   [User, Host]),
            {error, bad_request, String};
        {error, Reason} ->
            String = io_lib:format("Can't register user ~s@~s at node ~p: ~p",
                                   [User, Host, node(), Reason]),
            {error, internal, String};
        _ ->
            <<"User ", (jid:to_binary(JID))/binary, "successfully registered">>
    end.

unregister(Host, User) ->
    JID = jid:make(User, Host, <<>>),
    case ejabberd_auth:remove_user(JID) of
        ok ->
            <<"ok">>;
        error ->
            {error, bad_request, io_lib:format("Invalid jid: ~p@~p", [User, Host])};
        {error, not_allowed} ->
            {error, forbidden, "User does not exist or you are not authorised properly"}
    end.

send_message(From, To, Body) ->
    case parse_from_to(From, To) of
        {ok, FromJID, ToJID} ->
            Packet = build_message(From, To, Body),
            do_send_packet(FromJID, ToJID, Packet);
        Error ->
            Error
    end.

send_stanza(BinStanza) ->
    case exml:parse(BinStanza) of
        {ok, Packet} ->
            From = exml_query:attr(Packet, <<"from">>),
            To = exml_query:attr(Packet, <<"to">>),
            case parse_from_to(From, To) of
                {ok, FromJID, ToJID} ->
                    do_send_packet(FromJID, ToJID, Packet);
                {error, missing} ->
                    {error, bad_request, "both from and to are required"};
                {error, type_error, E} ->
                    {error, type_error, E}
            end;
        {error, Reason} ->
            {error, bad_request, io_lib:format("Malformed stanza: ~p", [Reason])}
    end.

do_send_packet(From, To, Packet) ->
    case mongoose_domain_api:get_domain_host_type(From#jid.lserver) of
        {ok, HostType} ->
            Acc = mongoose_acc:new(#{location => ?LOCATION,
                                      host_type => HostType,
                                      lserver => From#jid.lserver,
                                      element => Packet}),
            Acc1 = mongoose_hooks:user_send_packet(Acc, From, To, Packet),
            ejabberd_router:route(From, To, Acc1),
            ok;
        {error, not_found} ->
            {error, unknown_domain}
    end.

-spec parse_from_to(jid:jid() | binary() | undefined, jid:jid() | binary() | undefined) ->
    {ok, jid:jid(), jid:jid()} | {error, missing} | {error, type_error, string()}.
parse_from_to(F, T) when F == undefined; T == undefined ->
    {error, missing};
parse_from_to(F, T) ->
    case parse_jid_list([F, T]) of
        {ok, [Fjid, Tjid]} -> {ok, Fjid, Tjid};
        E -> E
    end.

list_contacts(Caller) ->
    CallerJID = #jid{lserver = LServer} = jid:from_binary(Caller),
    {ok, HostType} = mongoose_domain_api:get_domain_host_type(LServer),
    Acc0 = mongoose_acc:new(#{ location => ?LOCATION,
                               host_type => HostType,
                               lserver => LServer,
                               element => undefined }),
    Acc1 = mongoose_acc:set(roster, show_full_roster, true, Acc0),
    Acc2 = mongoose_hooks:roster_get(Acc1, CallerJID),
    Res = mongoose_acc:get(roster, items, Acc2),
    [roster_info(mod_roster:item_to_map(I)) || I <- Res].

roster_info(M) ->
    Jid = jid:to_binary(maps:get(jid, M)),
    #{subscription := Sub, ask := Ask} = M,
    #{jid => Jid, subscription => Sub, ask => Ask}.

add_contact(Caller, JabberID) ->
    add_contact(Caller, JabberID, <<"">>, []).

add_contact(Caller, JabberID, Name) ->
    add_contact(Caller, JabberID, Name, []).

add_contact(Caller, Other, Name, Groups) ->
    case parse_from_to(Caller, Other) of
        {ok, CallerJid = #jid{lserver = LServer}, OtherJid} ->
            case mongoose_domain_api:get_domain_host_type(LServer) of
                {ok, HostType} ->
                    mod_roster:set_roster_entry(HostType, CallerJid, OtherJid, Name, Groups);
                {error, not_found} ->
                    {error, unknown_domain}
            end;
        E ->
            E
    end.

delete_contacts(Caller, ToDelete) ->
    maybe_delete_contacts(Caller, ToDelete, []).

maybe_delete_contacts(_, [], NotDeleted) -> NotDeleted;
maybe_delete_contacts(Caller, [H | T], NotDeleted) ->
    case delete_contact(Caller, H) of
        ok ->
            maybe_delete_contacts(Caller, T, NotDeleted);
        {error, _Reason} ->
            maybe_delete_contacts(Caller, T, NotDeleted ++ [H])
    end.

delete_contact(Caller, Other) ->
    case parse_from_to(Caller, Other) of
        {ok, CallerJid = #jid{lserver = LServer}, OtherJid} ->
            case mongoose_domain_api:get_domain_host_type(LServer) of
                {ok, HostType} ->
                    mod_roster:remove_from_roster(HostType, CallerJid, OtherJid);
                {error, not_found} ->
                    {error, unknown_domain}
            end;
        E ->
            E
    end.

registered_commands() ->
    Items = collect_commands(),
    sort_commands(Items).

sort_commands(Items) ->
    WithKey = [{get_sorting_key(Item), Item} || Item <- Items],
    Sorted = lists:keysort(1, WithKey),
    [Item || {_Key, Item} <- Sorted].

get_sorting_key(Item) ->
    maps:get(path, Item).

collect_commands() ->
    [#{name => mongoose_commands:name(C),
       category => mongoose_commands:category(C),
       action => mongoose_commands:action(C),
       method => mongoose_api_common:action_to_method(mongoose_commands:action(C)),
       desc => mongoose_commands:desc(C),
       args => format_args(mongoose_commands:args(C)),
       path => mongoose_api_common:create_admin_url_path(C)
      } || C <- mongoose_commands:list(admin)].

format_args(Args) ->
    maps:from_list([{term_as_binary(Name), term_as_binary(Type)}
                    || {Name, Type} <- Args]).

term_as_binary(X) ->
    iolist_to_binary(io_lib:format("~p", [X])).

get_recent_messages(Caller, Before, Limit) ->
    get_recent_messages(Caller, undefined, Before, Limit).

get_recent_messages(Caller, With, 0, Limit) ->
    {MegaSecs, Secs, _} = os:timestamp(),
    % wait a while to make sure we return all messages
    Future = (MegaSecs + 1) * 1000000 + Secs,
    get_recent_messages(Caller, With, Future, Limit);
get_recent_messages(Caller, With, Before, Limit) ->
    Res = lookup_recent_messages(Caller, With, Before, Limit),
    lists:map(fun row_to_map/1, Res).

change_user_password(Host, User, Password) ->
    JID = jid:make(User, Host, <<>>),
    case ejabberd_auth:set_password(JID, Password) of
        ok -> ok;
        {error, empty_password} ->
            {error, bad_request, "empty password"};
        {error, not_allowed} ->
            {error, denied, "password change not allowed"};
        {error, invalid_jid} ->
            {error, bad_request, "invalid jid"}
    end.

-spec row_to_map(mod_mam:message_row()) -> map().
row_to_map(#{id := Id, jid := From, packet := Msg}) ->
    Jbin = jid:to_binary(From),
    {Msec, _} = mod_mam_utils:decode_compact_uuid(Id),
    MsgId = case xml:get_tag_attr(<<"id">>, Msg) of
                {value, MId} -> MId;
                false -> <<"">>
            end,
    Body = exml_query:path(Msg, [{element, <<"body">>}, cdata]),
    #{sender => Jbin, timestamp => round(Msec / 1000000), message_id => MsgId,
      body => Body}.

-spec build_message(From :: binary(), To :: binary(), Body :: binary()) -> exml:element().
build_message(From, To, Body) ->
    #xmlel{name = <<"message">>,
           attrs = [{<<"type">>, <<"chat">>},
                    {<<"id">>, mongoose_bin:gen_from_crypto()},
                    {<<"from">>, From},
                    {<<"to">>, To}],
           children = [#xmlel{name = <<"body">>,
                              children = [#xmlcdata{content = Body}]}]
          }.

lookup_recent_messages(_, _, _, Limit) when Limit > 500 ->
    throw({error, message_limit_too_high});
lookup_recent_messages(ArcJID, With, Before, Limit) when is_binary(ArcJID) ->
    lookup_recent_messages(jid:from_binary(ArcJID), With, Before, Limit);
lookup_recent_messages(ArcJID, With, Before, Limit) when is_binary(With) ->
    lookup_recent_messages(ArcJID, jid:from_binary(With), Before, Limit);
lookup_recent_messages(ArcJID, WithJID, Before, Limit) ->
    #jid{luser = LUser, lserver = LServer} = ArcJID,
    {ok, HostType} = mongoose_domain_api:get_domain_host_type(LServer),
    Params = #{archive_id => mod_mam:archive_id(LServer, LUser),
               owner_jid => ArcJID,
               borders => undefined,
               rsm => #rsm_in{direction = before, id = undefined}, % last msgs
               start_ts => undefined,
               end_ts => Before * 1000000,
               now => os:system_time(microsecond),
               with_jid => WithJID,
               search_text => undefined,
               page_size => Limit,
               limit_passed => false,
               max_result_limit => 1,
               is_simple => true},
    R = mod_mam:lookup_messages(HostType, Params),
    {ok, {_, _, L}} = R,
    L.

subscription(Caller, Other, Action) ->
    case decode_action(Action) of
        error ->
            {error, bad_request, <<"invalid action">>};
        Act ->
            case parse_from_to(Caller, Other) of
                {ok, CallerJid, OtherJid} ->
                    run_subscription(Act, CallerJid, OtherJid);
                E ->
                    E
            end
    end.

decode_action(<<"subscribe">>) -> subscribe;
decode_action(<<"subscribed">>) -> subscribed;
decode_action(_) -> error.

-spec run_subscription(subscribe | subscribed, jid:jid(), jid:jid()) -> ok.
run_subscription(Type, CallerJid, OtherJid) ->
    StanzaType = atom_to_binary(Type, latin1),
    El = #xmlel{name = <<"presence">>, attrs = [{<<"type">>, StanzaType}]},
    LServer = CallerJid#jid.lserver,
    {ok, HostType} = mongoose_domain_api:get_domain_host_type(LServer),
    Acc1 = mongoose_acc:new(#{ location => ?LOCATION,
                               from_jid => CallerJid,
                               to_jid => OtherJid,
                               host_type => HostType,
                               lserver => LServer,
                               element => El }),
    % set subscription to
    Acc2 = mongoose_hooks:roster_out_subscription(Acc1, CallerJid, OtherJid, Type),
    ejabberd_router:route(CallerJid, OtherJid, Acc2),
    ok.


set_subscription(Caller, Other, Action) ->
    case parse_from_to(Caller, Other) of
        {ok, CallerJid, OtherJid} ->
            case Action of
                A when A == <<"connect">>; A == <<"disconnect">> ->
                    do_set_subscription(CallerJid, OtherJid, Action);
                _ ->
                    {error, bad_request, <<"invalid action">>}
            end;
        E ->
            E
    end.

do_set_subscription(Caller, Other, <<"connect">>) ->
    add_contact(Caller, Other),
    add_contact(Other, Caller),
    subscription(Caller, Other, <<"subscribe">>),
    subscription(Other, Caller, <<"subscribe">>),
    subscription(Other, Caller, <<"subscribed">>),
    subscription(Caller, Other, <<"subscribed">>),
    ok;
do_set_subscription(Caller, Other, <<"disconnect">>) ->
    delete_contact(Caller, Other),
    delete_contact(Other, Caller),
    ok.

-spec parse_jid_list(BinJids :: [binary()]) -> {ok, [jid:jid()]} | {error, type_error, string()}.
parse_jid_list([_ | _] = BinJids) ->
    Jids = lists:map(fun parse_jid/1, BinJids),
    case [Msg || {error, Msg} <- Jids] of
        [] -> {ok, Jids};
        Errors -> {error, type_error, lists:join("; ", Errors)}
    end.

-spec parse_jid(binary() | jid:jid()) -> jid:jid() | {error, string()}.
parse_jid(#jid{} = Jid) ->
    Jid;
parse_jid(Jid) when is_binary(Jid) ->
    case jid:from_binary(Jid) of
        error -> {error, io_lib:format("Invalid jid: ~p", [Jid])};
        B -> B
    end.
