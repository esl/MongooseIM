-module(mod_commands).
-author('bartlomiej.gorny@erlang-solutions.com').

-behaviour(gen_mod).
-behaviour(mongoose_module_metrics).

-export([start/0, stop/0,
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
         send_message/3
        ]).

-include("mongoose.hrl").
-include("jlib.hrl").
-include("mongoose_rsm.hrl").

start() ->
    mongoose_commands:register(commands()).

stop() ->
    mongoose_commands:unregister(commands()).

start(_, _) -> start().
stop(_) -> stop().

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
    ejabberd_sm:route(
      jid:make(<<"">>, <<"">>, <<"">>),
      J,
      {broadcast, {exit, <<"kicked">>}}),
    <<"kicked">>.

list_sessions(Host) ->
    Lst = ejabberd_sm:get_vh_session_list(Host),
    [jid:to_binary(JID) || {JID, _, _, _} <- Lst].

registered_users(Host) ->
    Users = ejabberd_auth:get_vh_registered_users(Host),
    SUsers = lists:sort(Users),
    [jid:to_binary(US) || US <- SUsers].

register(Host, User, Password) ->
    case ejabberd_auth:try_register(User, Host, Password) of
        {error, exists} ->
            String = io_lib:format("User ~s@~s already registered at node ~p",
                                   [User, Host, node()]),
            throw({exists, String});
        {error, Reason} ->
            String = io_lib:format("Can't register user ~s@~s at node ~p: ~p",
                                   [User, Host, node(), Reason]),
            throw({error, String});
        _ ->
            list_to_binary(io_lib:format("User ~s@~s successfully registered",
                                         [User, Host]))
    end.

unregister(Host, User) ->
    ejabberd_auth:remove_user(User, Host),
    <<"">>.

send_message(From, To, Body) ->
    Packet = build_packet(message_chat, Body),
    F = jid:from_binary(From),
    T = jid:from_binary(To),
    ejabberd_hooks:run(user_send_packet,
                       F#jid.lserver,
                       [F, T, Packet]),
    %% privacy check is missing, but is it needed?
    ejabberd_router:route(F, T, Packet),
    ok.

list_contacts(Caller) ->
    CallerJID = jid:from_binary(Caller),
    Acc0 = mongoose_acc:new(#{ location => ?LOCATION,
                               lserver => CallerJID#jid.lserver,
                               element => undefined }),
    Acc1 = mongoose_acc:set(roster, show_full_roster, true, Acc0),
    {User, Host} = jid:to_lus(CallerJID),
    Acc2 = ejabberd_hooks:run_fold(roster_get, Host, Acc1, [{User, Host}]),
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

add_contact(Caller, JabberID, Name, Groups) ->
    CJid = jid:from_binary(Caller),
    mod_roster:set_roster_entry(CJid, JabberID, Name, Groups).

delete_contacts(Caller, ToDelete) ->
    maybe_delete_contacts(Caller, ToDelete, []).

maybe_delete_contacts(_, [], NotDeleted) -> NotDeleted;
maybe_delete_contacts(Caller, [H | T], NotDeleted) ->
    case delete_contact(Caller, H) of
        ok ->
            maybe_delete_contacts(Caller, T, NotDeleted);
        error ->
            maybe_delete_contacts(Caller, T, NotDeleted ++ [H])
    end.

delete_contact(Caller, JabberID) ->
    CJid = jid:from_binary(Caller),
    case jid_exists(Caller, JabberID) of
        false -> error;
        true ->
            mod_roster:remove_from_roster(CJid, JabberID)
    end.

-spec jid_exists(binary(), binary()) -> boolean().
jid_exists(CJid, Jid) ->
    FJid = jid:from_binary(CJid),
    Res = mod_roster:get_roster_entry(FJid#jid.luser, FJid#jid.lserver, Jid),
    Res =/= does_not_exist.

registered_commands() ->
    [#{name => mongoose_commands:name(C),
       category => mongoose_commands:category(C),
       action => mongoose_commands:action(C),
       desc => mongoose_commands:desc(C)
      } || C <- mongoose_commands:list(admin)].


get_recent_messages(Caller, Before, Limit) ->
    get_recent_messages(Caller, undefined, Before, Limit).

get_recent_messages(Caller, With, 0, Limit) ->
    {MegaSecs, Secs, _} = os:timestamp(),
    % wait a while to make sure we return all messages
    Future = (MegaSecs + 1) * 1000000 + Secs,
    get_recent_messages(Caller, With, Future, Limit);
get_recent_messages(Caller, With, Before, Limit) ->
    Res = lookup_recent_messages(Caller, With, Before, Limit),
    lists:map(fun record_to_map/1, Res).

change_user_password(Host, User, Password) ->
    ejabberd_auth:set_password(User, Host, Password),
    ok.

record_to_map({Id, From, Msg}) ->
    Jbin = jid:to_binary(From),
    {Msec, _} = mod_mam_utils:decode_compact_uuid(Id),
    MsgId = case xml:get_tag_attr(<<"id">>, Msg) of
                {value, MId} -> MId;
                false -> <<"">>
            end,
    Body = exml_query:path(Msg, [{element, <<"body">>}, cdata]),
    #{sender => Jbin, timestamp => round(Msec / 1000000), message_id => MsgId,
      body => Body}.

build_packet(message_chat, Body) ->
    #xmlel{name = <<"message">>,
           attrs = [{<<"type">>, <<"chat">>},
                    {<<"id">>, mongoose_bin:gen_from_crypto()}],
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
    Host = ArcJID#jid.server,
    Params = #{archive_id => mod_mam:archive_id(Host, ArcJID#jid.user),
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
    R = mod_mam:lookup_messages(Host, Params),
    {ok, {_, _, L}} = R,
    L.

subscription(Caller, Other, Action) ->
    Act = decode_action(Action),
    run_subscription(Act, jid:from_binary(Caller), jid:from_binary(Other)).

decode_action(<<"subscribe">>) -> subscribe;
decode_action(<<"subscribed">>) -> subscribed.

-spec run_subscription(subscribe | subscribed, jid:jid(), jid:jid()) -> ok.
run_subscription(Type, CallerJid, OtherJid) ->
    StanzaType = atom_to_binary(Type, latin1),
    El = #xmlel{name = <<"presence">>, attrs = [{<<"type">>, StanzaType}]},
    Acc1 = mongoose_acc:new(#{ location => ?LOCATION,
                               from_jid => CallerJid,
                               to_jid => OtherJid,
                               lserver => CallerJid#jid.lserver,
                               element => El }),
    % set subscription to
    Server = CallerJid#jid.server,
    LUser = CallerJid#jid.luser,
    LServer = CallerJid#jid.lserver,
    Acc2 = ejabberd_hooks:run_fold(roster_out_subscription,
                                   Server,
                                   Acc1,
                                   [LUser, LServer, OtherJid, Type]),
    ejabberd_router:route(CallerJid, OtherJid, Acc2),
    ok.

set_subscription(Caller, Other, <<"connect">>) ->
    add_contact(Caller, Other),
    add_contact(Other, Caller),
    subscription(Caller, Other, <<"subscribe">>),
    subscription(Other, Caller, <<"subscribe">>),
    subscription(Other, Caller, <<"subscribed">>),
    subscription(Caller, Other, <<"subscribed">>),
    ok;
set_subscription(Caller, Other, <<"disconnect">>) ->
    delete_contact(Caller, Other),
    delete_contact(Other, Caller),
    ok.
