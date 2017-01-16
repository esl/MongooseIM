-module(mod_commands).
-author('bartlomiej.gorny@erlang-solutions.com').

-behaviour(gen_mod).

-export([start/0, stop/0,
         start/2, stop/1,
         register/3,
         unregister/2,
         registered_commands/0,
         registered_users/1,
         change_user_password/3,
         list_sessions/1,
         kick_session/3,
         get_recent_messages/3,
         get_recent_messages/4,
         send_message/3
        ]).

-include("ejabberd.hrl").
-include("jlib.hrl").

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
            list_to_binary(io_lib:format("User ~s@~s successfully registered", [User, Host]))
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
    % privacy check is missing, but is it needed?
    ejabberd_router:route(F, T, Packet),
    ok.

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
    Future = (MegaSecs + 1) * 1000000 + Secs, % to make sure we return all messages
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
    #{sender => Jbin, timestamp => round(Msec / 1000000), message_id => MsgId, body => Body}.

build_packet(message_chat, Body) ->
    #xmlel{name = <<"message">>,
           attrs = [{<<"type">>, <<"chat">>}, {<<"id">>, list_to_binary(randoms:get_string())}],
           children = [#xmlel{ name = <<"body">>, children = [#xmlcdata{content = Body}]}]
          }.

lookup_recent_messages(_, _, _, Limit) when Limit > 500 ->
    throw({error, message_limit_too_high});
lookup_recent_messages(ArcJID, With, Before, Limit) when is_binary(ArcJID) ->
    lookup_recent_messages(jid:from_binary(ArcJID), With, Before, Limit);
lookup_recent_messages(ArcJID, With, Before, Limit) when is_binary(With) ->
    lookup_recent_messages(ArcJID, jid:from_binary(With), Before, Limit);
lookup_recent_messages(ArcJID, WithJID, Before, Limit) ->
    Host = ArcJID#jid.server,
    ArcID = mod_mam:archive_id(Host, ArcJID#jid.user),
    Borders = undefined,
    RSM = #rsm_in{direction = before, id = undefined}, % last msgs
    Start = undefined,
    End = Before * 1000000,
    Now = mod_mam_utils:now_to_microseconds(os:timestamp()),
    WithJID = WithJID,
    PageSize = Limit,
    LimitPassed = false,
    MaxResultLimit = 1,
    IsSimple = true,
    R = ejabberd_hooks:run_fold(mam_lookup_messages, Host, {ok, {0, 0, []}},
                                [Host, ArcID, ArcJID, RSM, Borders,
                                 Start, End, Now, WithJID,
                                 PageSize, LimitPassed, MaxResultLimit, IsSimple]),
    {ok, {_, _, L}} = R,
    L.

