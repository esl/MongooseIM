-module(mongoose_admin).
-author('bartlomiej.gorny@erlang-solutions.com').

-export([start/0, stop/0,
         start/2, stop/2,
         register/3,
         unregister/2,
         registered_commands/0,
         registered_users/1,
         change_user_password/2,
         listsessions/1,
         kick_session/3,
         get_recent_messages/3,
         send_message/3
        ]).

-include("ejabberd.hrl").
-include("jlib.hrl").

start() ->
    mongoose_commands:register(commands()).

stop() ->
    mongoose_commands:unregister(commands()).

start(_, _) -> start().
stop(_, _) -> stop().
%%%
%%% mongoose commands
%%%


commands() ->
    [
        [
            {name, listmethods},
            {category, commands},
            {desc, "List commands"},
            {module, ?MODULE},
            {function, registered_commands},
            {action, read},
            {args, []},
            {result, []}
        ],
        [
            {name, listusers},
            {category, users},
            {desc, "List registered users on this host"},
            {module, ?MODULE},
            {function, registered_users},
            {action, read},
            {args, [{host, binary}]},
            {result, []}
        ],
        [
            {name, registeruser},
            {category, users},
            {desc, "Register a user"},
            {module, ?MODULE},
            {function, register},
            {action, create},
            {args, [{host, binary}, {user, binary}, {password, binary}]},
            {result, {msg, binary}}
        ],
        [
            {name, unregisteruser},
            {category, users},
            {desc, "UnRegister a user"},
            {module, ?MODULE},
            {function, unregister},
            {action, delete},
            {args, [{host, binary}, {user, binary}]},
            {result, {msg, binary}}
        ],
%%        [
%%            {name, getuserresources},
%%            {category, session},
%%            {desc, "Get user resources"},
%%            {module, ?MODULE},
%%            {function, get_user_resources},
%%            {action, read},
%%            {args, [{jid, binary}]},
%%            {result, []}
%%        ],
        [
            {name, listsessions},
            {category, sessions},
            {desc, "Get session list"},
            {module, ?MODULE},
            {function, listsessions},
            {action, read},
            {args, [{host, binary}]},
            {result, []}
        ],
        [
            {name, kickuser},
            {category, sessions},
            {desc, "Terminate user connection"},
            {module, ?MODULE},
            {function, kick_session},
            {action, delete},
            {args, [{host, binary}, {user, binary}, {res, binary}]},
            {result, {msg, binary}}
        ],
        [
            {name, sendmessage},
            {category, messages},
            {desc, "Send chat message from to"},
            {module, ?MODULE},
            {function, send_message},
            {action, create},
            {security_policy, [user]},
            {args, [{caller, binary}, {to, binary}, {msg, binary}]},
            {result, ok}
        ],
        [
            {name, getmessages},
            {category, messages},
            {desc, "Get recent messages"},
            {module, ?MODULE},
            {function, get_recent_messages},
            {action, read},
            {security_policy, [user]},
            {args, [{caller, binary}, {other, binary}, {limit, integer}]},
            {result, []}
        ]
        % [
        %     {name, changepassword},
        %     {category, users},
        %     {desc, "Change user password"},
        %     {module, ?MODULE},
        %     {function, change_user_password},
        %     {action, update},
        %     {security_policy, [user]},
        %     {identifiers, [host, user]},
        %     {args, [{host, binary}, {user, binary}, {newpass, binary}]},
        %     {result, ok}
        % ]
    ].

%%get_user_resources(Jid) ->
%%    J = jid:from_binary(Jid),
%%    ejabberd_sm:get_user_resources(J#jid.user, J#jid.server).


kick_session(Host, User, Resource) ->
    J = jid:make(User, Host, Resource),
    ejabberd_sm:route(
        jid:make(<<"">>, <<"">>, <<"">>),
        J,
        {broadcast, {exit, <<"kicked">>}}),
    <<"kicked">>.

mk(E) ->
    Ss = element(1, E),
    {U, S, R} = Ss,
    <<U/binary, "@", S/binary, "/", R/binary>>.

listsessions(Host) ->
    Lst = ejabberd_sm:get_vh_session_list(Host),
    lists:map(fun mk/1, Lst).


registered_users(Host) ->
    Users = ejabberd_auth:get_vh_registered_users(Host),
    SUsers = lists:sort(Users),
    lists:map(fun({U, S}) -> <<U/binary, "@", S/binary>> end, SUsers).


register(Host, User, Password) ->
    case ejabberd_auth:try_register(User, Host, Password) of
        ok ->
            list_to_binary(io_lib:format("User ~s@~s successfully registered", [User, Host]));
        {error, exists} ->
            String = io_lib:format("User ~s@~s already registered at node ~p",
                [User, Host, node()]),
            throw({exists, String});
        {error, Reason} ->
            String = io_lib:format("Can't register user ~s@~s at node ~p: ~p",
                [User, Host, node(), Reason]),
            throw({error, String})
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
    <<"">>.


registered_commands() ->
    [#{
        name => mongoose_commands:name(C),
        category => mongoose_commands:category(C),
        action => mongoose_commands:action(C),
        desc => to_binary(mongoose_commands:desc(C))
    } || C <- mongoose_commands:list(admin)].


get_recent_messages(Caller, Other, Limit) ->
    Res = lookup_recent_messages(Caller, Other, Limit),
    lists:map(fun record_to_map/1, Res).


change_user_password(User, Password) ->
    Jid = jid:from_binary(User),
    ejabberd_auth:set_password(Jid#jid.luser, Jid#jid.lserver, Password).


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
    #xmlel{ name = <<"message">>,
        attrs = [{<<"type">>, <<"chat">>}, {<<"id">>, list_to_binary(randoms:get_string())}],
        children = [#xmlel{ name = <<"body">>, children = [#xmlcdata{content = Body}]}]
    }.

lookup_recent_messages(_, _, Limit) when Limit > 500 ->
    throw({error, message_limit_too_high});
lookup_recent_messages(ArcJID, Other, Limit) when is_binary(ArcJID) ->
    lookup_recent_messages(jid:from_binary(ArcJID), Other, Limit);
lookup_recent_messages(ArcJID, Other, Limit) when is_binary(Other) ->
    lookup_recent_messages(ArcJID, jid:from_binary(Other), Limit);
lookup_recent_messages(ArcJID, OtherJID, Limit) ->
    Host = ArcJID#jid.server,
    ArcID = mod_mam:archive_id(Host, ArcJID#jid.user),
    Borders = undefined,
    RSM = #rsm_in{direction = before, id = undefined}, % last msgs
    Start = undefined,
    End = undefined,
    Now = mod_mam_utils:now_to_microseconds(os:timestamp()),
    WithJID = OtherJID,
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

to_binary(V) ->
    list_to_binary(V).
