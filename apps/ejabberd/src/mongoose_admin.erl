-module(mongoose_admin).
-author('bartlomiej.gorny@erlang-solutions.com').

-export([start/0, stop/0,
         start/2, stop/2,
         register/3,
         unregister/1,
         registered_commands/0,
         registered_users/1,
         listsessions/1,
         kick_this_session/1,
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
            {category, list},
            {desc, "List commands"},
            {module, ?MODULE},
            {function, registered_commands},
            {action, read},
            {args, []},
            {result, []}
        ],
        [
            {name, listusers},
            {category, user},
            {desc, "List registered users on this host"},
            {module, ?MODULE},
            {function, registered_users},
            {action, read},
            {args, [{host, binary}]},
            {result, []}
        ],
        [
            {name, registeruser},
            {category, user},
            {desc, "Register a user"},
            {module, ?MODULE},
            {function, register},
            {action, create},
            {args, [{user, binary}, {host, binary}, {password, binary}]},
            {result, {msg, binary}}
        ],
        [
            {name, unregisteruser},
            {category, user},
            {desc, "UnRegister a user"},
            {module, ?MODULE},
            {function, unregister},
            {action, delete},
            {args, [{jid, binary}]},
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
            {category, session},
            {desc, "Get session list"},
            {module, ?MODULE},
            {function, listsessions},
            {action, read},
            {args, [{host, binary}]},
            {result, []}
        ],
        [
            {name, kickuser},
            {category, session},
            {desc, "Terminate user connection"},
            {module, ?MODULE},
            {function, kick_this_session},
            {action, delete},
            {args, [{jid, binary}]},
            {result, {msg, binary}}
        ],
        [
            {name, sendmessage},
            {category, message},
            {desc, "Send chat message from to"},
            {module, ?MODULE},
            {function, send_message},
            {action, create},
            {security_policy, [user]},
            {args, [{caller, binary}, {to, binary}, {msg, binary}]},
            {result, {msg, binary}}
        ]
    ].

get_user_resources(Jid) ->
    J = jid:from_binary(Jid),
    ejabberd_sm:get_user_resources(J#jid.user, J#jid.server).


kick_this_session(Jid) ->
    J = jid:from_binary(Jid),
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


register(User, Host, Password) ->
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

unregister(Jid) ->
    J = jid:from_binary(Jid),
    ejabberd_auth:remove_user(J#jid.luser, J#jid.lserver),
    <<"">>.


send_message(From, To, Body) ->
    Packet = build_packet(message_chat, Body),
    F = jid:from_binary(From),
    T = jid:from_binary(To),
    ejabberd_router:route(F, T, Packet),
    <<"">>.


registered_commands() ->
    [#{
        name => mongoose_commands:name(C),
        category => mongoose_commands:category(C),
        action => mongoose_commands:action(C),
        desc => mongoose_commands:desc(C)
    } || C <- mongoose_commands:list(admin)].




build_packet(message_chat, Body) ->
    #xmlel{ name = <<"message">>,
        attrs = [{<<"type">>, <<"chat">>}, {<<"id">>, list_to_binary(randoms:get_string())}],
        children = [#xmlel{ name = <<"body">>, children = [#xmlcdata{content = Body}]}]
    }.
