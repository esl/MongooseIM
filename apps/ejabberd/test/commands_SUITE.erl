%% @doc This suite tests both old ejabberd_commands module, which is slowly getting deprecated,
%% and the new mongoose_commands implementation.
-module(commands_SUITE).
-compile([export_all]).

-include_lib("exml/include/exml.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("ejabberd/include/ejabberd_commands.hrl").
-include_lib("ejabberd/include/jlib.hrl").

-define(PRT(X, Y), ct:pal("~p: ~p", [X, Y])).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% suite configuration
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

all() ->
    [
     {group, old_commands},
     {group, new_commands}
    ].

groups() ->
    [
     {old_commands, [sequence],
      [old_list,
       old_exec,
       old_access_ctl
      ]
     },
     {new_commands, [sequence],
      [new_type_checker,
       new_reg_unreg,
       new_failedreg,
       new_list,
       new_execute,
       different_types
      ]
     }
    ].

glo({auth_method, _}) ->
    dummy;
glo(Any) ->
    ?PRT("what do you want", Any),
    none.

ggo({access, experts_only, _}) ->
    [{allow, coder}, {allow, manager}, {deny, all}];
ggo({access, _, _}) ->
    [];
ggo(Any) ->
    ?PRT("global what do you want", Any),
    none.

init_per_suite(C) ->
    application:ensure_all_started(stringprep),
    ok = mnesia:start(),
    ok = acl:start(),
    acl:add(global, coder, {user, <<"zenek">>}),
    C.

end_per_suite(_) ->
    mnesia:stop(),
    mnesia:delete_schema([node()]),
    ok.

init_per_group(old_commands, C) ->
    Pid = spawn(fun ec_holder/0),
    [{helper_proc, Pid} | C];
init_per_group(new_commands, C) ->
    Pid = spawn(fun mc_holder/0),
    [{helper_proc, Pid} | C].

end_per_group(old_commands, C) ->
    ejabberd_commands:unregister_commands(commands_old()),
    stop_helper_proc(C),
    C;
end_per_group(new_commands, C) ->
    mongoose_commands:unregister(commands_new()),
    stop_helper_proc(C),
    C.

stop_helper_proc(C) ->
    Pid = proplists:get_value(helper_proc, C),
    Pid ! stop.

init_per_testcase(_, C) ->
    meck:new(ejabberd_config),
    meck:expect(ejabberd_config, get_local_option, fun glo/1),
    meck:expect(ejabberd_config, get_global_option, fun ggo/1),
    meck:new(ejabberd_auth_dummy, [non_strict]),
    meck:expect(ejabberd_auth_dummy, get_password_s, fun(_, _) -> <<"">> end),
    C.

end_per_testcase(_, C) ->
    meck:unload(ejabberd_config),
    meck:unload(ejabberd_auth_dummy),
    C.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% test methods
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


old_list(_C) ->
    %% list
    Rlist = ejabberd_commands:list_commands(),
    {command_one, _, "do nothing and return"} = proplists:lookup(command_one, Rlist),
    %% get definition
    Rget = ejabberd_commands:get_command_definition(command_one),
    % we should get back exactly the definition we provided
    [Cone | _] = commands_old(),
    Cone = Rget,
    %% get interface
    {Argspec, Retspec} = ejabberd_commands:get_command_format(command_one),
    [{msg, binary}] = Argspec,
    {res, restuple} = Retspec,
    %% list by tags
    Tagcomm = ejabberd_commands:get_tags_commands(),
    ?assertEqual(length(proplists:get_value("one", Tagcomm)), 1),
    ?assertEqual(length(proplists:get_value("two", Tagcomm)), 2),
    ?assertEqual(length(proplists:get_value("three", Tagcomm)), 1),
    ok.

old_exec(_C) ->
    %% execute
    <<"bzzzz">> = ejabberd_commands:execute_command(command_one, [<<"bzzzz">>]),
    Res = ejabberd_commands:execute_command(command_one, [123]),
    ?PRT("invalid type ignored", Res), %% there is no arg type check
    Res2 = ejabberd_commands:execute_command(command_two, [123]),
    ?PRT("invalid return type ignored", Res2), %% nor return
    %% execute unknown command
    {error, command_unknown} = ejabberd_commands:execute_command(command_seven, [123]),
    ok.

old_access_ctl(_C) ->
    %% with no auth method it is all fine
    checkauth(true, [], noauth),
    %% noauth fails if first item is not 'all' (users)
    checkauth(account_unprivileged, [{none, none, []}], noauth),
    %% if here we allow all commands to noauth
    checkauth(true, [{all, all, []}], noauth),
    %% and here only command_one
    checkauth(true, [{all, [command_one], []}], noauth),
    %% so this'd fail
    checkauth(account_unprivileged, [{all, [command_two], []}], noauth),
    % now we provide a role name, this requires a user and triggers password and acl check
    % this fails because password is bad
    checkauth(invalid_account_data, [{some_acl_role, [command_one], []}], {<<"zenek">>, <<"localhost">>, <<"bbb">>}),
    % this, because of acl
    checkauth(account_unprivileged, [{some_acl_role, [command_one], []}], {<<"zenek">>, <<"localhost">>, <<"">>}),
    % and this should work, because we define command_one as available to experts only, while acls in config
    % (see ggo/1) state that experts-only funcs are available to coders and managers, and zenek is a coder, gah.
    checkauth(true, [{experts_only, [command_one], []}], {<<"zenek">>, <<"localhost">>, <<"">>}),
    ok.


new_type_checker(_C) ->
    true = t_check_type({msg, binary}, <<"zzz">>),
    true = t_check_type({msg, integer}, 127),
    {false, _} = t_check_type({{a, binary}, {b, integer}}, 127),
    true = t_check_type({{a, binary}, {b, integer}}, {<<"z">>, 127}),
    true = t_check_type({ok, {msg, integer}}, {ok, 127}),
    true = t_check_type({ok, {msg, integer}, {val, binary}}, {ok, 127, <<"z">>}),
    {false, _} = t_check_type({k, {msg, integer}, {val, binary}}, {ok, 127, <<"z">>}),
    {false, _} = t_check_type({ok, {msg, integer}, {val, binary}}, {ok, 127, "z"}),
    {false, _} = t_check_type({ok, {msg, integer}, {val, binary}}, {ok, 127, <<"z">>, 333}),
    true = t_check_type([integer], []),
    true = t_check_type([integer], [1, 2, 3]),
    {false, _} = t_check_type([integer], [1, <<"z">>, 3]),
    true = t_check_type([], [1, 2, 3]),
    true = t_check_type([], []),
    ok.

t_check_type(Spec, Value) ->
    R = try mongoose_commands:check_type(Spec, Value) of
            true -> true
        catch
            E ->
                {false, E}
        end,
    R.

new_reg_unreg(_C) ->
    L1 = length(commands_new()),
    L2 = L1 + length(commands_new_temp()),
    ?assertEqual(length(mongoose_commands:list(admin)), L1),
    mongoose_commands:register(commands_new_temp()),
    ?assertEqual(length(mongoose_commands:list(admin)), L2),
    mongoose_commands:unregister(commands_new_temp()),
    ?assertEqual(length(mongoose_commands:list(admin)), L1),
    ok.

failedreg([]) -> ok;
failedreg([Cmd|Tail]) ->
    ?assertThrow({invalid_command_definition, _}, mongoose_commands:register([Cmd])),
    failedreg(Tail).

new_failedreg(_C) ->
    failedreg(commands_new_lame()).


new_list(_C) ->
    %% for admin
    Rlist = mongoose_commands:list(admin),
    [Cmd] = [C || C <- Rlist, mongoose_commands:name(C) == command_one],
    command_one = mongoose_commands:name(Cmd),
    <<"do nothing and return">> = mongoose_commands:desc(Cmd),
    %% list by category
    [_] = mongoose_commands:list(admin, <<"user">>),
    [] = mongoose_commands:list(admin, <<"nocategory">>),
    %% list by category and action
    [_] = mongoose_commands:list(admin, <<"user">>, read),
    [] = mongoose_commands:list(admin, <<"user">>, update),
    %% get definition
    Rget = mongoose_commands:get_command(admin, command_one),
    command_one = mongoose_commands:name(Rget),
    read = mongoose_commands:action(Rget),
    [] = mongoose_commands:identifiers(Rget),
    {error, denied, _} = mongoose_commands:get_command(ujid(), command_one),
    %% list for a user
    Ulist = mongoose_commands:list(ujid()),
    [UCmd] = [UC || UC <- Ulist, mongoose_commands:name(UC) == command_foruser],

    command_foruser = mongoose_commands:name(UCmd),
    URget = mongoose_commands:get_command(ujid(), command_foruser),
    command_foruser = mongoose_commands:name(URget),
    ok.


new_execute(_C) ->
    {ok, <<"bzzzz">>} = mongoose_commands:execute(admin, command_one, [<<"bzzzz">>]),
    Cmd = mongoose_commands:get_command(admin, command_one),
    {ok, <<"bzzzz">>} = mongoose_commands:execute(admin, Cmd, [<<"bzzzz">>]),
    %% call with a map
    {ok, <<"bzzzz">>} = mongoose_commands:execute(admin, command_one, #{msg => <<"bzzzz">>}),
    %% command which returns just ok
    ok = mongoose_commands:execute(admin, command_noreturn, [<<"bzzzz">>]),
    %% this user has no permissions
    {error, denied, _} = mongoose_commands:execute(ujid(), command_one, [<<"bzzzz">>]),
    %% command is not registered
    {error, not_implemented, _} = mongoose_commands:execute(admin, command_seven, [<<"bzzzz">>]),
    %% invalid arguments
    {error, type_error, _} = mongoose_commands:execute(admin, command_one, [123]),
    {error, type_error, _} = mongoose_commands:execute(admin, command_one, []),
    {error, type_error, _} = mongoose_commands:execute(admin, command_one, #{}),
    {error, type_error, _} = mongoose_commands:execute(admin, command_one, #{msg => 123}),
    {error, type_error, _} = mongoose_commands:execute(admin, command_one, #{notthis => <<"bzzzz">>}),
    {error, type_error, _} = mongoose_commands:execute(admin, command_one, #{msg => <<"bzzzz">>, redundant => 123}),
    %% backend func throws exception
    {error, internal, _} = mongoose_commands:execute(admin, command_one, [<<"throw">>]),
    %% backend func returns error
    ExpError = term_to_binary({func_returned_error, byleco}),
    {error, internal, ExpError} = mongoose_commands:execute(admin, command_one, [<<"error">>]),
    % user executes his command
    {ok, <<"bzzzz">>} = mongoose_commands:execute(ujid(), command_foruser, #{msg => <<"bzzzz">>}),
    % a caller arg
    % called by admin
    {ok, <<"admin@localhost/zbzzzz">>} = mongoose_commands:execute(admin,
                                                                   command_withcaller,
                                                                   #{caller => <<"admin@localhost/z">>,
                                                                     msg => <<"bzzzz">>}),
    % called by user
    {ok, <<"zenek@localhost/zbzzzz">>} = mongoose_commands:execute(<<"zenek@localhost">>,
                                                                   command_withcaller,
                                                                   #{caller => <<"zenek@localhost/z">>,
                                                                     msg => <<"bzzzz">>}),
    % call by user but jids do not match
    {error, denied, _} = mongoose_commands:execute(<<"wacek@localhost">>,
                                                   command_withcaller,
                                                   #{caller => <<"zenek@localhost/z">>,
                                                     msg => <<"bzzzz">>}),
    {ok, 30} = mongoose_commands:execute(admin, command_withoptargs, #{msg => <<"a">>}),
    {ok, 18} = mongoose_commands:execute(admin, command_withoptargs, #{msg => <<"a">>, value => 6}),
    ok.

different_types(_C) ->
    mongoose_commands:register(commands_new_temp2()),
    {ok, <<"response1">>} = mongoose_commands:execute(admin, command_two, [10, 15]),
    {ok, <<"response2">>} = mongoose_commands:execute(admin, command_three, [10, <<"binary">>]),
    mongoose_commands:unregister(commands_new_temp2()),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% definitions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

commands_new() ->
    [
        [
            {name, command_one},
            {category, <<"user">>},
            {desc, <<"do nothing and return">>},
            {module, ?MODULE},
            {function, cmd_one},
            {action, read},
            {args, [{msg, binary}]},
            {result, {msg, binary}}
        ],
        [
            {name, command_noreturn},
            {category, <<"message">>},
            {desc, <<"do nothing and return nothing">>},
            {module, ?MODULE},
            {function, cmd_one},
            {action, create},
            {args, [{msg, binary}]},
            {result, ok}
        ],
        [
            {name, command_foruser},
            {category, <<"another">>},
            {desc, <<"this is available for a user">>},
            {module, ?MODULE},
            {function, cmd_one},
            {action, read},
            {security_policy, [user]},
            {args, [{msg, binary}]},
            {result, {msg, binary}}
        ],
        [
            {name, command_withoptargs},
            {category, <<"yetanother">>},
            {desc, <<"this is available for a user">>},
            {module, ?MODULE},
            {function, cmd_one_withvalue},
            {action, read},
            {security_policy, [user]},
            {args, [{msg, binary}]},
            {optargs, [{value, integer, 10}]},
            {result, {nvalue, integer}}
        ],
        [
            {name, command_withcaller},
            {category, <<"another">>},
            {desc, <<"this has a 'caller' argument, returns caller ++ msg">>},
            {module, ?MODULE},
            {function, cmd_concat},
            {action, create},
            {security_policy, [user]},
            {args, [{caller, binary}, {msg, binary}]},
            {result, {msg, binary}}
        ]
    ].

commands_new_temp() ->
    %% this is to check registering/unregistering commands
    [
        [
            {name, command_temp},
            {category, <<"user">>},
            {desc, <<"do nothing and return">>},
            {module, ?MODULE},
            {function, cmd_one},
            {action, create}, % different action
            {args, [{msg, binary}]},
            {result, {msg, binary}}
        ],
        [
            {name, command_one_arity},
            {category, <<"user">>},
            {desc, <<"do nothing and return">>},
            {module, ?MODULE},
            {function, cmd_one},
            {action, read},
            {args, [{msg, binary}, {whatever, integer}]}, % different arity
            {result, {msg, binary}}
        ],
        [
            {name, command_one_two},
            {category, <<"user">>},
            {subcategory, <<"rosters">>}, % has subcategory
            {desc, <<"do nothing and return">>},
            {module, ?MODULE},
            {function, cmd_one},
            {action, read},
            {args, [{msg, binary}]},
            {result, {msg, binary}}
        ],
        [
            {name, command_temp2},
            {category, <<"user">>},
            {desc, <<"this one specifies identifiers">>},
            {module, ?MODULE},
            {function, cmd_one},
            {action, update},
            {identifiers, [ident]},
            {args, [{ident, integer}, {msg, binary}]},
            {result, {msg, binary}}
        ]
    ].

commands_new_temp2() ->
    %% This is for extra test with different arg types
    [
        [
            {name, command_two},
            {category, <<"animals">>},
            {desc, <<"some">>},
            {module, ?MODULE},
            {function, the_same_types},
            {action, read},
            {args, [{one, integer}, {two, integer}]},
            {result, {msg, binary}}
    ],
        [
            {name, command_three},
            {category, <<"music">>},
            {desc, <<"two args, different types">>},
            {module, ?MODULE},
            {function, different_types},
            {action, read},
            {args, [{one, integer}, {two, binary}]},
            {result, {msg, binary}}
        ]
    ].

commands_new_lame() ->
    [
        [
            {name, command_one} % missing values
        ],
        [
            {name, command_one},
            {category, []} %% should be binary
        ],
        [
            {name, command_one},
            {category, <<"user">>},
            {desc, <<"do nothing and return">>},
            {module, ?MODULE},
            {function, cmd_one},
            {action, andnowforsomethingcompletelydifferent} %% not one of allowed values
        ],
        [
            {name, command_one},
            {category, <<"user">>},
            {desc, <<"do nothing and return">>},
            {module, ?MODULE},
            {function, cmd_one},
            {action, delete},
            {args, [{msg, binary}, integer]}, %% args have to be a flat list of named arguments
            {result, {msg, binary}}
        ],
%%        We do not crash if command is already registered because some modules are loaded more then once
%%        [
%%            {name, command_one}, %% everything is fine, but it is already registered
%%            {category, another},
%%            {desc, "do nothing and return"},
%%            {module, ?MODULE},
%%            {function, cmd_one},
%%            {action, read},
%%            {args, [{msg, binary}]},
%%            {result, {msg, binary}}
%%        ],
        [
            {name, command_one},
            {category, <<"another">>},
            {desc, <<"do nothing and return">>},
            {module, ?MODULE},
            {function, cmd_one},
            {action, update}, %% an 'update' command has to specify identifiers
            {args, [{msg, binary}]},
            {result, {msg, binary}}
        ],
        [
            {name, command_one},
            {category, <<"another">>},
            {desc, <<"do nothing and return">>},
            {module, ?MODULE},
            {function, cmd_one},
            {action, update},
            {identifiers, [1]}, %% ...and they must be atoms...
            {args, [{msg, binary}]},
            {result, {msg, binary}}
        ],
        [
            {name, command_one},
            {category, <<"another">>},
            {desc, <<"do nothing and return">>},
            {module, ?MODULE},
            {function, cmd_one},
            {action, update},
            {identifiers, [ident]}, %% ...which are present in args
            {args, [{msg, binary}]},
            {result, {msg, binary}}
        ],
        [
            {name, command_seven}, %% name is different...
            {category, <<"user">>},
            {desc, <<"do nothing and return">>},
            {module, ?MODULE},
            {function, cmd_one},
            {action, read}, %% ...but another command with the same category and action and arity is already registered
            {args, [{msg, binary}]},
            {result, {msg, binary}}
        ],
        [
            {name, command_seven},
            {category, <<"user">>},
            {desc, <<"do nothing and return">>},
            {module, ?MODULE},
            {function, cmd_one},
            {action, delete},
            {security_policy, [wrong]}, % invalid security definition
            {args, [{msg, binary}]},
            {result, {msg, binary}}
%%        ],
%%        [
%%            {name, command_seven},
%%            {category, user},
%%            {desc, "do nothing and return"},
%%            {module, ?MODULE},
%%            {function, cmd_one},
%%            {action, delete},
%%            {security_policy, []}, % invalid security definition
%%            {args, [{msg, binary}]},
%%            {result, {msg, binary}}
        ]
    ].

commands_old() ->
    [
        #ejabberd_commands{name = command_one, tags = [one],
                           desc = "do nothing and return",
                           module = ?MODULE, function = cmd_one,
                           args = [{msg, binary}],
                           result = {res, restuple}},
        #ejabberd_commands{name = command_two, tags = [two],
                           desc = "this returns wrong type",
                           module = ?MODULE, function = cmd_two,
                           args = [{msg, binary}],
                           result = {res, restuple}},
        #ejabberd_commands{name = command_three, tags = [two, three],
                           desc = "do nothing and return",
                           module = ?MODULE, function = cmd_three,
                           args = [{msg, binary}],
                           result = {res, restuple}}
    ].

cmd_one(<<"throw">>) ->
    C = 12,
    <<"A", C/binary>>;
cmd_one(<<"error">>) ->
    {error, byleco};
cmd_one(M) ->
    M.

cmd_one_withvalue(_Msg, Value) ->
    Value * 3.

cmd_two(M) ->
    M.

the_same_types(10, 15) ->
    <<"response1">>;
the_same_types(_, _) ->
    <<"wrong response">>.

different_types(10, <<"binary">>) ->
	<<"response2">>;
different_types(_, _) ->
	<<"wrong content">>.

cmd_concat(A, B) ->
    <<A/binary, B/binary>>.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% utilities
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% this is a bit stupid, but we need a process which would hold ets table
ec_holder() ->
    ejabberd_commands:init(),
    ejabberd_commands:register_commands(commands_old()),
    receive
        _ -> ok
    end.

mc_holder() ->
    % we have to do it here to avoid race condition and random failures
    {ok, Pid} = ejabberd_hooks:start_link(),
    mongoose_commands:init(),
    mongoose_commands:register(commands_new()),
    receive
        _ -> ok
    end,
    erlang:exit(Pid, kill).

checkauth(true, AccessCommands, Auth) ->
    B = <<"bzzzz">>,
    B = ejabberd_commands:execute_command(AccessCommands, Auth, command_one, [B]);
checkauth(ErrMess, AccessCommands, Auth) ->
    B = <<"bzzzz">>,
    {error, ErrMess} = ejabberd_commands:execute_command(AccessCommands, Auth, command_one, [B]).

ujid() ->
    <<"zenek@localhost/k">>.
%%    #jid{user = <<"zenek">>, server = <<"localhost">>, resource = "k",
%%         luser = <<"zenek">>, lserver = <<"localhost">>, lresource = "k"}.
