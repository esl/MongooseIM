%% @doc This suite tests both old ejabberd_commands module, which is slowly getting deprecated,
%% and the new mongoose_commands implementation.
-module(commands_SUITE).
-compile([export_all]).

-include_lib("exml/include/exml.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("ejabberd/include/ejabberd_commands.hrl").
-include_lib("ejabberd/include/mongoose_commands.hrl").

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
             new_execute
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

init_per_group(old_commands, C) ->
    spawn(fun ec_holder/0),
    C;
init_per_group(new_commands, C) ->
    spawn(fun mc_holder/0),
    C.

end_per_group(old_commands, C) ->
    ejabberd_commands:unregister_commands(commands_old()),
    C;
end_per_group(new_commands, C) ->
    mongoose_commands:unregister(commands_new()),
    C.

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
    ?assertEqual(length(mongoose_commands:list(admin)), 1),
    mongoose_commands:register(commands_new_temp()),
    ?assertEqual(length(mongoose_commands:list(admin)), 2),
    mongoose_commands:unregister(commands_new_temp()),
    ?assertEqual(length(mongoose_commands:list(admin)), 1),
    ok.

failedreg([]) -> ok;
failedreg([Cmd|Tail]) ->
    ?assertThrow({invalid_command_definition, _}, mongoose_commands:register([Cmd])),
    failedreg(Tail).

new_failedreg(_C) ->
    failedreg(commands_new_lame()).


new_list(_C) ->
    Rlist = mongoose_commands:list(admin),
    [Cmd] = [C || C <- Rlist, C#mongoose_command.name == command_one],%%proplists:get_value(command_one, Rlist),
    command_one = mongoose_commands:name(Cmd),
    "do nothing and return" = mongoose_commands:desc(Cmd),
    %% list for a user
    [] = mongoose_commands:list(a_user),
    %% list by category
    [_] = mongoose_commands:list(admin, user),
    [] = mongoose_commands:list(admin, nocategory),
    %% list by category and action
    [_] = mongoose_commands:list(admin, user, read),
    [] = mongoose_commands:list(admin, user, update),
    %% get definition
    Rget = mongoose_commands:get_command(admin, command_one),
    command_one = mongoose_commands:name(Rget),
    read = mongoose_commands:action(Rget),
    {error, denied, _} = mongoose_commands:get_command(a_user, command_one),
    ok.


new_execute(_C) ->
    {ok, <<"bzzzz">>} = mongoose_commands:execute(admin, command_one, [<<"bzzzz">>]),
    Cmd = mongoose_commands:get_command(admin, command_one),
    {ok, <<"bzzzz">>} = mongoose_commands:execute(admin, Cmd, [<<"bzzzz">>]),
    %% this user has no permissions
    {error, denied, _} = mongoose_commands:execute(a_user, command_one, [<<"bzzzz">>]),
    %% command is not registered
    {error, not_implemented, _} = mongoose_commands:execute(admin, command_seven, [<<"bzzzz">>]),
    %% invalid arguments
    {error, type_error, _} = mongoose_commands:execute(admin, command_one, [123]),
    {error, type_error, _} = mongoose_commands:execute(admin, command_one, []),
    %% backend func throws exception
    {error, internal, _} = mongoose_commands:execute(admin, command_one, [<<"throw">>]),
    %% backend func returns error
    ExpError = term_to_binary({func_returned_error, byleco}),
    {error, internal, ExpError} = mongoose_commands:execute(admin, command_one, [<<"error">>]),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% definitions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

commands_new() ->
    [
        [
            {name, command_one},
            {category, user},
            {desc, "do nothing and return"},
            {module, ?MODULE},
            {function, cmd_one},
            {action, read},
            {args, [{msg, binary}]},
            {result, {msg, binary}}
        ]
    ].

commands_new_temp() ->
    %% this is to check registering/unregistering commands
    [
        [
            {name, command_temp},
            {category, user},
            {desc, "do nothing and return"},
            {module, ?MODULE},
            {function, cmd_one},
            {action, create},
            {args, [{msg, binary}]},
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
            {category, []} %% should be an atom
        ],
        [
            {name, command_one},
            {category, user},
            {desc, "do nothing and return"},
            {module, ?MODULE},
            {function, cmd_one},
            {action, andnowforsomethingcompletelydifferent} %% not one of allowed values
        ],
        [
            {name, command_one}, %% everything is fine, but it is already registered
            {category, another},
            {desc, "do nothing and return"},
            {module, ?MODULE},
            {function, cmd_one},
            {action, read},
            {args, [{msg, binary}]},
            {result, {msg, binary}}
        ],
        [
            {name, command_seven}, %% name is different...
            {category, user},
            {desc, "do nothing and return"},
            {module, ?MODULE},
            {function, cmd_one},
            {action, read}, %% ...but another command with the same category and action is already registered
            {args, [{msg, binary}]},
            {result, {msg, binary}}
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

cmd_two(M) ->
    M.

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
    mongoose_commands:init(),
    mongoose_commands:register(commands_new()),
    receive
        _ -> ok
    end.

checkauth(true, AccessCommands, Auth) ->
    B = <<"bzzzz">>,
    B = ejabberd_commands:execute_command(AccessCommands, Auth, command_one, [B]);
checkauth(ErrMess, AccessCommands, Auth) ->
    B = <<"bzzzz">>,
    {error, ErrMess} = ejabberd_commands:execute_command(AccessCommands, Auth, command_one, [B]).
