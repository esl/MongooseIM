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
        {group, new_commands},
        {group, new_commands_enc} % "enc" stands for encapsulated
    ].

groups() ->
    [
        {old_commands, [sequence],
            [
                old_list,
                old_exec,
                old_access_ctl
            ]
        },
        {new_commands, [sequence],
            [
                new_type_checker,
                new_list,
                new_execute
            ]
        },
        {new_commands_enc, [sequence],
            [
                new_failedreg_enc,
                new_list_enc,
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
    C;
init_per_group(new_commands_enc, C) ->
    spawn(fun mc_enc_holder/0),
    C.

end_per_group(_, C) ->
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
    ok.

t_check_type(Spec, Value) ->
    R = try mongoose_commands:check_type(Spec, Value) of
            true -> true
        catch
            E ->
                {false, E}
        end,
    R.


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


new_list(_C) ->
    %% list
    Rlist = mongoose_commands:list_commands(admin),
    {command_one, _, "do nothing and return"} = proplists:lookup(command_one, Rlist),
    %% list for a user
    [] = mongoose_commands:list_commands(a_user),
    %% get definition
    Rget = mongoose_commands:get_command_definition(admin, command_one),
    % we should get back exactly the definition we provided
    [Cone | _] = commands_new(),
    Cone = Rget,
    {error, denied, _} = mongoose_commands:get_command_definition(a_user, command_one),
    ok.

new_execute(_C) ->
    {ok, <<"bzzzz">>} = mongoose_commands:execute(admin, command_one, [<<"bzzzz">>]),
    {error, denied, _} = mongoose_commands:execute(a_user, command_one, [<<"bzzzz">>]),
    {error, not_implemented, _} = mongoose_commands:execute(admin, command_seven, [<<"bzzzz">>]),
    {error, type_error, _} = mongoose_commands:execute(admin, command_one, [123]),
    {error, type_error, _} = mongoose_commands:execute(admin, command_one, []),
    {error, internal, _} = mongoose_commands:execute(admin, command_one, [<<"throw">>]),
    ExpError = term_to_binary({func_returned_error, byleco}),
    {error, internal, ExpError} = mongoose_commands:execute(admin, command_one, [<<"error">>]),
    ok.

failedreg([]) -> ok;
failedreg([Cmd|Tail]) ->
    ?assertThrow({invalid_command_definition, _}, mongoose_commands:register([Cmd])),
    failedreg(Tail).

new_failedreg_enc(_C) ->
    failedreg(commands_new_lame_enc()).

new_list_enc(_C) ->
    Rlist = mongoose_commands:list(admin),
    #{name := command_one, desc := "do nothing and return"} = proplists:get_value(command_one, Rlist),
    %% list for a user
    [] = mongoose_commands:list_commands(a_user),
    %% get definition
    Rget = mongoose_commands:get_command(admin, command_one),
    command_one = maps:get(name, Rget),
    get = maps:get(action, Rget),
    {error, denied, _} = mongoose_commands:get_command_definition(a_user, command_one),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% definitions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

commands_new() ->
    [
        #mongoose_command{name = command_one, tags = [roster],
            desc = "do nothing and return",
            module = ?MODULE, function = cmd_one,
            action = get,
            args = [{msg, binary}],
            result = {ok, {msg, binary}}
        }
    ].

commands_new_enc() ->
    [
        [
            {name, command_one},
            {tags, [roster]},
            {desc, "do nothing and return"},
            {module, ?MODULE},
            {function, cmd_one},
            {action, get},
            {args, [{msg, binary}]},
            {result, {msg, binary}}
        ]
    ].

commands_new_lame_enc() ->
    [
        [
            {name, command_one} % missing values
        ],
        [
            {name, command_one},
            {tags, roster} %% should be a list
        ],
        [
            {name, command_one},
            {tags, [roster]},
            {desc, "do nothing and return"},
            {module, ?MODULE},
            {function, cmd_one},
            {action, andnowforsomethingcompletelydifferent} %% not one of allowed values
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
    mongoose_commands:register_commands(commands_new()),
    receive
        _ -> ok
    end.

mc_enc_holder() ->
    mongoose_commands:init(),
    mongoose_commands:register(commands_new_enc()),
    receive
        _ -> ok
    end.

checkauth(true, AccessCommands, Auth) ->
    B = <<"bzzzz">>,
    B = ejabberd_commands:execute_command(AccessCommands, Auth, command_one, [B]);
checkauth(ErrMess, AccessCommands, Auth) ->
    B = <<"bzzzz">>,
    {error, ErrMess} = ejabberd_commands:execute_command(AccessCommands, Auth, command_one, [B]).
