%% @headerfile "mongoose_commands.hrl"
%%
%% @doc Mongoose version of command management
%% The following is loosely based on old ejabberd_commands implementation,
%% with some modification related to type check, permission control
%% and the likes.
%%
%% This is a central registry of commands which can be exposed via
%% REST, XMPP as ad-hoc commands or in any other way. Any module can
%% define its commands and register them here.
%%
%% ==== Usage ====
%%
%% A module defines a list of commands; a command definition is a proplist
%% with the following keys:
%%      name :: atom()
%% name of the command by which we refer to it
%%      category :: [atom()]
%% this defines what group the command belongs to, like user, chatroom etc
%%      desc :: string()
%% long description
%%      module :: module()
%% module to call
%%      function :: atom()
%% function to call
%%      action :: command_action()
%% so that the HTTP side can decide which verb to require
%%      args = [] :: [argspec()]
%% Type spec - see below; this is both for introspection and type check on call
%%      security_policy = [atom()] (optional)
%% permissions required to run this command, defaults to [admin]
%%      result :: argspec()
%% Type spec of return value of the function to call; execute/3 eventually returns {ok, result}
%%
%% Commands are registered here upon the module's initialisation
%% (the module has to explicitly call mongoose_commands:register_commands/1
%% func, it doesn't happen automagically), also should be unregistered when module
%% terminates.
%%
%% Commands are executed by calling mongoose_commands:execute/3 method. This
%% can return:
%% {ok, Result}
%% {error, denied, Msg} if user has no permission
%% {error, not_implemented, Msg}
%% {error, type_error, Msg} if either arguments or return value does not match
%% {error, internal, Msg} if an exception was caught
%%
%% ==== Type check ====
%%
%% A command's definition includes specification of it arguments; when
%% it is called, arguments are check for compatibility. Examples of specs
%% and compliant arguments:
%%
%% a single type spec
%% integer                          2
%% binary                           <<"zzz">>
%% atom                             brrr
%% a list of arbitrary length, of a given type
%% [integer]                        []
%% [integer]                        [1]
%% [integer]                        [1,2,3,4]
%% a list of anything
%% []
%% a named argument (name is only for clarity)
%% {msg, binary}                    <<"zzz">>
%% a tuple of args
%% {integer, binary, float}         {1, <<"2">>, 3.0}
%% a tuple of named args
%% {{x, integer}, {y, binary}}      {1, <<"bbb">>}
%%
%% Arg specification is used at call-time for control, and also for introspection
%% (see list/1, list/2, mongoose_commands:get_command/2 and args/1)
%%
%% Return value is also specified, and this is a bit tricky: command definition
%% contains spec of return value - what the target func returns should comply to it.
%% The registry, namely execute/3, returns a tuple {ok, ValueReturnedByTheFunction}
%%
%% Called function may also return a tuple {error, term()}, this is returned by the registry
%% as {error, internal, Msg::binary()}
%%
%% ==== Permission control ====
%%
%% First argument to every function exported from this module is always
%% a user. If you call it from trusted place, you can pass 'admin' here and
%% the whole permission check is skipped. Otherwise, pass #jid record.
%%
%% A command MAY define a security policy to be applied
%% (and this is not yet designed)
%% If it doesn't, then the command is accessible to 'admin' calls only.
%%

-module(mongoose_commands).
-author("bartlomiej.gorny@erlang-solutions.com").
-include("mongoose_commands.hrl").
-include("ejabberd.hrl").
-include("jlib.hrl").

-type mongoose_command() :: #mongoose_command{}.
-type caller() :: admin|jid().

%%%% API

-export([check_type/2]).
-export([init/0]).

-export([register/1,
         unregister/1,
         list/1,
         list/2,
         list/3,
         register_commands/1,
         unregister_commands/1,
         get_command/2,
         execute/3,
         name/1,
         category/1,
         desc/1,
         args/1,
         action/1,
         result/1
    ]).

%% @doc Register mongoose commands. This can be run by any module that wants its commands exposed.
-spec register([{atom(), term()}]) -> ok.
register(Cmds) ->
    init(),
    Commands = [check_command(C) || C <- Cmds],
    register_commands(Commands).

%% @doc Unregister mongoose commands. Should be run when module is unloaded.
-spec unregister([{atom(), term()}]) -> ok.
unregister(Cmds) ->
    Commands = [check_command(C) || C <- Cmds],
    unregister_commands(Commands).

%% @doc List commands, available for this user.
-spec list(atom()|jid()) -> [mongoose_command()].
list(U) ->
    list(U, any, any).

%% @doc List commands, available for this user, filtered by category.
-spec list(atom()|jid(), atom()) -> [mongoose_command()].
list(U, C) ->
    list(U, C, any).

%% @doc List commands, available for this user, filtered by category and action.
-spec list(atom()|jid(), atom(), atom()) -> [mongoose_command()].
list(admin, Category, Action) ->
    command_list(Category, Action);
list(_Caller, _Category, _Action) ->
    [].

%% @doc Get command definition, if allowed for this user.
-spec get_command(atom()|jid(), atom()) -> mongoose_command().
get_command(admin, Name) ->
    case ets:lookup(mongoose_commands, Name) of
        [C] -> C;
        [] -> {error, not_implemented, <<"Command not implemented">>}
    end;
get_command(_Caller, _Name) ->
    {error, denied, <<"Command not available">>}.

%% accessors
-spec name(mongoose_command()) -> atom().
name(Cmd) ->
    Cmd#mongoose_command.name.

-spec category(mongoose_command()) -> atom().
category(Cmd) ->
    Cmd#mongoose_command.category.

-spec desc(mongoose_command()) -> list().
desc(Cmd) ->
    Cmd#mongoose_command.desc.

-spec args(mongoose_command()) -> term().
args(Cmd) ->
    Cmd#mongoose_command.args.

-spec action(mongoose_command()) -> command_action().
action(Cmd) ->
    Cmd#mongoose_command.action.

-spec result(mongoose_command()) -> term().
result(Cmd) ->
    Cmd#mongoose_command.result.

%% @doc Command execution.
-spec execute(caller(), atom()|mongoose_command(), [term()]) -> {ok, term()} | failure().
execute(admin, Name, Args) when is_atom(Name) ->
    case ets:lookup(mongoose_commands, Name) of
        [Command] ->
            try check_and_execute(Command, Args) of
                Res ->
                    {ok, Res}
            catch
                {type_mismatch, E} ->
                    {error, type_error, E};
                X:E ->
                    ?ERROR_MSG("Caught ~p:~p while executing ~p", [X, E, Name]),
                    {error, internal, term_to_binary(E)}
            end;
        [] -> {error, not_implemented, <<"This command is not supported">>}
    end;
execute(Caller, #mongoose_command{name = Name} = Command, Args) ->
    execute(Caller, Name, Args);
execute(_Caller, _Command, _Args) ->
    {error, denied, <<"currently only admin is supported">>}.

init() ->
    case ets:info(mongoose_commands) of
        undefined ->
            ets:new(mongoose_commands, [named_table, set, public,
                {keypos, #mongoose_command.name}]);
        _ ->
            ok
    end.

%%%% end of API

-spec register_commands([mongoose_command()]) -> ok.
register_commands(Commands) ->
    lists:foreach(
        fun(Command) ->
            check_registration(Command), %% may throw
            ets:insert_new(mongoose_commands, Command),
            ok
        end,
        Commands).


-spec unregister_commands([mongoose_command()]) -> ok.
unregister_commands(Commands) ->
    lists:foreach(
        fun(Command) ->
            ets:delete_object(mongoose_commands, Command)
        end,
        Commands).


-spec check_and_execute(mongoose_command(), [term()]) -> term().
check_and_execute(Command, Args) when is_map(Args) ->
    check_and_execute(Command, map_to_list(Args, Command#mongoose_command.args));
check_and_execute(Command, Args) ->
    SpecLen = length(Command#mongoose_command.args),
    ALen = length(Args),
    if SpecLen =/= ALen ->
        th("Invalid number of arguments: should be ~p, got ~p", [SpecLen, ALen]);
        true -> ok
    end,
    [check_type(S, A) || S <- Command#mongoose_command.args, A <- Args],
    Res = apply(Command#mongoose_command.module, Command#mongoose_command.function, Args),
    case Res of
        {error, E} ->
            throw({func_returned_error, E});
        _ ->
            % transitional
            ResSpec = case Command#mongoose_command.result of
                            {ok, R} -> R;
                            R -> R
                      end,
            check_type(ResSpec, Res),
            Res
    end.


check_type(A, A) ->
    true;
check_type({_Name, binary}, Value) when is_binary(Value) ->
    true;
check_type({_Name, integer}, Value) when is_integer(Value) ->
    true;
check_type(Spec, Value) when is_tuple(Spec) and not is_tuple(Value) ->
    th("~p is not a tuple", [Value]);
check_type(Spec, Value) when is_tuple(Spec) ->
    compare_tuples(Spec, Value);
check_type([_Spec], []) ->
    true;
check_type([], [_|_]) ->
    true;
check_type([], []) ->
    true;
check_type([Spec], [H|T]) ->
    check_type({none, Spec}, H),
    check_type([Spec], T);
check_type([], [_|_]) ->
    true;
check_type([], []) ->
    true;
check_type(Spec, Value) ->
    th("Catch-all: ~p vs ~p", [Spec, Value]).


compare_tuples(Spec, Val) ->
    Ssize = tuple_size(Spec),
    Vsize = tuple_size(Val),
    case Ssize of
        Vsize ->
            compare_lists(tuple_to_list(Spec), tuple_to_list(Val));
        _ ->
            th("Tuples of different size: ~p and ~p", [Spec, Val])
    end.


compare_lists([], []) ->
    true;
compare_lists([S|Sp], [V|Val]) ->
    check_type(S, V),
    compare_lists(Sp, Val).


th(Fmt, V) ->
    throw({type_mismatch, io_lib:format(Fmt, V)}).


check_command(PL) ->
    Fields = record_info(fields, mongoose_command),
    Lst = check_command([], PL, Fields),
    RLst = lists:reverse(Lst),
    list_to_tuple([mongoose_command|RLst]).

check_command(Cmd, _PL, []) ->
    Cmd;
check_command(Cmd, PL, [N|Tail]) ->
    V = proplists:get_value(N, PL),
    Val = check_value(N, V),
    check_command([Val|Cmd], PL, Tail).


check_value(name, V) when is_atom(V) ->
    V;
check_value(category, V) when is_atom(V) ->
    V;
check_value(desc, V) when is_list(V) ->
    V;
check_value(module, V) when is_atom(V) ->
    V;
check_value(function, V) when is_atom(V) ->
    V;
check_value(action, read) ->
    read;
check_value(action, send) ->
    send;
check_value(action, create) ->
    create;
check_value(action, update) ->
    update;
check_value(action, delete) ->
    delete;
check_value(args, V) when is_list(V) ->
    Filtered = [C || {C, _} <- V],
    if
        length(V) =/= length(Filtered) -> baddef(args, V);
        true -> V
    end;
check_value(security_policy, undefined) ->
    [admin];
check_value(security_policy, []) ->
    baddef(security_policy, []);
check_value(security_policy, V) when is_list(V) ->
    V;
check_value(result, undefined) ->
    baddef(result, undefined);
check_value(result, V) ->
    V;
check_value(K, V) ->
    baddef(K, V).

baddef(K, V) ->
    throw({invalid_command_definition, io_lib:format("~p=~p", [K, V])}).

command_list(Category, Action) ->
    Cmds = [C || [C] <- ets:match(mongoose_commands, '$1')],
    filter_commands(Category, Action, Cmds).

filter_commands(any, any, Cmds) ->
    Cmds;
filter_commands(Cat, any, Cmds) ->
    [C || C <- Cmds, C#mongoose_command.category == Cat];
filter_commands(any, _, _) ->
    throw({invalid_filter, ""});
filter_commands(Cat, Action, Cmds) ->
    [C || C <- Cmds, C#mongoose_command.category == Cat, C#mongoose_command.action == Action].

%% @doc make sure the command may be registered
%% it may not if either (a) command of that name is already registered,
%% (b) there is a command in the same category with the same action
check_registration(Command) ->
    Name = name(Command),
    Cat = category(Command),
    Act = action(Command),
    case ets:lookup(mongoose_commands, Name) of
        [] ->
            CatLst = list(admin, Cat),
            FCatLst = [C || C <- CatLst, C#mongoose_command.action == Act],
            case FCatLst of
                [] -> ok;
                [C] ->
                    baddef("There is command ~p in category ~p, action ~p", [name(C), Cat, Act])
            end;
        _ ->
            ?DEBUG("This command is already defined:~n~p", [Name])
    end.

mapget(K, Map) ->
    try maps:get(K, Map) of
        V -> V
    catch error:{badkey, K} ->
        th("Missing argument: ~p", [K])
    end.

map_to_list(Map, Args) ->
    SpecLen = length(Args),
    ALen = maps:size(Map),
    if SpecLen =/= ALen ->
        th("Invalid number of arguments: should be ~p, got ~p", [SpecLen, ALen]);
        true -> ok
    end,
    [mapget(K, Map) || {K, _} <- Args].

