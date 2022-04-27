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
%%      category :: binary()
%% this defines what group the command belongs to, like user, chatroom etc
%%      desc :: binary()
%% long description
%%      module :: module()
%% module to call
%%      function :: atom()
%% function to call
%%      action :: command_action()
%% so that the HTTP side can decide which verb to require
%%      args = [] :: [argspec()]
%% Type spec - see below; this is both for introspection and type check on call. Args spec is more
%% limited then return, it has to be a list of named arguments, like [{id, integer}, {msg, binary}]
%%      security_policy = [atom()] (optional)
%% permissions required to run this command, defaults to [admin]
%%      result :: argspec()
%% Type spec of return value of the function to call; execute/3 eventually returns {ok, result}
%%      identifiers :: [atom()] (optional, required in 'update' commands)
%%      optargs :: [{atom(), type(), term()] (optional args with type and default value.
%% Then a command is called, it fills missing arguments with values from here.
%% We have then two arities: arity of a command, which is only its required arguments,
%% and arity of the function to be called, which is required args + optional args.
%%
%% You can ignore return value of the target func by specifying return value as {result, ok}. The
%% execute/3 will then always return just 'ok' (or error).
%%
%% If action is 'update' then it MUST specify which args are to be used as identifiers of object
%% to update. It has no effect on how the engine does its job, but may be used by client code
%% to enforce proper structure of request. (this is bad programming practice but we didn't have
%% a better idea, we had to solve it for REST API)
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
%% ```
%% a single type spec
%% integer                          2
%% binary                           <<"zzz">>
%% atom                             brrr
%% a list of arbitrary length, of a given type
%% [integer]                        []
%% [integer]                        [1]
%% [integer]                        [1, 2, 3, 4]
%% a list of anything
%% []
%% a named argument (name is only for clarity)
%% {msg, binary}                    <<"zzz">>
%% a tuple of args
%% {integer, binary, float}         {1, <<"2">>, 3.0}
%% a tuple of named args
%% {{x, integer}, {y, binary}}      {1, <<"bbb">>}
%% '''
%%
%% Arg specification is used at call-time for control, and also for introspection
%% (see list/1, list/2, mongoose_commands:get_command/2 and args/1)
%%
%% Return value is also specified, and this is a bit tricky: command definition
%% contains spec of return value - what the target func returns should comply to it.
%% The registry, namely execute/3, returns a tuple {ok, ValueReturnedByTheFunction}
%% If return value is defined as 'ok' then whatever target func returns is ignored.
%% This is mostly to make a distinction between 'create' actions which actually create something
%% and return its identifier and those 'lame creators' which cause some action to be done and
%% something written to dbase (exemplum: sending a message), but there is no accessible resource.
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
-include("mongoose.hrl").
-include("jlib.hrl").

-record(mongoose_command,
        {
         %% name of the command by which we refer to it
         name :: atom(),
         %% groups commands related to the same functionality (user managment, messages/archive)
         category :: binary(),
         %% optimal subcategory
         subcategory = undefined :: undefined | binary(),
         %% long description
         desc :: binary(),
         %% module to call
         module :: module(),
         %% function to call
         function :: atom(),
         %% so that the HTTP side can decide which verb to require
         action :: action(),
         %% this is both for introspection and type check on call
         args = [] :: [argspec()],
         %% arg which has a default value and is optional
         optargs = [] :: [optargspec()],
         %% internal use
         caller_pos :: integer(),
         %% resource identifiers, a subset of args
         identifiers = [] :: [atom()],
         %% permissions required to run this command
         security_policy = [admin] :: security(),
         %% what the called func should return; if ok then return of called function is ignored
         result :: argspec()|ok
        }).

-opaque t() :: #mongoose_command{}.
-type caller() :: admin | binary() | user.
-type action() :: create | read | update | delete. %% just basic CRUD; sending a mesage is 'create'

-type typedef() :: [typedef_basic()] | typedef_basic().

-type typedef_basic() :: boolean | integer | binary | float. %% most basic primitives, string is a binary

-type argspec() :: typedef()
                  | {atom(), typedef()} %% a named argument
                  | {argspec()} % a tuple of a few args (can be of any size)
                  | [typedef()]. % a list, but one element

-type optargspec() :: {atom(), typedef(), term()}. % name, type, default value

-type security() :: [admin | user]. %% later acl option will be added

-type errortype() :: denied | not_implemented | bad_request | type_error | not_found | internal.
-type errorreason() :: term().

-type args() :: [{atom(), term()}] | map().
-type failure() :: {error, errortype(), errorreason()}.
-type success() :: ok | {ok, term()}.

-export_type([t/0]).
-export_type([caller/0]).
-export_type([action/0]).
-export_type([argspec/0]).
-export_type([optargspec/0]).
-export_type([errortype/0]).
-export_type([errorreason/0]).
-export_type([failure/0]).

-type command_properties() :: [{atom(), term()}].

%%%% API

-export([check_type/3]).
-export([init/0]).

-export([register/1,
         unregister/1,
         list/1,
         list/2,
         list/3,
         list/4,
         register_commands/1,
         unregister_commands/1,
         new/1,
         get_command/2,
         execute/3,
         name/1,
         category/1,
         subcategory/1,
         desc/1,
         args/1,
         optargs/1,
         arity/1,
         func_arity/1,
         identifiers/1,
         action/1,
         result/1
    ]).

-ignore_xref([check_type/3, func_arity/1, get_command/2, list/3, new/1,
              register_commands/1, unregister_commands/1, result/1]).

%% @doc creates new command object based on provided proplist
-spec new(command_properties()) -> t().
new(Props) ->
    Fields = record_info(fields, mongoose_command),
    Lst = check_command([], Props, Fields),
    RLst = lists:reverse(Lst),
    Cmd = list_to_tuple([mongoose_command|RLst]),
    check_identifiers(Cmd#mongoose_command.action,
                      Cmd#mongoose_command.identifiers,
                      Cmd#mongoose_command.args),
    % store position of caller in args (if present)
    Cmd#mongoose_command{caller_pos = locate_caller(Cmd#mongoose_command.args)}.


%% @doc Register mongoose commands. This can be run by any module that wants its commands exposed.
-spec register([command_properties()]) -> ok.
register(Cmds) ->
    Commands = [new(C) || C <- Cmds],
    register_commands(Commands).

%% @doc Unregister mongoose commands. Should be run when module is unloaded.
-spec unregister([command_properties()]) -> ok.
unregister(Cmds) ->
    Commands = [new(C) || C <- Cmds],
    unregister_commands(Commands).

%% @doc List commands, available for this user.
-spec list(caller()) -> [t()].
list(U) ->
    list(U, any, any, any).

%% @doc List commands, available for this user, filtered by category.
-spec list(caller(), binary() | any) -> [t()].
list(U, C) ->
    list(U, C, any, any).

%% @doc List commands, available for this user, filtered by category and action.
-spec list(caller(), binary() | any, atom()) -> [t()].
list(U, Category, Action) ->
    list(U, Category, Action, any).

%% @doc List commands, available for this user, filtered by category, action and subcategory
-spec list(caller(), binary() | any, atom(), binary() | any | undefined) -> [t()].
list(U, Category, Action, SubCategory) ->
    CL = command_list(Category, Action, SubCategory),
    lists:filter(fun(C) -> is_available_for(U, C) end, CL).

%% @doc Get command definition, if allowed for this user.
-spec get_command(caller(), atom()) -> t().
get_command(Caller, Name) ->
    case ets:lookup(mongoose_commands, Name) of
        [C] ->
            case is_available_for(Caller, C) of
                true ->
                    C;
                false ->
                    {error, denied, <<"Command not available">>}
            end;
        [] -> {error, not_implemented, <<"Command not implemented">>}
    end.

%% accessors
-spec name(t()) -> atom().
name(Cmd) ->
    Cmd#mongoose_command.name.

-spec category(t()) -> binary().
category(Cmd) ->
    Cmd#mongoose_command.category.

-spec subcategory(t()) -> binary() | undefined.
subcategory(Cmd) ->
    Cmd#mongoose_command.subcategory.

-spec desc(t()) -> binary().
desc(Cmd) ->
    Cmd#mongoose_command.desc.

-spec args(t()) -> term().
args(Cmd) ->
    Cmd#mongoose_command.args.

-spec optargs(t()) -> term().
optargs(Cmd) ->
    Cmd#mongoose_command.optargs.

-spec identifiers(t()) -> [atom()].
identifiers(Cmd) ->
    Cmd#mongoose_command.identifiers.

-spec action(t()) -> action().
action(Cmd) ->
    Cmd#mongoose_command.action.

-spec result(t()) -> term().
result(Cmd) ->
    Cmd#mongoose_command.result.

-spec arity(t()) -> integer().
arity(Cmd) ->
    length(Cmd#mongoose_command.args).

-spec func_arity(t()) -> integer().
func_arity(Cmd) ->
    length(Cmd#mongoose_command.args) + length(Cmd#mongoose_command.optargs).

%% @doc Command execution.
-spec execute(caller(), atom() | t(), args()) ->
        success() | failure().
execute(Caller, Name, Args) when is_atom(Name) ->
    case ets:lookup(mongoose_commands, Name) of
        [Command] -> execute_command(Caller, Command, Args);
        [] -> {error, not_implemented, {command_not_supported, Name, sizeof(Args)}}
    end;
execute(Caller, #mongoose_command{name = Name}, Args) ->
    execute(Caller, Name, Args).

init() ->
    ets:new(mongoose_commands, [named_table, set, public,
                                {keypos, #mongoose_command.name}]).

%%%% end of API
-spec register_commands([t()]) -> ok.
register_commands(Commands) ->
    lists:foreach(
        fun(Command) ->
            check_registration(Command), %% may throw
            ets:insert_new(mongoose_commands, Command),
            mongoose_hooks:register_command(Command),
            ok
        end,
        Commands).

-spec unregister_commands([t()]) -> ok.
unregister_commands(Commands) ->
    lists:foreach(
        fun(Command) ->
            ets:delete_object(mongoose_commands, Command),
            mongoose_hooks:unregister_command(Command)
        end,
        Commands).

-spec execute_command(caller(), atom() | t(), args()) ->
    success() | failure().
execute_command(Caller, Command, Args) ->
    try check_and_execute(Caller, Command, Args) of
        ignore_result ->
            ok;
        {error, Type, Reason} ->
            {error, Type, Reason};
        {ok, Res} ->
            {ok, Res}
    catch
        % admittedly, not the best style of coding, in Erlang at least. But we have to do plenty
        % of various checks, and in absence of something like Elixir's "with" construct we are
        % facing a choice between throwing stuff or using some more or less tortured syntax
        % to chain these checks.
        throw:{Type, Reason} ->
            {error, Type, Reason};
        Class:Reason:Stacktrace ->
            Err = #{what => command_failed,
                    command_name => Command#mongoose_command.name,
                    caller => Caller, args => Args,
                    class => Class, reason => Reason, stacktrace => Stacktrace},
            ?LOG_ERROR(Err),
            {error, internal, term_to_readable_binary(Err)}
    end.

term_to_readable_binary(X) ->
    iolist_to_binary(io_lib:format("~0p", [X])).

add_defaults(Args, Opts) when is_map(Args) ->
    COpts = [{K, V} || {K, _, V} <- Opts],
    Missing = lists:subtract(proplists:get_keys(Opts), maps:keys(Args)),
    lists:foldl(fun(K, Ar) -> maps:put(K, proplists:get_value(K, COpts), Ar) end,
        Args, Missing).

% @doc This performs many checks - types, permissions etc, may throw one of many exceptions
%% returns what the func returned or just ok if command spec tells so
-spec check_and_execute(caller(), t(), args()) -> success() | failure() | ignore_result.
check_and_execute(Caller, Command, Args) when is_map(Args) ->
    Args1 = add_defaults(Args, Command#mongoose_command.optargs),
    ArgList = maps_to_list(Args1, Command#mongoose_command.args, Command#mongoose_command.optargs),
    check_and_execute(Caller, Command, ArgList);
check_and_execute(Caller, Command, Args) ->
    % check permissions
    case is_available_for(Caller, Command) of
        true ->
            ok;
        false ->
            throw({denied, "Command not available for this user"})
        end,
    % check caller (if it is given in args, and the engine is called by a 'real' user, then it
    % must match
    check_caller(Caller, Command, Args),
    % check args
    % this is the 'real' spec of command - optional args included
    FullSpec = Command#mongoose_command.args
               ++ [{K, T} || {K, T, _} <- Command#mongoose_command.optargs],
    SpecLen = length(FullSpec),
    ALen = length(Args),
    case SpecLen =/= ALen of
        true ->
            type_error(argument, "Invalid number of arguments: should be ~p, got ~p", [SpecLen, ALen]);
        _ -> ok
    end,
    [check_type(argument, S, A) || {S, A} <- lists:zip(FullSpec, Args)],
    % run command
    Res = apply(Command#mongoose_command.module, Command#mongoose_command.function, Args),
    case Res of
        {error, Type, Reason} ->
            {error, Type, Reason};
        _ ->
            case Command#mongoose_command.result of
                ok ->
                    ignore_result;
                ResSpec ->
                    check_type(return, ResSpec, Res),
                    {ok, Res}
            end
    end.

check_type(_, ok, _) ->
    ok;
check_type(_, A, A) ->
    true;
check_type(_, {_Name, boolean}, Value) when is_boolean(Value) ->
    true;
check_type(Mode, {Name, boolean}, Value) ->
    type_error(Mode, "For ~p expected boolean, got ~p", [Name, Value]);
check_type(_, {_Name, binary}, Value) when is_binary(Value) ->
    true;
check_type(Mode, {Name, binary}, Value) ->
    type_error(Mode, "For ~p expected binary, got ~p", [Name, Value]);
check_type(_, {_Name, integer}, Value) when is_integer(Value) ->
    true;
check_type(Mode, {Name, integer}, Value) ->
    type_error(Mode, "For ~p expected integer, got ~p", [Name, Value]);
check_type(Mode, {_Name, [_] = LSpec}, Value) when is_list(Value) ->
    check_type(Mode, LSpec, Value);
check_type(Mode, Spec, Value) when is_tuple(Spec) and not is_tuple(Value) ->
    type_error(Mode, "~p is not a tuple", [Value]);
check_type(Mode, Spec, Value) when is_tuple(Spec) ->
    compare_tuples(Mode, Spec, Value);
check_type(_, [_Spec], []) ->
    true;
check_type(Mode, [Spec], [H|T]) ->
    check_type(Mode, {none, Spec}, H),
    check_type(Mode, [Spec], T);
check_type(_, [], [_|_]) ->
    true;
check_type(_, [], []) ->
    true;
check_type(Mode, Spec, Value) ->
    type_error(Mode, "Catch-all: ~p vs ~p", [Spec, Value]).

compare_tuples(Mode, Spec, Val) ->
    Ssize = tuple_size(Spec),
    Vsize = tuple_size(Val),
    case Ssize of
        Vsize ->
            compare_lists(Mode, tuple_to_list(Spec), tuple_to_list(Val));
        _ ->
            type_error(Mode, "Tuples of different size: ~p and ~p", [Spec, Val])
    end.

compare_lists(_, [], []) ->
    true;
compare_lists(Mode, [S|Sp], [V|Val]) ->
    check_type(Mode, S, V),
    compare_lists(Mode, Sp, Val).

type_error(argument, Fmt, V) ->
    throw({type_error, io_lib:format(Fmt, V)});
type_error(return, Fmt, V) ->
    throw({internal, io_lib:format(Fmt, V)}).

check_identifiers(update, [], _) ->
    baddef(identifiers, empty);
check_identifiers(update, Ids, Args) ->
    check_identifiers(Ids, Args);
check_identifiers(_, _, _) ->
    ok.

check_identifiers([], _) ->
    ok;
check_identifiers([H|T], Args) ->
    case proplists:get_value(H, Args) of
        undefined -> baddef(H, missing);
        _ -> check_identifiers(T, Args)
    end.

check_command(Cmd, _PL, []) ->
    Cmd;
check_command(Cmd, PL, [N|Tail]) ->
    V = proplists:get_value(N, PL),
    Val = check_value(N, V),
    check_command([Val|Cmd], PL, Tail).

check_value(name, V) when is_atom(V) ->
    V;
check_value(category, V) when is_binary(V) ->
    V;
check_value(subcategory, V) when is_binary(V) ->
    V;
check_value(subcategory, undefined) ->
    undefined;
check_value(desc, V) when is_binary(V) ->
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
    ArgCount = length(V),
    case length(Filtered) of
        ArgCount -> V;
        _ -> baddef(args, V)
    end;
check_value(security_policy, undefined) ->
    [admin];
check_value(security_policy, []) ->
    baddef(security_policy, empty);
check_value(security_policy, V) when is_list(V) ->
    lists:map(fun check_security_policy/1, V),
    V;
check_value(result, undefined) ->
    baddef(result, undefined);
check_value(result, V) ->
    V;
check_value(identifiers, undefined) ->
    [];
check_value(identifiers, V) ->
    V;
check_value(optargs, undefined) ->
    [];
check_value(optargs, V) ->
    V;
check_value(caller_pos, _) ->
    0;
check_value(K, V) ->
    baddef(K, V).

%% @doc Known security policies
check_security_policy(user) ->
    ok;
check_security_policy(admin) ->
    ok;
check_security_policy(Other) ->
    baddef(security_policy, Other).

baddef(K, V) ->
    throw({invalid_command_definition, io_lib:format("~p=~p", [K, V])}).

command_list(Category, Action, SubCategory) ->
    Cmds = [C || [C] <- ets:match(mongoose_commands, '$1')],
    filter_commands(Category, Action, SubCategory, Cmds).

filter_commands(any, any, _, Cmds) ->
    Cmds;
filter_commands(Cat, any, _, Cmds) ->
    [C || C <- Cmds, C#mongoose_command.category == Cat];
filter_commands(any, _, _, _) ->
    throw({invalid_filter, ""});
filter_commands(Cat, Action, any, Cmds) ->
    [C || C <- Cmds, C#mongoose_command.category == Cat,
                     C#mongoose_command.action == Action];
filter_commands(Cat, Action, SubCategory, Cmds) ->
    [C || C <- Cmds, C#mongoose_command.category == Cat,
                     C#mongoose_command.action == Action,
                     C#mongoose_command.subcategory == SubCategory].


%% @doc make sure the command may be registered
%% it may not if either (a) command of that name is already registered,
%% (b) there is a command in the same category and subcategory with the same action and arity
check_registration(Command) ->
    Name = name(Command),
    Cat = category(Command),
    Act = action(Command),
    Arity = arity(Command),
    SubCat = subcategory(Command),
    case ets:lookup(mongoose_commands, Name) of
        [] ->
            CatLst = list(admin, Cat),
            FCatLst = [C || C <- CatLst, action(C) == Act,
                                         subcategory(C) == SubCat,
                                         arity(C) == Arity],
            case FCatLst of
                [] -> ok;
                [C] ->
                    baddef("There is a command ~p in category ~p and subcategory ~p, action ~p",
                           [name(C), Cat, SubCat, Act])
            end;
        Other ->
            ?LOG_DEBUG(#{what => command_conflict,
                         text => <<"This command is already defined">>,
                         command_name => Name, registered => Other}),
            ok
    end.

mapget(K, Map) ->
    try maps:get(K, Map) of
        V -> V
    catch
        error:{badkey, K} ->
            type_error(argument, "Missing argument: ~p", [K]);
        error:bad_key ->
            type_error(argument, "Missing argument: ~p", [K])
    end.

maps_to_list(Map, Args, Optargs) ->
    SpecLen = length(Args) + length(Optargs),
    ALen = maps:size(Map),
    case SpecLen of
        ALen -> ok;
        _ -> type_error(argument, "Invalid number of arguments: should be ~p, got ~p", [SpecLen, ALen])
    end,
    [mapget(K, Map) || {K, _} <- Args] ++ [mapget(K, Map) || {K, _, _} <- Optargs].

%% @doc Main entry point for permission control - is this command available for this user
is_available_for(User, C) when is_binary(User) ->
    is_available_for(jid:from_binary(User), C);
is_available_for(admin, _C) ->
    true;
is_available_for(Jid, #mongoose_command{security_policy = Policies}) ->
    apply_policies(Policies, Jid).

%% @doc Check all security policies defined in the command - passes if any of them returns true
apply_policies([], _) ->
    false;
apply_policies([P|Policies], Jid) ->
    case apply_policy(P, Jid) of
        true ->
            true;
        false ->
            apply_policies(Policies, Jid)
    end.

%% @doc This is the only policy we know so far, but there will be others (like roles/acl control)
apply_policy(user, _) ->
    true;
apply_policy(_, _) ->
    false.

locate_caller(L) ->
    locate_caller(1, L).

locate_caller(_I, []) ->
    0;
locate_caller(I, [{caller, _}|_]) ->
    I;
locate_caller(I, [_|T]) ->
    locate_caller(I + 1, T).

check_caller(admin, _Command, _Args) ->
    ok;
check_caller(_Caller, #mongoose_command{caller_pos = 0}, _Args) ->
    % no caller in args
    ok;
check_caller(Caller, #mongoose_command{caller_pos = CallerPos}, Args) ->
    % check that server and user match (we don't care about resource)
    ACaller = lists:nth(CallerPos, Args),
    CallerJid = jid:from_binary(Caller),
    ACallerJid = jid:from_binary(ACaller),
    case jid:are_bare_equal(CallerJid, ACallerJid) of
        true ->
            ok;
        _ ->
            throw({denied, "Caller ids do not match"})
    end.

sizeof(#{} = M) -> maps:size(M);
sizeof([_|_] = L) -> length(L).
