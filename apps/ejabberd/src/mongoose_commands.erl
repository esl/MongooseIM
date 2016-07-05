%% @headerfile "mongoose_commands.hrl"
%%
%% @doc Mongoose version of command management
%% The following is based on old ejabberd_commands implementation,
%% with some modification related to type check, permission control
%% and the likes.
%%
%% This is a central registry of commands which can be exposed via
%% REST, XMPP as ad-hoc commands or in any other way. Any module can
%% define its commands and register them here.
%%
%% ==== Usage ====
%%
%% A module defines a list of #mongoose_command records. The record is
%% defined and described in the header file.
%%
%% Commands are then registered here upon the module's initialisation
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
%% a named argument (name is only for clarity)
%% {msg, binary}                    <<"zzz">>
%% a tuple of args
%% {integer, binary, float}         {1, <<"2">>, 3.0}
%% a tuple of named args
%% {{x, integer}, {y, binary}}      {1, <<"bbb">>}
%%
%% Arg specification is used at call-time for control, and also for introspection
%% (mongoose_commands:get_command_definition/2)
%%
%% Return value is also specified, and this is a bit tricky: command definition
%% contains spec of return value, which MUST be a tuple {ok, term()}, where
%% the "term()" is what the called function is expected to return. The
%% execute/3 method will return {ok, term()}. Return value is checked
%% and the registry returns an error if it does not match.
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
%% A command must define a security policy to be applied
%% (and this is not yet designed)
%%


-module(mongoose_commands).
-author("bartlomiej.gorny@erlang-solutions.com").
-include("mongoose_commands.hrl").
-include("ejabberd.hrl").
-include("jlib.hrl").

-type mongoose_command() :: #mongoose_command{}.
-type caller() :: admin|jid().

-define(PRT(X, Y), ct:pal("~p: ~p", [X, Y])).

%% API
-export([check_type/2]).
-export([init/0,
    list_commands/1,
    get_command_definition/2,
    register_commands/1,
    unregister_commands/1,
    execute/3
]).

%% Encapsulated API
-export([register/1, list/1, get_command/2]).

register(Cmds) ->
    Commands = [check_command(C) || C <- Cmds],
    register_commands(Commands).

list(U) ->
    [{Name, #{name => Name, args => Args, desc => Desc}} || {Name, Args, Desc} <- list_commands(U)].

get_command(admin, Name) ->
    case ets:lookup(mongoose_commands, Name) of
        [C] ->
            #{name => Name, desc => C#mongoose_command.desc, args => C#mongoose_command.args,
                action => C#mongoose_command.action, result => {ok, C#mongoose_command.result}};
        [] -> {error, not_implemented, <<"Command not implemented">>}
    end;
get_command(_Caller, _Name) ->
    {error, denied, <<"Command not available">>}.

%% execute stays the same

%% end of encapsulated API

init() ->
    case ets:info(mongoose_commands) of
        undefined ->
            ets:new(mongoose_commands, [named_table, set, public,
                {keypos, #mongoose_command.name}]);
        _ ->
            ok
    end.


%% @doc Get a list of all the available commands, arguments and description.
-spec list_commands(caller()) -> [{atom(), [argspec()], binary()}].
list_commands(admin) ->
    Commands = ets:match(mongoose_commands,
        #mongoose_command{name = '$1',
            args = '$2',
            desc = '$3',
            _ = '_'}),
    [{A, B, C} || [A, B, C] <- Commands];
list_commands(_Caller) ->
    [].


%% @doc Get the definition record of a command.
-spec get_command_definition(caller(), atom()) -> mongoose_command() | failure().
get_command_definition(admin, Name) ->
    case ets:lookup(mongoose_commands, Name) of
        [E] -> E;
        [] -> {error, not_implemented, <<"Command not implemented">>}
    end;
get_command_definition(_Caller, _Name) ->
    {error, denied, <<"Command not available">>}.


%% @doc Register mongoose commands. This can be run by any module that wants its commands exposed.
-spec register_commands([mongoose_command()]) -> ok.
register_commands(Commands) ->
    lists:foreach(
        fun(Command) ->
            case ets:insert_new(mongoose_commands, Command) of
                true ->
                    ok;
                false ->
                    ?DEBUG("This command is already defined:~n~p", [Command])
            end
        end,
        Commands).


%% @doc Unregister mongoose commands. A twin brother of register_commands/1.
-spec unregister_commands([mongoose_command()]) -> ok.
unregister_commands(Commands) ->
    lists:foreach(
        fun(Command) ->
            ets:delete_object(mongoose_commands, Command)
        end,
        Commands).


%% @doc Command execution.
-spec execute(caller(), atom(), [term()]) -> {ok, term()} | failure().
execute(admin, Name, Args) ->
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
execute(_Caller, _Command, _Args) ->
    {error, denied, <<"currently only admin is supported">>}.


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
check_type([Spec], [H|T]) ->
    check_type({none, Spec}, H),
    check_type([Spec], T);
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
%%    Fields = [name, tags, desc, mf, action, args, security_policy, result],
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
check_value(tags, V) when is_list(V) ->
    V;
check_value(tags, undefined) ->
    [];
check_value(desc, V) when is_list(V) ->
    V;
check_value(module, V) when is_atom(V) ->
    V;
check_value(function, V) when is_atom(V) ->
    V;
check_value(action, get) ->
    get;
check_value(action, send) ->
    send;
check_value(action, set) ->
    set;
check_value(action, delete) ->
    delete;
check_value(args, V) when is_list(V) ->
    V;
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

