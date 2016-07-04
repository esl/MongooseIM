-module(mongoose_commands).
-author("bartlomiej.gorny@erlang-solutions.com").
-include("mongoose_commands.hrl").
-include("ejabberd.hrl").
-include("jlib.hrl").

-type mongoose_command() :: #mongoose_command{}.
-type caller() :: admin|jid().


%% API
-export([check_type/2]).
-export([init/0,
    list_commands/1,
    get_command_definition/2,
    register_commands/1,
    unregister_commands/1,
    execute/3
]).

init() ->
    ets:new(mongoose_commands, [named_table, set, public,
        {keypos, #mongoose_command.name}]).


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
    {ok, ResSpec} = Command#mongoose_command.result,
    check_type(ResSpec, Res),
    Res.


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



