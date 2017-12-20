%%%----------------------------------------------------------------------
%%% File    : ejabberd_commands.erl
%%% Author  : Badlop <badlop@process-one.net>
%%% Purpose : Management of ejabberd commands
%%% Created : 20 May 2008 by Badlop <badlop@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2011   ProcessOne
%%%
%%% This program is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation; either version 2 of the
%%% License, or (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with this program; if not, write to the Free Software
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
%%% 02111-1307 USA
%%%
%%%----------------------------------------------------------------------

%%% @headerfile "ejabberd_commands.hrl"

%%% @doc Management of ejabberd commands.
%%%
%%% An ejabberd command is an abstract function identified by a name,
%%% with a defined number and type of calling arguments and type of
%%% result, that can be defined in any Erlang module and executed
%%% using any valid frontend.
%%%
%%%
%%% == Define a new ejabberd command ==
%%%
%%% ejabberd commands can be defined and registered in
%%% any Erlang module.
%%%
%%% Some commands are procedures; and their purpose is to perform an
%%% action in the server, so the command result is only some result
%%% code or result tuple.  Other commands are inspectors, and their
%%% purpose is to gather some information about the server and return
%%% a detailed response: it can be integer, string, atom, tuple, list
%%% or a mix of those ones.
%%%
%%% The arguments and result of an ejabberd command are strictly
%%% defined.  The number and format of the arguments provided when
%%% calling an ejabberd command must match the definition of that
%%% command.  The format of the result provided by an ejabberd command
%%% must be exactly its definition. For example, if a command is said
%%% to return an integer, it must always return an integer (except in
%%% case of a crash).
%%%
%%% If you are developing an Erlang module that will run inside
%%% ejabberd and you want to provide a new ejabberd command to
%%% administer some task related to your module, you only need to:
%%% implement a function, define the command, and register it.
%%%
%%%
%%% === Define a new ejabberd command ===
%%%
%%% An ejabberd command is defined using the Erlang record
%%% 'ejabberd_commands'.  This record has several elements that you
%%% must define. Note that 'tags', 'desc' and 'longdesc' are optional.
%%%
%%% For example let's define an ejabberd command 'pow' that gets the
%%% integers 'base' and 'exponent'. Its result will be an integer
%%% 'power':
%%%
%%% <pre>#ejabberd_commands{name = pow, tags = [test],
%%%                 desc = "Return the power of base for exponent",
%%%                 longdesc = "This is an example command. The formula is:\n"
%%%                 "  power = base ^ exponent",
%%%                 module = ?MODULE, function = pow,
%%%                 args = [{base, integer}, {exponent, integer}],
%%%                 result = {power, integer}}</pre>
%%%
%%%
%%% === Implement the function associated to the command ===
%%%
%%% Now implement a function in your module that matches the arguments
%%% and result of the ejabberd command.
%%%
%%% For example the function calc_power gets two integers Base and
%%% Exponent. It calculates the power and rounds to an integer:
%%%
%%% <pre>calc_power(Base, Exponent) ->
%%%    PowFloat = math:pow(Base, Exponent),
%%%    round(PowFloat).</pre>
%%%
%%% Since this function will be called by ejabberd_commands, it must be exported.
%%% Add to your module:
%%% <pre>-export([calc_power/2]).</pre>
%%%
%%% Only some types of result formats are allowed.
%%% If the format is defined as 'rescode', then your function must return:
%%%   ok | true | atom()
%%% where the atoms ok and true as considered positive answers,
%%% and any other response atom is considered negative.
%%%
%%% If the format is defined as 'restuple', then the command must return:
%%%   {rescode(), string()}
%%%
%%% If the format is defined as '{list, something()}', then the command
%%% must return a list of something().
%%%
%%%
%%% === Register the command ===
%%%
%%% Define this function and put inside the #ejabberd_command you
%%% defined in the beginning:
%%%
%%% <pre>commands() ->
%%%    [
%%%
%%%    ].</pre>
%%%
%%% You need to include this header file in order to use the record:
%%%
%%% <pre>-include("ejabberd_commands.hrl").</pre>
%%%
%%% When your module is initialized or started, register your commands:
%%%
%%% <pre>ejabberd_commands:register_commands(commands()), </pre>
%%%
%%% And when your module is stopped, unregister your commands:
%%%
%%% <pre>ejabberd_commands:unregister_commands(commands()), </pre>
%%%
%%% That's all! Now when your module is started, the command will be
%%% registered and any frontend can access it. For example:
%%%
%%% <pre>$ mongooseimctl help pow
%%%
%%%   Command Name: pow
%%%
%%%   Arguments: base::integer
%%%              exponent::integer
%%%
%%%   Returns: power::integer
%%%
%%%   Tags: test
%%%
%%%   Description: Return the power of base for exponent
%%%
%%% This is an example command. The formula is:
%%%  power = base ^ exponent
%%%
%%% $ mongooseimctl pow 3 4
%%% 81
%%% </pre>
%%%
%%%
%%% == Execute an ejabberd command ==
%%%
%%% ejabberd commands are mean to be executed using any valid
%%% frontend.  An ejabberd commands is implemented in a regular Erlang
%%% function, so it is also possible to execute this function in any
%%% Erlang module, without dealing with the associated ejabberd
%%% commands.
%%%
%%%
%%% == Frontend to ejabberd commands ==
%%%
%%% Currently there is one frontend to ejabberd commands: the shell
%%% script - mongooseimctl
%%%
%%% === mongooseimctl as a frontend to ejabberd commands ===
%%%
%%% It is possible to use mongooseimctl to get documentation of any
%%% command. But mongooseimctl does not support all the argument types
%%% allowed in ejabberd commands, so there are some ejabberd commands
%%% that cannot be executed using mongooseimctl.
%%%
%%% Also note that the mongooseimctl shell administration script also
%%% manages mongooseimctl commands, which are unrelated to ejabberd
%%% commands and can only be executed using mongooseimctl.
%%%
%%% TODO: consider this feature:
%%% All commands are catched. If an error happens, return the restuple:
%%%   {error, flattened error string}
%%% This means that ecomm call APIs ejabberd_ctl need to allows this.


-module(ejabberd_commands).
-author('badlop@process-one.net').

-export([init/0,
         list_commands/0,
         get_command_format/1,
         get_command_definition/1,
         get_tags_commands/0,
         register_commands/1,
         unregister_commands/1,
         execute_command/2,
         execute_command/4
        ]).

-include("ejabberd_commands.hrl").
-include("ejabberd.hrl").

%% Allowed types for arguments are integer, string, tuple and list.
-type atype() :: integer | string | binary | {tuple, [aterm()]} | {list, aterm()}.

%% A rtype is either an atom or a tuple with two elements.
-type rtype() :: integer | string | atom | binary | {tuple, [rterm()]}
               | {list, rterm()} | rescode | restuple.

%% An argument term is a tuple with the term name and the term type.
-type aterm() :: {Name::atom(), Type::atype()}.

%% A result term is a tuple with the term name and the term type.
-type rterm() :: {Name::atom(), Type::rtype()}.

-type cmd() :: #ejabberd_commands{
                  name :: atom(),
                  tags :: [atom()],
                  desc :: string(),
                  longdesc :: string(),
                  module :: module(),
                  function :: atom(),
                  args :: [ejabberd_commands:aterm()],
                  result :: ejabberd_commands:rterm()
                 }.

-type auth() :: {User :: binary(), Server :: binary(), Password :: binary()} | noauth.

-type cmd_error() :: command_unknown | account_unprivileged
                   | invalid_account_data | no_auth_provided.
-type access_cmd() :: {Access :: atom(),
                       CommandNames :: [atom()],
                       Arguments :: [term()]
                      }.
-type list_cmd() :: {Name::atom(), Args::[aterm()], Desc::string()}.

-export_type([rterm/0,
              aterm/0,
              cmd/0,
              auth/0,
              access_cmd/0,
              list_cmd/0]).


init() ->
    case ets:info(ejabberd_commands) of
        undefined ->
            ets:new(ejabberd_commands, [named_table, set, public,
                                        {keypos, #ejabberd_commands.name}]);
        _ ->
            ok
    end.


%% @doc Register ejabberd commands. If a command is already registered, a
%% warning is printed and the old command is preserved.
-spec register_commands([cmd()]) -> ok.
register_commands(Commands) ->
    lists:foreach(
      fun(Command) ->
              case ets:insert_new(ejabberd_commands, Command) of
                  true ->
                      ok;
                  false ->
                      ?DEBUG("This command is already defined:~n~p", [Command])
              end
      end,
      Commands).


%% @doc Unregister ejabberd commands.
-spec unregister_commands([cmd()]) -> ok.
unregister_commands(Commands) ->
    lists:foreach(
      fun(Command) ->
              ets:delete_object(ejabberd_commands, Command)
      end,
      Commands).


%% @doc Get a list of all the available commands, arguments and description.
-spec list_commands() -> [list_cmd()].
list_commands() ->
    Commands = ets:match(ejabberd_commands,
                         #ejabberd_commands{name = '$1',
                                            args = '$2',
                                            desc = '$3',
                                            _ = '_'}),
    [{A, B, C} || [A, B, C] <- Commands].


%% @doc Get the format of arguments and result of a command.
-spec get_command_format(Name::atom()) -> {Args::[aterm()], Result::rterm()}
                                        | {error, command_unknown}.
get_command_format(Name) ->
    Matched = ets:match(ejabberd_commands,
                        #ejabberd_commands{name = Name,
                                           args = '$1',
                                           result = '$2',
                                           _ = '_'}),
    case Matched of
        [] ->
            {error, command_unknown};
        [[Args, Result]] ->
            {Args, Result}
    end.


%% @doc Get the definition record of a command.
-spec get_command_definition(Name::atom()) -> cmd() | 'command_not_found'.
get_command_definition(Name) ->
    case ets:lookup(ejabberd_commands, Name) of
        [E] -> E;
        [] -> command_not_found
    end.


%% @doc Execute a command.
-spec execute_command(Name :: atom(),
                      Arguments :: list()
                     ) -> Result :: term() | {error, command_unknown}.
execute_command(Name, Arguments) ->
    execute_command([], noauth, Name, Arguments).


-spec execute_command(AccessCommands :: [access_cmd()],
                      Auth :: auth(),
                      Name :: atom(),
                      Arguments :: [term()]
                      ) -> Result :: term() | {error, cmd_error()}.
execute_command(AccessCommands, Auth, Name, Arguments) ->
    case ets:lookup(ejabberd_commands, Name) of
        [Command] ->
            try check_access_commands(AccessCommands, Auth, Name, Command, Arguments) of
                ok -> execute_command2(Command, Arguments)
            catch
                {error, Error} -> {error, Error}
            end;
        [] -> {error, command_unknown}
    end.


%% @private
execute_command2(Command, Arguments) ->
    Module = Command#ejabberd_commands.module,
    Function = Command#ejabberd_commands.function,
    ?DEBUG("Executing command ~p:~p with Args=~p", [Module, Function, Arguments]),
    apply(Module, Function, Arguments).


%% @doc Get all the tags and associated commands.
-spec get_tags_commands() -> [{Tag::string(), [CommandName::string()]}].
get_tags_commands() ->
    CommandTags = ets:match(ejabberd_commands,
                            #ejabberd_commands{
                              name = '$1',
                              tags = '$2',
                              _ = '_'}),
    Dict = lists:foldl(
             fun([CommandNameAtom, CTags], D) ->
                     CommandName = atom_to_list(CommandNameAtom),
                     case CTags of
                         [] ->
                             orddict:append("untagged", CommandName, D);
                         _ ->
                             lists:foldl(
                               fun(TagAtom, DD) ->
                                       Tag = atom_to_list(TagAtom),
                                       orddict:append(Tag, CommandName, DD)
                               end,
                               D,
                               CTags)
                     end
             end,
             orddict:new(),
             CommandTags),
    orddict:to_list(Dict).


%% -----------------------------
%% Access verification
%% -----------------------------


%% @doc Check access is allowed to that command.
%% At least one AccessCommand must be satisfied.
%% May throw {error, account_unprivileged | invalid_account_data}
-spec check_access_commands(AccessCommands :: [ access_cmd() ],
                            Auth :: auth(),
                            Method :: atom(),
                            Command :: tuple(),
                            Arguments :: [any()]
                            ) -> ok | none().
check_access_commands([], _Auth, _Method, _Command, _Arguments) ->
    ok;
check_access_commands(AccessCommands, Auth, Method, Command, Arguments) ->
    AccessCommandsAllowed =
        lists:filter(
          fun({Access, Commands, ArgumentRestrictions}) ->
                  case check_access(Access, Auth) of
                      true ->
                          check_access_command(Commands, Command, ArgumentRestrictions,
                                               Method, Arguments);
                      false ->
                          false
                  end
          end,
          AccessCommands),
    case AccessCommandsAllowed of
        [] -> throw({error, account_unprivileged});
        L when is_list(L) -> ok
    end.


%% @private
%% May throw {error, invalid_account_data}
-spec check_auth(auth()) -> {ok, User :: binary(), Server :: binary()} | no_return().
check_auth({User, Server, Password}) ->
    %% Check the account exists and password is valid
    AccountPass = ejabberd_auth:get_password_s(User, Server),
    AccountPassMD5 = get_md5(AccountPass),
    case Password of
        AccountPass -> {ok, User, Server};
        AccountPassMD5 -> {ok, User, Server};
        _ -> throw({error, invalid_account_data})
    end.


-spec get_md5(iodata()) -> string().
get_md5(AccountPass) ->
    lists:flatten([io_lib:format("~.16B", [X])
                   || X <- binary_to_list(crypto:hash(md5, AccountPass))]).


-spec check_access(Access :: acl:rule(), Auth :: auth()) -> boolean().
check_access(all, _) ->
    true;
check_access(_, noauth) ->
    false;
check_access(Access, Auth) ->
    {ok, User, Server} = check_auth(Auth),
    %% Check this user has access permission
    case acl:match_rule(Server, Access, jid:make(User, Server, <<"">>)) of
        allow -> true;
        deny -> false
    end.


-spec check_access_command(_, tuple(), _, _, _) -> boolean().
check_access_command(Commands, Command, ArgumentRestrictions, Method, Arguments) ->
    case Commands==all orelse lists:member(Method, Commands) of
        true -> check_access_arguments(Command, ArgumentRestrictions, Arguments);
        false -> false
    end.


-spec check_access_arguments(Command :: cmd(),
                             Restrictions :: [any()],
                             Args :: [any()]) -> boolean().
check_access_arguments(Command, ArgumentRestrictions, Arguments) ->
    ArgumentsTagged = tag_arguments(Command#ejabberd_commands.args, Arguments),
    lists:all(
      fun({ArgName, ArgAllowedValue}) ->
              %% If the call uses the argument, check the value is acceptable
              case lists:keysearch(ArgName, 1, ArgumentsTagged) of
                  {value, {ArgName, ArgValue}} -> ArgValue == ArgAllowedValue;
                  false -> true
              end
      end, ArgumentRestrictions).


-spec tag_arguments(ArgsDefs :: [{atom(), integer() | string() | {_, _}}],
                    Args :: [any()] ) -> [{_, _}].
tag_arguments(ArgsDefs, Args) ->
    lists:zipwith(
      fun({ArgName, _ArgType}, ArgValue) ->
              {ArgName, ArgValue}
      end,
      ArgsDefs,
      Args).
