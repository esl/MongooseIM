%%%-------------------------------------------------------------------
%%% File    : mod_admin_extra_accounts.erl
%%% Author  : Badlop <badlop@process-one.net>, Piotr Nosek <piotr.nosek@erlang-solutions.com>
%%% Purpose : Contributed administrative functions and commands
%%% Created : 10 Aug 2008 by Badlop <badlop@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2008   ProcessOne
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
%%%-------------------------------------------------------------------

-module(mod_admin_extra_accounts).
-author('badlop@process-one.net').

-export([
    commands/0,

    %% Accounts
    set_password/3,
    check_password_hash/4,
    delete_old_users/1,
    delete_old_users_vhost/2,
    ban_account/3,
    num_active_users/2,
    check_account/2,
    check_password/3]).

-include("ejabberd.hrl").
-include("ejabberd_commands.hrl").

%%%
%%% Register commands
%%%

-spec commands() -> [ejabberd_commands:cmd(), ...].
commands() ->
    [
        #ejabberd_commands{name = change_password, tags = [accounts],
                           desc = "Change the password of an account",
                           module = ?MODULE, function = set_password,
                           args = [{user, binary}, {host, binary}, {newpass, binary}],
                           result = {res, restuple}},
        #ejabberd_commands{name = check_password_hash, tags = [accounts],
                           desc = "Check if the password hash is correct",
                           longdesc = "Allowed hash methods: md5, sha.",
                           module = ?MODULE, function = check_password_hash,
                           args = [{user, binary}, {host, binary}, {passwordhash, string},
                                   {hashmethod, string}],
                           result = {res, restuple}},
        #ejabberd_commands{name = delete_old_users, tags = [accounts, purge],
                           desc = "Delete users that didn't log in last days, or that never logged",
                           module = ?MODULE, function = delete_old_users,
                           args = [{days, integer}],
                           result = {res, restuple}},
        #ejabberd_commands{name = delete_old_users_vhost, tags = [accounts, purge],
                           desc = "Delete users that didn't log in last days in vhost,"
                                  " or that never logged",
                           module = ?MODULE, function = delete_old_users_vhost,
                           args = [{host, binary}, {days, integer}],
                           result = {res, restuple}},
        #ejabberd_commands{name = ban_account, tags = [accounts],
                           desc = "Ban an account: kick sessions and set random password",
                           module = ?MODULE, function = ban_account,
                           args = [{user, binary}, {host, binary}, {reason, binary}],
                           result = {res, restuple}},
        #ejabberd_commands{name = num_active_users, tags = [accounts, stats],
                           desc = "Get number of users active in the last days",
                           module = ?MODULE, function = num_active_users,
                           args = [{host, binary}, {days, integer}],
                           result = {users, integer}},
        #ejabberd_commands{name = check_account, tags = [accounts],
                           desc = "Check if an account exists or not",
                           module = ?MODULE, function = check_account,
                           args = [{user, binary}, {host, binary}],
                           result = {res, restuple}},
        #ejabberd_commands{name = check_password, tags = [accounts],
                           desc = "Check if a password is correct",
                           module = ?MODULE, function = check_password,
                           args = [{user, binary}, {host, binary}, {password, binary}],
                           result = {res, restuple}}
        ].

%%%
%%% Accounts
%%%

-spec set_password(ejabberd:user(), ejabberd:server(), binary()) ->
    {error, string()} | {ok, string()}.
set_password(User, Host, Password) ->
    case ejabberd_auth:set_password(User, Host, Password) of
        ok ->
            {ok, io_lib:format("Password for user ~s@~s successfully changed", [User, Host])};
        {error, Reason} ->
            {error, Reason}
    end.

-spec check_password(ejabberd:user(), ejabberd:server(), binary()) ->  {Res, string()} when
    Res :: ok | incorrect | user_does_not_exist.
check_password(User, Host, Password) ->
    case ejabberd_auth:is_user_exists(User, Host) of
        true ->
            case ejabberd_auth:check_password(User, Host, Password) of
                true ->
                    {ok, io_lib:format("Password '~s' for user ~s@~s is correct",
                                       [Password, User, Host])};
                false ->
                    {incorrect, io_lib:format("Password '~s' for user ~s@~s is incorrect",
                                              [Password, User, Host])}
            end;
        false ->
            {user_does_not_exist,
            io_lib:format("Password '~s' for user ~s@~s is incorrect because this user does not"
                          " exist", [Password, User, Host])}
    end.

-spec check_account(ejabberd:user(), ejabberd:server()) -> {Res, string()} when
    Res :: ok | user_does_not_exist.
check_account(User, Host) ->
    case ejabberd_auth:is_user_exists(User, Host) of
        true ->
            {ok, io_lib:format("User ~s@~s exists", [User, Host])};
        false ->
            {user_does_not_exist, io_lib:format("User ~s@~s does not exist", [User, Host])}
    end.


-spec check_password_hash(ejabberd:user(), ejabberd:server(),
                          Hash :: binary(), Method :: string()) ->
    {error, string()} | {ok, string()} | {incorrect, string()}.
check_password_hash(User, Host, PasswordHash, HashMethod) ->
    AccountPass = ejabberd_auth:get_password_s(User, Host),
    AccountPassHash = case HashMethod of
        "md5" -> get_md5(AccountPass);
        "sha" -> get_sha(AccountPass);
        _ -> undefined
    end,
    case AccountPassHash of
        undefined ->
            {error, "Hash for password is undefined"};
        PasswordHash ->
            {ok, "Password hash is correct"};
        _->
            {incorrect, "Password hash is incorrect"}
    end.


-spec get_md5(binary()) -> string().
get_md5(AccountPass) ->
    lists:flatten([io_lib:format("~.16B", [X])
                   || X <- binary_to_list(crypto:hash(md5, AccountPass))]).
get_sha(AccountPass) ->
    lists:flatten([io_lib:format("~.16B", [X])
                   || X <- binary_to_list(crypto:hash(sha, AccountPass))]).


-spec num_active_users(ejabberd:server(), integer()) -> non_neg_integer().
num_active_users(Host, Days) ->
    {MegaSecs, Secs, _MicroSecs} = now(),
    TimeStamp = MegaSecs * 1000000 + Secs,
    TS = TimeStamp - Days * 86400,
    case catch mod_last:count_active_users(Host, TS) of
        {'EXIT', _Reason} ->
            0;
        Val ->
            Val
    end.


-spec delete_old_users(integer()) -> {'ok', string()}.
delete_old_users(Days) ->
    %% Get the list of registered users
    Users = ejabberd_auth:dirty_get_registered_users(),

    {removed, N, UR} = delete_old_users(Days, Users),
    {ok, io_lib:format("Deleted ~p users: ~p", [N, UR])}.


-spec delete_old_users_vhost(ejabberd:server(), integer()) -> {'ok', string()}.
delete_old_users_vhost(Host, Days) ->
    %% Get the list of registered users
    Users = ejabberd_auth:get_vh_registered_users(Host),

    {removed, N, UR} = delete_old_users(Days, Users),
    {ok, io_lib:format("Deleted ~p users: ~p", [N, UR])}.


-spec delete_old_users(Days :: integer(), Users :: [ejabberd:simple_bare_jid()]) ->
    {removed, non_neg_integer(), [ejabberd:simple_bare_jid()]}.
delete_old_users(Days, Users) ->
    %% Convert older time
    SecOlder = Days*24*60*60,

    %% Get current time
    {MegaSecs, Secs, _MicroSecs} = now(),
    TimeStampNow = MegaSecs * 1000000 + Secs,

    %% Apply the remove function to every user in the list
    UsersRemoved = lists:filter(fun(User) ->
                                        delete_old_user(User, TimeStampNow, SecOlder)
                                end, Users),
    {removed, length(UsersRemoved), UsersRemoved}.

-spec delete_old_user(User :: ejabberd:simple_bare_jid(),
                      TimeStampNow :: non_neg_integer(),
                      SecOlder :: non_neg_integer()) -> boolean().
delete_old_user({LUser, LServer}, TimeStampNow, SecOlder) ->
    %% Check if the user is logged
    case ejabberd_sm:get_user_resources(LUser, LServer) of
        [] -> delete_old_user_if_nonactive_long_enough(LUser, LServer, TimeStampNow, SecOlder);
        _ -> false
    end.

-spec delete_old_user_if_nonactive_long_enough(LUser :: ejabberd:luser(),
                                               LServer :: ejabberd:lserver(),
                                               TimeStampNow :: non_neg_integer(),
                                               SecOlder :: non_neg_integer()) -> boolean().
delete_old_user_if_nonactive_long_enough(LUser, LServer, TimeStampNow, SecOlder) ->
    case mod_last:get_last_info(LUser, LServer) of
        {ok, TimeStamp, _Status} ->
            %% get his age
            Sec = TimeStampNow - TimeStamp,
            %% If he is younger than SecOlder:
            case Sec < SecOlder of
                true ->
                    %% do nothing
                    false;
                %% older:
                false ->
                    %% remove the user
                    ejabberd_auth:remove_user(LUser, LServer),
                    true
            end;
        not_found ->
            ejabberd_auth:remove_user(LUser, LServer),
            true
    end.

-spec ban_account(ejabberd:user(), ejabberd:server(), binary() | string()) ->
    {ok, string()} | {error, string()}.
ban_account(User, Host, ReasonText) ->
    Reason = mod_admin_extra_sessions:prepare_reason(ReasonText),
    kick_sessions(User, Host, Reason),
    case set_random_password(User, Host, Reason) of
        ok ->
            {ok, io_lib:format("User ~s@~s successfully banned with reason: ~s",
                               [User, Host, ReasonText])};
        {error, ErrorReason} ->
            {error, ErrorReason}
    end.

-spec kick_sessions(ejabberd:user(), ejabberd:server(), binary()) -> [mongoose_acc:t()].
kick_sessions(User, Server, Reason) ->
    lists:map(
        fun(Resource) ->
                mod_admin_extra_sessions:kick_this_session(User, Server, Resource, Reason)
        end,
        ejabberd_sm:get_user_resources(User, Server)).


-spec set_random_password(User, Server, Reason) -> Result when
      User :: ejabberd:user(),
      Server :: ejabberd:server(),
      Reason :: binary(),
      Result :: 'ok' | {error, any()}.
set_random_password(User, Server, Reason) ->
    NewPass = build_random_password(Reason),
    ejabberd_auth:set_password(User, Server, NewPass).


-spec build_random_password(Reason :: binary()) -> binary().
build_random_password(Reason) ->
    {{Year, Month, Day}, {Hour, Minute, Second}} = calendar:universal_time(),
    Date = list_to_binary(
             lists:flatten(
               io_lib:format("~4..0w-~2..0w-~2..0wT~2..0w:~2..0w:~2..0wZ",
                             [Year, Month, Day, Hour, Minute, Second]))),
    RandomString = list_to_binary(randoms:get_string()),
    <<"BANNED_ACCOUNT--", Date/binary, "--", RandomString/binary, "--", Reason/binary>>.


