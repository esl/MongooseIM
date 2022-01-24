%%%-------------------------------------------------------------------
%%% File    : service_admin_extra_accounts.erl
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
%%% Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
%%%
%%%-------------------------------------------------------------------

-module(service_admin_extra_accounts).
-author('badlop@process-one.net').

-export([
    commands/0,

    %% Accounts
    set_password/3,
    check_password_hash/4,
    delete_old_users/1,
    delete_old_users_for_domain/2,
    ban_account/3,
    num_active_users/2,
    check_account/2,
    check_password/3]).

-ignore_xref([
    commands/0, set_password/3, check_password_hash/4,
    delete_old_users/1, delete_old_users_for_domain/2,
    ban_account/3, num_active_users/2, check_account/2, check_password/3
]).

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
                           module = ?MODULE, function = delete_old_users_for_domain,
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
                           result = {res, restuple}},
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

-spec set_password(jid:user(), jid:server(), binary()) ->
    mongoose_account_api:change_password_result().
set_password(User, Host, Password) ->
    mongoose_account_api:change_password(User, Host, Password).

-spec check_password(jid:user(), jid:server(), binary()) ->
    mongoose_account_api:check_password_result().
check_password(User, Host, Password) ->
    mongoose_account_api:check_password(User, Host, Password).

-spec check_account(jid:user(), jid:server()) -> mongoose_account_api:check_account_result().
check_account(User, Host) ->
    mongoose_account_api:check_account(User, Host).

-spec check_password_hash(jid:user(), jid:server(), string(), string()) ->
    mongoose_account_api:check_password_hash_result().
check_password_hash(User, Host, PasswordHash, HashMethod) ->
    mongoose_account_api:check_password_hash(User, Host, PasswordHash, HashMethod).

-spec num_active_users(jid:lserver(), integer()) -> {ok | cannot_count, string()}.
num_active_users(Domain, Days) ->
    case mongoose_account_api:num_active_users(Domain, Days) of
        {ok, Num} -> {ok, integer_to_list(Num)};
        Res -> Res
    end.

-spec delete_old_users(integer()) -> mongoose_account_api:delete_old_users().
delete_old_users(Days) ->
    {Res, _} = mongoose_account_api:delete_old_users(Days),
    Res.

-spec delete_old_users_for_domain(jid:server(), integer()) ->
    mongoose_account_api:delete_old_users().
delete_old_users_for_domain(Domain, Days) ->
    {Res, _} = mongoose_account_api:delete_old_users_for_domain(Domain, Days),
    Res.

-spec ban_account(jid:user(), jid:server(), binary() | string()) ->
    mongoose_account_api:change_password_result().
ban_account(User, Host, ReasonText) ->
    mongoose_account_api:ban_account(User, Host, ReasonText).
