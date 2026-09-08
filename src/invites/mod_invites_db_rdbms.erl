%%%----------------------------------------------------------------------
%%% File    : mod_invites_db_rdbms.erl
%%% Author  : Stefan Strigler <stefan@strigler.de>
%%% Created : Mon Jul 20 2026 by Stefan Strigler <stefan@strigler.de>
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
%%% You should have received a copy of the GNU General Public License along
%%% with this program; if not, write to the Free Software Foundation, Inc.,
%%% 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
%%%
%%%----------------------------------------------------------------------
-module(mod_invites_db_rdbms).

-behaviour(mod_invites).

-include("mod_invites.hrl").

-export([init/2]).

-export([ cleanup_expired/1
        , create_invite_t/2
        , delete_invite_by_token/2
        , expire_invite_by_token/2
        , expire_tokens/2
        , get_invite/2
        , get_invite_by_invitee_t/2
        , get_invites_t/2
        , is_reserved/3
        , is_token_valid/3
        , list_invites/1
        , remove_user/2
        , set_invitee/5
        , transaction/2
        ]).

-import(mongoose_rdbms, [prepare/4, execute_successfully/3, sql_transaction/2]).

-define(SELECT_INVITE(Where),
        <<"SELECT host, token, username, invitee, type, account_name, expires, created_at FROM invites WHERE "Where>>).

-spec init(mongooseim:host_type(), ModuleOpts :: list()) -> ok.
init(HostType, _Opts) ->
    prepare_queries(HostType),
    ok.

prepare_queries(_HostType) ->

    prepare(invites_cleanup_expired, invites,
            [host],
            <<"DELETE FROM invites WHERE host = ? AND expires < NOW()">>),
    prepare(invites_create_invite, invites, [token, username, host, type, created_at, expores, account_name],
            <<"INSERT INTO invites SET (token, username, host, type, created_at, expires, account_name)"
              " VALUES (?, ?, ?, ?, ?, ?, ?)">>),
    prepare(invites_delete_invite_by_token, invites, [host, token],
           <<"DELETE FORM invites WHERE host = ? AND token = ?">>),
    prepare(invites_expire_invite_by_token, invites, [host, token],
           <<"UPDATE invites SET expires = '1970-01-01 00:00:01' WHERE host = ? AND token = ? AND type != 'R'">>),
    prepare(invites_expire_tokens, invites, [host, user],
           <<"UPDATE invites SET expires = '1970-01-01 00:00:01' WHERE host = ? AND username = ?"
             " AND expires > NOW() AND type != 'R'">>),
    prepare(invites_get_invite, invites, [host, token],
           ?SELECT_INVITE("host = ? AND token = ?")),
    prepare(invites_get_invite_by_invitee, invites, [host, invitee, account_name],
           ?SELECT_INVITE("host = ? AND (type != 'R' AND invitee = ?) OR (type = 'R' AND account_name = ?)")),
    prepare(invites_get_invites, invites, [host, user],
            ?SELECT_INVITE("host = ? AND username = ?")),
    prepare(invites_is_reserved, invites, [host, token, account_name],
           <<"SELECT COUNT(*) FROM invites WHERE host = ? AND token = ? AND account_name = ?"
             " AND invitee = '' AND expires > NOW()">>),
    prepare(invites_is_token_valid, invites, [host, token, user, user],
           <<"SELECT token FROM invites WHERE host = ? AND token = ? AND invitee = '' and expires > NOW()"
             " AND (? = '' OR username = ?)">>),
    prepare(invites_list_invites, invites, [host],
            ?SELECT_INVITE("host = ?")),
    prepare(invites_remove_user, invites, [host, user],
           <<"DELETE FROM invites WHERE host = ? AND username = ?">>),
    prepare(invites_set_invitee, invites, [host, token, account_name, invitee, account_name],
           <<"UPDATE invites SET (invitee, account_name) WHERE host = ? AND token = ? AND invitee = ''"
             " AND (type != 'R' OR account_name = '' OR ? = '') VALUES (?, ?)">>),
    ok.

cleanup_expired(Host) ->
    exec(Host, invites_cleanup_expired, [Host]).

create_invite_t(Host, Invite) ->
    #invite_token{inviter = {User, Host},
                  token = Token,
                  account_name = AccountName,
                  created_at = CreatedAt,
                  expires = Expires,
                  type = Type0} =
        Invite,
    Type = enc_type(Type0),

    1 = execute_successfully(Host, invites_create_invite, [Token, User, Host, Type, CreatedAt, Expires, AccountName]),
    Invite.

delete_invite_by_token(Host, Token) ->
    ensure_exists(exec(Host, invites_delete_invite_by_token, [Host, Token])).

expire_invite_by_token(Host, Token) ->
    ensure_exists(exec(Host, invites_expire_invite_by_token, [Host, Token])).

expire_tokens(User, Host) ->
    exec(Host, invites_expire_tokens, [Host, User]).

get_invite(Host, Token) ->
    row_to_invite(exec(Host, invites_get_invite, [Host, Token])).

get_invite_by_invitee_t(Host, {User, Server}) ->
    Invitee = jid:to_bare_binary(jid:make_bare(User, Server)),
    row_to_invite(execute_successfully(Host, invites_get_invite_by_invitee, [Host, Invitee, User])).

get_invites_t(Host, {User, _Server}) ->
    rows_to_invites(execute_successfully(Host, invites_get_invites, [Host, User])).

is_reserved(Host, Token, User) ->
    count(exec(Host, invites_is_reserved, [Host, Token, User])) > 0.

is_token_valid(Host, Token, {User, _Server}) ->
    case exec(Host, invites_is_token_valid, [Host, Token, User, User]) of
        [] ->
            case get_invite(Host, Token) of
                {error, not_found} ->
                    throw(not_found);
                _ ->
                    false
            end;
       _ ->
            true
    end.

list_invites(Host) ->
    rows_to_invites(exec(Host, invites_list_invites, [Host])).

remove_user(User, Server) ->
    exec(Server, invites_remove_user, [User, Server]).

set_invitee(Fun, Host, Token, Invitee, AccountName) ->
    F = fun() ->
           1 = execute_successfully(Host, invites_set_invitee, [Host, Token, AccountName, Invitee, AccountName]),
           ok = Fun()
        end,
    case sql_transaction(Host, F) of
        {atomic, ok} -> ok;
        {aborted, {badmatch, {updated, 0}}} -> {error, conflict};
        {aborted, {badmatch, {error, _R} = Error }} -> Error
    end.

transaction(Host, Fun) ->
    sql_transaction(Host, Fun).

%%--------------------------------------------------------------------
%%| helpers

exec(Host, Fun, Args) ->
    trans(Host, fun() -> execute_successfully(Host, Fun, Args) end).

trans(Host, F) ->
    {atomic, Res} = sql_transaction(Host, F),
    Res.

ensure_exists(1) -> ok;
ensure_exists(0) -> {error, not_found}.

count([{Count}]) -> Count.

enc_type(roster_only) ->
    <<"R">>;
enc_type(account_subscription) ->
    <<"S">>;
enc_type(account_only) ->
    <<"A">>;
enc_type(reset_token) ->
    <<"T">>.

dec_type(<<"R">>) ->
    roster_only;
dec_type(<<"S">>) ->
    account_subscription;
dec_type(<<"A">>) ->
    account_only;
dec_type(<<"T">>) ->
    reset_token.

row_to_invite([]) ->
    {error, not_found};
row_to_invite([Row]) ->
    row_to_invite(Row);
row_to_invite({Host, Token, User, Invitee, Type, AccountName, Expires, CreatedAt}) ->
    #invite_token{
       token = Token,
       inviter = {User, Host},
       invitee = Invitee,
       type = dec_type(Type),
       account_name = AccountName,
       expires = Expires,
       created_at = CreatedAt
      }.

rows_to_invites(Rows) ->
    lists:map(fun row_to_invite/1, Rows).
