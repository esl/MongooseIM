%%%----------------------------------------------------------------------
%%% File    : ejabberd_auth_internal.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Authentification via mnesia
%%% Created : 12 Dec 2004 by Alexey Shchepin <alexey@process-one.net>
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

-module(ejabberd_auth_internal).
-author('alexey@process-one.net').

%% External exports
-behaviour(ejabberd_gen_auth).
-export([start/1,
         set_password/3,
         check_password/3,
         check_password/5,
         try_register/3,
         dirty_get_registered_users/0,
         get_vh_registered_users/1,
         get_vh_registered_users/2,
         get_vh_registered_users_number/1,
         get_vh_registered_users_number/2,
         get_password/2,
         get_password_s/2,
         is_user_exists/2,
         remove_user/2,
         remove_user/3,
         plain_password_required/0
        ]).
%% Exported for behaviour but not implemented
-export([login/2, get_password/3]).

-include("ejabberd.hrl").

-record(passwd, {us :: ejabberd:simple_bare_jid(),
                 password :: binary()
                }).
-record(reg_users_counter, {vhost :: binary(),
                            count :: integer()
                           }).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

-spec start(Host :: ejabberd:server()) -> ok.
start(Host) ->
    mnesia:create_table(passwd, [{disc_copies, [node()]},
                                 {attributes, record_info(fields, passwd)}]),
    mnesia:create_table(reg_users_counter,
                        [{ram_copies, [node()]},
                         {attributes, record_info(fields, reg_users_counter)}]),
    update_table(),
    update_reg_users_counter_table(Host),
    ok.


-spec update_reg_users_counter_table(Server :: ejabberd:server()) -> any().
update_reg_users_counter_table(Server) ->
    Set = get_vh_registered_users(Server),
    Size = length(Set),
    LServer = jlib:nameprep(Server),
    F = fun() ->
            mnesia:write(#reg_users_counter{vhost = LServer,
                                            count = Size})
        end,
    mnesia:sync_dirty(F).


plain_password_required() ->
    false.


-spec check_password(User :: ejabberd:user(),
                     Server :: ejabberd:server(),
                     Password :: binary()) -> boolean().
check_password(User, Server, Password) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    US = {LUser, LServer},
    case catch mnesia:dirty_read({passwd, US}) of
        [#passwd{password = Password}] ->
            Password /= <<>>;
        _ ->
            false
    end.


-spec check_password(User :: ejabberd:user(),
                     Server :: ejabberd:server(),
                     Password :: binary(),
                     Digest :: binary(),
                     DigestGen :: fun()) -> boolean().
check_password(User, Server, Password, Digest, DigestGen) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    US = {LUser, LServer},
    case catch mnesia:dirty_read({passwd, US}) of
        [#passwd{password = Passwd}] ->
            DigRes = if
                         Digest /= <<>> ->
                 Digest == DigestGen(Passwd);
                         true ->
                             false
                     end,
            if DigRes ->
                    true;
               true ->
                    (Passwd == Password) and (Password /= <<>>)
            end;
        _ ->
            false
    end.


-spec set_password(User :: ejabberd:user(),
             Server :: ejabberd:server(),
             Password :: binary()) -> ok | {error, not_allowed | invalid_jid}.
set_password(User, Server, Password) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    US = {LUser, LServer},
    if
        (LUser == error) or (LServer == error) ->
            {error, invalid_jid};
        true ->
            F = fun() ->
                        mnesia:write(#passwd{us = US,
                                             password = Password})
                end,
            {atomic, ok} = mnesia:transaction(F),
            ok
    end.


-spec try_register(User :: ejabberd:user(),
                   Server :: ejabberd:server(),
                   Password :: binary()
                   ) -> {atomic, ok | exists}
                      | {error, invalid_jid | not_allowed}
                      | {aborted, _}.
try_register(User, Server, Password) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    US = {LUser, LServer},
    if
        (LUser == error) or (LServer == error) ->
            {error, invalid_jid};
        true ->
            F = fun() ->
                        case mnesia:read({passwd, US}) of
                            [] ->
                                mnesia:write(#passwd{us = US,
                                                     password = Password}),
                                mnesia:dirty_update_counter(
                                                    reg_users_counter,
                                                    LServer, 1),
                                ok;
                            [_E] ->
                                exists
                        end
                end,
            mnesia:transaction(F)
    end.


%% @doc Get all registered users in Mnesia
-spec dirty_get_registered_users() -> [ejabberd:simple_jid()].
dirty_get_registered_users() ->
    mnesia:dirty_all_keys(passwd).


-spec get_vh_registered_users(Server :: ejabberd:server()
                             ) -> [ejabberd:simple_jid()].
get_vh_registered_users(Server) ->
    LServer = jlib:nameprep(Server),
    mnesia:dirty_select(
      passwd,
      [{#passwd{us = '$1', _ = '_'},
        [{'==', {element, 2, '$1'}, LServer}],
        ['$1']}]).


-type query_keyword() :: from | to | limit | offset | prefix.
-type query_value() :: integer() | string().
-spec get_vh_registered_users(Server :: ejabberd:server(),
                              Query :: [{query_keyword(), query_value()}]
                              ) -> [ejabberd:simple_jid()].
get_vh_registered_users(Server, [{from, Start}, {to, End}])
        when is_integer(Start) and is_integer(End) ->
    get_vh_registered_users(Server, [{limit, End-Start+1}, {offset, Start}]);
get_vh_registered_users(Server, [{limit, Limit}, {offset, Offset}])
        when is_integer(Limit) and is_integer(Offset) ->
    case get_vh_registered_users(Server) of
    [] ->
        [];
    Users ->
        Set = lists:keysort(1, Users),
        L = length(Set),
        Start = if Offset < 1 -> 1;
                   Offset > L -> L;
                   true -> Offset
                end,
        lists:sublist(Set, Start, Limit)
    end;
get_vh_registered_users(Server, [{prefix, Prefix}])
        when is_list(Prefix) ->
    Set = [{U,S} || {U, S} <- get_vh_registered_users(Server), lists:prefix(Prefix, U)],
    lists:keysort(1, Set);
get_vh_registered_users(Server, [{prefix, Prefix}, {from, Start}, {to, End}])
        when is_list(Prefix) and is_integer(Start) and is_integer(End) ->
    get_vh_registered_users(Server, [{prefix, Prefix}, {limit, End-Start+1}, {offset, Start}]);
get_vh_registered_users(Server, [{prefix, Prefix}, {limit, Limit}, {offset, Offset}])
        when is_list(Prefix) and is_integer(Limit) and is_integer(Offset) ->
    case [{U,S} || {U, S} <- get_vh_registered_users(Server), lists:prefix(Prefix, U)] of
    [] ->
        [];
    Users ->
        Set = lists:keysort(1, Users),
        L = length(Set),
        Start = if Offset < 1 -> 1;
                   Offset > L -> L;
                   true -> Offset
                end,
        lists:sublist(Set, Start, Limit)
    end;
get_vh_registered_users(Server, _) ->
    get_vh_registered_users(Server).


-spec get_vh_registered_users_number(Server :: ejabberd:server()
                                    ) -> non_neg_integer().
get_vh_registered_users_number(Server) ->
    LServer = jlib:nameprep(Server),
    Query = mnesia:dirty_select(
                reg_users_counter,
                [{#reg_users_counter{vhost = LServer, count = '$1'},
                  [],
                  ['$1']}]),
    case Query of
        [Count] ->
            Count;
        _ -> 0
    end.


-spec get_vh_registered_users_number(Server :: ejabberd:server(),
                                     Query :: [{prefix, string()}]
                                     ) -> integer().
get_vh_registered_users_number(Server, [{prefix, Prefix}]) when is_list(Prefix) ->
    Set = [{U, S} || {U, S} <- get_vh_registered_users(Server), lists:prefix(Prefix, U)],
    length(Set);
get_vh_registered_users_number(Server, _) ->
    get_vh_registered_users_number(Server).


-spec get_password(User :: ejabberd:user(),
                   Server :: ejabberd:server()) -> binary() | false.
get_password(User, Server) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    US = {LUser, LServer},
    case catch mnesia:dirty_read(passwd, US) of
        [#passwd{password = Password}] ->
            Password;
        _ ->
            false
    end.


-spec get_password_s(User :: ejabberd:user(),
                     Server :: ejabberd:server()) -> binary() | false.
get_password_s(User, Server) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    US = {LUser, LServer},
    case catch mnesia:dirty_read(passwd, US) of
        [#passwd{password = Password}] ->
            Password;
        _ ->
            []
    end.


-spec is_user_exists(User :: ejabberd:user(),
                     Server :: ejabberd:server()
                     ) -> boolean() | {error, atom()}.
is_user_exists(User, Server) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    US = {LUser, LServer},
    case catch mnesia:dirty_read({passwd, US}) of
        [] ->
            false;
        [_] ->
            true;
        Other ->
            {error, Other}
    end.


%% @doc Remove user.
%% Note: it returns ok even if there was some problem removing the user.
-spec remove_user(User :: ejabberd:user(),
                  Server :: ejabberd:server()
                  ) -> ok | error | {error, not_allowed}.
remove_user(User, Server) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    US = {LUser, LServer},
    F = fun() ->
                mnesia:delete({passwd, US}),
                mnesia:dirty_update_counter(reg_users_counter,
                                            LServer, -1)
        end,
    mnesia:transaction(F),
        ok.


%% @doc Remove user if the provided password is correct.
-spec remove_user(User :: ejabberd:user(),
                  Server :: ejabberd:server(),
                  Password :: binary()
                  ) -> ok | not_exists | not_allowed | bad_request | error.
remove_user(User, Server, Password) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    US = {LUser, LServer},
    F = fun() ->
                case mnesia:read({passwd, US}) of
                    [#passwd{password = Password}] ->
                        mnesia:delete({passwd, US}),
                        mnesia:dirty_update_counter(reg_users_counter,
                                                    LServer, -1),
                        ok;
                    [_] ->
                        not_allowed;
                    _ ->
                        not_exists
                end
        end,
    case mnesia:transaction(F) of
        {atomic, ok} ->
            ok;
        {atomic, Res} ->
            Res;
        _ ->
            bad_request
    end.


-spec update_table() -> ok | {atomic|aborted, _}.
update_table() ->
    Fields = record_info(fields, passwd),
    case mnesia:table_info(passwd, attributes) of
        Fields ->
            ok;
        [user, password] ->
            ?INFO_MSG("Converting passwd table from "
                      "{user, password} format", []),
            Host = ?MYNAME,
            {atomic, ok} = mnesia:create_table(
                             ejabberd_auth_internal_tmp_table,
                             [{disc_only_copies, [node()]},
                              {type, bag},
                              {local_content, true},
                              {record_name, passwd},
                              {attributes, record_info(fields, passwd)}]),
            mnesia:transform_table(passwd, ignore, Fields),
            F1 = fun() ->
                         mnesia:write_lock_table(ejabberd_auth_internal_tmp_table),
                         mnesia:foldl(
                           fun(#passwd{us = U} = R, _) ->
                                   mnesia:dirty_write(
                                     ejabberd_auth_internal_tmp_table,
                                     R#passwd{us = {U, Host}})
                           end, ok, passwd)
                 end,
            mnesia:transaction(F1),
            mnesia:clear_table(passwd),
            F2 = fun() ->
                         mnesia:write_lock_table(passwd),
                         mnesia:foldl(
                           fun(R, _) ->
                                   mnesia:dirty_write(R)
                           end, ok, ejabberd_auth_internal_tmp_table)
                 end,
            mnesia:transaction(F2),
            mnesia:delete_table(ejabberd_auth_internal_tmp_table);
        _ ->
            ?INFO_MSG("Recreating passwd table", []),
            mnesia:transform_table(passwd, ignore, Fields)
    end.


%% @doc gen_auth unimplemented callbacks
login(_User, _Server) -> erlang:error(not_implemented).
get_password(_User, _Server, _DefaultValue) -> erlang:error(not_implemented).
