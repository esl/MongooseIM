%%%----------------------------------------------------------------------
%%% File    : ejabberd_auth_rdbms.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Authentification via RDBMS
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
%%% Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
%%%
%%%----------------------------------------------------------------------

-module(ejabberd_auth_rdbms).
-author('alexey@process-one.net').

%% External exports
-behaviour(ejabberd_gen_auth).
-export([start/1,
         stop/1,
         authorize/1,
         set_password/3,
         try_register/3,
         dirty_get_registered_users/0,
         get_vh_registered_users/1,
         get_vh_registered_users/2,
         get_vh_registered_users_number/1,
         get_vh_registered_users_number/2,
         get_password/2,
         get_password_s/2,
         does_user_exist/2,
         remove_user/2,
         supports_sasl_module/2
        ]).

%% Internal
-export([check_password/3,
         check_password/5]).

-export([scram_passwords/2, scram_passwords/4]).

-import(mongoose_rdbms, [prepare/4, execute_successfully/3]).

-include("mongoose.hrl").
-include("scram.hrl").

-define(DEFAULT_SCRAMMIFY_COUNT, 10000).
-define(DEFAULT_SCRAMMIFY_INTERVAL, 1000).


%%%----------------------------------------------------------------------
%%% Types
%%%----------------------------------------------------------------------

-type prepared_password() ::
        #{password := binary(),
          details => binary()}.

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

start(Host) ->
    prepare_queries(Host),
    ok.

stop(_Host) ->
    ok.

-spec supports_sasl_module(jid:lserver(), cyrsasl:sasl_module()) -> boolean().
supports_sasl_module(_Host, cyrsasl_plain) -> true;
supports_sasl_module(Host, cyrsasl_digest) -> not mongoose_scram:enabled(Host);
supports_sasl_module(Host, Mechanism) -> mongoose_scram:enabled(Host, Mechanism).

-spec authorize(mongoose_credentials:t()) -> {ok, mongoose_credentials:t()}
                                           | {error, any()}.
authorize(Creds) ->
    ejabberd_auth:authorize_with_check_password(?MODULE, Creds).

-spec check_password(LUser :: jid:luser(),
                     LServer :: jid:lserver(),
                     Password :: binary()) -> boolean().
check_password(LUser, LServer, Password) ->
    try execute_get_password(LServer, LUser) of
        {selected, [{Password, null}]} ->
            Password /= <<"">>; %% Password is correct, and not empty
        {selected, [{_Password2, null}]} ->
            false;
        {selected, [{_Password2, PassDetails}]} ->
            case mongoose_scram:deserialize(PassDetails) of
                {ok, Scram} ->
                    mongoose_scram:check_password(Password, Scram);
                {error, Reason} ->
                    ?LOG_WARNING(#{what => scram_serialisation_incorrect, reason => Reason,
                                   user => LUser, server => LServer}),
                    false %% Password is not correct
            end;
        {selected, []} ->
            false %% Account does not exist
    catch
        error:Reason:StackTrace ->
            ?LOG_ERROR(#{what => check_password_failed,
                         user => LUser, server => LServer,
                         class => error, reason => Reason, stacktrace => StackTrace}),
            false %% Database error
    end.

-spec check_password(LUser :: jid:luser(),
                     LServer :: jid:lserver(),
                     Password :: binary(),
                     Digest :: binary(),
                     DigestGen :: fun()) -> boolean().
check_password(LUser, LServer, Password, Digest, DigestGen) ->
    try execute_get_password(LServer, LUser) of
        {selected, [{Passwd, null}]} ->
            ejabberd_auth:check_digest(Digest, DigestGen, Password, Passwd);
        {selected, [{_Passwd, PassDetails}]} ->
            case mongoose_scram:deserialize(PassDetails) of
                {ok, Scram} ->
                    mongoose_scram:check_digest(Scram, Digest, DigestGen, Password);
                {error, Reason} ->
                    ?LOG_WARNING(#{what => scram_serialisation_incorrect, reason => Reason,
                                   user => LUser, server => LServer}),
                    false
            end;
        {selected, []} ->
            false %% Account does not exist
    catch
        error:Reason:StackTrace ->
            ?LOG_ERROR(#{what => check_password_failed,
                         user => LUser, server => LServer,
                         class => error, reason => Reason, stacktrace => StackTrace}),
            false %% Database error
    end.

-spec set_password(LUser :: jid:luser(),
                   LServer :: jid:lserver(),
                   Password :: binary()
                   ) -> ok | {error, not_allowed}.
set_password(LUser, LServer, Password) ->
    PreparedPass = prepare_password(LServer, Password),
    try
        execute_set_password(LServer, LUser, PreparedPass),
        ok
    catch
        error:Reason:StackTrace ->
            ?LOG_ERROR(#{what => set_password_failed,
                         user => LUser, server => LServer,
                         class => error, reason => Reason, stacktrace => StackTrace}),
            {error, not_allowed}
    end.

-spec try_register(LUser :: jid:luser(),
                   LServer :: jid:lserver(),
                   Password :: binary()
                   ) -> ok | {error, exists}.
try_register(LUser, LServer, Password) ->
    PreparedPass = prepare_password(LServer, Password),
    try execute_add_user(LServer, LUser, PreparedPass) of
        {updated, 1} ->
            ok;
        {updated, 0} ->
            {error, exists}
    catch
        error:Reason:StackTrace ->
            ?LOG_ERROR(#{what => registration_failed,
                         user => LUser, server => LServer,
                         class => error, reason => Reason, stacktrace => StackTrace}),
            {error, exists} %% XXX wrong error type - fix type in a separate PR
    end.

-spec dirty_get_registered_users() -> [jid:simple_bare_jid()].
dirty_get_registered_users() ->
    Servers = ejabberd_config:get_vh_by_auth_method(rdbms),
    lists:flatmap(
      fun(Server) ->
              get_vh_registered_users(Server)
      end, Servers).


-spec get_vh_registered_users(LServer :: jid:lserver()) -> [jid:simple_bare_jid()].
get_vh_registered_users(LServer) ->
    get_vh_registered_users(LServer, []).

-spec get_vh_registered_users(LServer :: jid:lserver(), Opts :: list()
                             ) -> [jid:simple_bare_jid()].
get_vh_registered_users(LServer, Opts) ->
    try
        {selected, Res} = execute_list_users(LServer, maps:from_list(Opts)),
        [{U, LServer} || {U} <- Res]
    catch error:Reason:StackTrace ->
        ?LOG_ERROR(#{what => get_vh_registered_users_failed, server => LServer,
                     class => error, reason => Reason, stacktrace => StackTrace}),
        []
    end.

-spec get_vh_registered_users_number(LServer :: jid:lserver()
                                    ) -> integer().
get_vh_registered_users_number(LServer) ->
    get_vh_registered_users_number(LServer, []).

-spec get_vh_registered_users_number(LServer :: jid:lserver(),
                                     Opts :: list()) -> integer().
get_vh_registered_users_number(LServer, Opts) ->
    try
        mongoose_rdbms:selected_to_integer(execute_count_users(LServer, maps:from_list(Opts)))
    catch error:Reason:StackTrace ->
        ?LOG_ERROR(#{what => get_vh_registered_users_numbers_failed, server => LServer,
                     class => error, reason => Reason, stacktrace => StackTrace}),
        0
    end.

-spec get_password(jid:luser(), jid:lserver()) -> ejabberd_auth:passterm() | false.
get_password(LUser, LServer) ->
    try execute_get_password(LServer, LUser) of
        {selected, [{Password, null}]} ->
            Password; %% Plain password
        {selected, [{_Password, PassDetails}]} ->
            case mongoose_scram:deserialize(PassDetails) of
                {ok, Scram} ->
                    Scram;
                {error, Reason} ->
                    ?LOG_WARNING(#{what => scram_serialisation_incorrect, reason => Reason,
                                   user => LUser, server => LServer}),
                    false
            end;
        {selected, []} ->
            false
    catch
        error:Reason:StackTrace ->
            ?LOG_ERROR(#{what => get_password_failed,
                         user => LUser, server => LServer,
                         class => error, reason => Reason, stacktrace => StackTrace}),
            false
    end.


-spec get_password_s(LUser :: jid:user(),
                     LServer :: jid:server()) -> binary().
get_password_s(LUser, LServer) ->
    try execute_get_password(LServer, LUser) of
        {selected, [{Password, _}]} ->
            Password;
        {selected, []} ->
            <<>>
    catch
        error:Reason:StackTrace ->
            ?LOG_ERROR(#{what => get_password_s_failed,
                         user => LUser, server => LServer,
                         class => error, reason => Reason, stacktrace => StackTrace}),
            <<>>
    end.


-spec does_user_exist(LUser :: jid:luser(),
                     LServer :: jid:lserver()
                    ) -> boolean() | {error, atom()}.
does_user_exist(LUser, LServer) ->
    try execute_get_password(LServer, LUser) of
        {selected, [{_Password, _}]} ->
            true; %% Account exists
        {selected, []} ->
            false %% Account does not exist
    catch
        error:Reason:StackTrace ->
            ?LOG_ERROR(#{what => does_user_exist_failed,
                         user => LUser, server => LServer,
                         class => error, reason => Reason, stacktrace => StackTrace}),
            {error, Reason} %% Database error

    end.


%% @doc Remove user.
%% Note: it may return ok even if there was some problem removing the user.
-spec remove_user(LUser :: jid:luser(),
                  LServer :: jid:lserver()
                  ) -> ok.
remove_user(LUser, LServer) ->
    try
        execute_delete_user(LServer, LUser)
    catch
        error:Reason:StackTrace ->
            ?LOG_ERROR(#{what => remove_user_failed,
                         user => LUser, server => LServer,
                         class => error, reason => Reason, stacktrace => StackTrace})
    end,
    ok.

%%%------------------------------------------------------------------
%%% Scram
%%%------------------------------------------------------------------

-spec prepare_scrammed_password(Server, Iterations, Password) -> prepared_password() when
        Server :: jid:lserver(),
        Iterations :: pos_integer(),
        Password :: binary().
prepare_scrammed_password(Server, Iterations, Password) when is_integer(Iterations) ->
    Scram = mongoose_scram:password_to_scram(Server, Password, Iterations),
    #{password => <<>>,
      details => mongoose_scram:serialize(Scram)}.

-spec prepare_password(Server :: jid:server(), Password :: binary()) -> prepared_password().
prepare_password(Server, Password) ->
    case mongoose_scram:enabled(Server) of
        true ->
            prepare_scrammed_password(Server, mongoose_scram:iterations(Server), Password);
        false ->
            #{password => Password}
    end.

-spec scram_passwords(Server, ScramIterationCount) -> ok | {error, atom()} when
    Server :: jid:lserver(),
    ScramIterationCount :: pos_integer().
scram_passwords(Server, ScramIterationCount) ->
    scram_passwords(Server, ?DEFAULT_SCRAMMIFY_COUNT,
                    ?DEFAULT_SCRAMMIFY_INTERVAL, ScramIterationCount).

-spec scram_passwords(Server, Count, Interval, ScramIterationCount) ->
    ok | {error, atom()} when
        Server :: jid:lserver(),
        Count :: pos_integer(),
        Interval :: pos_integer(),
        ScramIterationCount :: pos_integer().
scram_passwords(Server, Count, Interval, ScramIterationCount) ->
    LServer = jid:nameprep(Server),
    ?LOG_INFO(#{what => scram_passwords, server => Server,
                text => <<"Converting the stored passwords into SCRAM bits">>}),
    ToConvertCount = mongoose_rdbms:selected_to_integer(execute_count_users_without_scram(LServer)),

    ?LOG_INFO(#{what => scram_passwords, server => Server,
                convert_count => ToConvertCount,
                text => <<"Users to scrammify">>}),
    scram_passwords1(LServer, Count, Interval, ScramIterationCount).

-spec scram_passwords1(LServer, Count, Interval, ScramIterationCount) ->
    ok | {error, interrupted} when
        LServer :: jid:lserver(),
        Count :: pos_integer(),
        Interval :: pos_integer(),
        ScramIterationCount :: pos_integer().
scram_passwords1(LServer, Count, Interval, ScramIterationCount) ->
    case execute_list_users_without_scram(LServer, Count) of
        {selected, []} ->
            ?LOG_INFO(#{what => scram_passwords_completed,
                        text => <<"All users scrammed">>}),
            ok;
        {selected, Results} ->
            ?LOG_INFO(#{what => scram_passwords_progress,
                        user_count => length(Results),
                        text => <<"Scramming users in progress...">>}),
            lists:foreach(
              fun({Username, Password}) ->
                ScrammedPassword = prepare_scrammed_password(LServer,
                                                             ScramIterationCount,
                                                             Password),
                execute_set_password(LServer, Username, ScrammedPassword)
              end, Results),
            ?LOG_INFO(#{what => scram_passwords_progress,
                        user_count => length(Results), interval => Interval,
                        text => io_lib:format("Scrammed. Waiting for ~pms", [Interval])}),
            timer:sleep(Interval),
            scram_passwords1(LServer, Count, Interval, ScramIterationCount)
    end.

%%%------------------------------------------------------------------
%%% DB Queries
%%%------------------------------------------------------------------

-spec prepare_queries(jid:lserver()) -> any().
prepare_queries(LServer) ->
    prepare(auth_get_password, users,
            [username],
            <<"SELECT password, pass_details FROM users WHERE username = ?">>),
    prepare(auth_set_password_scram, users,
            [password, pass_details, username],
            <<"UPDATE users SET password = ?, pass_details = ? WHERE username = ?">>),
    prepare(auth_set_password, users,
            [password, username],
            <<"UPDATE users SET password = ? WHERE username = ?">>),
    prepare(auth_add_user_scram, users,
            [username, password, pass_details],
            <<"INSERT INTO users(username, password, pass_details) VALUES (?, ?, ?)">>),
    prepare(auth_add_user, users,
            [username, password],
            <<"INSERT INTO users(username, password) VALUES (?, ?)">>),
    prepare(auth_delete_user, users,
            [username],
            <<"DELETE FROM users WHERE username = ?">>),
    prepare(auth_list_users, users, [],
            <<"SELECT username FROM users">>),
    LimitOffset = rdbms_queries:limit_offset_sql(),
    prepare(auth_list_users_range, users,
            [limit, offset],
            <<"SELECT username FROM users ORDER BY username ", LimitOffset/binary>>),
    prepare(auth_list_users_prefix, users,
            [username],
            <<"SELECT username FROM users WHERE username LIKE ? ESCAPE '$' ORDER BY username">>),
    prepare(auth_list_users_prefix_range, users,
            [username, limit, offset],
            <<"SELECT username FROM users WHERE username LIKE ? ESCAPE '$' ORDER BY username ",
              LimitOffset/binary>>),
    {LimitSQL, LimitMSSQL} = rdbms_queries:get_db_specific_limits_binaries(),
    prepare(auth_list_users_without_scram, users,
            [limit],
            <<"SELECT ", LimitMSSQL/binary, " username, password FROM users "
              "WHERE pass_details is NULL ", LimitSQL/binary>>),
    prepare(auth_count_users_prefix, users,
            [username],
            <<"SELECT COUNT(*) FROM users WHERE username LIKE ? ESCAPE '$'">>),
    prepare_count_users(LServer, mongoose_rdbms:db_engine(LServer)),
    prepare(auth_count_users_without_scram, users, [],
            <<"SELECT COUNT(*) FROM users WHERE pass_details is NULL">>).

-spec prepare_count_users(jid:lserver(), mysql | pgsql | odbc) -> any().
prepare_count_users(_Host, mysql) ->
    prepare(auth_count_users, 'information_schema.tables', [],
            <<"SELECT table_rows FROM information_schema.tables WHERE table_name='users'">>);
prepare_count_users(Host, pgsql) ->
    case ejabberd_config:get_local_option({pgsql_users_number_estimate, Host}) of
        true ->
            prepare(auth_count_users_estimate, pg_class, [],
                    <<"SELECT reltuples FROM pg_class WHERE oid = 'users'::regclass::oid">>);
        _ ->
            prepare(auth_count_users, users, [],
                    <<"SELECT COUNT(*) FROM users">>)
    end;
prepare_count_users(_Host, odbc) ->
    prepare(auth_count_users, users, [],
            <<"SELECT COUNT(*) FROM users">>).

-spec execute_get_password(jid:lserver(), jid:luser()) ->
          mongoose_rdbms:query_result().
execute_get_password(LServer, LUser) ->
    execute_successfully(LServer, auth_get_password, [LUser]).

-spec execute_set_password(jid:lserver(), jid:luser(), prepared_password()) ->
          mongoose_rdbms:query_result().
execute_set_password(LServer, LUser, #{password := Pass, details := PassDetails}) ->
    execute_successfully(LServer, auth_set_password_scram, [Pass, PassDetails, LUser]);
execute_set_password(LServer, LUser, #{password := Pass}) ->
    execute_successfully(LServer, auth_set_password, [Pass, LUser]).

-spec execute_add_user(jid:lserver(), jid:luser(), prepared_password()) ->
          mongoose_rdbms:query_result().
execute_add_user(LServer, LUser, #{password := Pass, details := PassDetails}) ->
    execute_successfully(LServer, auth_add_user_scram, [LUser, Pass, PassDetails]);
execute_add_user(LServer, LUser, #{password := Pass}) ->
    execute_successfully(LServer, auth_add_user, [LUser, Pass]).

-spec execute_delete_user(jid:lserver(), jid:luser()) -> mongoose_rdbms:query_result().
execute_delete_user(LServer, LUser) ->
    execute_successfully(LServer, auth_delete_user, [LUser]).

-spec execute_list_users(jid:lserver(), map()) -> mongoose_rdbms:query_result().
execute_list_users(LServer, #{from := Start, to := End} = OptMap) ->
    Map = maps:without([from, to], OptMap),
    execute_list_users(LServer, Map#{limit => End - Start + 1, offset => Start - 1});
execute_list_users(LServer, #{prefix := Prefix, limit := Limit, offset := Offset}) ->
    Args = [prefix_to_like(Prefix)] ++ rdbms_queries:limit_offset_args(Limit, Offset),
    execute_successfully(LServer, auth_list_users_prefix_range, Args);
execute_list_users(LServer, #{prefix := Prefix}) ->
    Args = [prefix_to_like(Prefix)],
    execute_successfully(LServer, auth_list_users_prefix, Args);
execute_list_users(LServer, #{limit := Limit, offset := Offset}) ->
    Args = rdbms_queries:limit_offset_args(Limit, Offset),
    execute_successfully(LServer, auth_list_users_range, Args);
execute_list_users(LServer, #{}) ->
    execute_successfully(LServer, auth_list_users, []).

-spec execute_list_users_without_scram(jid:lserver(), non_neg_integer()) ->
          mongoose_rdbms:query_result().
execute_list_users_without_scram(LServer, Limit) ->
    execute_successfully(LServer, auth_list_users_without_scram, [Limit]).

-spec execute_count_users(jid:lserver(), map()) -> mongoose_rdbms:query_result().
execute_count_users(LServer, #{prefix := Prefix}) ->
    Args = [prefix_to_like(Prefix)],
    execute_successfully(LServer, auth_count_users_prefix, Args);
execute_count_users(LServer, #{}) ->
    Query = case {ejabberd_config:get_local_option({pgsql_users_number_estimate, Host}),
                  mongoose_rdbms:db_engine(LServer)} of
                {true, pgsql} -> auth_count_users_estimate;
                _ -> auth_count_users
            end,
    execute_successfully(LServer, Query, []).

-spec execute_count_users_without_scram(jid:lserver()) -> mongoose_rdbms:query_result().
execute_count_users_without_scram(LServer) ->
    execute_successfully(LServer, auth_count_users_without_scram, []).

-spec prefix_to_like(binary()) -> binary().
prefix_to_like(Prefix) ->
    EscapedPrefix = mongoose_rdbms:escape_prepared_like(Prefix),
    <<EscapedPrefix/binary, $%>>.
