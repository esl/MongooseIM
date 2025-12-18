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
-behaviour(mongoose_gen_auth).

-export([start/1,
         stop/1,
         config_spec/0,
         authorize/1,
         set_password/4,
         try_register/4,
         get_registered_users/3,
         get_registered_users_number/3,
         get_password/3,
         get_password_s/3,
         does_user_exist/3,
         remove_user/3,
         remove_domain/2,
         supports_sasl_module/2,
         supported_features/0
        ]).

%% Internal
-export([check_password/4,
         check_password/6]).

-export([scram_passwords/2, scram_passwords/4]).

-ignore_xref([scram_passwords/2, scram_passwords/4]).

-import(mongoose_rdbms, [prepare/4, execute_successfully/3]).

-include("mongoose.hrl").
-include("mongoose_config_spec.hrl").

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

-spec start(mongooseim:host_type()) -> ok.
start(HostType) ->
    prepare_queries(HostType),
    ok.

-spec stop(mongooseim:host_type()) -> ok.
stop(_HostType) ->
    ok.

-spec config_spec() -> mongoose_config_spec:config_section().
config_spec() ->
    #section{
       items = #{<<"users_number_estimate">> => #option{type = boolean}},
       defaults = #{<<"users_number_estimate">> => false}
      }.

-spec supports_sasl_module(mongooseim:host_type(), cyrsasl:sasl_module()) -> boolean().
supports_sasl_module(_HostType, cyrsasl_plain) -> true;
supports_sasl_module(HostType, cyrsasl_digest) -> not mongoose_scram:enabled(HostType);
supports_sasl_module(HostType, Mechanism) -> mongoose_scram:enabled(HostType, Mechanism).

-spec authorize(mongoose_credentials:t()) -> {ok, mongoose_credentials:t()}
                                           | {error, any()}.
authorize(Creds) ->
    ejabberd_auth:authorize_with_check_password(?MODULE, Creds).

-spec check_password(HostType :: mongooseim:host_type(),
                     LUser :: jid:luser(),
                     LServer :: jid:lserver(),
                     Password :: binary()) -> boolean().
check_password(HostType, LUser, LServer, Password) ->
    try execute_get_password(HostType, LServer, LUser) of
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

-spec check_password(HostType :: mongooseim:host_type(),
                     LUser :: jid:luser(),
                     LServer :: jid:lserver(),
                     Password :: binary(),
                     Digest :: binary(),
                     DigestGen :: fun()) -> boolean().

check_password(HostType, LUser, LServer, Password, Digest, DigestGen) ->
    try execute_get_password(HostType, LServer, LUser) of
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

-spec set_password(HostType :: mongooseim:host_type(),
                   LUser :: jid:luser(),
                   LServer :: jid:lserver(),
                   Password :: binary()
                   ) -> ok | {error, not_allowed}.
set_password(HostType, LUser, LServer, Password) ->
    PreparedPass = prepare_password(HostType, Password),
    try
        execute_set_password(HostType, LServer, LUser, PreparedPass),
        ok
    catch
        error:Reason:StackTrace ->
            ?LOG_ERROR(#{what => set_password_failed,
                         user => LUser, server => LServer,
                         class => error, reason => Reason, stacktrace => StackTrace}),
            {error, not_allowed}
    end.

-spec try_register(HostType :: mongooseim:host_type(),
                   LUser :: jid:luser(),
                   LServer :: jid:lserver(),
                   Password :: binary()
                   ) -> ok | {error, exists}.
try_register(HostType, LUser, LServer, Password) ->
    PreparedPass = prepare_password(HostType, Password),
    try execute_add_user(HostType, LServer, LUser, PreparedPass) of
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

-spec get_registered_users(mongooseim:host_type(), jid:lserver(), Opts :: list()) ->
          [jid:simple_bare_jid()].
get_registered_users(HostType, LServer, Opts) ->
    try
        {selected, Res} = execute_list_users(HostType, LServer, maps:from_list(Opts)),
        [{U, LServer} || {U} <- Res]
    catch error:Reason:StackTrace ->
        ?LOG_ERROR(#{what => get_vh_registered_users_failed, server => LServer,
                     class => error, reason => Reason, stacktrace => StackTrace}),
        []
    end.

-spec get_registered_users_number(mongooseim:host_type(), jid:lserver(), Opts :: list()) ->
          non_neg_integer().
get_registered_users_number(HostType, LServer, Opts) ->
    try
        Selected = execute_count_users(HostType, LServer, maps:from_list(Opts)),
        mongoose_rdbms:selected_to_integer(Selected)
    catch error:Reason:StackTrace ->
        ?LOG_ERROR(#{what => get_vh_registered_users_numbers_failed, server => LServer,
                     class => error, reason => Reason, stacktrace => StackTrace}),
        0
    end.

-spec get_password(mongooseim:host_type(), jid:luser(), jid:lserver()) ->
          ejabberd_auth:passterm() | false.
get_password(HostType, LUser, LServer) ->
    try execute_get_password(HostType, LServer, LUser) of
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


-spec get_password_s(mongooseim:host_type(), jid:luser(), jid:lserver()) -> binary().
get_password_s(HostType, LUser, LServer) ->
    try execute_get_password(HostType, LServer, LUser) of
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


-spec does_user_exist(mongooseim:host_type(), jid:luser(), jid:lserver()) ->
          boolean() | {error, atom()}.
does_user_exist(HostType, LUser, LServer) ->
    try execute_get_password(HostType, LServer, LUser) of
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
-spec remove_user(mongooseim:host_type(), jid:luser(), jid:lserver()) -> ok.
remove_user(HostType, LUser, LServer) ->
    try
        execute_delete_user(HostType, LServer, LUser)
    catch
        error:Reason:StackTrace ->
            ?LOG_ERROR(#{what => remove_user_failed,
                         user => LUser, server => LServer,
                         class => error, reason => Reason, stacktrace => StackTrace})
    end,
    ok.

-spec remove_domain(mongooseim:host_type(), jid:lserver()) -> ok.
remove_domain(HostType, Domain) ->
    execute_successfully(HostType, auth_remove_domain, [Domain]),
    ok.

-spec supported_features() -> [atom()].
supported_features() -> [dynamic_domains].

%%%------------------------------------------------------------------
%%% Scram
%%%------------------------------------------------------------------

-spec prepare_scrammed_password(HostType, Iterations, Password) -> prepared_password() when
        HostType :: mongooseim:host_type(),
        Iterations :: pos_integer(),
        Password :: binary().
prepare_scrammed_password(HostType, Iterations, Password) when is_integer(Iterations) ->
    Scram = mongoose_scram:password_to_scram(HostType, Password, Iterations),
    #{password => <<>>,
      details => mongoose_scram:serialize(Scram)}.

-spec prepare_password(HostType :: mongooseim:host_type(), Password :: binary()) ->
          prepared_password().
prepare_password(HostType, Password) ->
    case mongoose_scram:enabled(HostType) of
        true ->
            prepare_scrammed_password(HostType, mongoose_scram:iterations(HostType), Password);
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
    {ok, HostType} = mongoose_domain_api:get_domain_host_type(LServer),
    ?LOG_INFO(#{what => scram_passwords, server => Server,
                text => <<"Converting the stored passwords into SCRAM bits">>}),
    Selected = execute_count_users_without_scram(HostType, LServer),
    ToConvertCount = mongoose_rdbms:selected_to_integer(Selected),

    ?LOG_INFO(#{what => scram_passwords, server => Server,
                convert_count => ToConvertCount,
                text => <<"Users to scrammify">>}),
    scram_passwords1(HostType, LServer, Count, Interval, ScramIterationCount).

-spec scram_passwords1(HostType, LServer, Count, Interval, ScramIterationCount) ->
    ok | {error, interrupted} when
        HostType :: mongooseim:host_type(),
        LServer :: jid:lserver(),
        Count :: pos_integer(),
        Interval :: pos_integer(),
        ScramIterationCount :: pos_integer().
scram_passwords1(HostType, LServer, Count, Interval, ScramIterationCount) ->
    case execute_list_users_without_scram(HostType, LServer, Count) of
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
                ScrammedPassword = prepare_scrammed_password(HostType,
                                                             ScramIterationCount,
                                                             Password),
                execute_set_password(HostType, LServer, Username, ScrammedPassword)
              end, Results),
            ?LOG_INFO(#{what => scram_passwords_progress,
                        user_count => length(Results), interval => Interval,
                        text => io_lib:format("Scrammed. Waiting for ~pms", [Interval])}),
            timer:sleep(Interval),
            scram_passwords1(HostType, LServer, Count, Interval, ScramIterationCount)
    end.

%%%------------------------------------------------------------------
%%% DB Queries
%%%------------------------------------------------------------------

-spec prepare_queries(mongooseim:host_type()) -> any().
prepare_queries(HostType) ->
    prepare(auth_get_password, users,
            [server, username],
            <<"SELECT password, pass_details FROM users "
              "WHERE server = ? AND username = ?">>),
    prepare(auth_set_password_scram, users,
            [password, pass_details, server, username],
            <<"UPDATE users SET password = ?, pass_details = ? "
              "WHERE server = ? AND username = ?">>),
    prepare(auth_set_password, users,
            [password, server, username],
            <<"UPDATE users SET password = ? WHERE server = ? AND username = ?">>),
    prepare(auth_add_user_scram, users,
            [server, username, password, pass_details],
            <<"INSERT INTO users(server, username, password, pass_details) VALUES (?, ?, ?, ?)">>),
    prepare(auth_add_user, users,
            [server, username, password],
            <<"INSERT INTO users(server, username, password) VALUES (?, ?, ?)">>),
    prepare(auth_delete_user, users,
            [server, username],
            <<"DELETE FROM users WHERE server = ? AND username = ?">>),
    prepare(auth_list_users, users, [server],
            <<"SELECT username FROM users WHERE server = ?">>),
    LimitOffset = rdbms_queries:limit_offset(),
    prepare(auth_list_users_range, users,
            [server, limit, offset],
            <<"SELECT username FROM users WHERE server = ? ORDER BY username ",
              LimitOffset/binary>>),
    prepare(auth_list_users_prefix, users,
            [server, username],
            <<"SELECT username FROM users "
              "WHERE server = ? AND username LIKE ? ESCAPE '$' ORDER BY username">>),
    prepare(auth_list_users_prefix_range, users,
            [server, username, limit, offset],
            <<"SELECT username FROM users "
              "WHERE server = ? AND username LIKE ? ESCAPE '$' ORDER BY username ",
              LimitOffset/binary>>),
    LimitSQL = rdbms_queries:limit(),
    prepare(auth_list_users_without_scram, users,
            [server, limit],
            <<"SELECT username, password FROM users "
              "WHERE server = ? AND pass_details is NULL", LimitSQL/binary>>),
    prepare(auth_count_users_prefix, users,
            [server, username],
            <<"SELECT COUNT(*) FROM users WHERE server = ? AND username LIKE ? ESCAPE '$'">>),
    prepare_count_users(HostType),
    prepare(auth_count_users_without_scram, users, [server],
            <<"SELECT COUNT(*) FROM users WHERE server = ? AND pass_details is NULL">>),
    prepare(auth_remove_domain, users, [server],
            <<"DELETE FROM users WHERE server = ?">>).

prepare_count_users(HostType) ->
    case {mongoose_config:get_opt([{auth, HostType}, rdbms, users_number_estimate]),
          mongoose_rdbms:db_engine(HostType)} of
        {true, mysql} ->
            prepare(auth_count_users_estimate, 'information_schema.tables', [],
                    <<"SELECT table_rows FROM information_schema.tables "
                      "WHERE table_name = 'users'">>);
        {true, Driver} when Driver =:= pgsql; Driver =:= cockroachdb ->
            prepare_count_users(),
            prepare(auth_count_users_estimate, pg_class, [],
                    <<"SELECT reltuples::numeric FROM pg_class "
                      "WHERE oid = 'users'::regclass::oid">>);
        _ ->
            prepare_count_users()
    end.

prepare_count_users() ->
    prepare(auth_count_users, users, [server], <<"SELECT COUNT(*) FROM users WHERE server = ?">>).

-spec execute_get_password(mongooseim:host_type(), jid:lserver(), jid:luser()) ->
          mongoose_rdbms:query_result().
execute_get_password(HostType, LServer, LUser) ->
    execute_successfully(HostType, auth_get_password, [LServer, LUser]).

-spec execute_set_password(mongooseim:host_type(), jid:lserver(), jid:luser(),
                           prepared_password()) ->
          mongoose_rdbms:query_result().
execute_set_password(HostType, LServer, LUser, #{password := Pass, details := PassDetails}) ->
    execute_successfully(HostType, auth_set_password_scram, [Pass, PassDetails, LServer, LUser]);
execute_set_password(HostType, LServer, LUser, #{password := Pass}) ->
    execute_successfully(HostType, auth_set_password, [Pass, LServer, LUser]).

-spec execute_add_user(mongooseim:host_type(), jid:lserver(), jid:luser(), prepared_password()) ->
          mongoose_rdbms:query_result().
execute_add_user(HostType, LServer, LUser, #{password := Pass, details := PassDetails}) ->
    execute_successfully(HostType, auth_add_user_scram, [LServer, LUser, Pass, PassDetails]);
execute_add_user(HostType, LServer, LUser, #{password := Pass}) ->
    execute_successfully(HostType, auth_add_user, [LServer, LUser, Pass]).

-spec execute_delete_user(mongooseim:host_type(), jid:lserver(), jid:luser()) ->
          mongoose_rdbms:query_result().
execute_delete_user(HostType, LServer, LUser) ->
    execute_successfully(HostType, auth_delete_user, [LServer, LUser]).

-spec execute_list_users(mongooseim:host_type(), jid:lserver(), map()) ->
          mongoose_rdbms:query_result().
execute_list_users(HostType, LServer, #{from := Start, to := End} = OptMap) ->
    Map = maps:without([from, to], OptMap),
    execute_list_users(HostType, LServer, Map#{limit => End - Start + 1, offset => Start - 1});
execute_list_users(HostType, LServer, #{prefix := Prefix, limit := Limit, offset := Offset}) ->
    Args = [LServer, prefix_to_like(Prefix), Limit, Offset],
    execute_successfully(HostType, auth_list_users_prefix_range, Args);
execute_list_users(HostType, LServer, #{prefix := Prefix}) ->
    Args = [LServer, prefix_to_like(Prefix)],
    execute_successfully(HostType, auth_list_users_prefix, Args);
execute_list_users(HostType, LServer, #{limit := Limit, offset := Offset}) ->
    Args = [LServer, Limit, Offset],
    execute_successfully(HostType, auth_list_users_range, Args);
execute_list_users(HostType, LServer, #{}) ->
    execute_successfully(HostType, auth_list_users, [LServer]).

-spec execute_list_users_without_scram(mongooseim:host_type(), jid:lserver(), non_neg_integer()) ->
          mongoose_rdbms:query_result().
execute_list_users_without_scram(HostType, LServer, Limit) ->
    execute_successfully(HostType, auth_list_users_without_scram, [LServer, Limit]).

-spec execute_count_users(mongooseim:host_type(), jid:lserver(), map()) ->
          mongoose_rdbms:query_result().
execute_count_users(HostType, LServer, #{prefix := Prefix}) ->
    Args = [LServer, prefix_to_like(Prefix)],
    execute_successfully(HostType, auth_count_users_prefix, Args);
execute_count_users(HostType, LServer, #{}) ->
    case {mongoose_config:get_opt([{auth, HostType}, rdbms, users_number_estimate]),
          mongoose_rdbms:db_engine(LServer)} of
        {true, mysql} ->
            execute_successfully(HostType, auth_count_users_estimate, []);
        {true, Driver} when Driver =:= pgsql; Driver =:= cockroachdb ->
            case execute_successfully(HostType, auth_count_users_estimate, []) of
                {selected,[{<<"-1">>}]} ->
                    execute_successfully(HostType, auth_count_users, [LServer]);
                Otherwise ->
                    Otherwise
            end;
        _ ->
            execute_successfully(HostType, auth_count_users, [LServer])
    end.

-spec execute_count_users_without_scram(mongooseim:host_type(), jid:lserver()) ->
          mongoose_rdbms:query_result().
execute_count_users_without_scram(HostType, LServer) ->
    execute_successfully(HostType, auth_count_users_without_scram, [LServer]).

-spec prefix_to_like(binary()) -> binary().
prefix_to_like(Prefix) ->
    EscapedPrefix = mongoose_rdbms:escape_prepared_like(Prefix),
    <<EscapedPrefix/binary, $%>>.
