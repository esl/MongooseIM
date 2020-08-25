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

-include("mongoose.hrl").
-include("scram.hrl").

-define(DEFAULT_SCRAMMIFY_COUNT, 10000).
-define(DEFAULT_SCRAMMIFY_INTERVAL, 1000).


%%%----------------------------------------------------------------------
%%% Types
%%%----------------------------------------------------------------------

%% Used to construct queries
-type escaped_password() :: mongoose_rdbms:escaped_string().
-type escaped_password_details() :: mongoose_rdbms:escaped_string().

-type prepared_scrammed_password() ::
        #{password => escaped_password(),
          details => escaped_password_details()}.
%% Both non-scram and scram versions
-type prepared_password() :: escaped_password() | prepared_scrammed_password().

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

start(_Host) ->
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
    Username = mongoose_rdbms:escape_string(LUser),
    true == check_password_wo_escape(LUser, Username, LServer, Password).


-spec check_password(LUser :: jid:luser(),
                     LServer :: jid:lserver(),
                     Password :: binary(),
                     Digest :: binary(),
                     DigestGen :: fun()) -> boolean().
check_password(LUser, LServer, Password, Digest, DigestGen) ->
    Username = mongoose_rdbms:escape_string(LUser),
    try rdbms_queries:get_password(LServer, Username) of
        %% Account exists, check if password is valid
        {selected, [{Passwd, null}]} ->
            ejabberd_auth:check_digest(Digest, DigestGen, Password, Passwd);
        {selected, [{_Passwd, PassDetails}]} ->
            case mongoose_scram:deserialize(PassDetails) of
                {ok, Scram} ->
                    mongoose_scram:check_digest(Scram, Digest, DigestGen, Password);
                {error, Reason} ->
                    ?LOG_WARNING(#{what => scram_serialisation_incorrect, reason => Reason,
                                   user => Username, server => LServer}),
                    false
            end;
        {selected, []} ->
            false; %% Account does not exist
        {error, Error} ->
            ?LOG_ERROR(#{what => check_password_failed,
                         user => LUser, server => LServer,
                         reason => Error}),
            false %% Typical error is that table doesn't exist
    catch
        Class:Reason:StackTrace ->
            ?LOG_ERROR(#{what => check_password_failed,
                         user => LUser, server => LServer,
                         class => Class, reason => Reason, stacktrace => StackTrace}),
            false %% Typical error is database not accessible
    end.

-spec check_password_wo_escape(LUser :: jid:luser(),
                               Username::mongoose_rdbms:escaped_string(),
                               LServer::jid:lserver(),
                               Password::binary()) -> boolean() | not_exists.
check_password_wo_escape(LUser, Username, LServer, Password) ->
    try rdbms_queries:get_password(LServer, Username) of
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
                                   user => Username, server => LServer}),
                    false %% Password is not correct
            end;
        {selected, []} ->
            not_exists; %% Account does not exist
        {error, Error} ->
            ?LOG_ERROR(#{what => check_password_failed,
                         user => LUser, server => LServer,
                         reason => Error}),
            false %% Typical error is that table doesn't exist
    catch
        Class:Reason:StackTrace ->
            ?LOG_ERROR(#{what => check_password_failed,
                         user => LUser, server => LServer,
                         class => Class, reason => Reason, stacktrace => StackTrace}),
            false %% Typical error is database not accessible
    end.


-spec set_password(LUser :: jid:luser(),
                   LServer :: jid:lserver(),
                   Password :: binary()
                   ) -> ok | {error, not_allowed}.
set_password(LUser, LServer, Password) ->
    Username = mongoose_rdbms:escape_string(LUser),
    PreparedPass = prepare_password(LServer, Password),
    case catch rdbms_queries:set_password_t(LServer, Username, PreparedPass) of
        {atomic, ok} ->
            ok;
        Error ->
            ?LOG_WARNING(#{what => set_password_failed,
                           text => <<"Failed SQL request for set_password">>,
                           user => LUser, server => LServer, reason => Error}),
            {error, not_allowed}
    end.

-spec try_register(LUser :: jid:luser(),
                   LServer :: jid:lserver(),
                   Password :: binary()
                   ) -> ok | {error, exists}.
try_register(LUser, LServer, Password) ->
    Username = mongoose_rdbms:escape_string(LUser),
    PreparedPass = prepare_password(LServer, Password),
    case catch rdbms_queries:add_user(LServer, Username, PreparedPass) of
        {updated, 1} ->
            ok;
        {updated, 0} ->
            {error, exists};
        Other ->
            ?LOG_ERROR(#{what => registration_failed,
                         text => <<"Failed SQL request for try_register">>,
                         user => LUser, server => LServer, reason => Other}),
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
    try rdbms_queries:list_users(LServer) of
        {selected, Res} ->
            [{U, LServer} || {U} <- Res];
        Other ->
            ?LOG_ERROR(#{what => get_vh_registered_users_failed,
                         server => LServer, reason => Other}),
            []
    catch Class:Reason:StackTrace ->
        ?LOG_ERROR(#{what => get_vh_registered_users_failed, server => LServer,
                     class => Class, reason => Reason, stacktrace => StackTrace}),
        []
    end.


-spec get_vh_registered_users(LServer :: jid:lserver(), Opts :: list()
                             ) -> [jid:simple_bare_jid()].
get_vh_registered_users(LServer, Opts) ->
    try rdbms_queries:list_users(LServer, Opts) of
        {selected, Res} ->
            [{U, LServer} || {U} <- Res];
        Other ->
            ?LOG_ERROR(#{what => get_vh_registered_users_failed,
                         server => LServer, opts => Opts, reason => Other}),
            []
    catch Class:Reason:StackTrace ->
        ?LOG_ERROR(#{what => get_vh_registered_users_failed, server => LServer,
                     class => Class, reason => Reason, stacktrace => StackTrace}),
        []
    end.


-spec get_vh_registered_users_number(LServer :: jid:lserver()
                                    ) -> integer().
get_vh_registered_users_number(LServer) ->
    try rdbms_queries:users_number(LServer) of
        {selected, [{Res}]} when is_integer(Res) ->
            Res;
        {selected, [{Res}]} ->
            mongoose_rdbms:result_to_integer(Res);
        Other ->
            ?LOG_ERROR(#{what => get_vh_registered_users_numbers_failed,
                         server => LServer, reason => Other}),
            0
    catch Class:Reason:StackTrace ->
        ?LOG_ERROR(#{what => get_vh_registered_users_numbers_failed, server => LServer,
                     class => Class, reason => Reason, stacktrace => StackTrace}),
        0
    end.


-spec get_vh_registered_users_number(LServer :: jid:lserver(),
                                     Opts :: list()) -> integer().
get_vh_registered_users_number(LServer, Opts) ->
    case catch rdbms_queries:users_number(LServer, Opts) of
        {selected, [{Res}]} ->
            list_to_integer(Res);
        Other ->
            ?LOG_ERROR(#{what => get_vh_registered_users_numbers_failed,
                         server => LServer, opts => Opts, reason => Other}),
            0
    end.


-spec get_password(jid:luser(), jid:lserver()) -> ejabberd_auth:passterm() | false.
get_password(LUser, LServer) ->
    Username = mongoose_rdbms:escape_string(LUser),
    case catch rdbms_queries:get_password(LServer, Username) of
        {selected, [{Password, null}]} ->
            Password; %%Plain password
        {selected, [{_Password, PassDetails}]} ->
            case mongoose_scram:deserialize(PassDetails) of
                {ok, Scram} ->
                    Scram;
                {error, Reason} ->
                    ?LOG_WARNING(#{what => scram_serialisation_incorrect, reason => Reason,
                                   user => Username, server => LServer}),
                    false
            end;
        {selected, []} ->
            false;
        Other ->
            ?LOG_ERROR(#{what => get_password_failed,
                         user => LUser, server => LServer, reason => Other}),
            false
    end.


-spec get_password_s(LUser :: jid:user(),
                     LServer :: jid:server()) -> binary().
get_password_s(LUser, LServer) ->
    Username = mongoose_rdbms:escape_string(LUser),
    case catch rdbms_queries:get_password(LServer, Username) of
        {selected, [{Password, _}]} ->
            Password;
        {selected, []} ->
            <<>>;
        Other ->
            ?LOG_ERROR(#{what => get_password_s_failed,
                         user => LUser, server => LServer, reason => Other}),
            <<>>
    end.


-spec does_user_exist(LUser :: jid:luser(),
                     LServer :: jid:lserver()
                    ) -> boolean() | {error, atom()}.
does_user_exist(LUser, LServer) ->
    Username = mongoose_rdbms:escape_string(LUser),
    try rdbms_queries:get_password(LServer, Username) of
        {selected, [{_Password, _}]} ->
            true; %% Account exists
        {selected, []} ->
            false; %% Account does not exist
        {error, Error} ->
            ?LOG_ERROR(#{what => does_user_exist_failed,
                         user => LUser, server => LServer, reason => Error}),
            {error, Error} %% Typical error is that table doesn't exist
    catch
        Class:Reason:StackTrace ->
            ?LOG_ERROR(#{what => does_user_exist_failed,
                         user => LUser, server => LServer,
                         class => Class, reason => Reason, stacktrace => StackTrace}),
            {error, Reason} %% Typical error is database not accessible
    end.


%% @doc Remove user.
%% Note: it may return ok even if there was some problem removing the user.
-spec remove_user(LUser :: jid:luser(),
                  LServer :: jid:lserver()
                  ) -> ok.
remove_user(LUser, LServer) ->
    Username = mongoose_rdbms:escape_string(LUser),
    try rdbms_queries:del_user(LServer, Username)
    catch Class:Reason:StackTrace ->
        ?LOG_ERROR(#{what => remove_user_failed,
                     user => LUser, server => LServer,
                     class => Class, reason => Reason, stacktrace => StackTrace}),
        ok
    end,
    ok.

%%%------------------------------------------------------------------
%%% SCRAM
%%%------------------------------------------------------------------

-spec prepare_scrammed_password(Server, Iterations, Password) ->
    prepared_scrammed_password() when
        Server :: jid:lserver(),
        Iterations :: pos_integer(),
        Password :: binary().
prepare_scrammed_password(Server, Iterations, Password) when is_integer(Iterations) ->
    Scram = mongoose_scram:password_to_scram(Server, Password, Iterations),
    PassDetails = mongoose_scram:serialize(Scram),
    PassDetailsEscaped = mongoose_rdbms:escape_string(PassDetails),
    EmptyPassword = mongoose_rdbms:escape_string(<<>>),
    #{password => EmptyPassword,
      details => PassDetailsEscaped}.

-spec prepare_password(Server :: jid:server(), Password :: binary()) ->
    PreparedPassword :: prepared_password().
prepare_password(Server, Password) ->
    case mongoose_scram:enabled(Server) of
        true ->
            prepare_scrammed_password(Server, mongoose_scram:iterations(Server), Password);
        _ ->
            mongoose_rdbms:escape_string(Password)
    end.

scram_passwords(Server, ScramIterationCount) ->
    scram_passwords(Server, ?DEFAULT_SCRAMMIFY_COUNT,
                    ?DEFAULT_SCRAMMIFY_INTERVAL, ScramIterationCount).

scram_passwords(Server, Count, Interval, ScramIterationCount) ->
    LServer = jid:nameprep(Server),
    ?LOG_INFO(#{what => scram_passwords, server => Server,
                text => <<"Converting the stored passwords into SCRAM bits">>}),
    ToConvertCount = case catch rdbms_queries:get_users_without_scram_count(LServer) of
        {selected, [{Res}]} -> binary_to_integer(Res);
        _ -> 0
    end,

    ?LOG_INFO(#{what => scram_passwords, server => Server,
                convert_count => ToConvertCount,
                text => <<"Users to scrammify">>}),
    scram_passwords1(LServer, Count, Interval, ScramIterationCount).

scram_passwords1(LServer, Count, Interval, ScramIterationCount) ->
    case rdbms_queries:get_users_without_scram(LServer, Count) of
        {selected, []} ->
            ?LOG_INFO(#{what => scram_passwords_completed,
                        text => <<"All users scrammed">>});
        {selected, Results} ->
            ?LOG_INFO(#{what => scram_passwords_progress,
                        user_count => length(Results),
                        text => <<"Scramming users in progress...">>}),
            lists:foreach(
              fun({Username, Password}) ->
                ScrammedPassword = prepare_scrammed_password(LServer,
                                                             ScramIterationCount,
                                                             Password),
                write_scrammed_password_to_rdbms(LServer, Username, ScrammedPassword)
              end, Results),
            ?LOG_INFO(#{what => scram_passwords_progress,
                        user_count => length(Results), interval => Interval,
                        text => io_lib:format("Scrammed. Waiting for ~pms", [Interval])}),
            timer:sleep(Interval),
            scram_passwords1(LServer, Count, Interval, ScramIterationCount);
        Other ->
            ?LOG_ERROR(#{what => scram_passwords_failed,
                         text => <<"Interrupted scramming">>,
                         server => LServer, reason => Other})
    end.

write_scrammed_password_to_rdbms(LServer, Username, ScrammedPassword) ->
    case catch rdbms_queries:set_password_t(LServer, Username,
                                            ScrammedPassword) of
        {atomic, ok} -> ok;
        Other ->
            ?LOG_ERROR(#{what => scrammify_user_failed,
                         text => <<"Could not scrammify user">>,
                         user => Username, server => LServer, reason => Other}),
            Other
    end.
