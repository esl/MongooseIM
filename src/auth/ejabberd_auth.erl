%%% File    : ejabberd_auth.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Authentification
%%% Created : 23 Nov 2002 by Alexey Shchepin <alexey@process-one.net>
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

-module(ejabberd_auth).
-author('alexey@process-one.net').

%% External exports
-export([start/0,
         start/1,
         stop/1,
         authorize/1,
         set_password/2,
         check_password/2,
         check_password/4,
         try_register/2,
         get_vh_registered_users/1,
         get_vh_registered_users/2,
         get_vh_registered_users_number/1,
         get_vh_registered_users_number/2,
         get_password_s/1,
         get_passterm_with_authmodule/2,
         does_user_exist/1,
         does_user_exist/3,
         does_stored_user_exist/2,
         does_method_support/2,
         remove_user/1,
         supports_sasl_module/2,
         entropy/1,
         config_spec/1
        ]).

-export([check_digest/4]).

-export([auth_modules/1,
         auth_methods/1,
         auth_modules_for_host_type/1,
         methods_to_modules/1]).

%% Library functions for reuse in ejabberd_auth_* modules
-export([authorize_with_check_password/2]).

%% Hook handlers
-export([remove_domain/3]).
-export([on_does_user_exist/3]).

-ignore_xref([
    auth_methods/1, auth_modules/1, check_password/4, get_vh_registered_users/2,
    get_vh_registered_users_number/2, start/1, stop/1]).

-include("mongoose.hrl").
-include("jlib.hrl").

-export_type([authmodule/0,
              passterm/0,
              exist_type/0]).

-type authmodule() :: module().
-type passterm() :: binary() | mongoose_scram:scram_tuple() | mongoose_scram:scram_map().
-type exist_type() :: stored | with_anonymous.

%% Types defined below are used in call_auth_modules_*
-type mod_res() :: any().
-type host_type_mod_fun() :: fun((mongooseim:host_type(), authmodule()) -> mod_res()).
-type mod_fun() :: fun((authmodule()) -> mod_res()).
-type mod_fold_fun() :: fun((authmodule(), mod_res()) -> continue |
                                                         {continue, mod_res()} |
                                                         {stop, mod_res()}).
-type call_opts() :: #{default => mod_res(), op => map, metric => atom()}.

-define(METRIC(Name), [backends, auth, Name]).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
-spec start() -> 'ok'.
start() ->
    lists:foreach(fun start/1, ?ALL_HOST_TYPES).

-spec start(HostType :: mongooseim:host_type()) -> 'ok'.
start(HostType) ->
    ensure_metrics(HostType),
    F = fun(Mod) -> mongoose_gen_auth:start(Mod, HostType) end,
    call_auth_modules_for_host_type(HostType, F, #{op => map}),
    gen_hook:add_handlers(hooks(HostType)),
    ok.

-spec stop(HostType :: mongooseim:host_type()) -> 'ok'.
stop(HostType) ->
    gen_hook:delete_handlers(hooks(HostType)),
    F = fun(Mod) -> mongoose_gen_auth:stop(Mod, HostType) end,
    call_auth_modules_for_host_type(HostType, F, #{op => map}),
    ok.

-spec hooks(mongooseim:host_type()) -> gen_hook:hook_list().
hooks(HostType) ->
    [
        %% These hooks must run in between those of mod_cache_users
        {does_user_exist, HostType, fun ?MODULE:on_does_user_exist/3, #{}, 50},
        %% It is important that this handler happens _before_ all other modules
        {remove_domain, HostType, fun ?MODULE:remove_domain/3, #{}, 10}
    ].

-spec supports_sasl_module(mongooseim:host_type(), cyrsasl:sasl_module()) -> boolean().
supports_sasl_module(HostType, SASLModule) ->
    F = fun(Mod) ->
                case mongoose_gen_auth:supports_sasl_module(Mod, HostType, SASLModule) of
                    true -> {stop, true};
                    false -> continue
                end
        end,
    call_auth_modules_for_host_type(HostType, F, #{default => false}).

-spec authorize(mongoose_credentials:t()) -> {ok, mongoose_credentials:t()}
                                           | {error, not_authorized}.
authorize(Creds) ->
    F = fun(Mod, {_CurResult, CurCreds}) ->
                case mongoose_gen_auth:authorize(Mod, CurCreds) of
                    {ok, NewCreds} ->
                        {stop, {ok, mongoose_credentials:register(NewCreds, Mod, success)}};
                    Error ->
                        NewCreds = mongoose_credentials:register(CurCreds, Mod, {failure, Error}),
                        {continue, {not_authorized, NewCreds}}
                end
        end,
    Opts = #{default => {not_authorized, Creds}, metric => authorize},
    case call_auth_modules_with_creds(Creds, F, Opts) of
        Res = {ok, _Creds} -> Res;
        {not_authorized, _Creds} -> {error, not_authorized}
    end.

%% @doc Check if at least one authentication method accepts the user and the password.
-spec check_password(JID :: jid:jid() | error, Password :: binary()) -> boolean().
check_password(error, _Password) ->
    false;
check_password(#jid{luser = LUser, lserver = LServer}, Password) ->
    F = fun(HostType, Mod) ->
                case  mongoose_gen_auth:check_password(Mod, HostType, LUser, LServer, Password) of
                    true -> {stop, true};
                    false -> continue
                end
        end,
    Opts = #{default => false, metric => check_password},
    call_auth_modules_for_domain(LServer, F, Opts).

%% @doc Check if at least one authentication method accepts the user and the password.
-spec check_password(JID :: jid:jid() | error,
                     Password :: binary(),
                     Digest :: binary(),
                     DigestGen :: fun((binary()) -> binary())) -> boolean().
check_password(error, _, _, _) ->
    false;
check_password(#jid{luser = LUser, lserver = LServer}, Password, Digest, DigestGen) ->
    F = fun(HostType, Mod) ->
                case mongoose_gen_auth:check_password(Mod, HostType, LUser, LServer,
                                                      Password, Digest, DigestGen) of
                    true -> {stop, true};
                    false -> continue
                end
        end,
    Opts = #{default => false, metric => check_password},
    call_auth_modules_for_domain(LServer, F, Opts).

-spec check_digest(binary(), fun((binary()) -> binary()), binary(), binary()) -> boolean().
check_digest(<<>>, _, <<>>, _) ->
    false; %% empty digest and password
check_digest(Digest, DigestGen, _Password, Passwd) ->
    Digest == DigestGen(Passwd).

-spec set_password(jid:jid() | error, binary()) ->
    ok | {error, empty_password | not_allowed | invalid_jid}.
set_password(_, <<"">>) ->
    {error, empty_password};
set_password(error, _) ->
    {error, invalid_jid};
set_password(#jid{luser = LUser, lserver = LServer}, Password) ->
    F = fun(HostType, Mod) ->
                case mongoose_gen_auth:set_password(Mod, HostType, LUser, LServer, Password) of
                    ok -> {stop, ok};
                    {error, Error} -> {continue, {error, Error}}
                end
        end,
    Opts = #{default => {error, not_allowed}},
    case ejabberd_auth:does_user_exist(jid:make_bare(LUser, LServer)) of
        true ->
            call_auth_modules_for_domain(LServer, F, Opts);
        false ->
            {error, not_allowed}
    end.

-spec try_register(jid:jid() | error, binary()) ->
    ok | {error, exists | not_allowed | invalid_jid | null_password | limit_per_domain_exceeded}.
try_register(_, <<>>) ->
    {error, null_password};
try_register(#jid{luser = <<>>}, _) ->
    {error, invalid_jid};
try_register(error, _) ->
    {error, invalid_jid};
try_register(JID, Password) ->
    Exists = does_user_exist(JID),
    do_try_register_if_does_not_exist(Exists, JID, Password).

-spec do_try_register_if_does_not_exist(boolean(), jid:jid(), binary()) ->
    ok | {error, exists | not_allowed | invalid_jid | null_password | limit_per_domain_exceeded}.
do_try_register_if_does_not_exist(true, _, _) ->
    {error, exists};
do_try_register_if_does_not_exist(_, JID, Password) ->
    {LUser, LServer} = jid:to_lus(JID),
    F = fun(HostType, Mod) ->
                case mongoose_gen_auth:try_register(Mod, HostType, LUser, LServer, Password) of
                    ok ->
                        mongoose_hooks:register_user(HostType, LServer, LUser),
                        {stop, ok};
                    {error, _Error} ->
                        continue
                end
        end,
    Opts = #{default => {error, not_allowed}, metric => try_register},
    case is_user_number_below_limit(LServer) of
        true ->
            call_auth_modules_for_domain(LServer, F, Opts);
        false ->
            {error, limit_per_domain_exceeded}
    end.

%% @doc Registered users list do not include anonymous users logged
-spec get_vh_registered_users(Server :: jid:server()) -> [jid:simple_bare_jid()].
get_vh_registered_users(Server) ->
    get_vh_registered_users(Server, []).

-spec get_vh_registered_users(Server :: jid:server(), Opts :: [any()]) ->
    [jid:simple_bare_jid()].
get_vh_registered_users(Server, Opts) ->
    LServer = jid:nameprep(Server),
    do_get_vh_registered_users(LServer, Opts).

do_get_vh_registered_users(error, _) ->
    [];
do_get_vh_registered_users(LServer, Opts) ->
    F = fun(HostType, Mod) ->
                mongoose_gen_auth:get_registered_users(Mod, HostType, LServer, Opts)
        end,
    lists:append(call_auth_modules_for_domain(LServer, F, #{default => [], op => map})).

-spec get_vh_registered_users_number(Server :: jid:server()) -> integer().
get_vh_registered_users_number(Server) ->
    get_vh_registered_users_number(Server, []).

-spec get_vh_registered_users_number(Server :: jid:server(), Opts :: list()) -> integer().
get_vh_registered_users_number(Server, Opts) ->
    LServer = jid:nameprep(Server),
    do_get_vh_registered_users_number(LServer, Opts).

do_get_vh_registered_users_number(error, _) ->
    0;
do_get_vh_registered_users_number(LServer, Opts) ->
    F = fun(HostType, Mod) ->
                mongoose_gen_auth:get_registered_users_number(Mod, HostType, LServer, Opts)
        end,
    lists:sum(call_auth_modules_for_domain(LServer, F, #{default => [], op => map})).

-spec get_password_s(JID :: jid:jid() | error) -> binary().
get_password_s(#jid{luser = LUser, lserver = LServer}) ->
    F = fun(HostType, Mod) ->
                case mongoose_gen_auth:get_password_s(Mod, HostType, LUser, LServer) of
                    <<>> -> continue;
                    Password when is_binary(Password) -> {stop, Password}
                end
        end,
    call_auth_modules_for_domain(LServer, F, #{default => <<>>}).

%% @doc Get the password(like thing) of the user and the auth module.
-spec get_passterm_with_authmodule(mongooseim:host_type(), error | jid:jid()) ->
          {passterm(), authmodule()} | false.
get_passterm_with_authmodule(HostType, #jid{luser = LUser, lserver = LServer}) ->
    F = fun(Mod) ->
                case mongoose_gen_auth:get_password(Mod, HostType, LUser, LServer) of
                    false -> continue;
                    PassTerm -> {stop, {PassTerm, Mod}}
                end
        end,
    call_auth_modules_for_host_type(HostType, F, #{default => false}).

%% @doc Returns true if the user exists in the DB
%% or if an anonymous user is logged under the given name
%% Returns 'false' in case of an error
-spec does_user_exist(JID :: jid:jid() | error) -> boolean().
does_user_exist(#jid{luser = LUser, lserver = LServer}) ->
    F = fun(HostType, Mod) -> does_user_exist_in_module(HostType, LUser, LServer, Mod) end,
    case call_auth_modules_for_domain(LServer, F, #{default => false, metric => does_user_exist}) of
        {error, _Error} -> false;
        Result -> Result
    end;
does_user_exist(error) ->
    false.

%% Hook interface
-spec does_user_exist(mongooseim:host_type(), jid:jid(), exist_type()) -> boolean().
does_user_exist(HostType, Jid, RequestType) ->
    mongoose_hooks:does_user_exist(HostType, Jid, RequestType).

%% @doc does_user_exist hook handler
%% Returns 'false' in case of an error
-spec on_does_user_exist(Acc, Params, Extra) -> {ok, Acc} when
    Acc :: boolean(),
    Params :: map(),
    Extra :: map().
on_does_user_exist(false, #{jid := Jid, request_type := stored}, #{host_type := HostType}) ->
    {ok, true =:= does_stored_user_exist(HostType, Jid)};
on_does_user_exist(false,
                   #{jid := #jid{luser = LUser, lserver = LServer}, request_type := with_anonymous},
                   #{host_type := HostType}) ->
    F = fun(Mod) -> does_user_exist_in_module(HostType, LUser, LServer, Mod) end,
    {ok, call_auth_modules_for_host_type(HostType, F, #{default => false, metric => does_user_exist})};
on_does_user_exist(Status, _, _) ->
    {ok, Status}.

%% @doc Returns true if the user exists in the DB
%% In case of a backend error, it is propagated to the caller
-spec does_stored_user_exist(mongooseim:host_type(), jid:jid() | error) ->
          boolean() | {error, any()}.
does_stored_user_exist(HostType, #jid{luser = LUser, lserver = LServer}) ->
    F = fun(ejabberd_auth_anonymous) -> continue;
           (Mod) -> does_user_exist_in_module(HostType, LUser, LServer, Mod)
        end,
    call_auth_modules_for_host_type(HostType, F, #{default => false, metric => does_user_exist});
does_stored_user_exist(_HostType, error) ->
    false.

does_user_exist_in_module(HostType, LUser, LServer, Mod) ->
    case mongoose_gen_auth:does_user_exist(Mod, HostType, LUser, LServer) of
        true -> {stop, true};
        false -> continue;
        {error, Reason} = Error ->
            ?LOG_ERROR(#{what => does_user_exist_failed,
                         text => <<"The authentication module returned an error">>,
                         auth_module => Mod, reason => Reason,
                         user => LUser, server => LServer}),
            {continue, Error}
    end.

-spec does_method_support(AuthMethod :: atom(), Feature :: atom()) -> boolean().
does_method_support(AuthMethod, Feature) ->
    Module = auth_method_to_module(AuthMethod),
    lists:member(Feature, mongoose_gen_auth:supported_features(Module)).

%% @doc Remove user.
%% Note: it may return ok even if there was some problem removing the user.
-spec remove_user(JID :: jid:jid()) -> ok | {error, not_allowed | user_does_not_exist};
                 (error) -> error.
remove_user(error) -> error;
remove_user(#jid{luser = LUser, lserver = LServer}) ->
    JID = jid:make_bare(LUser, LServer),
    F = fun(HostType, Mod) ->
                case mongoose_gen_auth:remove_user(Mod, HostType, LUser, LServer) of
                    ok -> {continue, {ok, HostType}};
                    {error, _Error} -> continue
                end
        end,
    case ejabberd_auth:does_user_exist(JID) of
        true ->
            case call_auth_modules_for_domain(LServer, F, #{default => {error, not_allowed}}) of
                {ok, HostType} ->
                    Acc = mongoose_acc:new(#{location => ?LOCATION,
                                                host_type => HostType,
                                                lserver => LServer,
                                                element => undefined}),
                    mongoose_hooks:remove_user(Acc, LServer, LUser),
                    ok;
                Error ->
                    ?LOG_ERROR(#{what => backend_disallows_user_removal,
                    user => LUser, server => LServer,
                    reason => Error}),
                    Error
                end;
        false ->
            {error, user_does_not_exist}
        end.

%% @doc Calculate informational entropy.
-spec entropy(iolist()) -> float().
entropy(IOList) ->
    case binary_to_list(iolist_to_binary(IOList)) of
        "" ->
            0.0;
        InputList ->
            Set = lists:foldl(
                    fun(IOContent, Acc) ->
                            get_type_information(IOContent, Acc)
                    end, [0, 0, 0, 0, 0], InputList),
            length(InputList) * math:log(lists:sum(Set))/math:log(2)
    end.

-spec config_spec(atom()) -> mongoose_config_spec:config_section().
config_spec(Method) ->
    mongoose_gen_auth:config_spec(auth_method_to_module(Method)).

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------
%% Return the list of authenticated modules for a given domain
%% TODO: rework is_anonymous_user/1 at mongoose_users module,
%% so there is no need for exporting auth_modules/1 function.
%% after that completely get rid of this interface, we should
%% use auth_modules_for_host_type/1 function instead.
-spec auth_modules(Server :: jid:lserver()) -> [authmodule()].
auth_modules(LServer) ->
    case mongoose_domain_api:get_domain_host_type(LServer) of
        {ok, HostType} -> auth_modules_for_host_type(HostType);
        {error, not_found} -> []
    end.

%% Return the list of authenticated modules for a given host type
-spec auth_modules_for_host_type(HostType :: mongooseim:host_type()) -> [authmodule()].
auth_modules_for_host_type(HostType) ->
    Methods = auth_methods(HostType),
    methods_to_modules(Methods).

-spec methods_to_modules([atom()]) -> [authmodule()].
methods_to_modules(Methods) ->
    [auth_method_to_module(M) || M <- Methods].

-spec auth_methods(mongooseim:host_type()) -> [atom()].
auth_methods(HostType) ->
    mongoose_config:get_opt([{auth, HostType}, methods]).

-spec auth_method_to_module(atom()) -> authmodule().
auth_method_to_module(Method) ->
    list_to_atom("ejabberd_auth_" ++ atom_to_list(Method)).

-spec remove_domain(mongoose_domain_api:remove_domain_acc(), map(), map()) ->
    {ok | stop, mongoose_domain_api:remove_domain_acc()}.
remove_domain(Acc, #{domain := Domain}, #{host_type := HostType}) ->
    F = fun() ->
            FAuth = fun(Mod) -> mongoose_gen_auth:remove_domain(Mod, HostType, Domain) end,
            call_auth_modules_for_host_type(HostType, FAuth, #{op => map}),
            Acc
        end,
    mongoose_domain_api:remove_domain_wrapper(Acc, F, ?MODULE).

ensure_metrics(Host) ->
    Metrics = [authorize, check_password, try_register, does_user_exist],
    [mongoose_metrics:ensure_metric(Host, ?METRIC(Metric), histogram)
     || Metric <- Metrics].

%% Library functions for reuse in ejabberd_auth_* modules
-spec authorize_with_check_password(Module, Creds) -> {ok, Creds}
                                                    | {error, any()} when
      Module :: authmodule(),
      Creds :: mongoose_credentials:t().
authorize_with_check_password(Module, Creds) ->
    User      = mongoose_credentials:get(Creds, username),
    LUser     = jid:nodeprep(User),
    LUser == error andalso error({nodeprep_error, User}),
    LServer   = mongoose_credentials:lserver(Creds),
    HostType  = mongoose_credentials:host_type(Creds),
    Password  = mongoose_credentials:get(Creds, password),
    Digest    = mongoose_credentials:get(Creds, digest, undefined),
    DigestGen = mongoose_credentials:get(Creds, digest_gen, undefined),
    Result = case {Digest, DigestGen} of
                 _ when Digest /= undefined andalso DigestGen /= undefined ->
                     mongoose_gen_auth:check_password(Module, HostType, LUser, LServer,
                                                      Password, Digest, DigestGen);
                 _  ->
                     mongoose_gen_auth:check_password(Module, HostType, LUser, LServer, Password)
             end,
    case Result of
        true -> {ok, mongoose_credentials:set(Creds, auth_module, Module)};
        false -> {error, not_authorized}
    end.

-spec get_type_information(integer(), list()) -> list().
get_type_information(IOContent, [Digit, Printable, _, HiLetter, Other])
  when IOContent >= $a andalso IOContent =< $z ->
    [Digit, Printable, 26, HiLetter, Other];
get_type_information(IOContent, [_, Printable, LowLetter, HiLetter, Other])
  when IOContent >= $0 andalso IOContent =< $9 ->
    [9, Printable, LowLetter, HiLetter, Other];
get_type_information(IOContent, [Digit, Printable, LowLetter, _, Other])
  when IOContent >= $A andalso IOContent =< $Z ->
    [Digit, Printable, LowLetter, 26, Other];
get_type_information(IOContent, [Digit, _, LowLetter, HiLetter, Other])
  when IOContent >= 16#21 andalso IOContent =< 16#7e ->
    [Digit, 33, LowLetter, HiLetter, Other];
get_type_information(_IOContent, [Digit, Printable, LowLetter, HiLetter, _Other]) ->
    [Digit, Printable, LowLetter, HiLetter, 128].

%% @doc If the domain corresponds to a valid host type, call auth modules for that host type
-spec call_auth_modules_for_domain(jid:lserver(), host_type_mod_fun(), call_opts()) ->
          mod_res() | [mod_res()].
call_auth_modules_for_domain(Domain, F, Opts = #{default := Default}) ->
    case mongoose_domain_api:get_domain_host_type(Domain) of
        {ok, HostType} ->
            StepF = bind_host_type(HostType, F),
            case maps:take(metric, Opts) of
                {Metric, NewOpts} ->
                    {Time, Result} = timer:tc(fun call_auth_modules_for_host_type/3,
                                              [HostType, StepF, NewOpts]),
                    mongoose_metrics:update(HostType, ?METRIC(Metric), Time),
                    Result;
                error ->
                    call_auth_modules_for_host_type(HostType, StepF, Opts)
            end;
        {error, not_found} ->
            Default
    end.

-spec bind_host_type(mongooseim:host_type(), host_type_mod_fun()) -> mod_fun().
bind_host_type(HostType, F) when is_function(F, 2) ->
    fun(Mod) -> F(HostType, Mod) end.

-spec call_auth_modules_for_host_type(mongooseim:host_type(),
                                      mod_fun() | mod_fold_fun(), call_opts()) ->
          mod_res() | [mod_res()].
call_auth_modules_for_host_type(HostType, F, Opts) ->
    Modules = auth_modules_for_host_type(HostType),
    case maps:take(metric, Opts) of
        {Metric, NewOpts} ->
            {Time, Result} = timer:tc(fun call_auth_modules/3, [Modules, F, NewOpts]),
            mongoose_metrics:update(HostType, ?METRIC(Metric), Time),
            Result;
        error ->
            call_auth_modules(Modules, F, Opts)
    end.

-spec call_auth_modules_with_creds(mongoose_credentials:t(),
                                   mod_fun() | mod_fold_fun(), call_opts()) ->
          mod_res() | [mod_res()].
call_auth_modules_with_creds(Creds, F, Opts) ->
    Modules = mongoose_credentials:auth_modules(Creds),
    case maps:take(metric, Opts) of
        {Metric, NewOpts} ->
            HostType = mongoose_credentials:host_type(Creds),
            {Time, Result} = timer:tc(fun call_auth_modules/3,
                                      [Modules, F, NewOpts]),
            mongoose_metrics:update(HostType, ?METRIC(Metric), Time),
            Result;
        error ->
            call_auth_modules(Modules, F, Opts)
    end.


%% @doc Perform a map or a fold operation with function F over the provided Modules
-spec call_auth_modules([authmodule()], mod_fun() | mod_fold_fun(), call_opts()) ->
          mod_res() | [mod_res()].
call_auth_modules(Modules, F, #{op := map}) when is_function(F, 1) ->
    lists:map(F, Modules);
call_auth_modules(Modules, F, Opts) when is_function(F, 1) ->
    call_auth_modules(Modules, fun(Mod, _) -> F(Mod) end, Opts);
call_auth_modules(Modules, F, #{default := Default}) when is_function(F, 2) ->
    fold_auth_modules(Modules, F, Default).

%% @doc Apply function F to all consecutive auth modules with an accumulator and a stop condition
-spec fold_auth_modules([authmodule()], mod_fold_fun(), mod_res()) -> mod_res().
fold_auth_modules([], _F, FinalAcc) ->
    FinalAcc;
fold_auth_modules([AuthModule | AuthModules], F, CurAcc) ->
    case F(AuthModule, CurAcc) of
        continue ->
            fold_auth_modules(AuthModules, F, CurAcc);
        {continue, NewAcc} ->
            fold_auth_modules(AuthModules, F, NewAcc);
        {stop, Value} ->
            Value
    end.

is_user_number_below_limit(Domain) ->
    case mongoose_domain_api:get_domain_host_type(Domain) of
        {ok, HostType} ->
            Limit = mongoose_config:get_opt([{auth, HostType}, max_users_per_domain]),
            Current = get_vh_registered_users_number(Domain),
            Current < Limit;
        {error, not_found} ->
            true
    end.
