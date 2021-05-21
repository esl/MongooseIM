%%%=============================================================================
%%% @copyright (C) 1999-2018, Erlang Solutions Ltd
%%% @author Denys Gonchar <denys.gonchar@erlang-solutions.com>
%%% @doc demo PKI auth backend.
%%%
%%% it authorises all the certificates with Common Name (used as client's
%%% "username"), assuming that all of the certificates are valid.
%%%
%%% certificate verification can be configured for c2s listener.
%%%
%%% as we cannot track properly the list of valid user, does_user_exist/2
%%% function is stubbed to true (this one is used by MAM)
%%% @end
%%%=============================================================================
-module(ejabberd_auth_pki).
-copyright("2018, Erlang Solutions Ltd.").
-author('denys.gonchar@erlang-solutions.com').

-include("jlib.hrl").

-behaviour(mongoose_gen_auth).

%% mongoose_gen_auth API
-export([start/1,
         stop/1,
         supports_sasl_module/2,
         authorize/1,
         does_user_exist/3,
         supported_features/0
        ]).

-spec start(HostType :: mongooseim:host_type()) -> ok.
start(_) -> ok.

-spec stop(HostType :: mongooseim:host_type()) -> ok.
stop(_) -> ok.

-spec supports_sasl_module(binary(), cyrsasl:sasl_module()) -> boolean().
supports_sasl_module(_, Module) -> Module =:= cyrsasl_external.

-spec authorize(mongoose_credentials:t()) -> {ok, mongoose_credentials:t()} | {error, any()}.
authorize(Creds) ->
    {ok, mongoose_credentials:extend(Creds, [{auth_module, ?MODULE}])}.

-spec does_user_exist(mongooseim:host_type(), jid:luser(), jid:lserver()) -> boolean().
does_user_exist(_, _, _) -> true.

-spec supported_features() -> [atom()].
supported_features() -> [dynamic_domains].
