-module(mongoose_fips).

-include("mongoose.hrl").

-export([notify/0]).
-export([status/0]).
-export([supports_sasl_module/1]).

-ignore_xref([status/0]).

notify() ->
    case application:get_env(crypto, fips_mode, false) andalso crypto:info_fips() of
        false ->
            ok;
        enabled ->
            ?LOG_NOTICE(#{what => fips_mode_enabled,
                          text => <<"FIPS mode enabled">>});
        not_enabled ->
            ?LOG_ERROR(#{what => fips_mode_disabled,
                         text => <<"FIPS mode disabled although it should be enabled">>});
        not_supported ->
            ?LOG_INFO(#{what => fips_mode_not_supported,
                        text => <<"Used Erlang/OTP does not support FIPS mode">>})
    end.

status() ->
    crypto:info_fips().

-spec supports_sasl_module(cyrsasl:sasl_module()) -> boolean().
supports_sasl_module(Module) ->
    case crypto:info_fips() of
        enabled ->
            Module =/= cyrsasl_digest;
        _ ->
            true
    end.
