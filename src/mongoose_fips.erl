-module(mongoose_fips).

-include("mongoose.hrl").

-export([notify/0]).
-export([status/0]).
-export([supports_sasl_module/1]).

-ifndef(fips_mode).

notify() -> ok.
status() -> disabled.

-spec supports_sasl_module(cyrsasl:sasl_module()) -> boolean().
supports_sasl_module(_Module) ->
    true.

-else.

notify() ->
    case application:get_env(crypto, fips_mode) of
        {ok, true} ->
            do_notify();
        _ ->
            ok
    end.

do_notify() ->
    code:ensure_loaded(crypto),
    case erlang:function_exported(crypto, info_fips, 0) of
        true ->
            case crypto:info_fips() of
                enabled ->
                    ?WARNING_MSG("FIPS mode enabled", []);
                _ ->
                    ?ERROR_MSG("FIPS mode disabled although it should be enabled", [])
            end;
        _ ->
            ?INFO_MSG("Used Erlang/OTP does not support FIPS mode", [])
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

-endif.
