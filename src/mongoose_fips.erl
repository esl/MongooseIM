-module(mongoose_fips).

-include("ejabberd.hrl").

-export([notify/0]).
-export([status/0]).
-export([maybe_register_mech/3]).

-ifndef(fips_mode).

notify() -> ok.
status() -> disabled.

-spec maybe_register_mech(Mechanism, Module, PasswordType) -> Result when
      Mechanism :: cyrsasl:mechanism(),
      Module :: module(),
      PasswordType :: cyrsasl:password_type(),
      Result :: true.

maybe_register_mech(MechFeature, Module, PasswordType) ->
    do_register(MechFeature, Module, PasswordType).

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

maybe_register_mech(MechFeature, Module, MechName) ->
    case crypto:info_fips() of
        enabled ->
            ok;
        _ ->
            do_register(MechFeature, Module, MechName)
    end.

-endif.

do_register(MechFeature, Module, PasswordType) ->
    cyrsasl:register_mechanism(MechFeature, Module, PasswordType).

