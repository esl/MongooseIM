-module(ejabberd_auth_dummy).

%% API
-export([start/1,
         stop/1,
         config_spec/0,
         check_password/4,
         check_password/6,
         authorize/1,
         remove_domain/2,
         supported_features/0]).

%% ejabberd_auth compatibility layer - not supported features
-behaviour(mongoose_gen_auth).

-export([set_password/4,
         try_register/4,
         get_password/3,
         get_password_s/3,
         does_user_exist/3,
         remove_user/3,
         supports_sasl_module/2,
         scram_passwords/0]).

-ignore_xref([scram_passwords/0]).

-include("mongoose.hrl").
-include("mongoose_config_spec.hrl").

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

-spec start(HostType :: mongooseim:host_type()) -> ok.
start(_HostType) ->
    ok.

-spec stop(HostType :: mongooseim:host_type()) -> ok.
stop(_HostType) ->
    ok.

-spec config_spec() -> mongoose_config_spec:config_section().
config_spec() ->
    #section{
       items = #{<<"base_time">> => #option{type = integer,
                                            validate = non_negative},
                 <<"variance">> => #option{type = integer,
                                           validate = positive}},
       defaults = #{<<"base_time">> => 50,
                    <<"variance">> => 450}
      }.

authorize(Creds) ->
    HostType = mongoose_credentials:host_type(Creds),
    #{base_time := Base,
      variance := Variance} = mongoose_config:get_opt([{auth, HostType}, dummy]),
    timer:sleep(Base + rand:uniform(Variance)),
    {ok, mongoose_credentials:set(Creds, auth_module, ?MODULE)}.

check_password(_HostType, _User, _Server, _Password) ->
    true.

check_password(_HostType, User, Server, _Password, _Digest, _DigestGen) ->
    ?LOG_DEBUG(#{what => digest_auth_unsupported,
                 text => <<"no support for digest authentication">>,
                 user => User, server => Server}),
    false.

-spec set_password(mongooseim:host_type(), jid:luser(), jid:lserver(), binary()) -> ok.
set_password(_HostType, _User, _Server, _Password) ->
    ok.

-spec try_register(mongooseim:host_type(), jid:luser(), jid:lserver(), binary()) -> ok.
try_register(_HostType, _User, _Server, _Password) ->
    ok.

-spec get_password(mongooseim:host_type(), jid:luser(), jid:lserver()) -> binary().
get_password(_HostType, _User, _Server) ->
    <<>>.

-spec get_password_s(mongooseim:host_type(), jid:luser(), jid:lserver()) -> binary().
get_password_s(_HostType, _User, _Server) ->
    <<>>.

-spec does_user_exist(mongooseim:host_type(), jid:luser(), jid:lserver()) -> boolean().
does_user_exist(_HostType, _User, _Server) ->
    true.

%% @doc Remove user.
%% Note: it returns ok even if there was some problem removing the user.
-spec remove_user(mongooseim:host_type(), jid:luser(), jid:lserver()) -> ok.
remove_user(_HostType, _User, _Server) ->
    ok.

-spec remove_domain(mongooseim:host_type(), jid:lserver()) -> ok.
remove_domain(_HostType, _Server) -> ok.

-spec supported_features() -> [atom()].
supported_features() -> [dynamic_domains].

-spec supports_sasl_module(mongooseim:host_type(), cyrsasl:sasl_module()) -> boolean().
supports_sasl_module(_, cyrsasl_plain) -> true;
supports_sasl_module(_, _) -> false.

scram_passwords() ->
    not_supported.
