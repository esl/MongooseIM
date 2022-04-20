-module(domain_rest_helper).

%% Config helpers
-export([set_invalid_creds/1,
         set_no_creds/1,
         set_valid_creds/1]).

%% Requests API
-export([rest_patch_enabled/3,
         rest_put_domain/3,
         putt_domain_with_custom_body/2,
         rest_select_domain/2,
         rest_delete_domain/3,
         delete_custom/4,
         patch_custom/4]).

%% Handler
-export([start_listener/1,
         stop_listener/1]).

-import(distributed_helper, [mim/0, mim2/0, require_rpc_nodes/1, rpc/4]).
-import(config_parser_helper, [default_config/1, config/2]).

-define(TEST_PORT, 8866).

set_invalid_creds(Config) ->
    [{auth_creds, invalid}|Config].

set_no_creds(Config) ->
    [{auth_creds, false}|Config].

set_valid_creds(Config) ->
    [{auth_creds, valid}|Config].

domain_path(Domain) ->
    <<"/domains/", Domain/binary>>.

make_creds(Config) ->
    case proplists:get_value(auth_creds, Config) of
        valid ->
            {<<"admin">>, <<"secret">>};
        invalid ->
            {<<"admin">>, <<"badsecret">>};
        _ ->
            false
    end.

%% Requests API
rest_patch_enabled(Config, Domain, Enabled) ->
    Params = #{enabled => Enabled},
    rest_helper:make_request(#{ role => admin, method => <<"PATCH">>,
                                port => ?TEST_PORT,
                                path => domain_path(Domain),
                                creds => make_creds(Config),
                                body => Params }).

rest_put_domain(Config, Domain, Type) ->
    Params = #{host_type => Type},
    rest_helper:make_request(#{ role => admin, method => <<"PUT">>,
                                port => ?TEST_PORT,
                                path => domain_path(Domain),
                                creds => make_creds(Config),
                                body => Params }).

putt_domain_with_custom_body(Config, Body) ->
    rest_helper:make_request(#{ role => admin, method => <<"PUT">>,
                                port => ?TEST_PORT,
                                path => <<"/domains/example.db">>,
                                creds => make_creds(Config),
                                body => Body }).

rest_select_domain(Config, Domain) ->
    Params = #{},
    rest_helper:make_request(#{ role => admin, method => <<"GET">>,
                                port => ?TEST_PORT,
                                path => domain_path(Domain),
                                creds => make_creds(Config),
                                body => Params }).

rest_delete_domain(Config, Domain, HostType) ->
    Params = #{<<"host_type">> => HostType},
    rest_helper:make_request(#{ role => admin, method => <<"DELETE">>,
                                port => ?TEST_PORT,
                                path => domain_path(Domain),
                                creds => make_creds(Config),
                                body => Params }).

delete_custom(Config, Role, Path, Body) ->
    rest_helper:make_request(#{ role => Role, method => <<"DELETE">>,
                                port => ?TEST_PORT,
                                path => Path, creds => make_creds(Config),
                                body => Body }).

patch_custom(Config, Role, Path, Body) ->
    rest_helper:make_request(#{ role => Role, method => <<"PATCH">>,
                                port => ?TEST_PORT,
                                path => Path, creds => make_creds(Config),
                                body => Body }).

%% REST handler setup
start_listener(Params) ->
    rpc(mim(), mongoose_listener, start_listener, [listener_opts(Params)]).

stop_listener(Params) ->
    rpc(mim(), mongoose_listener, stop_listener, [listener_opts(Params)]).

listener_opts(Params) ->
    config([listen, http],
           #{port => ?TEST_PORT,
             ip_tuple => {127, 0, 0, 1},
             ip_address => "127.0.0.1",
             module => ejabberd_cowboy,
             handlers => [domain_handler(Params)],
             transport => config([listen, http, transport], #{num_acceptors => 10})}).

domain_handler(Params) ->
    {"localhost", "/api", mongoose_domain_handler, handler_opts(Params)}.

handler_opts(#{skip_auth := true}) ->
    [];
handler_opts(_Params) ->
    [{password, <<"secret">>}, {username, <<"admin">>}].
