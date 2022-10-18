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
         request_delete_domain/3,
         delete_custom/4,
         patch_custom/4]).

-import(distributed_helper, [mim/0, mim2/0, require_rpc_nodes/1, rpc/4]).

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
                                path => domain_path(Domain),
                                creds => make_creds(Config),
                                body => Params }).

rest_put_domain(Config, Domain, Type) ->
    Params = #{host_type => Type},
    rest_helper:make_request(#{ role => admin, method => <<"PUT">>,
                                path => domain_path(Domain),
                                creds => make_creds(Config),
                                body => Params }).

putt_domain_with_custom_body(Config, Body) ->
    rest_helper:make_request(#{ role => admin, method => <<"PUT">>,
                                path => <<"/domains/example.db">>,
                                creds => make_creds(Config),
                                body => Body }).

rest_select_domain(Config, Domain) ->
    Params = #{},
    rest_helper:make_request(#{ role => admin, method => <<"GET">>,
                                path => domain_path(Domain),
                                creds => make_creds(Config),
                                body => Params }).

rest_delete_domain(Config, Domain, HostType) ->
    Params = #{<<"host_type">> => HostType},
    rest_helper:make_request(#{ role => admin, method => <<"DELETE">>,
                                path => domain_path(Domain),
                                creds => make_creds(Config),
                                body => Params }).

request_delete_domain(Config, Domain, HostType) ->
    Params = #{<<"host_type">> => HostType, <<"request">> => true},
    rest_helper:make_request(#{ role => admin, method => <<"DELETE">>,
                                path => domain_path(Domain),
                                creds => make_creds(Config),
                                body => Params }).

delete_custom(Config, Role, Path, Body) ->
    rest_helper:make_request(#{ role => Role, method => <<"DELETE">>,
                                path => Path, creds => make_creds(Config),
                                body => Body }).

patch_custom(Config, Role, Path, Body) ->
    rest_helper:make_request(#{ role => Role, method => <<"PATCH">>,
                                path => Path, creds => make_creds(Config),
                                body => Body }).
