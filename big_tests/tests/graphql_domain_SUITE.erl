-module(graphql_domain_SUITE).

 -include_lib("common_test/include/ct.hrl").
 -include_lib("eunit/include/eunit.hrl").
 -include_lib("exml/include/exml.hrl").

 -compile([export_all, nowarn_export_all]).

-import(distributed_helper, [mim/0, require_rpc_nodes/1, rpc/4]).
-import(graphql_helper, [execute/3, get_listener_port/1, get_listener_config/1]).

-define(HOST_TYPE, <<"dummy auth">>).

suite() ->
    require_rpc_nodes([mim]) ++ escalus:suite().

 all() ->
     [{group, domain_handler}].

groups() ->
     [{domain_handler, [sequence], domain_handler()}].

domain_handler() -> 
    [create_domain,
     disable_domain,
     enable_domain,
     get_domains_by_host_type,
     get_domain_details,
     delete_domain,
     get_domains_after_deletion].

init_per_suite(Config) ->
    Endpoint = admin,
    Opts = get_listener_opts(Endpoint),
    case proplists:is_defined(username, Opts) of
        true ->
            case mongoose_helper:is_rdbms_enabled(?HOST_TYPE) of
                true -> escalus:init_per_suite([{schema_endpoint, Endpoint} | Config]);
                false -> {skip, require_rdbms}
            end;
        false ->
            ct:fail(<<"Admin credentials are not defined in config">>)
    end.

end_per_suite(Config) ->
    escalus:end_per_suite(Config).

 init_per_testcase(CaseName, Config) ->
     escalus:init_per_testcase(CaseName, Config).

 end_per_testcase(CaseName, Config) ->
     escalus:end_per_testcase(CaseName, Config).

create_domain(Config) ->
    DomainName1 = <<"exampleDomain">>,
    DomainName2 = <<"exampleDomain2">>,

    Vars = #{domain => DomainName1, hostType => ?HOST_TYPE},
    Result1 = execute_auth(#{query => create_domain_call(), variables => Vars,
                   operationName => <<"M1">>}, Config),
    ParsedResult1 = ok_result(<<"domains">>, <<"addDomain">>, Result1),
    ?assertEqual(#{<<"domain">> => DomainName1,
                   <<"hostType">> => ?HOST_TYPE,
                   <<"enabled">> => null}, ParsedResult1),

    Vars2 = #{domain => DomainName2, hostType => ?HOST_TYPE},
    Result2 = execute_auth(#{query => create_domain_call(), variables => Vars2,
                   operationName => <<"M1">>}, Config),
    ParsedResult2 = ok_result(<<"domains">>, <<"addDomain">>, Result2),
    ?assertEqual(#{<<"domain">> => DomainName2,
                   <<"hostType">> => ?HOST_TYPE,
                   <<"enabled">> => null}, ParsedResult2).

disable_domain(Config) ->
    Vars = #{domain => <<"exampleDomain">>},
    Result = execute_auth(#{query => disable_domain_call(), variables => Vars,
                   operationName => <<"M1">>}, Config),
    ParsedResult = ok_result(<<"domains">>, <<"disableDomain">>, Result),
    ?assertEqual(#{<<"domain">> => <<"exampleDomain">>, <<"enabled">> => false}, ParsedResult).

enable_domain(Config) ->
    Vars = #{domain => <<"exampleDomain">>},
    Result = execute_auth(#{query => enable_domain_call(), variables => Vars,
                   operationName => <<"M1">>}, Config),
    ParsedResult = ok_result(<<"domains">>, <<"enableDomain">>, Result),
    ?assertEqual(#{<<"domain">> => <<"exampleDomain">>, <<"enabled">> => true}, ParsedResult).

get_domains_by_host_type(Config) ->
    Vars = #{hostType => ?HOST_TYPE},
    Result = execute_auth(#{query => get_domains_by_host_type_call(),
                   variables => Vars,
                   operationName => <<"Q1">>}, Config),
    ParsedResult = ok_result(<<"domains">>, <<"domainsByHostType">>, Result),
    ?assertEqual(lists:sort([<<"exampleDomain">>, <<"exampleDomain2">>]),
                 lists:sort(ParsedResult)).

get_domain_details(Config) ->
    Vars = #{domain => <<"exampleDomain">>},
    Result = execute_auth(#{query => get_domain_details_call(),
                   variables => Vars,
                   operationName => <<"Q1">>}, Config),
    ParsedResult = ok_result(<<"domains">>, <<"domainDetails">>, Result),
    ?assertEqual(#{<<"domain">> => <<"exampleDomain">>,
                   <<"hostType">> => ?HOST_TYPE,
                   <<"enabled">> => true}, ParsedResult).

delete_domain(Config) ->
    Vars = #{domain => <<"exampleDomain">>, hostType => ?HOST_TYPE},
    Result1 = execute_auth(#{query => delete_domain_call(), variables => Vars,
                   operationName => <<"M1">>}, Config),
    ParsedResult1 = ok_result(<<"domains">>, <<"removeDomain">>, Result1),
    ?assertEqual(#{<<"msg">> => <<"Domain removed!">>,
                   <<"domain">> => #{<<"domain">> => <<"exampleDomain">>}}, 
                   ParsedResult1),

    Vars2 = #{domain => <<"exampleDomain2">>, hostType => ?HOST_TYPE},
    Result2 = execute_auth(#{query => delete_domain_call(), variables => Vars2,
                   operationName => <<"M1">>}, Config),
    ParsedResult2 = ok_result(<<"domains">>, <<"removeDomain">>, Result2),
    ?assertEqual(#{<<"msg">> => <<"Domain removed!">>,
                   <<"domain">> => #{<<"domain">> => <<"exampleDomain2">>}}, 
                   ParsedResult2).
    
get_domains_after_deletion(Config) ->
    Vars = #{hostType => ?HOST_TYPE},
    Result = execute_auth(#{query => get_domains_by_host_type_call(),
                   variables => Vars,
                   operationName => <<"Q1">>}, Config),
    ParsedResult = ok_result(<<"domains">>, <<"domainsByHostType">>, Result),
    ?assertEqual([], ParsedResult).

create_domain_call() ->
    <<"mutation M1($domain: String!, $hostType: String!)
           {domains
               {addDomain(domain: $domain, hostType: $hostType)
                   {
                       domain
                       hostType
                       enabled
                   }
               }
           }">>.

enable_domain_call() ->
    <<"mutation M1($domain: String!)
           {domains
               {enableDomain(domain: $domain)
                   {
                       enabled
                       domain
                   }
               }
           }">>.

disable_domain_call() ->
    <<"mutation M1($domain: String!)
           {domains
               {disableDomain(domain: $domain)
                   {
                       enabled
                       domain
                   }
               }
           }">>.

get_domains_by_host_type_call() ->
    <<"query Q1($hostType: String!)
           {domains
               {domainsByHostType(hostType: $hostType)}
           }">>.

get_domain_details_call() ->
    <<"query Q1($domain: String!)
           {domains
               {domainDetails(domain: $domain)
                   {
                       domain
                       hostType
                       enabled
                   }
               }
           }">>.

delete_domain_call() ->
    <<"mutation M1($domain: String!, $hostType: String!)
           {domains
               {removeDomain(domain: $domain, hostType: $hostType)
                   {
                       msg 
                       domain
                           {domain}
                   }
               }
           }">>.

%% Helpers

execute_auth(Body, Config) ->
     Ep = ?config(schema_endpoint, Config),
     Opts = get_listener_opts(Ep),
     User = proplists:get_value(username, Opts),
     Password = proplists:get_value(password, Opts),
     execute(Ep, Body, {User, Password}).

ok_result(What1, What2, {{<<"200">>, <<"OK">>}, #{<<"data">> := Data}}) ->
   maps:get(What2, maps:get(What1, Data)).

get_listener_opts(EpName) ->
    {_, ejabberd_cowboy, Opts} = get_listener_config(EpName),
    {value, {modules, Modules}} = lists:keysearch(modules, 1, Opts),
    [Opts2] = lists:filtermap(
        fun
            ({_, _Path, mongoose_graphql_cowboy_handler, Args}) ->
                {true, Args};
            (_) ->
                false
        end, Modules),
    Opts2.
