-module(graphql_domain_SUITE).

 -include_lib("common_test/include/ct.hrl").
 -include_lib("eunit/include/eunit.hrl").
 -include_lib("exml/include/exml.hrl").

 -compile([export_all, nowarn_export_all]).

-import(distributed_helper, [mim/0, require_rpc_nodes/1, rpc/4]).
-import(graphql_helper, [execute_auth/2, init_admin_handler/1]).

-define(HOST_TYPE, <<"dummy auth">>).
-define(SECOND_HOST_TYPE, <<"test type">>).

suite() ->
    require_rpc_nodes([mim]) ++ escalus:suite().

all() ->
     [{group, domain_handler}].

groups() ->
     [{domain_handler, [sequence], domain_handler()}].

domain_handler() ->
    [create_domain,
     unknown_host_type_error_formatting,
     static_domain_error_formatting,
     domain_duplicate_error_formatting,
     domain_not_found_error_formatting_after_mutation_disable_domain,
     domain_not_found_error_formatting_after_mutation_enable_domain,
     domain_not_found_error_formatting_after_query,
     wrong_host_type_error_formatting,
     disable_domain,
     enable_domain,
     get_domains_by_host_type,
     get_domain_details,
     delete_domain,
     get_domains_after_deletion].

init_per_suite(Config) ->
    case mongoose_helper:is_rdbms_enabled(?HOST_TYPE) of
        true -> escalus:init_per_suite(init_admin_handler(Config));
        false -> {skip, require_rdbms}
    end.

end_per_suite(Config) ->
    escalus_fresh:clean(),
    escalus:end_per_suite(Config).

init_per_testcase(CaseName, Config) ->
     escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
     escalus:end_per_testcase(CaseName, Config).

create_domain(Config) ->
    create_domain(Config, <<"exampleDomain">>),
    create_domain(Config, <<"exampleDomain2">>).

create_domain(Config, DomainName) ->
    Vars = #{domain => DomainName, hostType => ?HOST_TYPE},
    Result = execute_auth(#{query => create_domain_call(), variables => Vars,
        operationName => <<"M1">>}, Config),
    ParsedResult = ok_result(<<"domains">>, <<"addDomain">>, Result),
    ?assertEqual(#{<<"domain">> => DomainName,
        <<"hostType">> => ?HOST_TYPE,
        <<"enabled">> => null}, ParsedResult).

unknown_host_type_error_formatting(Config) ->
    DomainName = <<"exampleDomain">>,
    HostType = <<"NonExistingHostType">>,
    Vars = #{domain => DomainName, hostType => HostType},
    Result = execute_auth(#{query => create_domain_call(), variables => Vars,
                   operationName => <<"M1">>}, Config),
    ParsedResult = error_result(1, Result),
    ?assertEqual(#{<<"extensions">> =>
                     #{<<"code">> => <<"unknown_host_type">>,
                       <<"hostType">> => HostType},
                   <<"message">> => <<"Unknown host type">>,
                   <<"path">> => [<<"domains">>, <<"addDomain">>]}, ParsedResult).

static_domain_error_formatting(Config) ->
    DomainName = <<"localhost">>,
    Vars = #{domain => DomainName, hostType => ?HOST_TYPE},
    Result = execute_auth(#{query => create_domain_call(), variables => Vars,
                   operationName => <<"M1">>}, Config),
    ParsedResult = error_result(1, Result),
    ?assertEqual(#{<<"extensions">> =>
                     #{<<"code">> => <<"domain_static">>,
                       <<"domain">> => DomainName},
                   <<"message">> => <<"Domain static">>,
                   <<"path">> => [<<"domains">>, <<"addDomain">>]}, ParsedResult).

domain_duplicate_error_formatting(Config) ->
    DomainName = <<"exampleDomain">>,
    Vars = #{domain => DomainName, hostType => ?SECOND_HOST_TYPE},
    Result = execute_auth(#{query => create_domain_call(), variables => Vars,
                   operationName => <<"M1">>}, Config),
    ParsedResult = error_result(1, Result),
    ?assertEqual(#{<<"extensions">> =>
                     #{<<"code">> => <<"domain_duplicate">>,
                       <<"domain">> => DomainName},
                   <<"message">> => <<"Domain already exists">>,
                   <<"path">> => [<<"domains">>, <<"addDomain">>]}, ParsedResult).

domain_not_found_error_formatting_after_mutation_enable_domain(Config) ->
    DomainName = <<"NonExistingDomain">>,
    Vars = #{domain => DomainName},
    Result = execute_auth(#{query => enable_domain_call(), variables => Vars,
                   operationName => <<"M1">>}, Config),
    domain_not_found_error_formatting(Result, DomainName, <<"enableDomain">>).

domain_not_found_error_formatting_after_mutation_disable_domain(Config) ->
    DomainName = <<"NonExistingDomain">>,
    Vars = #{domain => DomainName},
    Result = execute_auth(#{query => disable_domain_call(), variables => Vars,
                   operationName => <<"M1">>}, Config),
    domain_not_found_error_formatting(Result, DomainName, <<"disableDomain">>).

domain_not_found_error_formatting_after_query(Config) ->
    DomainName = <<"NonExistingDomain">>,
    Vars = #{domain => DomainName},
    Result = execute_auth(#{query => get_domain_details_call(), variables => Vars,
                   operationName => <<"Q1">>}, Config),
    domain_not_found_error_formatting(Result, DomainName, <<"domainDetails">>).

domain_not_found_error_formatting(Result, DomainName, GraphqlCall) ->
    ParsedResult = error_result(1, Result),
    ?assertEqual(#{<<"extensions">> =>
                     #{<<"code">> => <<"domain_not_found">>,
                       <<"domain">> => DomainName},
                   <<"message">> => <<"Given domain does not exist">>,
                   <<"path">> => [<<"domains">>, GraphqlCall]}, ParsedResult).

wrong_host_type_error_formatting(Config) ->
    DomainName = <<"exampleDomain">>,
    Vars = #{domain => DomainName, hostType => ?SECOND_HOST_TYPE},
    Result = execute_auth(#{query => delete_domain_call(), variables => Vars,
                   operationName => <<"M1">>}, Config),
    ParsedResult = error_result(1, Result),
    ?assertEqual(#{<<"extensions">> =>
                     #{<<"code">> => <<"wrong_host_type">>,
                       <<"hostType">> => ?SECOND_HOST_TYPE},
                   <<"message">> => <<"Wrong host type">>,
                   <<"path">> => [<<"domains">>, <<"removeDomain">>]}, ParsedResult).

disable_domain(Config) ->
    Vars = #{domain => <<"exampleDomain">>},
    Result = execute_auth(#{query => disable_domain_call(), variables => Vars,
                   operationName => <<"M1">>}, Config),
    ParsedResult = ok_result(<<"domains">>, <<"disableDomain">>, Result),
    ?assertEqual(#{<<"domain">> => <<"exampleDomain">>, <<"enabled">> => false}, ParsedResult),
    {ok, Domain} = rpc(mim(), mongoose_domain_sql, select_domain, [<<"exampleDomain">>]),
    ?assertEqual(#{host_type => ?HOST_TYPE,
                   enabled => false}, Domain).


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
ok_result(What1, What2, {{<<"200">>, <<"OK">>}, #{<<"data">> := Data}}) ->
    maps:get(What2, maps:get(What1, Data)).

error_result(ErrorNumber, {{<<"200">>, <<"OK">>}, #{<<"errors">> := Errors}}) ->
    lists:nth(ErrorNumber, Errors).
