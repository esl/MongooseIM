-module(mongoose_domain_core_SUITE).

-compile(export_all).

all() ->
    [
        core_lookup_works,
        core_lookup_not_found,
        core_static_domain,
        core_cannot_insert_static,
        core_get_all_static,
        core_get_domains_by_host_type
    ].

init_per_suite(Config) ->
    Pairs = [{<<"example.cfg">>, <<"type1">>},
              {<<"erlang-solutions.com">>, <<"type2">>},
              {<<"erlang-solutions.local">>, <<"type2">>}],
    CommonTypes = [<<"type1">>, <<"type2">>, <<"dbgroup">>, <<"dbgroup2">>, <<"cfggroup">>],
    mongoose_domain_core:start(Pairs, CommonTypes),
    Config.

end_per_suite(Config) ->
    mongoose_domain_core:stop(),
    Config.

core_lookup_works(_) ->
    {ok, <<"type1">>} = mongoose_domain_core:get_host_type(<<"example.cfg">>).

core_lookup_not_found(_) ->
    {error, not_found} = mongoose_domain_core:get_host_type(<<"example.missing">>).

core_static_domain(_) ->
    true = mongoose_domain_core:is_static(<<"example.cfg">>).

core_cannot_insert_static(_) ->
    {error, static} = mongoose_domain_core:insert(<<"example.cfg">>, <<"type1">>).

%% See also db_get_all_static
core_get_all_static(_) ->
    %% Could be in any order
    [{<<"erlang-solutions.com">>, <<"type2">>},
     {<<"erlang-solutions.local">>, <<"type2">>},
     {<<"example.cfg">>, <<"type1">>}] =
        lists:sort(mongoose_domain_core:get_all_static()).

core_get_domains_by_host_type(_) ->
    [<<"erlang-solutions.com">>, <<"erlang-solutions.local">>] =
    lists:sort(mongoose_domain_core:get_domains_by_host_type(<<"type2">>)),
    [<<"example.cfg">>] = mongoose_domain_core:get_domains_by_host_type(<<"type1">>),
    [] = mongoose_domain_core:get_domains_by_host_type( <<"type6">>).