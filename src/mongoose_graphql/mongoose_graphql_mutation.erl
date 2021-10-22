-module(mongoose_graphql_mutation).

-export([execute/4]).

-ignore_xref([execute/4]).

execute(_Ctx, _Obj, <<"addDomain">>, #{<<"name">> := Name, <<"hostType">> := HostType}) ->
    Domain = #{'__schema__' => domain, id => 5, name => Name, host_type => HostType},
    {ok, Domain};
execute(_Ctx, _Obj, <<"removeDomain">>, #{<<"name">> := _Name}) ->
    {ok, true}.
