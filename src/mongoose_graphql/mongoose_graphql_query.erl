-module(mongoose_graphql_query).

-export([execute/4]).

-ignore_xref([execute/4]).

execute(_Ctx, _Obj, <<"domains">>, _Args) ->
    {ok, example_domains()};
execute(_Ctx, _Obj, <<"domain">>, #{<<"name">> := Name}) ->
    case lists:filter(fun({ok, #{name := Fname}}) -> Fname == Name end, example_domains()) of
        [{ok, Film}] -> 
            {ok, Film};
        _ -> {error, <<"Not found">>}
    end;
execute(_Ctx, _Obj, _Field, _Args) ->
    {error, <<"Not found object">>}.

example_domains() ->
    [
     {ok, #{'__schema__' => domain, id => 1, name => <<"localhost">>, host_type => <<"default">>}},
     {ok, #{'__schema__' => domain, id => 2, name => <<"esl.com">>, host_type => <<"default">>}},
     {ok, #{'__schema__' => domain, id => 3, name => <<"example-domain.com">>, host_type => <<"test host">>}},
     {ok, #{'__schema__' => domain, id => 4, name => <<"example-domain2.com">>, host_type => <<"test host">>}}
    ].

