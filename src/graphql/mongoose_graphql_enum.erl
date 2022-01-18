-module(mongoose_graphql_enum).

-export([input/2, output/2]).

-ignore_xref([input/2, output/2]).

input(<<"AuthStatus">>, <<"AUTHORIZED">>) -> {ok, 'AUTHORIZED'};
input(<<"AuthStatus">>, <<"UNAUTHORIZED">>)  -> {ok, 'UNAUTHORIZED'}.

output(<<"AuthStatus">>, Status) ->
    {ok, atom_to_binary(Status, utf8)}.
