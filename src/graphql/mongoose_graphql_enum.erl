-module(mongoose_graphql_enum).

-export([input/2, output/2]).

-ignore_xref([input/2, output/2]).

input(<<"PresenceShow">>, Show) ->
    {ok, list_to_binary(string:to_lower(binary_to_list(Show)))};
input(<<"PresenceType">>, Type) ->
    {ok, list_to_binary(string:to_lower(binary_to_list(Type)))};
input(<<"AuthStatus">>, <<"AUTHORIZED">>) -> {ok, 'AUTHORIZED'};
input(<<"AuthStatus">>, <<"UNAUTHORIZED">>)  -> {ok, 'UNAUTHORIZED'}.

output(<<"PresenceShow">>, Show) ->
    {ok, list_to_binary(string:to_upper(binary_to_list(Show)))};
output(<<"PresenceType">>, Type) ->
    {ok, list_to_binary(string:to_upper(binary_to_list(Type)))};
output(<<"AuthStatus">>, Status) ->
    {ok, atom_to_binary(Status, utf8)}.
