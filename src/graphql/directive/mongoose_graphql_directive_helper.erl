-module(mongoose_graphql_directive_helper).

-export([name/1, get_arg/2, op_name/1]).

op_name(undefined) ->
    <<"ROOT">>;
op_name(Name) ->
    Name.

name({name, _, N}) ->
    N;
name(N) when is_binary(N) ->
    N.

get_arg(Name, Args) when is_binary(Name) ->
    Path = binary:split(Name, <<".">>, [global]),
    get_arg(Path, Args);
get_arg([], Value) ->
    Value;
get_arg(_, undefined) ->
    undefined;
get_arg(Path, List) when is_list(List) ->
    [get_arg(Path, ArgsMap) || ArgsMap <- List];
get_arg([Name | Path], ArgsMap) ->
    get_arg(Path, maps:get(Name, ArgsMap, undefined)).
