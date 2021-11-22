%% @doc Implements callbacks that format custom errors returned from resolvers or crashes. 
-module(mongoose_graphql_errors).

-export([err/2, crash/2]).

-ignore_xref([err/2, crash/2]).

% callback invoked when resorer returns error tuple
err(_Ctx, domain_not_found) ->
    #{message => <<"Given domain does not exist">>, extensions => #{code => resolver_error}};
err(_Ctx, ErrorTerm) ->
    #{message => iolist_to_binary(io_lib:format("~p", [ErrorTerm])),
      extensions => #{code => resolver_error}}.

% callback invoked when resoler crashes
crash(_Ctx, #{type := Type}) ->
    #{message => <<"Unexpected ", Type/binary, " resolver crash">>,
      extensions => #{code => resolver_crash}}.
