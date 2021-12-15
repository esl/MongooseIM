%% @doc Implements callbacks that format custom errors returned from resolvers or crashes. 
-module(mongoose_graphql_errors).

-export([format_error/1, format_errors/1, err/2, crash/2]).

-ignore_xref([format_error/1, format_errors/1, err/2, crash/2]).

% callback invoked when resolver returns error tuple
err(_Ctx, domain_not_found) ->
    #{message => <<"Given domain does not exist">>, extensions => #{code => resolver_error}};
err(_Ctx, ErrorTerm) ->
    #{message => iolist_to_binary(io_lib:format("~p", [ErrorTerm])),
      extensions => #{code => resolver_error}}.

% callback invoked when resolver crashes
crash(_Ctx, #{type := Type}) ->
    #{message => <<"Unexpected ", Type/binary, " resolver crash">>,
      extensions => #{code => resolver_crash}}.

format_errors(Errors) ->
    [format_error(E) || E <- Errors].

format_error(#{phase := Phase, error_term := Term}) when Phase =:= authorize;
                                                         Phase =:= parse ->
    #{extensions => #{code => err_code(Phase, Term)},
      message => iolist_to_binary(err_msg(Phase, Term))};
format_error(#{error_term := _} = Err) ->
    graphql:format_errors(#{}, [Err]);
format_error(Err) ->
    #{extensions => #{code => uncathegorized_error},
      message => iolist_to_binary(io_lib:format("~p", Err))}.

%% Internal

err_code(authorize, _Term) ->
    authorization_error;
err_code(parse, Term) ->
    element(1, Term).

err_msg(parse, Result) ->
    parse_err_msg(Result);
err_msg(authorize, Result) ->
    authorize_err_msg(Result).

authorize_err_msg({request_error, {header, <<"authorization">>}, _}) ->
    "Malformed authorization header.Please consult the relevant specification.";
authorize_err_msg({no_permissions, Op}) ->
    io_lib:format("Cannot execute query ~s without permissions", [Op]).

parse_err_msg({parser_error, {Line, graphql_parser, Msg}}) ->
    io_lib:format("Cannot parse line ~B because of ~s", [Line, Msg]);
parse_err_msg({scanner_error, {Line, graphql_scanner, Msg}}) ->
    Formatted = lists:flatten(graphql_scanner:format_error(Msg)),
    io_lib:format("Cannot scan line ~B because of ~s", [Line, Formatted]).
