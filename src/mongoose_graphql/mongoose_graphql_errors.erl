%% @doc Implements callbacks that format custom errors returned from resolvers or crashes. 
-module(mongoose_graphql_errors).

-export([format_error/1, format_errors/1, err/2, crash/2]).

-ignore_xref([format_error/1, format_errors/1, err/2, crash/2]).

%% callback invoked when resolver returns error tuple
err(_Ctx, domain_not_found) ->
    #{message => <<"Given domain does not exist">>, extensions => #{code => resolver_error}};
err(_Ctx, ErrorTerm) ->
    #{message => iolist_to_binary(io_lib:format("~p", [ErrorTerm])),
      extensions => #{code => resolver_error}}.

%% callback invoked when resolver crashes
crash(_Ctx, #{type := Type}) ->
    #{message => <<"Unexpected ", Type/binary, " resolver crash">>,
      extensions => #{code => resolver_crash}}.

%% @doc Format errors that occured in any phase.
format_errors(Errors) ->
    [format_error(E) || E <- Errors].

%% @doc Format error that occurred in any phase including HTTP request decoding.
format_error(#{phase := Phase, error_term := Term}) when Phase =:= authorize;
                                                         Phase =:= decode;
                                                         Phase =:= parse ->
    #{extensions => #{code => err_code(Phase, Term)},
      message => iolist_to_binary(err_msg(Phase, Term))};
format_error(#{error_term := _, phase := Phase} = Err) when Phase =:= execute;
                                                            Phase =:= type_check;
                                                            Phase =:= validate;
                                                            Phase =:= uncategorized ->
    graphql:format_errors(#{}, [Err]);
format_error(Err) ->
    #{extensions => #{code => uncathegorized},
      message => iolist_to_binary(io_lib:format("~p", [Err]))}.

%% Internal

err_code(_, Term) ->
    simplify(Term).

simplify(A) when is_atom(A) -> A;
simplify(B) when is_binary(B) -> B;
simplify(T) when is_tuple(T) -> element(1, T).

err_msg(parse, Result) ->
    parse_err_msg(Result);
err_msg(decode, Result) ->
    decode_err_mgs(Result);
err_msg(authorize, Result) ->
    authorize_err_msg(Result).

authorize_err_msg({request_error, {header, <<"authorization">>}, _}) ->
    "Malformed authorization header. Please consult the relevant specification";
authorize_err_msg({no_permissions, Op}) ->
    io_lib:format("Cannot execute query ~s without permissions", [Op]).

parse_err_msg({parser_error, {Line, graphql_parser, Msg}}) ->
    io_lib:format("Cannot parse line ~B because of ~s", [Line, Msg]);
parse_err_msg({scanner_error, {Line, graphql_scanner, Msg}}) ->
    Formatted = lists:flatten(graphql_scanner:format_error(Msg)),
    io_lib:format("Cannot scan line ~B because of ~s", [Line, Formatted]).

decode_err_mgs(no_query_supplied) ->
    "The query was not supplied in request body";
decode_err_mgs(invalid_json_body) ->
    "The request json body is invalid";
decode_err_mgs(variables_invalid_json) ->
    "The variables' json is invalid".
