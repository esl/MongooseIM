%% @doc Implements callbacks that format custom errors returned from resolvers or crashes.
%% In addition, it can format each type of error that occurred in any graphql
%% or mongoose_graphql phase.
%% @end
-module(mongoose_graphql_errors).

-export([format_error/1, err/2, crash/2]).

-ignore_xref([format_error/1, err/2, crash/2]).

-type err_msg() :: #{message := binary(), extensions => map(), path => list()}.

%% callback invoked when resolver returns error tuple
-spec err(map(), term()) -> err_msg().
err(_Ctx, domain_not_found) ->
    #{message => <<"Given domain does not exist">>, extensions => #{code => resolver_error}};
err(_Ctx, ErrorTerm) ->
    #{message => iolist_to_binary(io_lib:format("~p", [ErrorTerm])),
      extensions => #{code => resolver_error}}.

%% callback invoked when resolver crashes
-spec crash(map(), term()) -> err_msg().
crash(_Ctx, #{type := Type}) ->
    #{message => <<"Unexpected ", Type/binary, " resolver crash">>,
      extensions => #{code => resolver_crash}}.

%% @doc Format error that occurred in any phase including HTTP request decoding.
-spec format_error(term())-> {integer(), err_msg()}.
format_error(#{phase := Phase, error_term := Term} = Err) when Phase =:= authorize;
                                                               Phase =:= decode;
                                                               Phase =:= parse ->
    Msg = #{extensions => #{code => err_code(Phase, Term)},
            message => iolist_to_binary(err_msg(Phase, Term))},
    {err_http_code(Phase), add_path(Err, Msg)};
format_error(#{error_term := _, phase := Phase} = Err) when Phase =:= execute;
                                                            Phase =:= type_check;
                                                            Phase =:= validate;
                                                            Phase =:= uncategorized ->
    Err2 = maps:merge(#{path => []}, Err),
    [ErrMsg] = graphql:format_errors(#{}, [Err2]),
    {400, ErrMsg};
format_error(internal_crash) ->
    Msg = #{message => <<"GraphQL Internal Server Error">>,
            extensions => #{code => internal_server_error}},
    {500, Msg};
format_error(Err) ->
    Msg = #{extensions => #{code => uncategorized},
            message => iolist_to_binary(io_lib:format("~p", [Err]))},
    {400, Msg}.

%% Internal

err_http_code(authorize) ->
    401;
err_http_code(_) ->
    400.

err_code(_, Term) ->
    simplify(Term).

simplify(A) when is_atom(A) -> A;
simplify(B) when is_binary(B) -> B;
simplify(T) when is_tuple(T) -> element(1, T).

err_msg(parse, Result) ->
    parse_err_msg(Result);
err_msg(decode, Result) ->
    decode_err_msg(Result);
err_msg(authorize, Result) ->
    authorize_err_msg(Result).

authorize_err_msg({request_error, {header, <<"authorization">>}, _}) ->
    "Malformed authorization header. Please consult the relevant specification";
authorize_err_msg(wrong_credentials) ->
    "The provided credentials are wrong";
authorize_err_msg({no_permissions, Op}) ->
    io_lib:format("Cannot execute query ~s without permissions", [Op]).

parse_err_msg({parser_error, {Line, graphql_parser, Msg}}) ->
    io_lib:format("Cannot parse line ~B because of ~s", [Line, Msg]);
parse_err_msg({scanner_error, {Line, graphql_scanner, Msg}}) ->
    Formatted = lists:flatten(graphql_scanner:format_error(Msg)),
    io_lib:format("Cannot scan line ~B because of ~s", [Line, Formatted]).

decode_err_msg(no_query_supplied) ->
    "The query was not supplied in the request body";
decode_err_msg(invalid_json_body) ->
    "The request JSON body is invalid";
decode_err_msg(variables_invalid_json) ->
    "The variables' JSON is invalid".

add_path(#{path := Path}, ErrMsg) ->
    ErrMsg#{path => Path};
add_path(_, ErrMsg) ->
    ErrMsg.
