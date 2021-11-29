-module(mongoose_graphql).

-export([init/0, get_endpoint/1, execute/2]).

-type request() :: #{document := binary(),
                     operation_name := binary(),
                     vars := map() | undefined,
                     authorized := boolean(),
                     ctx := map()}.

-export_type([request/0]).

-define(USER_EP_NAME, user_schema_ep).
-define(ADMIN_EP_NAME, admin_schema_ep).

-spec init() -> ok.
init() ->
    create_endpoint(?USER_EP_NAME, user_mapping_rules(), "user"),
    create_endpoint(?ADMIN_EP_NAME, admin_mapping_rules(), "admin"),
    ok.

-spec get_endpoint(atom()) -> {ok, graphql:endpoint_context()} | {error, term()}.
get_endpoint(admin) ->
    {ok, graphql_schema:get_endpoint_ctx(?ADMIN_EP_NAME)};
get_endpoint(user) ->
    {ok, graphql_schema:get_endpoint_ctx(?USER_EP_NAME)};
get_endpoint(_) ->
    {error, unknown_endpoint}.

-spec execute(graphql:endpoint_context(), request() | binary()) ->
    {ok, map()} | {error, term()}.
execute(Ep, #{document := Doc,
              operation_name := OpName,
              authorized := AuthStatus,
              vars := Vars,
              ctx := Ctx}) ->
    try
        {ok, Ast} = graphql_parse(Doc),
        {ok, #{ast := Ast2,
               fun_env := FunEnv}} = graphql:type_check(Ep, Ast),
        ok = graphql:validate(Ast2),
        ok = mongoose_graphql_permissions:check_permissions(OpName, AuthStatus, Ast2),
        Coerced = graphql:type_check_params(Ep, FunEnv, OpName, Vars),
        Ctx2 = Ctx#{params => Coerced,
                    operation_name => OpName,
                    error_module => mongoose_graphql_errors},
        {ok, graphql:execute(Ep, Ctx2, Ast2)}
    catch
        throw:{error, Err} ->
            {error, Err}
    end;
execute(Ep, Doc)  ->
    Req = #{document => Doc,
            operation_name => <<>>,
            vars => undefined,
            authorized => true,
            ctx => #{}},
    execute(Ep, Req).

% Internal

-spec create_endpoint(atom(), map(), string()) -> ok.
create_endpoint(Name, Mapping, DirName) ->
    graphql_schema:start_link(Name),
    Ep = graphql_schema:get_endpoint_ctx(Name),
    Pattern =
        filename:join([code:priv_dir(mongooseim),
                       "graphql/schemas",
                       DirName, "*.gql"]),
    {ok, SchemaData} = load_multiple_file_schema(Pattern),
    ok = graphql:load_schema(Ep, Mapping, SchemaData),
    ok = graphql:validate_schema(Ep),
    ok.

graphql_parse(Doc) ->
    case graphql:parse(Doc) of
        {ok, _} = Ok ->
            Ok;
        {error, _} = Err ->
            throw(Err)
    end.

admin_mapping_rules() ->
    #{objects => #{
            'AdminQuery' => mongoose_graphql_admin_query,
            'AdminMutation' => mongoose_graphql_admin_mutation,
            'Domain' => mongoose_graphql_domain,
            default => mongoose_graphql_default
           }
         }.

user_mapping_rules() ->
    #{objects => #{
            'UserQuery' => mongoose_graphql_user_query,
            'UserMutation' => mongoose_graphql_user_mutation,
            default => mongoose_graphql_default
           }
         }.

load_multiple_file_schema(Pattern) ->
    Paths = filelib:wildcard(Pattern),
    try
        SchemaData = [read_schema_file(P) || P <- Paths],
        {ok, lists:flatten(SchemaData)}
    catch
        _:_ ->
            {error, cannot_load}
    end.

read_schema_file(Path) ->
    {ok, Data} = file:read_file(Path),
    binary_to_list(Data).
