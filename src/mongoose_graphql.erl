%% @doc This module provides main interface to graphql. It initializes schemas
%% and allows executing queries with permissions checks.
%% @end
-module(mongoose_graphql).

-include_lib("kernel/include/logger.hrl").

%API
-export([init/0,
         get_endpoint/1,
         create_endpoint/3,
         execute/2,
         execute/3]).

-ignore_xref([create_endpoint/3]).

-type request() :: #{document := binary(),
                     operation_name := binary() | undefined,
                     vars := map(),
                     authorized := boolean(),
                     ctx := map()}.

-export_type([request/0]).

-define(USER_EP_NAME, user_schema_ep).
-define(ADMIN_EP_NAME, admin_schema_ep).

%% @doc Create and initialize endpoints for user and admin.
-spec init() -> ok.
init() ->
    create_endpoint(?USER_EP_NAME, user_mapping_rules(), schema_global_patterns("user")),
    create_endpoint(?ADMIN_EP_NAME, admin_mapping_rules(), schema_global_patterns("admin")),
    ok.

%% @doc Get endpoint_context for passed endpoint name.
-spec get_endpoint(atom()) -> graphql:endpoint_context().
get_endpoint(admin) ->
    graphql_schema:get_endpoint_ctx(?ADMIN_EP_NAME);
get_endpoint(user) ->
    graphql_schema:get_endpoint_ctx(?USER_EP_NAME);
get_endpoint(Name) ->
    graphql_schema:get_endpoint_ctx(Name).

%% @doc Create a new endpoint and load schema.
-spec create_endpoint(atom(), map(), [file:filename_all()]) -> gen:start_ret().
create_endpoint(Name, Mapping, Patterns) ->
    Res = graphql_schema:start_link(Name),
    Ep = graphql_schema:get_endpoint_ctx(Name),
    {ok, SchemaData} = load_multiple_file_schema(Patterns),
    ok = graphql:load_schema(Ep, Mapping, SchemaData),
    ok = graphql:validate_schema(Ep),
    Res.

%% @doc Execute request on a given endpoint.
-spec execute(graphql:endpoint_context(), request()) ->
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
    end.

%% @doc Execute selected operation on a given endpoint with authorization.
-spec execute(graphql:endpoint_context(), undefined | binary(), binary()) ->
    {ok, map()} | {error, term()}.
execute(Ep, OpName, Doc)  ->
    Req = #{document => Doc,
            operation_name => OpName,
            vars => #{},
            authorized => true,
            ctx => #{}},
    execute(Ep, Req).

% Internal

-spec schema_global_patterns(file:name_all()) -> [file:filename_all()].
schema_global_patterns(SchemaDir) ->
    [schema_pattern(SchemaDir), schema_pattern("global")].

-spec schema_pattern(file:name_all()) -> file:filename_all().
schema_pattern(DirName) ->
    schema_pattern(DirName, "*.gql").

-spec schema_pattern(file:name_all(), file:name_all()) -> file:filename_all().
schema_pattern(DirName, Pattern) ->
    filename:join([code:priv_dir(mongooseim), "graphql/schemas", DirName, Pattern]).

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

load_multiple_file_schema(Patterns) ->
    Paths = lists:flatmap(fun(P) -> filelib:wildcard(P) end, Patterns),
    try
        SchemaData = [read_schema_file(P) || P <- Paths],
        {ok, lists:flatten(SchemaData)}
    catch
        throw:{error, Reason, Path} ->
            ?LOG_ERROR(#{what => graphql_cannot_load_schema,
                         reason => Reason, path => Path}),
            {error, cannot_load}
    end.

read_schema_file(Path) ->
    case file:read_file(Path) of
         {ok, Data} ->
            binary_to_list(Data);
         {error, Reason} ->
            throw({error, Reason, Path})
    end.
