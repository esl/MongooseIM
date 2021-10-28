-module(mongoose_graphql).

-export([init/0, execute/1, execute/3]).

-ignore_xref([execute/1, execute/3]).

-define(SCHEMA_PATH, "graphql/api_schema.gql").

-spec init() -> ok.
init() ->
    graphql_schema:reset(),
    PrivDir = code:priv_dir(mongooseim),
    {ok, SchemaData} = file:read_file( filename:join(PrivDir, ?SCHEMA_PATH)), 
    Mapping = mapping_rules(),
    ok = graphql:load_schema(Mapping, SchemaData),
    ok = setup_root(),
    ok = graphql:validate_schema(),
    ok.

-spec execute(binary()) -> {ok, map()} | {error, term()}.
execute(Doc) ->
    execute(<<>>, admin, Doc).

-spec execute(binary(), mongoose_graphql_permission:role(), binary()) -> {ok, map()} | {error, term()}.
execute(OpName, Role, Doc) ->
    case graphql:parse(Doc) of
        {ok, Ast} ->
            try
                {ok, #{ast := Ast2 }} = graphql:type_check(Ast),
                ok = graphql:validate(Ast2),
                Ctx = #{params => #{}, operation_name => OpName,
                                    role => Role},
                {ok, graphql:execute(Ctx, Ast2)}
            catch
                throw:{error, Err} ->
                    {error, Err}
            end;
        {error, Err} ->
            {error, Err}
    end.

% Internal

mapping_rules() ->
    #{objects => #{
        'Query' => mongoose_graphql_query,
        'Mutation' => mongoose_graphql_mutation,
        'AdminQuery' => mongoose_graphql_admin_query,
        'AdminMutation' => mongoose_graphql_admin_mutation,
        'UserQuery' => mongoose_graphql_user_query,
        'UserMutation' => mongoose_graphql_user_mutation,
        'Domain' => mongoose_graphql_domain,
        'default' => mongoose_graphql_default
        }
    }.

setup_root() ->
    Root = {root,
            #{ query => 'Query',
               mutation => 'Mutation'
            }},
    ok = graphql:insert_root(Root),
    ok.
