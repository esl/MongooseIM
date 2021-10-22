-module(mongoose_graphql).

-export([init/0, execute/1, execute/2]).

-ignore_xref([execute/1, execute/2]).

-define(SCHEMA_PATH, "graphql/api_schema.gql").

-spec init() -> ok.
init() ->
    application:stop(graphql),
    application:start(graphql),
    PrivDir = code:priv_dir(mongooseim),
    {ok, SchemaData} = file:read_file( filename:join(PrivDir, ?SCHEMA_PATH)), 
    Mapping = mapping_rules(),
    ok = graphql:load_schema(Mapping, SchemaData),
    ok = setup_root(),
    ok = graphql:validate_schema(),
    ok.

-spec execute(binary()) -> any().
execute(Doc) ->
    execute(<<>>, Doc).

-spec execute(binary(), binary()) -> any().
execute(OpName, Doc) ->
    {ok, Ast} = graphql:parse(Doc),
    {ok, #{ast := AST2 }} = graphql:type_check(Ast),
    ok = graphql:validate(AST2),
    Ctx = #{params => #{}, operation_name => OpName},
    graphql:execute(Ctx, AST2).


mapping_rules() ->
    #{objects => #{
        'Query' => mongoose_graphql_query,
        'Mutation' => mongoose_graphql_mutation,
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
