-module(graphql_helper).

-include_lib("common_test/include/ct.hrl").

-import(distributed_helper, [mim/0, rpc/4]).

-export([load_test_schema/2]).

load_test_schema(Name, Config) ->
    Path = filename:join([?config(mim_data_dir, Config), "schema.gql"]),
    {ok, SchemaData} = file:read_file(Path),
    Ep = rpc(mim(), mongoose_graphql, get_endpoint, [Name]),
    ok = rpc(mim(), graphql_schema, reset, [Ep]),
    ok = rpc(mim(), graphql, load_schema, [Ep, test_schema_mapping(), SchemaData]),
    ok = rpc(mim(), graphql, validate_schema, [Ep]),
    ok.

test_schema_mapping() ->
    #{objects => #{
            'Query' => mongoose_graphql_default,
            'Mutation' => mongoose_graphql_default,
            default => mongoose_graphql_default
           }
         }.
