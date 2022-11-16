-module(mongoose_graphql_check_categories).

-export([process_ast/2]).

-include_lib("graphql/src/graphql_schema.hrl").
-include_lib("graphql/src/graphql_internal.hrl").
-include_lib("graphql/include/graphql.hrl").
-include_lib("jid/include/jid.hrl").

-type document() :: #document{}.
-type categories() :: [binary()].

-include("mongoose.hrl").

-spec process_ast(document(), categories()) -> document().
process_ast(#document{definitions = Definitions} = Document, Categories) ->
    case Categories of
        [] ->
            Document;
        _ ->
            Definitions2 = lists:map(fun(#op{schema = Schema} = Op) ->
                parse_schema(Schema, Op, Categories)
            end, Definitions),
            #document{definitions = Definitions2}
    end.

parse_schema(#object_type{fields = Fields} = Schema, Op, Categories) ->
    Fields2 = maps:map(fun(Key, Value) ->
        case lists:member(Key, Categories) of
            true -> Value;
            false ->
                case Value of
                    #schema_field{resolve = undefined} ->
                        Fun = category_disabled_fun(Key),
                        Value#schema_field{resolve = Fun};
                    _ ->
                        Value
                end
        end
    end, Fields),
    Schema2 = Schema#object_type{fields = Fields2},
    Op#op{schema = Schema2};
parse_schema(_, Op, _) ->
    Op.

-spec category_disabled_fun(binary()) -> resolver().
category_disabled_fun(Category) ->
    Msg = <<"Category disabled">>,
    Extra = #{category => Category},
    fun(_, _, _, _) -> mongoose_graphql_helper:make_error(category_disabled, Msg, Extra) end.
