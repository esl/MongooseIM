%% @doc Process directives in order to validate or modify the given document.
%%
%% GraphQL has directives that allow attaching additional information to the schema, objects,
%% fields, and more. We decided to use directives to check if the user is allowed to execute
%% commands and if commands require loaded modules and services.
%%
%% The behavior consists of two callbacks. One handles field directives, and the second one
%% takes object directives. Callbacks can modify the document AST.
-module(mongoose_graphql_directive).

-export([process_directives/2]).

-include_lib("graphql/src/graphql_schema.hrl").
-include_lib("graphql/src/graphql_internal.hrl").
-include_lib("graphql/include/graphql.hrl").

-import(mongoose_graphql_directive_helper, [name/1, op_name/1]).

-type document() :: #document{}.
-type ctx() :: #{atom() => term()}.
-type parent() :: term().
-type process_set_result() :: #{parent := parent(), set := [selection_set()]}.
-type process_result() :: #{parent := parent(), schema := selection_set()}.
-type directive() :: graphql:directive().

-export_type([ctx/0]).

%% Can modify field or throw an graphql error to stop document execution.
-callback handle_directive(Dir :: directive(), Field :: schema_field(), Ctx :: map()) ->
                              schema_field().
%% Can modify object and ctx which is later passed to the field directives handler.
-callback handle_object_directive(Dir :: directive(),
                                  Object :: schema_object(),
                                  Ctx :: ctx()) ->
                                     {schema_object(), ctx()}.

dir_handlers() ->
    #{<<"use">> => mongoose_graphql_directive_use,
      <<"protected">> => mongoose_graphql_directive_protected}.

%% @doc Traverse given document and process schema directives. The directive handlers can modify
%% the AST to impact the result or raise a graphql exception.
-spec process_directives(ctx(), document()) -> document().
process_directives(Ctx, #document{definitions = Definitions}) ->
    #{operation_name := OpName} = Ctx,
    F = fun(#op{schema = RootObject, selection_set = Set} = Op) ->
           {RootObject2, Ctx2} = handle_object_directives(RootObject, Ctx),
           Ctx3 =
               Ctx2#{parent => RootObject2,
                     definitions => Definitions,
                     path => [op_name(OpName)]},
           #{set := ResSet, parent := RootObject3} = process_selection_set(Set, Ctx3),
           Op#op{schema = RootObject3, selection_set = ResSet}
        end,
    Definitions2 = lists:map(fun(D) -> if_req_operation(D, OpName, F) end, Definitions),
    #document{definitions = Definitions2}.

%% Internal

-spec process_selection_set([selection_set()], ctx()) -> process_set_result().
process_selection_set(Set, #{parent := InParent} = Ctx) ->
    Fun = fun(S, #{set := AccSet, parent := AccParent}) ->
             Ctx2 = Ctx#{parent := AccParent},
             #{schema := Schema, parent := Parent} = process_selection(S, Ctx2),
             #{set => [Schema | AccSet], parent => Parent}
          end,
    lists:foldl(Fun, #{set => [], parent => InParent}, Set).

%% Process object field's directives. Return modified parent object and field.
-spec process_selection(selection_set(), ctx()) -> process_result().
%% Process generic fields e.g. `{ categoryA { commandA } }`
process_selection(F = #field{id = Id,
                             selection_set = Set,
                             args = FieldArgs,
                             schema = Schema},
                  Ctx) ->
    case Schema of
        #schema_field{ty = FieldType, directives = Directives} ->
            Ctx2 = append_path(Ctx, name(Id)),
            %% Process field type (object) directives
            {FieldType2, FieldTypeCtx} = handle_object_directives(unwrap_type(FieldType), Ctx2),
            %% Process field directives
            ObjField =
                handle_directives(Directives,
                                  get_object_field(Id, Ctx2),
                                  set_field_args(FieldArgs, Ctx2)),
            %% Process field type fields
            #{set := ResSet, parent := FieldType3} =
                process_selection_set(Set, set_parent_object(FieldType2, FieldTypeCtx)),
            FieldType4 = wrap_new_type(FieldType, FieldType3),
            #{parent => update_object_field(Id, ObjField, Ctx2),
              schema =>
                  F#field{selection_set = ResSet, schema = Schema#schema_field{ty = FieldType4}}};
        _ ->
            % Schema is not a `schema_field()` when a field is an introspection field
            #{parent => maps:get(parent, Ctx), schema => F}
    end;
%% Process inline fragments e.g. `{ category {commandA { ... on Domain { domain }}}}`
process_selection(F = #frag{selection_set = Set, schema = ObjectType}, Ctx) ->
    % FIXME think if frag is able to have annotations?
    {ObjectType2, ObjectCtx} = handle_object_directives(ObjectType, Ctx),
    #{set := ResSet, parent := ObjectType3} =
        process_selection_set(Set, set_parent_object(ObjectType2, ObjectCtx)),
    #{parent => maps:get(parent, Ctx), % Return unmodified parent object
      schema => F#frag{selection_set = ResSet, schema = ObjectType3}};
%% Process fragments e.g.
%% ```
%% fragment DomainParts on Domain { domain enabled }
%% query { category {commandA { ...DomainParts }}}
%% ```
process_selection(F = #frag_spread{id = Id}, Ctx) ->
    Res = process_selection(get_fragment(Id, Ctx), Ctx),
    Res#{schema := F}.

-spec handle_object_directives(schema_object(), ctx()) -> {schema_object(), ctx()}.
handle_object_directives(Object, Ctx) ->
    Fun = fun(D, {Obj, ObjCtx}) -> handle_object_directive(D, Obj, ObjCtx) end,
    lists:foldl(Fun, {Object, Ctx#{field_args => []}}, get_directives(Object)).

-spec get_directives(schema_object()) -> [directive()].
get_directives(#interface_type{directives = Directives}) ->
    Directives;
get_directives(#object_type{directives = Directives}) ->
    Directives;
get_directives(#union_type{directives = Directives}) ->
    Directives;
get_directives(_) ->
    [].

-spec handle_object_directive(directive(), object_type(), ctx()) ->
                                 {object_type(), ctx()}.
handle_object_directive(#directive{id = {name, _, Name}} = D, Field, Ctx) ->
    handle_object_directive(D#directive{id = Name}, Field, Ctx);
handle_object_directive(#directive{id = Name} = D, Field, Ctx) ->
    Module = maps:get(Name, dir_handlers()),
    Module:handle_object_directive(D, Field, Ctx).

-spec handle_directives([directive()], schema_field(), ctx()) -> schema_field().
handle_directives(Directives, Field, Ctx) ->
    Fun = fun(D, FieldSchema) -> handle_directive(D, FieldSchema, Ctx) end,
    lists:foldl(Fun, Field, Directives).

-spec handle_directive(directive(), schema_field(), ctx()) -> schema_field().
handle_directive(#directive{id = {name, _, Name}} = D, Field, Ctx) ->
    handle_directive(D#directive{id = Name}, Field, Ctx);
handle_directive(#directive{id = Name} = D, Field, Ctx) ->
    Module = maps:get(Name, dir_handlers()),
    Module:handle_directive(D, Field, Ctx).

-spec get_fragment(graphql:name(), ctx()) -> frag().
get_fragment(Id, #{definitions := Definitions}) ->
    Name = name(Id),
    Fun = fun (#frag{id = FId}) ->
                  name(FId) =:= Name;
              (_) ->
                  false
          end,
    hd(lists:filter(Fun, Definitions)).

if_req_operation(#op{id = 'ROOT'} = Op, undefined, Fun) ->
    Fun(Op);
if_req_operation(#op{id = {name, _, Name}} = Op, Name, Fun) ->
    Fun(Op);
if_req_operation(Op, _, _) ->
    Op.

field_args_to_map(FieldArgs, Params) ->
    maps:from_list([prepare_arg(ArgName, Type, Params) || {ArgName, Type} <- FieldArgs]).

prepare_arg(ArgName, #{value := #var{id = Name}}, Vars) ->
    {ArgName, maps:get(name(Name), Vars, null)};
prepare_arg(ArgName, #{value := Val}, _) ->
    {ArgName, Val}.

%% GraphQL type helpers

unwrap_type({non_null, {list, T}}) ->
    T;
unwrap_type({non_null, T}) ->
    T;
unwrap_type({list, T}) ->
    T;
unwrap_type(T) ->
    T.

wrap_new_type({non_null, {list, _}}, T) ->
    {non_null, {list, T}};
wrap_new_type({list, _}, T) ->
    {list, T};
wrap_new_type({non_null, _}, T) ->
    {non_null, T};
wrap_new_type(_, T) ->
    T.

update_wrapped_type({non_null, {list, T}}, Fun) ->
    {non_null, {list, Fun(T)}};
update_wrapped_type({non_null, T}, Fun) ->
    {non_null, Fun(T)};
update_wrapped_type({list, T}, Fun) ->
    {list, Fun(T)};
update_wrapped_type(T, Fun) ->
    Fun(T).

%% Context helpers

append_path(#{path := Path} = Ctx, FieldName) ->
    Ctx#{path => [FieldName | Path]}.

set_field_args(FieldArgs, #{params := Params} = Ctx) ->
    Ctx#{field_args => field_args_to_map(FieldArgs, Params)}.

set_parent_object(Parent, Ctx) ->
    Ctx#{parent => Parent}.

get_object_field(Id, #{parent := Parent}) ->
    Fields =
        case unwrap_type(Parent) of
            #object_type{fields = Fs} ->
                Fs;
            #interface_type{fields = Fs} ->
                Fs
        end,
    maps:get(name(Id), Fields).

update_object_field(Id, ObjField, #{parent := ParentType}) ->
    Fun = fun (#object_type{} = Obj) ->
                  Fields = maps:put(name(Id), ObjField, Obj#object_type.fields),
                  Obj#object_type{fields = Fields};
              (#interface_type{} = Int) ->
                  Fields = maps:put(name(Id), ObjField, Int#interface_type.fields),
                  Int#interface_type{fields = Fields}
          end,
    update_wrapped_type(ParentType, Fun).
