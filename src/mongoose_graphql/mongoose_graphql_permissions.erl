%% @doc Checks if a requested query can be executed with provided permissions.
%%
%% GraphQL has directives that allow attaching additional information to schema,
%% to objects, to fields, and more. The custom directive `@protected' is created
%% to mark which objects or fields could be accessed only by an authorized request.
%% This module analyzes the AST and tries to find if there is at least one protected
%% resource. The `@protected' directive can be attached to <b>field definitions</b>
%% to <b>objects</b>, or to <b>interfaces</b>.
%%
%% Interfaces and objects permissions are checked independently. This means that when
%% an interface is protected or has protected fields, then all implementing objects
%% should be protected or have the same fields protected. <strong>This demands to mark all
%% protected resources at every occurrence with the directive</strong>. Otherwise permissions
%% will be different for interface and implementing objects.
%%
%% If an unauthorized request wants to execute a query that contains protected resources,
%% an error is thrown.
%%
%% Directives can have arguments, so if needed this functionality can be easily
%% extended. For example, to allow access to resources only to the user that belongs
%% to a specific group.
%% @end
-module(mongoose_graphql_permissions).

-export([check_permissions/3]).

-include_lib("graphql/src/graphql_schema.hrl").
-include_lib("graphql/src/graphql_internal.hrl").
-include_lib("graphql/include/graphql.hrl").

-type auth_status() :: boolean().
-type document() :: #document{}.

%% @doc Checks if query can be executed by unauthorized request. If not, throws
%% an error. When request is authorized, just skip.
%% @end
-spec check_permissions(binary(), auth_status(), document()) -> ok.
check_permissions(OpName, false, #document{definitions = Definitions}) ->
    Op = lists:filter(fun(D) -> is_req_operation(D, OpName) end, Definitions),
    case Op of
        [#op{schema = Schema, selection_set = Set} = Op1] ->
            case is_object_protected(Schema, Set, Definitions) of
                true ->
                    % Seems that the introspection fields belong to the query object.
                    % When an object is protected we need to ensure that the request
                    % query contains only introspection fields to execute it without
                    % authorization. Otherwise, a user couldn't access documentation
                    % without logging in.
                    case is_introspection_op(Op1) of
                        true ->
                            ok;
                        false ->
                            graphql_err:abort([], authorize, {no_permissions, op_name(OpName)})
                    end;
                false ->
                    ok
            end;
        _ ->
            ok
    end;
check_permissions(_, true, _) ->
    ok.

% Internal

op_name(undefined) ->
    <<"ROOT">>;
op_name(Name) ->
    Name.

is_req_operation(#op{id = 'ROOT'}, undefined) ->
    true;
is_req_operation(#op{id = {name, _, Name}}, Name) ->
    true;
is_req_operation(_, _) ->
    false.

is_protected_directive(#directive{id = {name, _, <<"protected">>}}) ->
    true;
is_protected_directive(_) ->
    false.

is_introspection_op(#op{selection_set = Set}) ->
    lists:all(fun is_introspection_field/1, Set).

is_introspection_field(#field{id = {name, _, <<"__schema">>}}) ->
    true;
is_introspection_field(#field{id = {name, _, <<"__type">>}}) ->
    true;
is_introspection_field(_) ->
    false.

is_object_protected(_, [], _) ->
    false;
is_object_protected(#schema_field{ty = Ty}, Set, Definitions) ->
    is_object_protected(Ty, Set, Definitions);
is_object_protected({non_null, Obj}, Set, Definitions) ->
    is_object_protected(Obj, Set, Definitions);
is_object_protected({list, Obj}, Set, Definitions) ->
    is_object_protected(Obj, Set, Definitions);
is_object_protected(Object, Set, Definitions) ->
    case is_object_protected(Object) of
        false ->
            lists:any(fun(S) -> is_field_protected(Object, S, Definitions) end, Set);
        true ->
            true
    end.

is_object_protected(#interface_type{directives = Directives}) ->
    lists:any(fun is_protected_directive/1, Directives);
is_object_protected(#object_type{directives = Directives}) ->
    lists:any(fun is_protected_directive/1, Directives);
is_object_protected(_) ->
    false.

is_field_protected(_, #frag_spread{id = {name, _, Name}}, Definitions) ->
    [#frag{schema = Schema, selection_set = Set}] =
        lists:filter(fun(#frag{id = {name, _, Name2}}) -> Name == Name2;
                        (_) -> false end, Definitions),
    is_object_protected(Schema, Set, Definitions);
is_field_protected(_, #frag{schema = Object, selection_set = Set}, Definitions) ->
    is_object_protected(Object, Set, Definitions);
is_field_protected(Parent,
                   #field{id = {name, _, Name}, schema = Object, selection_set = Set},
                   Definitions) ->
    {ok, #schema_field{directives = Directives}} = maps:find(Name, fields(Parent)),
    case lists:any(fun is_protected_directive/1, Directives) of
        false ->
            is_object_protected(Object, Set, Definitions);
        true ->
            true
    end.

fields(#object_type{fields = Fields}) -> Fields;
fields(#interface_type{fields = Fields}) -> Fields.
