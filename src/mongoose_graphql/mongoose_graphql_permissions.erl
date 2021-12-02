%% @doc Module that checks if a requested query can be executed with provided
%% permissions.
%%
%% GraphQL has directives that allow attaching additional information to schema,
%% objects, fields, and more. The custom directive `@protected' is created to mark
%% which objects or fields could be accessed only by authorized request. This module
%% analyzes the AST and tries to find if there is at least one protected resource.
%%
%% If unauthorized request want to execute a query that contains protected resources,
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

%% @doc Checks if query can be executed by unauthorized request. If not, throws
%% an error. When request is authorized just skip.
%% @end
-spec check_permissions(binary(), auth_status(), #document{}) -> ok.
check_permissions(OpName, false, #document{definitions = Definitions}) ->
    % Currently permissions are checked only for root Query/Mutation objects.
    % TODO Check permissions for fields.
    % TODO Check permissions of child objects and their fields (check deeper).
    Op = lists:filter(fun(D) -> is_req_operation(D, OpName) end, Definitions),
    case Op of
        [#op{schema = Schema} = Op1] ->
            case is_object_protected(Schema) of
                true ->
                    % Seems that the introspection fields belong to the query object.
                    % When an object is protected we need to ensure that the request
                    % query contains only introspection fields to execute it without
                    % authorization. Otherwise, a user couldn't access documentation
                    % without login in.
                    case is_introspection_op(Op1) of
                        true ->
                            ok;
                        false ->
                            throw({error, no_permissions})
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

is_req_operation(#op{id = 'ROOT'}, undefined) ->
    true;
is_req_operation(#op{id = {name, _, Name}}, Name) ->
    true;
is_req_operation(_, _) ->
    false.

is_object_protected(#object_type{directives = Directives}) ->
    lists:any(fun is_protected_directive/1, Directives);
is_object_protected(_) ->
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
