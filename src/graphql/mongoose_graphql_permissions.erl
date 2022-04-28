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

-export([check_permissions/2]).

-include_lib("graphql/src/graphql_schema.hrl").
-include_lib("graphql/src/graphql_internal.hrl").
-include_lib("graphql/include/graphql.hrl").
-include_lib("jid/include/jid.hrl").

-type auth_status() :: boolean().
-type auth_role() :: user | admin | domain_admin.
-type params() :: map().
-type auth_ctx() :: #{operation_name := binary(),
                      params := params(),
                      authorized := auth_status(),
                      authorized_as => auth_role(),
                      user => jid:jid(),
                      admin => jid:jid(),
                      atom() => any()}.
-type fail_acc() :: #{path := [binary()],
                      type := atom(),
                      invalid := [binary()]}.
-type document() :: #document{}.
-type definitions() :: [any()].

%% @doc Checks if query can be executed by unauthorized request. If not, throws
%% an error. When request is authorized, just skip.
%% @end
-spec check_permissions(auth_ctx(), document()) -> ok.
check_permissions(#{operation_name := OpName, authorized := false},
                  #document{definitions = Definitions}) ->
    check_unauthorized_request_permissions(OpName, Definitions);
check_permissions(#{operation_name := OpName, authorized_as := domain_admin,
                    admin := #jid{lserver = Domain}, params := Params},
                  #document{definitions = Definitions}) ->
    check_domain_authorized_request_permissions(OpName, Domain, Params, Definitions);
check_permissions(#{authorized := true}, _) ->
    ok.

-spec check_unauthorized_request_permissions(binary(), definitions()) -> ok.
check_unauthorized_request_permissions(OpName, Definitions) ->
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
                            OpName2 = op_name(OpName),
                            graphql_err:abort([OpName2], authorize, {no_permissions, OpName2})
                    end;
                false ->
                    ok
            end;
        _ ->
            ok
    end.

-spec check_domain_authorized_request_permissions(binary(), binary(),
                                                  params(), definitions()) -> ok.
check_domain_authorized_request_permissions(OpName, Domain, Params, Definitions) ->
    Op = lists:filter(fun(D) -> is_req_operation(D, OpName) end, Definitions),
    case Op of
        [#op{selection_set = Set}] ->
            case check_fields(#{domain => Domain}, Params, Set) of
                [] ->
                    ok;
                [#{invalid := Args, path := Path, type := Type}| _] ->
                    OpName2 = op_name(OpName),
                    Error = {no_permissions, OpName2, Type, Args},
                    Path2 = lists:reverse([OpName2 | Path]),
                    graphql_err:abort(Path2, authorize, Error)
            end;
        _ ->
            ok
    end.

% Internal

-spec check_fields(map(), map(), [field()]) -> [fail_acc()].
check_fields(Ctx, Params, Fields) ->
    % Return empty list when permissions are valid.
    lists:flatten(lists:filtermap(fun(F) -> check_field(F, Ctx, Params) end, Fields)).

-spec check_field(field(), map(), params()) -> false | {true, fail_acc() | [fail_acc()]}.
check_field(#field{id = Name, selection_set = Set, args = Args,
                   schema = #schema_field{directives = Directives}}, Ctx, Params) ->
    Args2 = maps:from_list([prepare_arg(ArgName, Type, Params) || {ArgName, Type} <- Args]),
    Res =
        case lists:filter(fun is_protected_directive/1, Directives) of
            [#directive{} = Dir] ->
                #{type := Type, args := PArgs} = protected_dir_args_to_map(Dir),
                Acc = check_field_args(Type, Ctx, PArgs, Args2),
                acc_path(name(Name), Acc);
            [] ->
               false
        end,
    check_field_type(Res, Ctx, Params, Set, Name).

check_field_type(false, Ctx, Params, Set, Name) ->
    case check_fields(Ctx, Params, Set) of
        [] -> false;
        Invalid -> {true, [acc_path(Name, Acc) || Acc <- Invalid]}
    end;
check_field_type(Acc, _, _, _, _) ->
    Acc.

prepare_arg(ArgName, #{value := #var{id = Name}}, Vars) ->
    {ArgName, maps:get(name(Name), Vars)};
prepare_arg(ArgName, #{value := Val}, _) ->
    {ArgName, Val}.

check_field_args({enum, <<"DOMAIN">>}, #{domain := Domain}, ProtectedArgs, Params) ->
    Res = lists:filter(fun(Arg) -> not arg_eq(maps:get(Arg, Params), Domain) end, ProtectedArgs),
    acc(Res, domain);
check_field_args({enum, <<"DEFAULT">>}, _AuthCtx, _ProtectedArgs, _Args) ->
    acc([], default).

arg_eq(Domain, Domain) -> true;
arg_eq(#jid{lserver = Domain}, Domain) ->  true;
arg_eq(_, _) ->  false.

acc([], Type) when is_atom(Type) -> false;
acc(Invalid, Type) when is_atom(Type) -> {true, #{type => Type, path => [], invalid => Invalid}}.

acc_path(_Field, false) -> false;
acc_path(Field, {true, Acc}) -> {true, acc_path(Field, Acc)};
acc_path(Field, #{path := Path} = Acc) ->
    Acc#{path => [Field | Path]}.

protected_dir_args_to_map(#directive{args = Args}) ->
    Default = #{type => {enum, <<"DEFAULT">>}, args => []},
    ArgsMap = maps:from_list([{binary_to_atom(name(N)), V} || {N, V} <- Args]),
    maps:merge(Default, ArgsMap).

name({name, _, N}) -> N;
name(N) when is_binary(N) -> N.

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
