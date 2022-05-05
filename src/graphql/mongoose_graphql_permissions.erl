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
-type no_access_info() :: #{path := [binary()],
                            type := atom(),
                            invalid := [binary()]}.
-type field_check_result() :: ok | no_access_info().
-type document() :: #document{}.
-type definitions() :: [any()].

%% @doc Checks if query can be executed by unauthorized request or authorized as one
%% of the roles (USER, ADMIN, DOMAIN_ADMIN). If not, throw an error.
%%
%% The USER and ADMIN can execute each query because they are on separated GraphQL
%% instances that serves different queries.
%%
%% The DOMAIN_ADMIN use the same GraphQL instance as ADMIN, but have permissions
%% only to administrate own domain.
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
                ok ->
                    ok;
                #{invalid := Args, path := Path, type := Type} ->
                    OpName2 = op_name(OpName),
                    Error = {no_permissions, OpName2, Type, Args},
                    Path2 = lists:reverse([OpName2 | Path]),
                    graphql_err:abort(Path2, authorize, Error)
            end;
        _ ->
            ok
    end.

% Internal

-spec check_fields(map(), map(), [any()]) -> field_check_result().
check_fields(Ctx, Params, Fields) ->
    Fun = fun(F, ok) -> check_field(F, Ctx, Params);
             (_, NoAccessInfo) -> NoAccessInfo
          end,
    lists:foldl(Fun, ok, Fields).

-spec check_field(field() | any(), map(), map()) -> field_check_result().
check_field(#field{id = Name, selection_set = Set, args = Args,
                   schema = #schema_field{directives = Directives}}, Ctx, Params) ->
    Args2 = maps:from_list([prepare_arg(ArgName, Type, Params) || {ArgName, Type} <- Args]),
    Res = check_field_args(Ctx, Args2, Directives),
    Res2 = check_field_type(Res, Ctx, Params, Set),
    add_path(Res2, name(Name));
check_field(_, _, _) -> ok.

-spec check_field_args(map(), map(), [graphql:directive()]) -> field_check_result().
check_field_args(Ctx, Args, Directives) ->
    case lists:filter(fun is_protected_directive/1, Directives) of
        [#directive{} = Dir] ->
            #{type := {enum, Type}, args := PArgs} = protected_dir_args_to_map(Dir),
            check_field_args(Type, Ctx, PArgs, Args);
        [] ->
            ok
    end.

-spec check_field_args(binary(), map(), [binary()], map()) -> field_check_result().
check_field_args(<<"DOMAIN">>, #{domain := Domain}, ProtectedArgs, Args) ->
    InvalidArgs =
        lists:filter(fun(N) -> not arg_eq(get_arg(N, Args), Domain) end, ProtectedArgs),
    make_result(InvalidArgs, domain);
check_field_args(<<"GLOBAL">>, #{domain := _}, _, _Args) ->
    make_result(no_args, global);
check_field_args(<<"DEFAULT">>, _Ctx, _ProtectedArgs, _Args) ->
    ok.

-spec check_field_type(field_check_result(), map(), map(), [any()]) -> field_check_result().
check_field_type(ok, Ctx, Params, Set) ->
    check_fields(Ctx, Params, Set);
check_field_type(NoAccessInfo, _, _, _) ->
    NoAccessInfo.

prepare_arg(ArgName, #{value := #var{id = Name}}, Vars) ->
    {ArgName, maps:get(name(Name), Vars)};
prepare_arg(ArgName, #{value := Val}, _) ->
    {ArgName, Val}.

get_arg(Name, Args) when is_binary(Name)->
    Path = binary:split(Name, <<".">>, [global]),
    get_arg(Path, Args);
get_arg([], Value) -> Value;
get_arg(_, null) -> null;
get_arg(Path, List) when is_list(List) ->
    [get_arg(Path, ArgsMap) || ArgsMap <- List];
get_arg([Name | Path], ArgsMap) ->
    get_arg(Path, maps:get(Name, ArgsMap, null)).

arg_eq(Args, Domain) when is_list(Args) ->
    lists:all(fun(Arg) -> arg_eq(Arg, Domain) end, Args);
arg_eq(Domain, Domain) ->
    true;
arg_eq(Subdomain, Domain) when is_binary(Subdomain), is_binary(Domain) ->
    nomatch =/= re:run(Subdomain, io_lib:format("\\.~s$", [Domain]));
arg_eq(#jid{lserver = Domain1}, Domain2) ->
    arg_eq(Domain1, Domain2);
arg_eq(null, _) ->
    % The arg is optional, and the value is not present, so we assume that
    % the domain admin has access.
    true;
arg_eq(_, _) ->
    false.

make_result([], _) ->
    ok;
make_result(no_args, Type) when is_atom(Type) ->
    #{type => Type, path => [], invalid => []};
make_result(InvalidArgs, Type) when is_atom(Type) ->
    #{type => Type, path => [], invalid => InvalidArgs}.

add_path(ok, _) -> ok;
add_path(#{path := Path} = Acc, FieldName) ->
    Acc#{path => [FieldName | Path]}.

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
