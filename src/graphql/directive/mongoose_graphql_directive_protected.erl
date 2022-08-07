%% @doc The custom directive `@protected' is created to mark which objects or fields
%% could be accessed only by an authorized request.
%% This module analyzes the AST and tries to find if there is at least one protected
%% resource. The `@protected' directive can be attached to <b>field definitions</b>
%% to <b>objects</b>, or to <b>interfaces</b>.
%%
%% Interfaces and objects permissions are checked independently. This means that when
%% an interface is protected or has protected fields, then all implementing objects
%% should be protected or have the same fields protected. <strong>This demands to mark all
%% protected resources at every occurrence with the directive</strong>. Otherwise, permissions
%% will be different for interface and implementing objects.
%%
%% If an unauthorized request wants to execute a query that contains protected resources,
%% an error is thrown.

-module(mongoose_graphql_directive_protected).

-behaviour(mongoose_graphql_directive).

-export([handle_directive/3, handle_object_directive/3]).

-include_lib("graphql/src/graphql_schema.hrl").
-include_lib("graphql/include/graphql.hrl").
-include_lib("jid/include/jid.hrl").

-import(mongoose_graphql_directive_helper, [name/1, op_name/1, get_arg/2]).

-type error_type() :: global | domain.

%% @doc Checks if command can be executed by unauthorized request or authorized as one
%% of the roles (USER, ADMIN, DOMAIN_ADMIN). If not, throw an error.
%%
%% The USER and ADMIN can execute each query because they are on separated GraphQL
%% instances that serves different queries.
%%
%% The DOMAIN_ADMIN use the same GraphQL instance as ADMIN, but have permissions
%% only to administrate own domain.
handle_directive(#directive{id = <<"protected">>},
                 _Field,
                 #{authorized := false, operation_name := OpName}) ->
    OpName2 = op_name(OpName),
    graphql_err:abort([OpName2], authorize, {no_permissions, OpName2});
handle_directive(#directive{id = <<"protected">>} = Dir,
                 #schema_field{} = Field,
                 #{field_args := FieldArgs,
                   operation_name := OpName,
                   authorized_as := domain_admin,
                   path := Path,
                   admin := #jid{lserver = Domain}}) ->
    #{type := {enum, Type}, args := PArgs} = protected_dir_args_to_map(Dir),
    Ctx = #{domain => Domain,
            path => Path,
            operation_name => OpName},
    check_field_args(Type, Ctx, PArgs, FieldArgs),
    Field;
handle_directive(#directive{id = <<"protected">>}, Field, #{authorized := true}) ->
    Field.

%% @doc Checks if category can be executed by unauthorized request.
handle_object_directive(#directive{id = <<"protected">>},
                        _Object,
                        #{authorized := false, operation_name := OpName}) ->
    OpName2 = op_name(OpName),
    graphql_err:abort([OpName2], authorize, {no_permissions, OpName2});
handle_object_directive(#directive{id = <<"protected">>},
                        Object,
                        #{authorized := true} = Ctx) ->
    {Object, Ctx}.

%% Internal

-spec protected_dir_args_to_map(graphql:directive()) -> map().
protected_dir_args_to_map(#directive{args = Args}) ->
    Default = #{type => {enum, <<"DEFAULT">>}, args => []},
    ArgsMap = maps:from_list([{binary_to_atom(name(N)), V} || {N, V} <- Args]),
    maps:merge(Default, ArgsMap).

-spec check_field_args(binary(), map(), [binary()], map()) -> ok.
check_field_args(<<"DOMAIN">>, #{domain := Domain} = Ctx, ProtectedArgs, Args) ->
    case lists:filter(fun(N) -> not arg_eq(get_arg(N, Args), Domain) end, ProtectedArgs) of
        [] ->
            ok;
        InvalidArgs ->
            raise_authorize_error(Ctx, domain, InvalidArgs)
    end;
check_field_args(<<"GLOBAL">>, Ctx, _, _Args) ->
    raise_authorize_error(Ctx, global, undefined);
check_field_args(<<"DEFAULT">>, _Ctx, _ProtectedArgs, _Args) ->
    ok.

-spec raise_authorize_error(map(), error_type(), [binary()]) -> no_return().
raise_authorize_error(Ctx, Type, InvalidArgs) ->
    #{path := Path, operation_name := OpName} = Ctx,
    Error = {no_permissions, op_name(OpName), #{type => Type, invalid_args => InvalidArgs}},
    graphql_err:abort(Path, authorize, Error).

-spec arg_eq(ToMatchDomain, Domain) -> boolean()
    when Domain :: jid:lserver(),
         ToMatchDomain :: [jid:jid() | jid:lserver()] | jid:jid() | jid:lserver().
arg_eq(Args, Domain) when is_list(Args) ->
    lists:all(fun(Arg) -> arg_eq(Arg, Domain) end, Args);
arg_eq(Domain, Domain) ->
    true;
arg_eq(Subdomain, Domain) when is_binary(Subdomain), is_binary(Domain) ->
    check_subdomain(Subdomain, Domain);
arg_eq(#jid{lserver = Domain1}, Domain2) ->
    arg_eq(Domain1, Domain2);
arg_eq(_, _) ->
    false.

-spec check_subdomain(jid:lserver(), jid:lserver()) -> boolean().
check_subdomain(Subdomain, Domain) ->
    case mongoose_domain_api:get_subdomain_info(Subdomain) of
        {ok, #{parent_domain := ParentDomain}} ->
            ParentDomain =:= Domain;
        {error, not_found} ->
            false
    end.
