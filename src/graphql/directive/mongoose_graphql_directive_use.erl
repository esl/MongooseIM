%% @doc The custom directive `@use' specifies which modules or services have to be loaded
%% to execute the command. We can annotate both objects and fields. The args from object
%% annotation are aggregated and checked for each annotated object's field. Thus annotating
%% only a category is not enough because, on the object level, we do not know the host type
%% needed to check loaded modules.
%%
%% In below example <i>command1</i> will be checked for loaded modules, but <i>command2</i>
%% will not be because it is not annotated. The admin endpoint does not have a host type in context,
%% so we need to specify the `arg'.
%% ```
%% type Category @use(modules: ["module_a"]){
%%     command1(domain: String!): String @use(arg: "domain")
%%     command2: String
%%}
%%'''
%%
%% The user's endpoint context contains the authenticated user, so the host type is there,
%% and we do not need to specify the `arg'.
%% ```
%% type Category @use(modules: ["module_a"]){
%%     command1: String @use
%%     command2: String
%%}
%%'''

-module(mongoose_graphql_directive_use).

-behaviour(mongoose_graphql_directive).

-export([handle_directive/3, handle_object_directive/3]).

-include_lib("graphql/src/graphql_schema.hrl").
-include_lib("graphql/include/graphql.hrl").
-include_lib("jid/include/jid.hrl").

-include("mongoose.hrl").

-import(mongoose_graphql_directive_helper, [name/1, get_arg/2]).

-type host_type() :: mongooseim:host_type().
-type ctx() :: mongoose_graphql_directive:ctx().
-type use_ctx() ::
    #{modules := [binary()],
      services := [binary()],
      internal_databases := [binary()],
      arg => binary(),
      atom => term()}.

%% @doc Check the collected modules and services and swap the field resolver if any of them
%% is not loaded. The new field resolver returns the error that some modules or services
%% are not loaded.
handle_directive(#directive{id = <<"use">>, args = Args}, #schema_field{} = Field, Ctx) ->
    #{modules := Modules, services := Services, internal_databases := DB} =
        UseCtx = aggregate_use_ctx(Args, Ctx),
    UnloadedServices = filter_unloaded_services(Services),
    UnloadedModules = filter_unloaded_modules(UseCtx, Ctx, Modules),
    UnloadedDBs = filter_unloaded_db(DB),
    case {UnloadedModules, UnloadedServices, UnloadedDBs} of
        {[], [], []} ->
            Field;
        {_, _, _} ->
            Fun = resolve_not_loaded_fun(UnloadedModules, UnloadedServices, UnloadedDBs),
            Field#schema_field{resolve = Fun}
    end.

%% @doc Collect the used modules and services to be checked for each field separately.
%% It cannot be checked here because the object directives have no access to the domain sometimes.
handle_object_directive(#directive{id = <<"use">>, args = Args}, Object, Ctx) ->
    {Object, Ctx#{use_dir => aggregate_use_ctx(Args, Ctx)}}.

%% Internal

-spec get_arg_value(use_ctx(), ctx()) -> jid:jid() | jid:lserver() | mongooseim:host_type().
get_arg_value(#{arg := DomainArg}, #{field_args := FieldArgs}) ->
    get_arg(DomainArg, FieldArgs);
get_arg_value(_UseCtx, #{user := #jid{lserver = Domain}}) ->
    Domain;
get_arg_value(_UseCtx, #{admin := #jid{lserver = Domain}}) ->
    Domain.

-spec aggregate_use_ctx(list(), ctx()) -> use_ctx().
aggregate_use_ctx(Args, #{use_dir := #{modules := Modules0, services := Services0,
                                       internal_databases := DB0}}) ->
    #{modules := Modules, services := Services, internal_databases := DB} =
        UseCtx = prepare_use_dir_args(Args),
    UpdatedModules = Modules0 ++ Modules,
    UpdatedServices = Services0 ++ Services,
    UpdatedDB = DB0 ++ DB,
    UseCtx#{modules => UpdatedModules, services => UpdatedServices,
            internal_databases => UpdatedDB};
aggregate_use_ctx(Args, _Ctx) ->
    prepare_use_dir_args(Args).

-spec prepare_use_dir_args([{graphql:name(), term()}]) -> use_ctx().
prepare_use_dir_args(Args) ->
    Default = #{modules => [], services => [], internal_databases => []},
    RdyArgs = maps:from_list([{binary_to_existing_atom(name(N)), V} || {N, V} <- Args]),
    maps:merge(Default, RdyArgs).

-spec host_type_from_arg(jid:jid() | jid:lserver() | mongooseim:host_type()) ->
                            {ok, mongooseim:host_type()} | {error, not_found}.
host_type_from_arg(#jid{lserver = Domain}) ->
    host_type_from_arg(Domain);
host_type_from_arg(ArgValue) ->
    case mongoose_domain_api:get_host_type(ArgValue) of
        {ok, HostType} ->
            {ok, HostType};
        {error, not_found} ->
            case lists:member(ArgValue, ?ALL_HOST_TYPES) of
                true ->
                    {ok, ArgValue};
                false ->
                    {error, not_found}
            end
    end.

-spec filter_unloaded_modules(use_ctx(), ctx(), [binary()]) -> [binary()].
filter_unloaded_modules(_UseCtx, _Ctx, []) ->
    [];
filter_unloaded_modules(UseCtx, Ctx, Modules) ->
    ArgValue = get_arg_value(UseCtx, Ctx),
    % Assume that loaded modules can be checked only when host type can be obtained
    case host_type_from_arg(ArgValue) of
        {ok, HostType} ->
            filter_unloaded_modules(HostType, Modules);
        {error, not_found} ->
            []
    end.

-spec filter_unloaded_modules(host_type(), [binary()]) -> [binary()].
filter_unloaded_modules(HostType, Modules) ->
    lists:filter(fun(M) -> not gen_mod:is_loaded(HostType, binary_to_existing_atom(M)) end,
                 Modules).

-spec filter_unloaded_services([binary()]) -> [binary()].
filter_unloaded_services(Services) ->
    lists:filter(fun(S) -> not mongoose_service:is_loaded(binary_to_existing_atom(S)) end,
                 Services).

-spec filter_unloaded_db([binary()]) -> [binary()].
filter_unloaded_db(DBs) ->
    lists:filter(fun(DB) ->
                    mongoose_config:get_opt([internal_databases,
                                             binary_to_existing_atom(DB)], undefined) == undefined
                 end, DBs).

-spec resolve_not_loaded_fun([binary()], [binary()], [binary()]) -> resolver().
resolve_not_loaded_fun(Modules, Services, []) ->
    Msg = <<"Some of required modules or services are not loaded">>,
    Extra = #{not_loaded_modules => Modules, not_loaded_services => Services},
    fun(_, _, _, _) -> mongoose_graphql_helper:make_error(deps_not_loaded, Msg, Extra) end;
resolve_not_loaded_fun(_Modules, _Services, Databases) ->
    Msg = <<"The required internal databases are not configured">>,
    Extra = #{not_loaded_databases => Databases},
    fun(_, _, _, _) -> mongoose_graphql_helper:make_error(deps_not_loaded, Msg, Extra) end.
