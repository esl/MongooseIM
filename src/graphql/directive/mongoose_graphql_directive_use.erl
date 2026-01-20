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
%%     command1(domain: String!): String @use(args: ["domain"])
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

-type ctx() :: mongoose_graphql_directive:ctx().
-type use_ctx() ::
    #{modules := [binary()],
      services := [binary()],
      internal_databases := [binary()],
      args => [binary()],
      atom => term()}.
-type dependency_type() :: internal_databases | modules | services.
-type dependency_name() :: binary().

%% @doc Check the collected modules and services and swap the field resolver if any of them
%% is not loaded. The new field resolver returns the error that some modules or services
%% are not loaded.
handle_directive(#directive{id = <<"use">>, args = Args}, #schema_field{} = Field, Ctx) ->
    #{modules := Modules, services := Services, internal_databases := DB} =
        UseCtx = aggregate_use_ctx(Args, Ctx),
        Items = [{modules, filter_unloaded_modules(UseCtx, Ctx, Modules)},
                 {services, filter_unloaded_services(Services)},
                 {internal_databases, filter_unloaded_db(DB)}],
    case lists:filter(fun({_, List}) -> List =/= [] end, Items) of
        [] ->
            Field;
        NotLoaded ->
            Fun = resolve_not_loaded_fun(NotLoaded),
            Field#schema_field{resolve = Fun}
    end.

%% @doc Collect the used modules and services to be checked for each field separately.
%% It cannot be checked here because the object directives have no access to the domain sometimes.
handle_object_directive(#directive{id = <<"use">>, args = Args}, Object, Ctx) ->
    {Object, Ctx#{use_dir => aggregate_use_ctx(Args, Ctx)}}.

%% Internal

-spec get_args(use_ctx(), ctx()) -> [jid:jid() | jid:lserver()].
get_args(#{args := Args}, #{field_args := FieldArgs}) ->
    lists:flatten([get_arg(Arg, FieldArgs) || Arg <- Args]);
get_args(_UseCtx, #{user := #jid{lserver = Domain}}) ->
    [Domain];
get_args(_UseCtx, #{admin := #jid{lserver = Domain}}) ->
    [Domain].

-spec aggregate_use_ctx(list(), ctx()) -> use_ctx().
aggregate_use_ctx(Args, #{use_dir := #{modules := Modules0, services := Services0,
                                       internal_databases := Databases0}}) ->
    #{modules := Modules, services := Services, internal_databases := Databases} =
        UseCtx = prepare_use_dir_args(Args),
    UpdatedModules = Modules0 ++ Modules,
    UpdatedServices = Services0 ++ Services,
    UpdatedDatabases = Databases0 ++ Databases,
    UseCtx#{modules => UpdatedModules, services => UpdatedServices,
            internal_databases => UpdatedDatabases};
aggregate_use_ctx(Args, _Ctx) ->
    prepare_use_dir_args(Args).

-spec prepare_use_dir_args([{graphql:name(), term()}]) -> use_ctx().
prepare_use_dir_args(Args) ->
    Default = #{modules => [], services => [], internal_databases => []},
    RdyArgs = maps:from_list([{binary_to_existing_atom(name(N)), V} || {N, V} <- Args]),
    maps:merge(Default, RdyArgs).

-spec host_type_from_arg(jid:jid() | jid:lserver() | mongooseim:host_type()) ->
                            {true, mongooseim:host_type()} | false.
host_type_from_arg(#jid{lserver = Domain}) ->
    host_type_from_arg(Domain);
host_type_from_arg(ArgValue) ->
    % Assume that loaded modules can be checked only when host type can be obtained
    case mongoose_domain_api:get_host_type(ArgValue) of
        {ok, HostType} ->
            {true, HostType};
        {error, not_found} ->
            case lists:member(ArgValue, ?ALL_HOST_TYPES) of
                true -> {true, ArgValue};
                false -> false
            end
    end.

-spec filter_unloaded_modules(use_ctx(), ctx(), [binary()]) -> [binary()].
filter_unloaded_modules(_UseCtx, _Ctx, []) ->
    [];
filter_unloaded_modules(UseCtx, Ctx, Modules) ->
    ArgValues = get_args(UseCtx, Ctx),
    HostTypes = lists:filtermap(fun host_type_from_arg/1, ArgValues),
    filter_unloaded_modules(HostTypes, Modules).

-spec filter_unloaded_modules([mongooseim:host_type()], [binary()]) -> [binary()].
filter_unloaded_modules(HostTypes, Modules) ->
    NotLoaded = lists:flatmap(fun(HT) ->
                                  Modules -- lists:map(fun atom_to_binary/1, gen_mod:loaded_modules(HT))
                              end,
                              HostTypes),
    lists:usort(NotLoaded).

-spec filter_unloaded_services([binary()]) -> [binary()].
filter_unloaded_services(Services) ->
    lists:filter(fun(S) -> not mongoose_service:is_loaded(binary_to_existing_atom(S)) end,
                 Services).

-spec filter_unloaded_db([binary()]) -> [binary()].
filter_unloaded_db(DBs) ->
    lists:filter(fun(DB) -> is_database_unloaded(DB) end, DBs).

-spec is_database_unloaded(binary()) -> boolean().
is_database_unloaded(DB) ->
    mongoose_config:lookup_opt([internal_databases,
                                binary_to_existing_atom(DB)]) == {error, not_found}.

-spec resolve_not_loaded_fun([{dependency_type(), [dependency_name()]}]) -> resolver().
resolve_not_loaded_fun(NotLoaded) ->
    Msg = not_loaded_message(NotLoaded),
    Extra = maps:from_list([{error_key(Type), Names} || {Type, Names} <- NotLoaded]),
    fun(_, _, _, _) -> mongoose_graphql_helper:make_error(deps_not_loaded, Msg, Extra) end.

-spec not_loaded_message([{dependency_type(), [dependency_name()]}]) -> binary().
not_loaded_message(NotLoaded) ->
    MsgPrefix = <<"Some of the required ">>,
    MsgList = string:join([dependency_type_to_string(Item) || {Item, _} <- NotLoaded], " and "),
    MsgBinaryList = list_to_binary(MsgList),
    <<MsgPrefix/binary, MsgBinaryList/binary, " are not loaded">>.

-spec dependency_type_to_string(dependency_type()) -> [string()].
dependency_type_to_string(Type) ->
    string:replace(atom_to_list(Type), "_", " ").

-spec error_key(dependency_type()) -> atom().
error_key(Type) ->
    list_to_atom("not_loaded_" ++ atom_to_list(Type)).
