%% @doc This module provides main interface to graphql. It initializes schemas
%% and allows executing queries with permissions checks.
%% @end
-module(mongoose_graphql).

-include_lib("kernel/include/logger.hrl").

%API
-export([init/0,
         get_endpoint/1,
         create_endpoint/3,
         execute/2, prepare/2,
         execute/3,
         execute_cli/3]).

-ignore_xref([create_endpoint/3]).

-type request() :: #{document := binary(),
                     operation_name := binary() | undefined,
                     vars := map(),
                     authorized := boolean(),
                     ctx := map(),
                     ast => graphql:ast()}.
-type context() :: map().
-type object() :: term().
-type field() :: binary().
-type args() :: map().

-type result() :: {ok, term()} | {ok, term(), Aux :: term()} | {error, term()}.
-callback execute(Ctx :: context(), Obj :: object(), Field :: field(), Args :: args()) ->
            result().

-export_type([request/0, context/0, object/0, field/0, args/0]).

%% gen:start_ret() type is not exported from the gen module
-type gen_start_ret()  :: {ok, pid()} | ignore | {error, term()}.

-define(USER_EP_NAME, user_schema_ep).
-define(ADMIN_EP_NAME, admin_schema_ep).

%% @doc Create and initialize endpoints for user and admin.
-spec init() -> ok.
init() ->
    create_endpoint(?USER_EP_NAME, user_mapping_rules(), schema_global_patterns("user")),
    create_endpoint(?ADMIN_EP_NAME, admin_mapping_rules(), schema_global_patterns("admin")),
    ok.

%% @doc Get endpoint_context for passed endpoint name.
-spec get_endpoint(atom()) -> graphql:endpoint_context().
get_endpoint(admin) ->
    graphql_schema:get_endpoint_ctx(?ADMIN_EP_NAME);
get_endpoint(domain_admin) ->
    graphql_schema:get_endpoint_ctx(?ADMIN_EP_NAME);
get_endpoint(user) ->
    graphql_schema:get_endpoint_ctx(?USER_EP_NAME);
get_endpoint(Name) ->
    graphql_schema:get_endpoint_ctx(Name).

%% @doc Create a new endpoint and load schema.
-spec create_endpoint(atom(), map(), [file:filename_all()]) -> gen_start_ret().
create_endpoint(Name, Mapping, Patterns) ->
    Res = graphql_schema:start_link(Name),
    Ep = graphql_schema:get_endpoint_ctx(Name),
    {ok, SchemaData} = load_multiple_file_schema(Patterns),
    ok = graphql:load_schema(Ep, Mapping, SchemaData),
    ok = graphql:validate_schema(Ep),
    Res.

%% @doc Execute request on a given endpoint.
-spec execute(graphql:endpoint_context(), request()) ->
    {ok, map()} | {error, term()}.
execute(Ep, Req = #{ast := _}) ->
    execute_graphql(Ep, Req);
execute(Ep, Req) ->
    case prepare(Ep, Req) of
        {error, _} = Error ->
            Error;
        {ok, Req1} ->
            execute_graphql(Ep, Req1)
    end.

-spec prepare(graphql:endpoint_context(), request()) -> {ok, request()} | {error, term()}.
prepare(Ep, Req) ->
    try {ok, prepare_request(Ep, Req)}
    catch
        throw:{error, Err} ->
            {error, Err};
        Class:Reason:Stacktrace ->
            Err = #{what => graphql_internal_crash,
                    class => Class, reason => Reason,
                    stacktrace => Stacktrace},
            ?LOG_ERROR(Err),
            {error, internal_crash}
    end.

prepare_request(Ep, #{document := Doc,
                      operation_name := OpName,
                      authorized := AuthStatus,
                      vars := Vars,
                      ctx := Ctx} = Request) ->
    {ok, Ast} = graphql_parse(Doc),
    {ok, #{ast := Ast2,
           fun_env := FunEnv}} = graphql:type_check(Ep, Ast),
    ok = graphql:validate(Ast2),
    Vars2 = remove_null_args(Vars),
    Coerced = graphql:type_check_params(Ep, FunEnv, OpName, Vars2),
    Ctx2 = Ctx#{params => Coerced,
                operation_name => OpName,
                authorized => AuthStatus,
                error_module => mongoose_graphql_errors},
    Ast3 = mongoose_graphql_directive:process_directives(Ctx2, Ast2),
    mongoose_graphql_operations:verify_operations(Ctx2, Ast3),
    AllowedCategories = maps:get(allowed_categories, Ctx2, []),
    Ast4 = mongoose_graphql_check_categories:process_ast(Ast3, AllowedCategories),
    Request#{ast => Ast4, ctx := Ctx2}.

execute_graphql(Ep, #{ast := Ast, ctx := Ctx}) ->
    {ok, graphql:execute(Ep, Ctx, Ast)}.

%% @doc Execute selected operation on a given endpoint with authorization.
-spec execute(graphql:endpoint_context(), undefined | binary(), binary()) ->
    {ok, map()} | {error, term()}.
execute(Ep, OpName, Doc)  ->
    Req = #{document => Doc,
            operation_name => OpName,
            vars => #{},
            authorized => true,
            ctx => #{}},
    execute(Ep, Req).

-spec execute_cli(graphql:endpoint_context(), undefined | binary(), binary()) ->
    {ok, map()} | {error, term()}.
execute_cli(Ep, OpName, Doc)  ->
    Req = #{document => Doc,
            operation_name => OpName,
            vars => #{},
            authorized => true,
            ctx => #{method => cli}},
    execute(Ep, Req).

% Internal

-spec schema_global_patterns(file:name_all()) -> [file:filename_all()].
schema_global_patterns(SchemaDir) ->
    [schema_pattern(SchemaDir), schema_pattern("global")].

-spec schema_pattern(file:name_all()) -> file:filename_all().
schema_pattern(DirName) ->
    schema_pattern(DirName, "*.gql").

-spec schema_pattern(file:name_all(), file:name_all()) -> file:filename_all().
schema_pattern(DirName, Pattern) ->
    filename:join([code:priv_dir(mongooseim), "graphql/schemas", DirName, Pattern]).

graphql_parse(Doc) ->
    case graphql:parse(Doc) of
        {ok, _} = Ok ->
            Ok;
        {error, Err} ->
            graphql_err:abort([], parse, Err)
    end.

remove_null_args(Vars) ->
    maps:filter(fun(_Key, Value) -> Value /= null end, Vars).

admin_mapping_rules() ->
    #{objects => #{
        'AdminQuery' => mongoose_graphql_admin_query,
        'AdminMutation' => mongoose_graphql_admin_mutation,
        'AdminSubscription' => mongoose_graphql_admin_subscription,
        'AdminAuthInfo' => mongoose_graphql_admin_auth_info,
        'DomainAdminQuery' => mongoose_graphql_domain_admin_query,
        'GdprAdminQuery' => mongoose_graphql_gdpr_admin_query,
        'DomainAdminMutation' => mongoose_graphql_domain_admin_mutation,
        'InboxAdminMutation' => mongoose_graphql_inbox_admin_mutation,
        'SessionAdminMutation' => mongoose_graphql_session_admin_mutation,
        'SessionAdminQuery' => mongoose_graphql_session_admin_query,
        'StanzaAdminMutation' => mongoose_graphql_stanza_admin_mutation,
        'StatsAdminQuery' => mongoose_graphql_stats_admin_query,
        'TokenAdminMutation' => mongoose_graphql_token_admin_mutation,
        'GlobalStats' => mongoose_graphql_stats_global,
        'DomainStats' => mongoose_graphql_stats_domain,
        'StanzaAdminQuery' => mongoose_graphql_stanza_admin_query,
        'StanzaAdminSubscription' => mongoose_graphql_stanza_admin_subscription,
        'ServerAdminQuery' => mongoose_graphql_server_admin_query,
        'ServerAdminMutation' => mongoose_graphql_server_admin_mutation,
        'LastAdminMutation' => mongoose_graphql_last_admin_mutation,
        'LastAdminQuery' => mongoose_graphql_last_admin_query,
        'AccountAdminQuery' => mongoose_graphql_account_admin_query,
        'AccountAdminMutation' => mongoose_graphql_account_admin_mutation,
        'MUCAdminMutation' => mongoose_graphql_muc_admin_mutation,
        'MUCAdminQuery' => mongoose_graphql_muc_admin_query,
        'MUCLightAdminMutation' => mongoose_graphql_muc_light_admin_mutation,
        'MUCLightAdminQuery' => mongoose_graphql_muc_light_admin_query,
        'MnesiaAdminMutation' => mongoose_graphql_mnesia_admin_mutation,
        'MnesiaAdminQuery' => mongoose_graphql_mnesia_admin_query,
        'CETSAdminQuery' => mongoose_graphql_cets_admin_query,
        'OfflineAdminMutation' => mongoose_graphql_offline_admin_mutation,
        'PrivateAdminMutation' => mongoose_graphql_private_admin_mutation,
        'PrivateAdminQuery' => mongoose_graphql_private_admin_query,
        'RosterAdminQuery' => mongoose_graphql_roster_admin_query,
        'VcardAdminMutation' => mongoose_graphql_vcard_admin_mutation,
        'VcardAdminQuery' => mongoose_graphql_vcard_admin_query,
        'HttpUploadAdminMutation' => mongoose_graphql_http_upload_admin_mutation,
        'RosterAdminMutation' => mongoose_graphql_roster_admin_mutation,
        'Domain' => mongoose_graphql_domain,
        'MetricAdminQuery' => mongoose_graphql_metric_admin_query,
        default => mongoose_graphql_default},
      interfaces => #{default => mongoose_graphql_default},
      scalars => #{default => mongoose_graphql_scalar},
      enums => #{default => mongoose_graphql_enum},
      unions => #{default => mongoose_graphql_union}}.

user_mapping_rules() ->
    #{objects => #{
        'UserQuery' => mongoose_graphql_user_query,
        'UserMutation' => mongoose_graphql_user_mutation,
        'UserSubscription' => mongoose_graphql_user_subscription,
        'AccountUserQuery' => mongoose_graphql_account_user_query,
        'AccountUserMutation' => mongoose_graphql_account_user_mutation,
        'InboxUserMutation' => mongoose_graphql_inbox_user_mutation,
        'MUCUserMutation' => mongoose_graphql_muc_user_mutation,
        'MUCUserQuery' => mongoose_graphql_muc_user_query,
        'MUCLightUserMutation' => mongoose_graphql_muc_light_user_mutation,
        'MUCLightUserQuery' => mongoose_graphql_muc_light_user_query,
        'PrivateUserMutation' => mongoose_graphql_private_user_mutation,
        'PrivateUserQuery' => mongoose_graphql_private_user_query,
        'RosterUserQuery' => mongoose_graphql_roster_user_query,
        'RosterUserMutation' => mongoose_graphql_roster_user_mutation,
        'VcardUserMutation' => mongoose_graphql_vcard_user_mutation,
        'VcardUserQuery' => mongoose_graphql_vcard_user_query,
        'LastUserMutation' => mongoose_graphql_last_user_mutation,
        'LastUserQuery' => mongoose_graphql_last_user_query,
        'SessionUserQuery' => mongoose_graphql_session_user_query,
        'StanzaUserMutation' => mongoose_graphql_stanza_user_mutation,
        'TokenUserMutation' => mongoose_graphql_token_user_mutation,
        'StanzaUserQuery' => mongoose_graphql_stanza_user_query,
        'StanzaUserSubscription' => mongoose_graphql_stanza_user_subscription,
        'HttpUploadUserMutation' => mongoose_graphql_http_upload_user_mutation,
        'UserAuthInfo' => mongoose_graphql_user_auth_info,
        default => mongoose_graphql_default},
      interfaces => #{default => mongoose_graphql_default},
      scalars => #{default => mongoose_graphql_scalar},
      enums => #{default => mongoose_graphql_enum},
      unions => #{default => mongoose_graphql_union}}.

load_multiple_file_schema(Patterns) ->
    Paths = lists:flatmap(fun(P) -> filelib:wildcard(P) end, Patterns),
    try
        SchemaData = [read_schema_file(P) || P <- Paths],
        {ok, lists:flatten(SchemaData)}
    catch
        throw:{error, Reason, Path} ->
            ?LOG_ERROR(#{what => graphql_cannot_load_schema,
                         reason => Reason, path => Path}),
            {error, cannot_load}
    end.

read_schema_file(Path) ->
    case file:read_file(Path) of
         {ok, Data} ->
            binary_to_list(Data);
         {error, Reason} ->
            throw({error, Reason, Path})
    end.
