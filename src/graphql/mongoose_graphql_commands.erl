%% @doc Management and execution of administration commands with GraphQL API

-module(mongoose_graphql_commands).

%% API
-export([start/0, stop/0, process/1]).

%% Only for tests
-export([get_specs/0]).

-ignore_xref([get_specs/0]).

%% This level of nesting is needed for basic type introspection, e.g. see below for [String!]!
%%                         NON_NULL          LIST              NON_NULL          SCALAR
-define(TYPE_QUERY, "{name kind ofType {name kind ofType {name kind ofType {name kind}}}}").

-type context() :: #{args := [string()],
                     category => category(),
                     cat_spec => category_spec(),
                     command => command(),
                     doc => doc(),
                     vars => json_map(),
                     reason => atom()}.
-type result() :: {ok, #{atom() => graphql:json()}} | {error, any()}.
-type specs() :: #{category() => category_spec()}.
-type category() :: binary().
-type category_spec() :: #{command() => command_spec()}.
-type command() :: binary().
-type command_spec() :: #{op_type := op_type(),
                          args := [arg_spec()],
                          fields := [field_spec()],
                          doc := doc()}.
-type arg_spec() :: #{name := binary(), type := binary(), wrap := [list | required]}.
-type field_spec() :: #{name := binary(), fields => [field_spec()]}.
-type op_type() :: binary().
-type doc() :: binary().
-type ep() :: graphql:endpoint_context().
-type json_map() :: #{binary() => graphql:json()}.

%% API

-spec start() -> ok.
start() ->
    Ep = mongoose_graphql:get_endpoint(admin),
    CatSpecs = get_category_specs(Ep),
    CommandMap = lists:foldl(fun({Category, CategorySpec}, Acc) ->
                                     insert_category(Category, CategorySpec, Acc)
                             end, #{}, CatSpecs),
    persistent_term:put(?MODULE, CommandMap).

-spec stop() -> ok.
stop() ->
    persistent_term:erase(?MODULE),
    ok.

-spec process([string()]) -> result().
process(Args) ->
    lists:foldl(fun(_, {error, _} = Error) -> Error;
                   (StepF, Ctx) -> StepF(Ctx)
                end,
                #{args => Args},
                [fun find_category/1, fun find_command/1, fun parse_vars/1, fun execute/1]).

-spec get_specs() -> specs().
get_specs() ->
    persistent_term:get(?MODULE).

%% Internals

-spec find_category(context()) -> context() | {error, context()}.
find_category(CtxIn = #{args := [CategoryStr | Args]}) ->
    Category = list_to_binary(CategoryStr),
    Ctx = CtxIn#{category => Category, args => Args},
    case get_specs() of
        #{Category := CatSpec} ->
            Ctx#{cat_spec => CatSpec};
        #{} ->
            {error, Ctx#{reason => unknown_category}}
    end;
find_category(Ctx = #{args := []}) ->
    {error, Ctx#{reason => no_args}}.

-spec find_command(context()) -> context() | {error, context()}.
find_command(CtxIn = #{args := [CommandStr | Args]}) ->
    Command = list_to_binary(CommandStr),
    Ctx = #{cat_spec := CatSpec} = CtxIn#{command => Command, args => Args},
    case CatSpec of
        #{Command := CommandSpec} ->
            #{doc := Doc} = CommandSpec,
            Ctx#{doc => Doc};
        #{} ->
            {error, Ctx#{reason => unknown_command}}
    end;
find_command(Ctx) ->
    {error, Ctx#{reason => no_command}}.

-spec parse_vars(context()) -> context() | {error, context()}.
parse_vars(Ctx = #{args := [VarsStr]}) ->
    try jiffy:decode(VarsStr, [return_maps]) of
        Vars ->
            Ctx#{vars => Vars}
    catch _:_ ->
            {error, Ctx#{reason => invalid_vars}}
    end;
parse_vars(Ctx = #{args := []}) ->
    {error, Ctx#{reason => no_vars}};
parse_vars(Ctx = #{args := [_|_]}) ->
    {error, Ctx#{reason => too_many_args}}.

-spec execute(context()) -> result().
execute(#{doc := Doc, vars := Vars}) ->
    execute(mongoose_graphql:get_endpoint(admin), Doc, Vars).

%% Internals

-spec get_category_specs(ep()) -> [{category(), category_spec()}].
get_category_specs(Ep) ->
    {ok, #{data := #{<<"__schema">> := Schema}}} =
        mongoose_graphql:execute(
          Ep, undefined,
          <<"{ __schema { queryType {name fields {name type {name fields {name}}}} "
            "             mutationType {name fields {name type {name fields {name}}}} } }">>),
    #{<<"queryType">> := #{<<"fields">> := Queries},
      <<"mutationType">> := #{<<"fields">> := Mutations}} = Schema,
    get_category_specs(Ep, <<"query">>, Queries) ++ get_category_specs(Ep, <<"mutation">>, Mutations).

-spec get_category_specs(ep(), op_type(), [json_map()]) -> [{category(), category_spec()}].
get_category_specs(Ep, OpType, Categories) ->
    [get_category_spec(Ep, OpType, Category) || Category <- Categories].

-spec get_category_spec(ep(), op_type(), json_map()) -> {category(), category_spec()}.
get_category_spec(Ep, OpType, #{<<"name">> := Category,
                                <<"type">> := #{<<"name">> := CategoryType}}) ->
    Doc = <<"query ($type: String!) { __type(name: $type) "
            "{name fields {name args {name type ", ?TYPE_QUERY, "} type ", ?TYPE_QUERY, "}}}">>,
    Vars = #{<<"type">> => CategoryType},
    {ok, #{data := #{<<"__type">> := #{<<"fields">> := Commands}}}} = execute(Ep, Doc, Vars),
    CommandSpecs = [get_command_spec(Ep, Category, OpType, Command) || Command <- Commands],
    {Category, maps:from_list(CommandSpecs)}.

-spec get_command_spec(ep(), category(), op_type(), json_map()) -> {command(), command_spec()}.
get_command_spec(Ep, Category, OpType,
                 #{<<"name">> := Name, <<"args">> := Args, <<"type">> := Type}) ->
    Spec = #{op_type => OpType, args => get_args(Args), fields => get_fields(Ep, Type)},
    Doc = prepare_doc(Category, Name, Spec),
    {Name, Spec#{doc => Doc}}.

-spec get_args([json_map()]) -> [arg_spec()].
get_args(Args) ->
    lists:map(fun get_arg_info/1, Args).

-spec get_arg_info(json_map()) -> arg_spec().
get_arg_info(#{<<"name">> := ArgName, <<"type">> := Arg}) ->
    (get_arg_type(Arg, []))#{name => ArgName}.

get_arg_type(#{<<"kind">> := <<"NON_NULL">>, <<"ofType">> := Type}, Wrap) ->
    get_arg_type(Type, [required | Wrap]);
get_arg_type(#{<<"kind">> := <<"LIST">>, <<"ofType">> := Type}, Wrap) ->
    get_arg_type(Type, [list | Wrap]);
get_arg_type(#{<<"name">> := Type, <<"kind">> := Kind}, Wrap) when Kind =:= <<"SCALAR">>;
                                                                   Kind =:= <<"ENUM">>;
                                                                   Kind =:= <<"INPUT_OBJECT">> ->
    #{type => Type, wrap => lists:reverse(Wrap)}.

-spec get_fields(ep(), json_map()) -> [field_spec()].
get_fields(_Ep, #{<<"kind">> := Kind})
  when Kind =:= <<"SCALAR">>;
       Kind =:= <<"ENUM">>;
       Kind =:= <<"UNION">> -> []; %% TODO implement support for UNION
get_fields(Ep, #{<<"kind">> := Kind, <<"ofType">> := Type})
  when Kind =:= <<"NON_NULL">>;
       Kind =:= <<"LIST">> ->
    get_fields(Ep, Type);
get_fields(Ep, #{<<"name">> := Name, <<"kind">> := <<"OBJECT">>}) ->
    Fields = get_object_fields(Ep, Name),
    [get_field(Ep, Field) || Field <- Fields].

-spec get_field(ep(), json_map()) -> field_spec().
get_field(Ep, #{<<"type">> := Type, <<"name">> := Name}) ->
    case get_fields(Ep, Type) of
        [] -> #{name => Name};
        Fields -> #{name => Name, fields => Fields}
    end.

-spec get_object_fields(ep(), binary()) -> [json_map()].
get_object_fields(Ep, ObjectType) ->
    Doc = <<"query ($type: String!) { __type(name: $type) "
            "{name fields {name type ", ?TYPE_QUERY, "}}}">>,
    Vars = #{<<"type">> => ObjectType},
    {ok, #{data := #{<<"__type">> := #{<<"fields">> := Fields}}}} = execute(Ep, Doc, Vars),
    Fields.

-spec insert_category(category(), category_spec(), specs()) -> specs().
insert_category(Category, NewCatSpec, Specs) ->
    case Specs of
        #{Category := OldCatSpec} ->
            case maps:with(maps:keys(OldCatSpec), NewCatSpec) of
                Common when Common =:= #{} ->
                    Specs#{Category := maps:merge(OldCatSpec, NewCatSpec)};
                Common ->
                    error(#{what => overlapping_commands, commands => maps:keys(Common)})
            end;
        _ ->
            Specs#{Category => NewCatSpec}
    end.

-spec prepare_doc(category(), command(), map()) -> doc().
prepare_doc(Category, Command, #{op_type := OpType, args := Args, fields := Fields}) ->
    iolist_to_binary([OpType, " (", declare_variables(Args), ") { ", Category, " { ", Command,
                     "(", use_variables(Args), ")", return_fields(Fields), " } }"]).

-spec declare_variables([arg_spec()]) -> iolist().
declare_variables(Args) ->
    lists:join(", ", lists:map(fun declare_variable/1, Args)).

-spec declare_variable(arg_spec()) -> iolist().
declare_variable(#{name := Name, type := Type, wrap := Wrap}) ->
    ["$", Name, ": ", wrap_type(Wrap, Type)].

-spec wrap_type([required | list], binary()) -> iolist().
wrap_type([required | Wrap], Type) ->
    [wrap_type(Wrap, Type), $!];
wrap_type([list | Wrap], Type) ->
    [$[, wrap_type(Wrap, Type), $]];
wrap_type([], Type) ->
    Type.

-spec use_variables([arg_spec()]) -> iolist().
use_variables(Args) ->
    lists:join(", ", lists:map(fun use_variable/1, Args)).

-spec use_variable(arg_spec()) -> iolist().
use_variable(#{name := Name}) ->
    [Name, ": $", Name].

-spec return_fields([field_spec()]) -> iolist().
return_fields([]) -> "";
return_fields(Fields) ->
    [" { ", lists:join(" ", [return_field(F) || F <- Fields]), " }"].

-spec return_field(field_spec()) -> iodata().
return_field(#{name := Name, fields := Fields}) ->
    [Name, return_fields(Fields)];
return_field(#{name := Name}) ->
    Name.

-spec execute(ep(), doc(), json_map()) -> result().
execute(Ep, Doc, Vars) ->
    mongoose_graphql:execute(Ep, #{document => Doc,
                                   operation_name => undefined,
                                   vars => Vars,
                                   authorized => true,
                                   ctx => #{}}).
