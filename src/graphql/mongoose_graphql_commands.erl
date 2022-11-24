%% @doc Management and execution of administration commands with GraphQL API

-module(mongoose_graphql_commands).

%% API
-export([start/0, stop/0, process/1]).

%% Internal API
-export([wrap_type/2]).

%% Only for tests
-export([build_specs/1, get_specs/0]).

-ignore_xref([build_specs/1, get_specs/0]).

% Needed to get the 'agent' vCard Fields inside a vCard
-define(MAX_TYPE_RECURSION_DEPTH, 2).

% Needed to handle e.g. [String!]!, which has 3 wrapper types: NON_NULL, LIST, NON_NULL
-define(MAX_INTROSPECTION_DEPTH, 3).

-type context() :: #{args := [string()],
                     category => category(),
                     commands => command_map(),
                     command => command(),
                     args_spec => [arg_spec()],
                     doc => doc(),
                     vars => json_map(),
                     reason => atom() | tuple(),
                     result => result(),
                     status => executed | error | usage}.
-type result() :: {ok, #{atom() => graphql:json()}} | {error, any()}.
-type specs() :: #{category() => category_spec()}.
-type category() :: binary().
-type category_spec() :: #{desc := binary(), commands := command_map()}.
-type command_map() :: #{command() => command_spec()}.
-type command() :: binary().
-type command_spec() :: #{desc := binary(),
                          op_type := op_type(),
                          args := [arg_spec()],
                          fields := [field_spec()],
                          doc := doc()}.
-type arg_spec() :: #{name := binary(), type := binary(), kind := binary(), wrap := [list | required]}.
-type field_spec() :: #{name | on := binary(), fields => [field_spec()]}.
-type op_type() :: binary().
-type doc() :: binary().
-type ep() :: graphql:endpoint_context().
-type json_map() :: #{binary() => graphql:json()}.

-export_type([category/0, command/0, command_map/0, arg_spec/0, context/0]).

%% API

-spec start() -> ok.
start() ->
    Specs = build_specs(admin),
    persistent_term:put(?MODULE, Specs).

-spec stop() -> ok.
stop() ->
    persistent_term:erase(?MODULE),
    ok.

%% The returned context has 'status' with the following values:
%%   - 'executed' means that a GraphQL command was called, and 'result' contains the returned value
%%   - 'error' means that the arguments were incorrect, and 'reason' contains more information
%%   - 'usage' means that help needs to be displayed
-spec process([string()]) -> context().
process(Args) ->
    lists:foldl(fun(_, #{status := _} = Ctx) -> Ctx;
                   (StepF, Ctx) -> StepF(Ctx)
                end, #{args => Args}, steps()).

%% Internal API

-spec build_specs(atom()) -> specs().
build_specs(EpName) ->
    Ep = mongoose_graphql:get_endpoint(EpName),
    CatSpecs = get_category_specs(Ep),
    lists:foldl(fun({Category, CategorySpec}, Acc) ->
                        insert_category(Category, CategorySpec, Acc)
                end, #{}, CatSpecs).

-spec get_specs() -> specs().
get_specs() ->
    persistent_term:get(?MODULE).

%% Internals

steps() ->
    [fun find_category/1, fun find_command/1, fun parse_args/1, fun check_args/1, fun execute/1].

-spec find_category(context()) -> context().
find_category(CtxIn = #{args := [CategoryStr | Args]}) ->
    Category = list_to_binary(CategoryStr),
    Ctx = CtxIn#{category => Category, args => Args},
    case get_specs() of
        #{Category := #{commands := Commands}} ->
            Ctx#{commands => Commands};
        #{} ->
            Ctx#{status => error, reason => unknown_category}
    end;
find_category(Ctx = #{args := []}) ->
    Ctx#{status => error, reason => no_args}.

-spec find_command(context()) -> context().
find_command(CtxIn = #{args := [CommandStr | Args]}) ->
    Command = list_to_binary(CommandStr),
    Ctx = #{commands := Commands} = CtxIn#{command => Command, args => Args},
    case Commands of
        #{Command := CommandSpec} ->
            #{doc := Doc, args := ArgSpec} = CommandSpec,
            Ctx#{doc => Doc, args_spec => ArgSpec};
        #{} ->
            Ctx#{status => error, reason => unknown_command}
    end;
find_command(Ctx) ->
    Ctx#{status => usage}.

-spec parse_args(context()) -> context().
parse_args(Ctx = #{args := ["--help"]}) ->
    Ctx#{status => usage};
parse_args(Ctx) ->
    parse_args_loop(Ctx#{vars => #{}}).

parse_args_loop(Ctx = #{vars := Vars,
                        args_spec := ArgsSpec,
                        args := ["--" ++ ArgNameStr, ArgValueStr | Rest]}) ->
    ArgName = list_to_binary(ArgNameStr),
    case lists:filter(fun(#{name := Name}) -> Name =:= ArgName end, ArgsSpec) of
        [] ->
            Ctx#{status => error, reason => {unknown_arg, ArgName}};
        [ArgSpec] ->
            ArgValue = list_to_binary(ArgValueStr),
            try parse_arg(ArgValue, ArgSpec) of
                ParsedValue ->
                    NewVars = Vars#{ArgName => ParsedValue},
                    parse_args_loop(Ctx#{vars := NewVars, args := Rest})
            catch _:_ ->
                    Ctx#{status => error, reason => {invalid_arg_value, ArgName, ArgValue}}
            end
    end;
parse_args_loop(Ctx = #{args := []}) ->
    Ctx;
parse_args_loop(Ctx) ->
    Ctx#{status => error, reason => invalid_args}.

-spec parse_arg(binary(), arg_spec()) -> jiffy:json_value().
parse_arg(Value, ArgSpec = #{type := Type}) ->
    case is_json_arg(ArgSpec) of
        true ->
            jiffy:decode(Value, [return_maps]);
        false ->
            convert_input_type(Type, Value)
    end.

%% Used input types that are not parsed from binaries should be handled here
convert_input_type(Type, Value) when Type =:= <<"Int">>;
                                     Type =:= <<"PosInt">>;
                                     Type =:= <<"NonNegInt">> -> binary_to_integer(Value);
convert_input_type(_, Value) -> Value.

%% Complex argument values should be provided in JSON
-spec is_json_arg(arg_spec()) -> boolean().
is_json_arg(#{kind := <<"INPUT_OBJECT">>}) -> true;
is_json_arg(#{kind := Kind, wrap := Wrap}) when Kind =:= <<"SCALAR">>;
                                                Kind =:= <<"ENUM">> ->
    lists:member(list, Wrap).

-spec check_args(context()) -> context().
check_args(Ctx = #{args_spec := ArgsSpec, vars := Vars}) ->
    MissingArgs = [Name || #{name := Name, wrap := [required|_]} <- ArgsSpec,
                           not maps:is_key(Name, Vars)],
    case MissingArgs of
        [] -> Ctx;
        _ -> Ctx#{status => error, reason => {missing_args, MissingArgs}}
    end.

-spec execute(context()) -> context().
execute(#{doc := Doc, vars := Vars} = Ctx) ->
    Ctx#{status => executed, result => execute(mongoose_graphql:get_endpoint(admin), Doc, Vars)}.

-spec get_category_specs(ep()) -> [{category(), category_spec()}].
get_category_specs(Ep) ->
    lists:flatmap(fun(OpType) -> get_category_specs(Ep, OpType) end, op_types()).

get_category_specs(Ep, OpType) ->
    OpTypeName = <<OpType/binary, "Type">>,
    Doc = iolist_to_binary(["{ __schema { ", OpTypeName, " ", category_spec_query(), " } }"]),
    {ok, #{data := #{<<"__schema">> := Schema}}} =  mongoose_graphql:execute(Ep, undefined, Doc),
    #{OpTypeName := #{<<"fields">> := Categories}} = Schema,
    get_category_specs(Ep, OpType, Categories).

op_types() ->
    [<<"query">>, <<"mutation">>, <<"subscription">>].

-spec get_category_specs(ep(), op_type(), [json_map()]) -> [{category(), category_spec()}].
get_category_specs(Ep, OpType, Categories) ->
    [get_category_spec(Ep, OpType, Category) || Category <- Categories, is_category(Category)].

is_category(#{<<"name">> := <<"checkAuth">>}) ->
    false;
is_category(#{}) ->
    true.

-spec get_category_spec(ep(), op_type(), json_map()) -> {category(), category_spec()}.
get_category_spec(Ep, OpType, #{<<"name">> := Category, <<"description">> := Desc,
                                <<"type">> := #{<<"name">> := CategoryType}}) ->
    Doc = iolist_to_binary(
            ["query ($type: String!) { __type(name: $type) "
             "{name fields {name description args {name type ", arg_type_query(), "} type ",
             field_type_query(), "}}}"]),
    Vars = #{<<"type">> => CategoryType},
    {ok, #{data := #{<<"__type">> := #{<<"fields">> := Commands}}}} = execute(Ep, Doc, Vars),
    CommandSpecs = [get_command_spec(Ep, Category, OpType, Command) || Command <- Commands],
    {Category, #{desc => Desc, commands => maps:from_list(CommandSpecs)}}.

-spec get_command_spec(ep(), category(), op_type(), json_map()) -> {command(), command_spec()}.
get_command_spec(Ep, Category, OpType,
                 #{<<"name">> := Name, <<"args">> := Args, <<"type">> := TypeMap} = Map) ->
    Spec = #{op_type => OpType, args => get_args(Args), fields => get_fields(Ep, TypeMap, [])},
    Doc = prepare_doc(Category, Name, Spec),
    {Name, add_description(Spec#{doc => Doc}, Map)}.

add_description(Spec, #{<<"description">> := Desc}) ->
    Spec#{desc => Desc};
add_description(Spec, #{}) ->
    Spec.

-spec get_args([json_map()]) -> [arg_spec()].
get_args(Args) ->
    lists:map(fun get_arg_info/1, Args).

-spec get_arg_info(json_map()) -> arg_spec().
get_arg_info(#{<<"name">> := ArgName, <<"type">> := Arg}) ->
    (get_arg_type(Arg, []))#{name => ArgName}.

get_arg_type(#{<<"kind">> := <<"NON_NULL">>, <<"ofType">> := TypeMap}, Wrap) ->
    get_arg_type(TypeMap, [required | Wrap]);
get_arg_type(#{<<"kind">> := <<"LIST">>, <<"ofType">> := TypeMap}, Wrap) ->
    get_arg_type(TypeMap, [list | Wrap]);
get_arg_type(#{<<"name">> := Type, <<"kind">> := Kind}, Wrap) when Kind =:= <<"SCALAR">>;
                                                                   Kind =:= <<"ENUM">>;
                                                                   Kind =:= <<"INPUT_OBJECT">> ->
    #{type => Type, kind => Kind, wrap => lists:reverse(Wrap)}.

-spec get_fields(ep(), json_map(), [binary()]) -> [field_spec()].
get_fields(_Ep, #{<<"kind">> := Kind}, _Path)
  when Kind =:= <<"SCALAR">>;
       Kind =:= <<"ENUM">> -> [];
get_fields(Ep, #{<<"kind">> := <<"UNION">>, <<"possibleTypes">> := TypeMaps}, Path) ->
    [get_union_type(Ep, TypeMap, Path) || TypeMap <- TypeMaps];
get_fields(Ep, #{<<"kind">> := Kind, <<"ofType">> := Type}, Path)
  when Kind =:= <<"NON_NULL">>;
       Kind =:= <<"LIST">> ->
    get_fields(Ep, Type, Path);
get_fields(Ep, #{<<"kind">> := <<"OBJECT">>, <<"name">> := Type}, Path) ->
    case length([T || T <- Path, T =:= Type]) >= ?MAX_TYPE_RECURSION_DEPTH of
        true ->
            [#{name => <<"__typename">>}]; % inform about the type of the trimmed subtree
        false ->
            Fields = get_object_fields(Ep, Type),
            [get_field(Ep, Field, [Type | Path]) || Field <- Fields]
    end.

-spec get_union_type(ep(), json_map(), [binary()]) -> field_spec().
get_union_type(Ep, #{<<"kind">> := <<"OBJECT">>, <<"name">> := Type} = M, Path) ->
    #{on => Type, fields => get_fields(Ep, M, Path)}.

-spec get_field(ep(), json_map(), [binary()]) -> field_spec().
get_field(Ep, #{<<"type">> := Type, <<"name">> := Name}, Path) ->
    case get_fields(Ep, Type, Path) of
        [] -> #{name => Name};
        Fields -> #{name => Name, fields => Fields}
    end.

-spec get_object_fields(ep(), binary()) -> [json_map()].
get_object_fields(Ep, ObjectType) ->
    Doc = iolist_to_binary(["query ($type: String!) { __type(name: $type) "
                            "{name fields {name type ", field_type_query(), "}}}"]),
    Vars = #{<<"type">> => ObjectType},
    {ok, #{data := #{<<"__type">> := #{<<"fields">> := Fields}}}} = execute(Ep, Doc, Vars),
    Fields.

-spec insert_category(category(), category_spec(), specs()) -> specs().
insert_category(Category, NewCatSpec = #{commands := NewCommands}, Specs) ->
    case Specs of
        #{Category := #{desc := OldDesc, commands := OldCommands}} ->
            case maps:with(maps:keys(OldCommands), NewCommands) of
                Common when Common =:= #{} ->
                    Specs#{Category := #{desc => OldDesc,
                                         commands => maps:merge(OldCommands, NewCommands)}};
                Common ->
                    error(#{what => overlapping_graphql_commands,
                            text => <<"GraphQL query and mutation names are not unique">>,
                            category => Category,
                            commands => maps:keys(Common)})
            end;
        _ ->
            Specs#{Category => NewCatSpec}
    end.

-spec prepare_doc(category(), command(), map()) -> doc().
prepare_doc(Category, Command, #{op_type := OpType, args := Args, fields := Fields}) ->
    iolist_to_binary([OpType, " ", declare_variables(Args), "{ ", Category, " { ", Command,
                      use_variables(Args), return_fields(Fields), " } }"]).

-spec declare_variables([arg_spec()]) -> iolist().
declare_variables([]) -> "";
declare_variables(Args) ->
    ["(", lists:join(", ", lists:map(fun declare_variable/1, Args)), ") "].

-spec declare_variable(arg_spec()) -> iolist().
declare_variable(#{name := Name, type := Type, wrap := Wrap}) ->
    ["$", Name, ": ", wrap_type(Wrap, Type)].

-spec wrap_type([required | list], binary()) -> iolist().
wrap_type([required | Wrap], Type) ->
    [wrap_type(Wrap, Type), $!];
wrap_type([list | Wrap], Type) ->
    [$[, wrap_type(Wrap, Type), $]];
wrap_type([], Type) ->
    [Type].

-spec use_variables([arg_spec()]) -> iolist().
use_variables([]) -> "";
use_variables(Args) ->
    ["(", lists:join(", ", lists:map(fun use_variable/1, Args)), ")"].

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
    Name;
return_field(#{on := Type, fields := Fields}) ->
    ["... on ", Type, return_fields(Fields)].

-spec execute(ep(), doc(), json_map()) -> result().
execute(Ep, Doc, Vars) ->
    mongoose_graphql:execute(Ep, #{document => Doc,
                                   operation_name => undefined,
                                   vars => Vars,
                                   authorized => true,
                                   ctx => #{method => cli}}).

field_type_query() ->
    nested_type_query("name kind possibleTypes {name kind}").

arg_type_query() ->
    nested_type_query("name kind").

nested_type_query(BasicQuery) ->
    lists:foldl(fun(_, QueryAcc) -> ["{ ", BasicQuery, " ofType ", QueryAcc, " }"] end,
                ["{ ", BasicQuery, " }"], lists:seq(1, ?MAX_INTROSPECTION_DEPTH)).

category_spec_query() ->
    "{name fields {name description type {name fields {name}}}}".
