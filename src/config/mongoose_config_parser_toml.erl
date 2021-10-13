%% @doc Config parsing and processing for the TOML format
-module(mongoose_config_parser_toml).

-behaviour(mongoose_config_parser).

-export([parse_file/1]).

-ifdef(TEST).
-export([parse/1,
         extract_errors/1]).
-endif.

-include("mongoose.hrl").
-include("mongoose_config_spec.hrl").
-include("ejabberd_config.hrl").

%% Used to create per-host config when the list of hosts is not known yet
-define(HOST_F(Expr), [fun(Host) -> Expr end]).

%% Input: TOML parsed by tomerl
-type toml_key() :: binary().
-type toml_value() :: tomerl:value().
-type toml_section() :: tomerl:section().

%% Output: list of config records, containing key-value pairs
-type option_value() :: atom() | binary() | string() | float(). % parsed leaf value
-type config_part() :: term(). % any part of a top-level option value, may contain config errors
-type top_level_config() :: #config{} | #local_config{}.
-type config_error() :: #{class := error, what := atom(), text := string(), any() => any()}.
-type override() :: {override, atom()}.
-type config() :: top_level_config() | config_error() | override().
-type config_list() :: [config() | fun((jid:server()) -> [config()])]. % see HOST_F

-type list_processor() :: fun((path(), [config_part()]) -> config_part())
                        | fun(([config_part()]) -> config_part()).

-type processor() :: fun((path(), config_part()) -> config_part())
                   | fun((config_part()) -> config_part()).

-type step() :: parse | validate | process | format.

%% Path from the currently processed config node to the root
%%   - toml_key(): key in a toml_section()
%%   - item: item in a list
%%   - {host, Host}: item in the list of hosts in host_config
-type path() :: [toml_key() | item | {host, jid:server()}].

-export_type([toml_key/0, option_value/0, config_part/0, list_processor/0, processor/0]).

-spec parse_file(FileName :: string()) -> mongoose_config_parser:state().
parse_file(FileName) ->
    case tomerl:read_file(FileName) of
        {ok, Content} ->
            process(Content);
        {error, Error} ->
            Text = tomerl:format_error(Error),
            error(config_error([#{what => toml_parsing_failed, text => Text}]))
    end.

-spec process(toml_section()) -> mongoose_config_parser:state().
process(Content) ->
    Config = parse(Content),
    Hosts = get_key(Config, hosts),
    HostTypes = get_key(Config, host_types),
    {FOpts, Config1} = lists:partition(fun(Opt) -> is_function(Opt, 1) end, Config),
    {Overrides, Opts} = lists:partition(fun({override, _}) -> true;
                                           (_) -> false
                                        end, Config1),
    HostsOpts = lists:flatmap(fun(F) -> lists:flatmap(F, Hosts) end, FOpts),
    HostTypesOpts = lists:flatmap(fun(F) -> lists:flatmap(F, HostTypes) end, FOpts),
    AllOpts = Opts ++ HostsOpts ++ HostTypesOpts,
    case extract_errors(AllOpts) of
        [] ->
            build_state(Hosts, HostTypes, AllOpts, Overrides);
        Errors ->
            error(config_error(Errors))
    end.

config_error(Errors) ->
    {config_error, "Could not read the TOML configuration file", Errors}.

-spec parse(toml_section()) -> config_list().
parse(Content) ->
    handle([], Content, mongoose_config_spec:root()).

set_overrides(Overrides, State) ->
    lists:foldl(fun({override, Scope}, CurrentState) ->
                        mongoose_config_parser:override(Scope, CurrentState)
                end, State, Overrides).

%% TODO replace with binary_to_existing_atom where possible, prevent atom leak
b2a(B) -> binary_to_atom(B, utf8).

-spec ensure_keys([toml_key()], toml_section()) -> any().
ensure_keys(Keys, Section) ->
    case lists:filter(fun(Key) -> not maps:is_key(Key, Section) end, Keys) of
        [] -> ok;
        MissingKeys -> error(#{what => missing_mandatory_keys, missing_keys => MissingKeys})
    end.

-spec parse_section(path(), toml_section(), mongoose_config_spec:config_section()) ->
          [config_part()].
parse_section(Path, M, #section{items = Items}) ->
    lists:flatmap(fun({K, V}) ->
                          handle([K|Path], V, get_spec_for_key(K, Items))
                  end, lists:sort(maps:to_list(M))).

-spec get_spec_for_key(toml_key(), map()) -> mongoose_config_spec:config_node().
get_spec_for_key(Key, Items) ->
    case maps:is_key(Key, Items) of
        true ->
            maps:get(Key, Items);
        false ->
            case maps:find(default, Items) of
                {ok, Spec} -> Spec;
                error -> error(#{what => unexpected_key, key => Key})
            end
    end.

-spec parse_list(path(), [toml_value()], mongoose_config_spec:config_list()) -> [config_part()].
parse_list(Path, L, #list{items = ItemSpec}) ->
    lists:flatmap(fun(Elem) ->
                          Key = item_key(Path, Elem),
                          handle([Key|Path], Elem, ItemSpec)
                  end, L).

-spec handle(path(), toml_value(), mongoose_config_spec:config_node()) -> config_part().
handle(Path, Value, Spec) ->
    lists:foldl(fun(_, [#{what := _, class := error}] = Error) ->
                        Error;
                   (Step, Acc) ->
                        try_step(Step, Path, Value, Acc, Spec)
                end, Value, [parse, validate, process, format]).

-spec handle_step(step(), path(), toml_value(), mongoose_config_spec:config_node()) ->
          config_part().
handle_step(parse, Path, Value, Spec) ->
    ParsedValue = case Spec of
                      #section{} = Spec when is_map(Value) ->
                          check_required_keys(Spec, Value),
                          validate_keys(Spec, Value),
                          parse_section(Path, Value, Spec);
                      #list{} when is_list(Value) ->
                          parse_list(Path, Value, Spec);
                      #option{type = Type} when not is_list(Value), not is_map(Value) ->
                          convert(Value, Type)
                  end,
    case extract_errors(ParsedValue) of
        [] -> ParsedValue;
        Errors -> Errors
    end;
handle_step(validate, _Path, ParsedValue, Spec) ->
    validate(ParsedValue, Spec),
    ParsedValue;
handle_step(process, Path, ParsedValue, Spec) ->
    process(Path, ParsedValue, process_spec(Spec));
handle_step(format, Path, ProcessedValue, Spec) ->
    format(Path, ProcessedValue, format_spec(Spec)).

-spec check_required_keys(mongoose_config_spec:config_section(), toml_section()) -> any().
check_required_keys(#section{required = all, items = Items}, Section) ->
    ensure_keys(maps:keys(Items), Section);
check_required_keys(#section{required = Required}, Section) ->
    ensure_keys(Required, Section).

-spec validate_keys(mongoose_config_spec:config_section(), toml_section()) -> any().
validate_keys(#section{validate_keys = Validator}, Section) ->
    lists:foreach(fun(Key) ->
                          mongoose_config_validator:validate(b2a(Key), atom, Validator)
                  end, maps:keys(Section)).

-spec validate(config_part(), mongoose_config_spec:config_node()) -> any().
validate(Value, #section{validate = Validator}) ->
    mongoose_config_validator:validate_section(Value, Validator);
validate(Value, #list{validate = Validator}) ->
    mongoose_config_validator:validate_list(Value, Validator);
validate(Value, #option{type = Type, validate = Validator}) ->
    mongoose_config_validator:validate(Value, Type, Validator).

-spec process_spec(mongoose_config_spec:config_section() |
                   mongoose_config_spec:config_list()) -> undefined | list_processor();
                  (mongoose_config_spec:config_option()) -> undefined | processor().
process_spec(#section{process = Process}) -> Process;
process_spec(#list{process = Process}) -> Process;
process_spec(#option{process = Process}) -> Process.

-spec process(path(), config_part(), undefined | processor()) -> config_part().
process(_Path, V, undefined) -> V;
process(_Path, V, F) when is_function(F, 1) -> F(V);
process(Path, V, F) when is_function(F, 2) -> F(Path, V).

-spec convert(toml_value(), mongoose_config_spec:option_type()) -> option_value().
convert(V, boolean) when is_boolean(V) -> V;
convert(V, binary) when is_binary(V) -> V;
convert(V, string) -> binary_to_list(V);
convert(V, atom) -> b2a(V);
convert(<<"infinity">>, int_or_infinity) -> infinity; %% TODO maybe use TOML '+inf'
convert(V, int_or_infinity) when is_integer(V) -> V;
convert(V, int_or_atom) when is_integer(V) -> V;
convert(V, int_or_atom) -> b2a(V);
convert(V, integer) when is_integer(V) -> V;
convert(V, float) when is_float(V) -> V.

-spec format_spec(mongoose_config_spec:config_node()) -> mongoose_config_spec:format().
format_spec(#section{format = Format}) -> Format;
format_spec(#list{format = Format}) -> Format;
format_spec(#option{format = Format}) -> Format.

-spec format(path(), config_part(), mongoose_config_spec:format()) -> config_part().
format(Path, KVs, {foreach, Format}) when is_atom(Format) ->
    Keys = lists:map(fun({K, _}) -> K end, KVs),
    mongoose_config_validator:validate_list(Keys, unique),
    lists:flatmap(fun({K, V}) -> format(Path, V, {Format, K}) end, KVs);
format([Key|_] = Path, V, host_local_config) ->
    format(Path, V, {host_local_config, b2a(Key)});
format([Key|_] = Path, V, local_config) ->
    format(Path, V, {local_config, b2a(Key)});
format([Key|_] = Path, V, config) ->
    format(Path, V, {config, b2a(Key)});
format(Path, V, {host_local_config, Key}) ->
    case get_host(Path) of
        global -> ?HOST_F([#local_config{key = {Key, Host}, value = V}]);
        Host -> [#local_config{key = {Key, Host}, value = V}]
    end;
format(Path, V, {local_config, Key}) ->
    global = get_host(Path),
    [#local_config{key = Key, value = V}];
format([Key|_] = Path, V, {host_or_global_config, Tag}) ->
    [#config{key = {Tag, b2a(Key), get_host(Path)}, value = V}];
format(Path, V, {config, Key}) ->
    global = get_host(Path),
    [#config{key = Key, value = V}];
format(Path, V, override) ->
    global = get_host(Path),
    [{override, V}];
format([item|_] = Path, V, default) ->
    format(Path, V, item);
format([Key|_] = Path, V, default) ->
    format(Path, V, {kv, b2a(Key)});
format(_Path, V, {kv, Key}) ->
    [{Key, V}];
format(_Path, V, item) ->
    [V];
format(_Path, _V, skip) ->
    [];
format([Key|_], V, prepend_key) ->
    L = [b2a(Key) | tuple_to_list(V)],
    [list_to_tuple(L)];
format(_Path, V, none) when is_list(V) ->
    V.

-spec get_host(path()) -> jid:server() | global.
get_host(Path) ->
    case lists:reverse(Path) of
        [<<"host_config">>, {host, Host} | _] -> Host;
        _ -> global
    end.

-spec try_step(step(), path(), toml_value(), term(),
               mongoose_config_spec:config_node()) -> config_part().
try_step(Step, Path, Value, Acc, Spec) ->
    try
        handle_step(Step, Path, Acc, Spec)
    catch error:Reason:Stacktrace ->
            BasicFields = #{what => toml_processing_failed,
                            class => error,
                            stacktrace => Stacktrace,
                            text => error_text(Step),
                            toml_path => path_to_string(Path),
                            toml_value => Value},
            ErrorFields = error_fields(Reason),
            [maps:merge(BasicFields, ErrorFields)]
    end.

-spec error_text(step()) -> string().
error_text(parse) -> "Malformed option in the TOML configuration file";
error_text(validate) -> "Incorrect option value in the TOML configuration file";
error_text(process) -> "Unable to process a value the TOML configuration file";
error_text(format) -> "Unable to format an option in the TOML configuration file".

-spec error_fields(any()) -> map().
error_fields(#{what := Reason} = M) -> maps:remove(what, M#{reason => Reason});
error_fields(Reason) -> #{reason => Reason}.

-spec path_to_string(path()) -> string().
path_to_string(Path) ->
    Items = lists:flatmap(fun node_to_string/1, lists:reverse(Path)),
    string:join(Items, ".").

node_to_string(item) -> [];
node_to_string({host, _}) -> [];
node_to_string(Node) -> [binary_to_list(Node)].

-spec item_key(path(), toml_value()) -> {host, jid:server()} | item.
item_key([<<"host_config">>], #{<<"host_type">> := Host}) -> {host, Host};
item_key([<<"host_config">>], #{<<"host">> := Host}) -> {host, Host};
item_key(_, _) -> item.

%% Processing of the parsed options

-spec get_key(config_list(), mongoose_config_parser:key()) ->
    [mongoose_config_parser:value()].
get_key(Config, Key) ->
    FilterFn = fun(#config{key = K}) when K =:= Key -> true;
                  (_) -> false
               end,
    case lists:filter(FilterFn, Config) of
        [] -> [];
        [#config{value = Value}] -> Value
    end.

-spec build_state([jid:server()], [jid:server()], [top_level_config()], [override()]) ->
          mongoose_config_parser:state().
build_state(Hosts, HostTypes, Opts, Overrides) ->
    lists:foldl(fun(F, StateIn) -> F(StateIn) end,
                mongoose_config_parser:new_state(),
                [fun(S) -> mongoose_config_parser:set_hosts(Hosts, S) end,
                 fun(S) -> mongoose_config_parser:set_host_types(HostTypes, S) end,
                 fun(S) -> mongoose_config_parser:set_opts(Opts, S) end,
                 fun mongoose_config_parser:dedup_state_opts/1,
                 fun mongoose_config_parser:add_dep_modules/1,
                 fun(S) -> set_overrides(Overrides, S) end]).

%% Any nested config_part() may be a config_error() - this function extracts them all recursively
-spec extract_errors([config()]) -> [config_error()].
extract_errors(Config) ->
    extract(fun(#{what := _, class := error}) -> true;
               (_) -> false
            end, Config).

-spec extract(fun((config_part()) -> boolean()), config_part()) -> [config_part()].
extract(Pred, Data) ->
    case Pred(Data) of
        true -> [Data];
        false -> extract_items(Pred, Data)
    end.

-spec extract_items(fun((config_part()) -> boolean()), config_part()) -> [config_part()].
extract_items(Pred, L) when is_list(L) -> lists:flatmap(fun(El) -> extract(Pred, El) end, L);
extract_items(Pred, T) when is_tuple(T) -> extract_items(Pred, tuple_to_list(T));
extract_items(Pred, M) when is_map(M) -> extract_items(Pred, maps:to_list(M));
extract_items(_, _) -> [].
