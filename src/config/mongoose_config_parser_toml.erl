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
-type option() :: term(). % a part of a config value OR a list of them, may contain config errors
-type top_level_option() :: #config{} | #local_config{} | acl:acl().
-type config_error() :: #{class := error, what := atom(), text := string(), any() => any()}.
-type override() :: {override, atom()}.
-type config() :: top_level_option() | config_error() | override().
-type config_list() :: [config() | fun((ejabberd:server()) -> [config()])]. % see HOST_F

%% Path from the currently processed config node to the root
%%   - toml_key(): key in a toml_section()
%%   - item: item in a list
%%   - tuple(): item in a list, tagged with data from the item, e.g. host name
-type path() :: [toml_key() | item | tuple()].

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
    Hosts = get_hosts(Config),
    {FOpts, Config1} = lists:partition(fun(Opt) -> is_function(Opt, 1) end, Config),
    {Overrides, Opts} = lists:partition(fun({override, _}) -> true;
                                           (_) -> false
                                        end, Config1),
    HOpts = lists:flatmap(fun(F) -> lists:flatmap(F, Hosts) end, FOpts),
    AllOpts = Opts ++ HOpts,
    case extract_errors(AllOpts) of
        [] ->
            build_state(Hosts, AllOpts, Overrides);
        Errors ->
            error(config_error(Errors))
    end.

config_error(Errors) ->
    {config_error, "Could not read the TOML configuration file", Errors}.

%% Config processing functions are annotated with TOML paths
%% Path syntax: dotted, like TOML keys with the following additions:
%%   - '[]' denotes an element in a list
%%   - '( ... )' encloses an optional prefix
%%   - '*' is a wildcard for names - usually that name is passed as an argument
%% If the path is the same as for the previous function, it is not repeated.
%%
%% Example: (host_config[].)access.*
%% Meaning: either a key in the 'access' section, e.g.
%%            [access]
%%              local = ...
%%          or the same, but prefixed for a specific host, e.g.
%%            [[host_config]]
%%              host = "myhost"
%%              host_config.access
%%                local = ...

%% root path
-spec parse(toml_section()) -> config_list().
parse(Content) ->
    handle([], Content).

-spec parse_root(path(), toml_section()) -> config_list().
parse_root(Path, Content) ->
    ensure_keys([<<"general">>], Content),
    parse_section(Path, Content).

%% path: host_config[]
-spec process_host_item(path(), toml_section()) -> config_list().
process_host_item(Path, M) ->
    {_Host, Sections} = maps:take(<<"host">>, M),
    parse_section(Path, Sections).

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

-spec parse_section(path(), toml_section()) -> [option()].
parse_section(Path, M) ->
    lists:flatmap(fun({K, V}) ->
                          handle([K|Path], V)
                  end, lists:sort(maps:to_list(M))).

-spec parse_list(path(), [toml_value()]) -> [option()].
parse_list(Path, L) ->
    lists:flatmap(fun(Elem) ->
                          Key = item_key(Path, Elem),
                          handle([Key|Path], Elem)
                  end, L).

-spec handle(path(), toml_value()) -> option().
handle(Path, Value) ->
    lists:foldl(fun(_, [#{what := _, class := error}] = Error) ->
                        Error;
                   (StepName, AccIn) ->
                        try_call(handle_step(StepName, AccIn), StepName, Path, Value)
                end, Path, [handle, parse, validate, process, format]).

handle_step(handle, _) ->
    fun(Path, _Value) -> handler(Path) end;
handle_step(parse, Spec) when is_tuple(Spec) ->
    fun(Path, Value) ->
            ParsedValue = case Spec of
                              #section{} = Spec when is_map(Value) ->
                                  check_required_keys(Spec, Value),
                                  validate_keys(Spec, Value),
                                  parse_section(Path, Value);
                              #list{} when is_list(Value) ->
                                  parse_list(Path, Value);
                              #option{type = Type} when not is_list(Value), not is_map(Value) ->
                                  convert(Value, Type)
                          end,
            case extract_errors(ParsedValue) of
                [] -> {ParsedValue, Spec};
                Errors -> Errors
            end
    end;
handle_step(parse, Handler) ->
    Handler;
handle_step(validate, {ParsedValue, Spec}) ->
    fun(_Path, _Value) ->
            validate(ParsedValue, Spec),
            {ParsedValue, Spec}
    end;
handle_step(validate, ParsedValue) ->
    fun(Path, _Value) ->
            mongoose_config_validator_toml:validate(Path, ParsedValue),
            ParsedValue
    end;
handle_step(process, {ParsedValue, Spec}) ->
    fun(Path, _Value) ->
            ProcessedValue = process(Path, ParsedValue, process_spec(Spec)),
            {ProcessedValue, Spec}
    end;
handle_step(process, V) ->
    fun(_, _) -> V end;
handle_step(format, {ParsedValue, Spec}) ->
    fun(Path, _Value) ->
            format(Path, ParsedValue, format_spec(Spec))
    end;
handle_step(format, V) ->
    fun(_, _) -> V end.

check_required_keys(#section{required = all, items = Items}, Section) ->
    ensure_keys(maps:keys(Items), Section);
check_required_keys(#section{required = Required}, Section) ->
    ensure_keys(Required, Section).

validate_keys(#section{validate_keys = undefined}, _Section) -> ok;
validate_keys(#section{validate_keys = Validator}, Section) ->
    lists:foreach(fun(Key) ->
                          mongoose_config_validator_toml:validate(b2a(Key), atom, Validator)
                  end, maps:keys(Section)).

validate(Value, #section{validate = Validator}) ->
    mongoose_config_validator_toml:validate_section(Value, Validator);
validate(Value, #list{validate = Validator}) ->
    mongoose_config_validator_toml:validate_list(Value, Validator);
validate(Value, #option{type = Type, validate = Validator}) ->
    mongoose_config_validator_toml:validate(Value, Type, Validator).

process_spec(#section{process = Process}) -> Process;
process_spec(#list{process = Process}) -> Process;
process_spec(#option{process = Process}) -> Process.

process(_Path, V, undefined) -> V;
process(_Path, V, F) when is_function(F, 1) -> F(V);
process(Path, V, F) when is_function(F, 2) -> F(Path, V).

convert(V, boolean) when is_boolean(V) -> V;
convert(V, binary) when is_binary(V) -> V;
convert(V, string) -> binary_to_list(V);
convert(V, atom) -> b2a(V);
convert(<<"infinity">>, int_or_infinity) -> infinity; %% TODO maybe use TOML '+inf'
convert(V, int_or_infinity) when is_integer(V) -> V;
convert(<<"infinity">>, int_or_infinity_or_atom) -> infinity;
convert(<<"no_buffer">>, int_or_infinity_or_atom) -> no_buffer;
convert(V, int_or_infinity_or_atom) when is_integer(V) -> V;
convert(V, int_or_atom) when is_integer(V) -> V;
convert(V, int_or_atom) -> b2a(V);
convert(V, integer) when is_integer(V) -> V;
convert(V, float) when is_float(V) -> V.

format_spec(#section{format = Format}) -> Format;
format_spec(#list{format = Format}) -> Format;
format_spec(#option{format = Format}) -> Format.

format(Path, KVs, {foreach, Format}) when is_atom(Format) ->
    Keys = lists:map(fun({K, _}) -> K end, KVs),
    mongoose_config_validator_toml:validate_list(Keys, unique),
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
format([item, Key|_] = Path, V, host_or_global_acl) ->
    [acl:to_record(get_host(Path), b2a(Key), V)];
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
format([Key|_], V, prepend_key) ->
    L = [b2a(Key) | tuple_to_list(V)],
    [list_to_tuple(L)];
format(_Path, V, none) ->
    V.

get_host(Path) ->
    case lists:reverse(Path) of
        [<<"host_config">>, {host, Host} | _] -> Host;
        _ -> global
    end.

-spec try_call(fun((path(), any()) -> option()), atom(), path(), toml_value()) -> option().
try_call(F, StepName, Path, Value) ->
    try
        F(Path, Value)
    catch error:Reason:Stacktrace ->
            BasicFields = #{what => toml_processing_failed,
                            class => error,
                            stacktrace => Stacktrace,
                            text => error_text(StepName),
                            toml_path => path_to_string(Path),
                            toml_value => Value},
            ErrorFields = error_fields(Reason),
            [maps:merge(BasicFields, ErrorFields)]
    end.

-spec error_text(atom()) -> string().
error_text(handle) -> "Unexpected option in the TOML configuration file";
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

-spec handler(path()) ->
          fun((path(), toml_value()) -> option()) | mongoose_config_spec:config_node().
handler([]) -> fun parse_root/2;
handler([<<"host_config">>]) -> fun parse_list/2;

%% host_config
handler([_, <<"host_config">>]) -> fun process_host_item/2;
handler([<<"auth">>, _, <<"host_config">>] = P) -> handler_for_host(P);
handler([<<"modules">>, _, <<"host_config">>] = P) -> handler_for_host(P);
handler([<<"general">>, _, <<"host_config">>]) -> fun parse_section/2;
handler([_, <<"general">>, _, <<"host_config">>] = P) -> handler_for_host(P);
handler([_, <<"s2s">>, _, <<"host_config">>] = P) -> handler_for_host(P);
handler(Path) ->
    reverse_handler(lists:reverse(Path)).

reverse_handler([<<"host_config">>, {host, _} | Subtree]) ->
    handler(lists:reverse(Subtree));
reverse_handler(Path) ->
    mongoose_config_spec:handler(Path).

%% 1. Strip host_config, choose the handler for the remaining path
%% 2. Wrap the handler in a fun that calls the resulting function F for the current host
-spec handler_for_host(path()) ->
          fun((path(), toml_value()) -> option()) | mongoose_config_spec:config_node().
handler_for_host(Path) ->
    [<<"host_config">>, {host, Host} | Rest] = lists:reverse(Path),
    case handler(lists:reverse(Rest)) of
        Handler when is_function(Handler) ->
            fun(PathArg, ValueArg) ->
                    ConfigFunctions = Handler(PathArg, ValueArg),
                    lists:flatmap(fun(F) -> F(Host) end, ConfigFunctions)
            end;
        Spec ->
            Spec
    end.

-spec item_key(path(), toml_value()) -> tuple() | item.
item_key([<<"host_config">>], #{<<"host">> := Host}) -> {host, Host};
item_key(_, _) -> item.

%% Processing of the parsed options

-spec get_hosts(config_list()) -> [ejabberd:server()].
get_hosts(Config) ->
    case lists:filter(fun(#config{key = hosts}) -> true;
                         (_) -> false
                      end, Config) of
        [] -> [];
        [#config{value = Hosts}] -> Hosts
    end.

-spec build_state([ejabberd:server()], [top_level_option()], [override()]) ->
          mongoose_config_parser:state().
build_state(Hosts, Opts, Overrides) ->
    lists:foldl(fun(F, StateIn) -> F(StateIn) end,
                mongoose_config_parser:new_state(),
                [fun(S) -> mongoose_config_parser:set_hosts(Hosts, S) end,
                 fun(S) -> mongoose_config_parser:set_opts(Opts, S) end,
                 fun mongoose_config_parser:dedup_state_opts/1,
                 fun mongoose_config_parser:add_dep_modules/1,
                 fun(S) -> set_overrides(Overrides, S) end]).

%% Any nested option() may be a config_error() - this function extracts them all recursively
-spec extract_errors([config()]) -> [config_error()].
extract_errors(Config) ->
    extract(fun(#{what := _, class := error}) -> true;
               (_) -> false
            end, Config).

-spec extract(fun((option()) -> boolean()), option()) -> [option()].
extract(Pred, Data) ->
    case Pred(Data) of
        true -> [Data];
        false -> extract_items(Pred, Data)
    end.

-spec extract_items(fun((option()) -> boolean()), option()) -> [option()].
extract_items(Pred, L) when is_list(L) -> lists:flatmap(fun(El) -> extract(Pred, El) end, L);
extract_items(Pred, T) when is_tuple(T) -> extract_items(Pred, tuple_to_list(T));
extract_items(Pred, M) when is_map(M) -> extract_items(Pred, maps:to_list(M));
extract_items(_, _) -> [].
