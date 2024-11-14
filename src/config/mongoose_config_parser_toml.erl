%% @doc Config parsing and processing for the TOML format
-module(mongoose_config_parser_toml).

-behaviour(mongoose_config_parser).

-export([parse_file/1]).

-ifdef(TEST).
-export([process/1,
         extract_errors/1]).
-endif.

-include("mongoose_config_spec.hrl").

%% Input: TOML parsed by tomerl
-type toml_key() :: binary().
-type toml_value() :: tomerl:value().
-type toml_section() :: tomerl:section().

%% Output: list of config records, containing key-value pairs
-type option_value() :: atom() | binary() | string() | float(). % parsed leaf value
-type config_part() :: term(). % any part of a top-level option value, may contain config errors
-type top_level_config() :: {mongoose_config:key(), mongoose_config:value()}.
-type config_error() :: #{class := error, what := atom(), text := string(), any() => any()}.
-type config() :: top_level_config() | config_error().

-type processor() :: processor_fun() | [processor_fun()].
-type processor_fun() :: fun((path(), config_part()) -> config_part())
                       | fun((config_part()) -> config_part()).

-type step() ::
        parse        % Recursive processing (section/list) or type conversion (leaf option)

      | validate     % Value check with one of the predefined validators

      | format_items % Optional formatting of section/list items as a map

      | process      % Optional processing of the value with a custom function

      | wrap.        % Wrapping the value into a list, which will be concatenated
                     % with other items of the parent node.
                     % In case of a KV pair the key is also added here.

%% Path from the currently processed config node to the root
%%   - toml_key(): key in a toml_section()
%%   - item: item in a list
%%   - {host, Host}: item in the list of hosts in host_config
-type path() :: [toml_key() | item | {host, jid:server()}].

-export_type([toml_key/0, toml_value/0, toml_section/0,
              option_value/0, config/0, config_error/0, config_part/0,
              processor/0, path/0]).

-spec parse_file(FileName :: string()) -> mongoose_config_parser:state().
parse_file(FileName) ->
    case tomerl:read_file(FileName) of
        {ok, Content} ->
            process(Content);
        {error, Error} ->
            error(config_error([#{what => toml_parsing_failed, text => Error}]))
    end.

-spec process(toml_section()) -> mongoose_config_parser:state().
process(Content) ->
    Config = parse(Content),
    Hosts = proplists:get_value(hosts, Config, []),
    HostTypes = proplists:get_value(host_types, Config, []),
    case extract_errors(Config) of
        [] ->
            mongoose_config_parser:build_state(Hosts, HostTypes, Config);
        Errors ->
            error(config_error(Errors))
    end.

config_error(Errors) ->
    {config_error, "Could not read the TOML configuration file", Errors}.

-spec parse(toml_section()) -> [config()].
parse(Content) ->
    handle([], Content, mongoose_config_spec:root()).

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
parse_section(Path, M, #section{items = Items, defaults = Defaults}) ->
    FilteredDefaults = maps:filter(fun(K, _V) -> not maps:is_key(K, M) end, Defaults),
    M1 = maps:merge(get_always_included(Items), M),
    ProcessedConfig = maps:map(fun(K, V) -> handle([K|Path], V, get_spec_for_key(K, Items)) end, M1),
    ProcessedDefaults = maps:map(fun(K, V) -> handle_default([K|Path], V, maps:get(K, Items)) end,
                                 FilteredDefaults),
    lists:flatmap(fun({_K, ConfigParts}) -> ConfigParts end,
                  lists:keysort(1, maps:to_list(maps:merge(ProcessedDefaults, ProcessedConfig)))).

-spec get_spec_for_key(toml_key(), map()) -> mongoose_config_spec:config_node().
get_spec_for_key(Key, Items) ->
    case maps:is_key(Key, Items) of
        true ->
            maps:get(Key, Items);
        false ->
            case maps:find(default, Items) of
                {ok, Spec} -> Spec;
                error -> error(#{what => unexpected_key, key => Key, items => Items})
            end
    end.

get_always_included(Items) ->
    maps:from_list([{K, #{}} || {K, #section{include = always}} <- maps:to_list(Items)]).

-spec parse_list(path(), [toml_value()], mongoose_config_spec:config_list()) -> [config_part()].
parse_list(Path, L, #list{items = ItemSpec}) ->
    lists:flatmap(fun(Elem) ->
                          Key = item_key(Path, Elem),
                          handle([Key|Path], Elem, ItemSpec)
                  end, L).

-spec handle(path(), toml_value(), mongoose_config_spec:config_node()) -> [config_part()].
handle(Path, Value, Spec = #option{}) ->
    handle(Path, Value, Spec, [parse, validate, process, wrap]);
handle(Path, Value, Spec) ->
    handle(Path, Value, Spec, [parse, validate, format_items, process, wrap]).

-spec handle_default(path(), toml_value(), mongoose_config_spec:config_node()) -> [config_part()].
handle_default(Path, Value, Spec) ->
    handle(Path, Value, Spec, [wrap]).

-spec handle(path(), toml_value(), mongoose_config_spec:config_node(), [step()]) -> [config_part()].
handle(Path, Value, Spec, Steps) ->
    lists:foldl(fun(_, [#{what := _, class := error}|_] = Errors) ->
                        Errors;
                   (Step, Acc) ->
                        try_step(Step, Path, Value, Acc, Spec)
                end, Value, Steps).

-spec handle_step(step(), path(), toml_value(), mongoose_config_spec:config_node()) ->
          config_part().
handle_step(parse, Path, Value, Spec) ->
    ParsedValue = case Spec of
                      #section{} when is_map(Value) ->
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
handle_step(format_items, _Path, Items, Spec) ->
    format_items(Items, format_items_spec(Spec));
handle_step(validate, _Path, ParsedValue, Spec) ->
    validate(ParsedValue, Spec),
    ParsedValue;
handle_step(process, Path, ParsedValue, Spec) ->
    process(Path, ParsedValue, process_spec(Spec));
handle_step(wrap, Path, ProcessedValue, Spec) ->
    wrap(Path, ProcessedValue, wrap_spec(Spec)).

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

-spec format_items_spec(mongoose_config_spec:config_node()) -> mongoose_config_spec:format_items().
format_items_spec(#section{format_items = FormatItems}) -> FormatItems;
format_items_spec(#list{format_items = FormatItems}) -> FormatItems.

-spec format_items(config_part(), mongoose_config_spec:format_items()) -> config_part().
format_items(KVs, map) ->
    Keys = lists:map(fun({K, _}) -> K end, KVs),
    mongoose_config_validator:validate_list(Keys, unique),
    maps:from_list(KVs);
format_items(Value, list) when is_list(Value) ->
    Value.

-spec validate(config_part(), mongoose_config_spec:config_node()) -> any().
validate(Value, #section{validate = Validator}) ->
    mongoose_config_validator:validate_section(Value, Validator);
validate(Value, #list{validate = Validator}) ->
    mongoose_config_validator:validate_list(Value, Validator);
validate(Value, #option{type = Type, validate = Validator}) ->
    mongoose_config_validator:validate(Value, Type, Validator).

-spec process_spec(mongoose_config_spec:config_section() |
                   mongoose_config_spec:config_list() |
                   mongoose_config_spec:config_option()) -> processor().
process_spec(#section{process = Process}) -> Process;
process_spec(#list{process = Process}) -> Process;
process_spec(#option{process = Process}) -> Process.

-spec process(path(), config_part(), processor()) -> config_part().
process(Path, V, Functions) when is_list(Functions) ->
    lists:foldl(fun(F, Value) -> process(Path, Value, F) end, V, Functions);
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

-spec wrap_spec(mongoose_config_spec:config_node()) -> mongoose_config_spec:wrapper().
wrap_spec(#section{wrap = Wrap}) -> Wrap;
wrap_spec(#list{wrap = Wrap}) -> Wrap;
wrap_spec(#option{wrap = Wrap}) -> Wrap.

-spec wrap(path(), config_part(), mongoose_config_spec:wrapper()) -> [config_part()].
wrap([Key|_] = Path, V, host_config) ->
    [{{b2a(Key), get_host(Path)}, V}];
wrap([Key|_] = Path, V, global_config) ->
    global = get_host(Path),
    [{b2a(Key), V}];
wrap([item|_], V, default) ->
    [V];
wrap([Key|_], V, default) ->
    [{b2a(Key), V}];
wrap(_Path, V, item) ->
    [V];
wrap(_Path, _V, remove) ->
    [];
wrap(_Path, V, none) when is_list(V) ->
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
                            text => "TOML configuration error: " ++ error_text(Step),
                            toml_path => path_to_string(Path),
                            toml_value => Value},
            ErrorFields = error_fields(Reason),
            [maps:merge(BasicFields, ErrorFields)]
    end.

-spec error_text(step()) -> string().
error_text(parse) -> "Malformed node";
error_text(validate) -> "Invalid node value";
error_text(format_items) -> "List or section has invalid key-value pairs";
error_text(process) -> "Node could not be processed";
error_text(wrap) -> "Node could not be wrapped in a config option".

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
