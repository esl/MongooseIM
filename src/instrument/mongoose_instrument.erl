-module(mongoose_instrument).

-export([config_spec/0]).

-include("mongoose_config_spec.hrl").

-export([start_link/0]).

-type handler_key() :: prometheus | exometer | log.

start_link() ->
    application:set_env(instrument, handlers, handler_config()),
    instrument:start_link().

%% @doc Specifies the `instrumentation' section of the config file
-spec config_spec() -> mongoose_config_spec:config_section().
config_spec() ->
    Items = [{atom_to_binary(Key), config_spec(Key)} || Key <- all_handler_keys()],
    Options = #{<<"probe_interval">> => #option{type = integer, validate = positive}},
    #section{items = maps:merge(maps:from_list(Items), Options),
             defaults = #{<<"probe_interval">> => 15},
             wrap = global_config,
             include = always}.

config_spec(exometer) ->
    #section{items = #{<<"all_metrics_are_global">> => #option{type = boolean},
                       <<"report">> => report_config_spec()},
             defaults = #{<<"all_metrics_are_global">> => false}
            };
config_spec(prometheus) ->
    #section{};
config_spec(log) ->
    #section{items = #{<<"level">> => #option{type = atom, validate = instrumentation_loglevel}},
             defaults = #{<<"level">> => debug}}.

-spec report_config_spec() -> mongoose_config_spec:config_section().
report_config_spec() ->
    Reporters = [<<"graphite">>],
    Common = common_reporter_config_spec(),
    ReporterSpecs = [{Reporter, #list{items = reporter_config_spec(Common, Reporter), wrap = none}}
                     || Reporter <- Reporters],
    #section{items = maps:from_list(ReporterSpecs),
             include = always}.

-spec reporter_config_spec(mongoose_config_spec:config_section(), binary()) ->
          mongoose_config_spec:config_section().
reporter_config_spec(BaseSpec, Reporter) ->
    mongoose_config_utils:merge_sections(BaseSpec, reporter_config_spec(Reporter)).

-spec reporter_config_spec(binary()) -> mongoose_config_spec:config_section().
reporter_config_spec(<<"graphite">>) ->
    #section{items = #{<<"host">> => #option{type = string, validate = network_address},
                       <<"port">> => #option{type = integer, validate = port},
                       <<"connect_timeout">> => #option{type = integer, validate = positive},
                       <<"prefix">> => #option{type = string},
                       <<"env_prefix">> => #option{type = string},
                       <<"api_key">> => #option{type = string}},
             defaults = #{<<"port">> => 2003,
                          <<"connect_timeout">> => 5000, % milliseconds
                          <<"api_key">> => ""},
             required = [<<"host">>],
             process = fun ?MODULE:process_graphite_reporter/2}.

-spec common_reporter_config_spec() -> mongoose_config_spec:config_section().
common_reporter_config_spec() ->
    #section{items = #{<<"interval">> => #option{type = integer, validate = positive}},
             defaults = #{<<"interval">> => 60000} % milliseconds
            }.

-spec process_graphite_reporter(mongoose_config_parser_toml:path(), map()) ->
          {instrument_handler_exometer:reporter_name(),
           instrument_handler_exometer:reporter_opts()}.
process_graphite_reporter(_Path, #{host := Host, port := Port} = Opts) ->
    Name = list_to_atom(lists:flatten(io_lib:format("graphite:~s:~p", [Host, Port]))),
    {Name, Opts#{module => exometer_report_graphite}}.

-spec all_handler_keys() -> [handler_key()].
all_handler_keys() ->
    [prometheus, exometer, log].

-spec handler_module(handler_key()) -> module().
handler_module(Key) ->
    list_to_existing_atom("instrument_" ++ atom_to_list(Key)).

-spec handler_config() -> mongoose_config:config_value().
handler_config() ->
    #{handler_module(Key) => Opts
      || Key := Opts = #{} <- mongoose_config:get_opt(instrumentation)}.

