-module(mongoose_module_metrics).

-export([opts_for_module/3]).

-ignore_xref([behaviour_info/1]).

-callback config_metrics(mongooseim:host_type()) -> any().

-optional_callbacks([config_metrics/1]).

-spec opts_for_module(mongooseim:host_type(), module(), list()) -> list() | tuple().
opts_for_module(HostType, Module, OptsToReport) ->
    try
        Opts = gen_mod:get_module_opts(HostType, Module),
        lists:map(
            fun({OptToReport, DefaultValue}) ->
                    Value = proplists:get_value(OptToReport, Opts, DefaultValue),
                    {OptToReport, Value}
            end, OptsToReport)
    catch
        _:_ -> {none, none}
    end.
