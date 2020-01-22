-module(mongoose_module_metrics).

-export([opts_for_module/3]).

-callback config_metrics(string()) -> any().

-optional_callbacks([config_metrics/1]).

opts_for_module(Host, Module, OptsToReport) ->
    try
        Opts = gen_mod:opts_for_module(Host, Module),
        lists:map(
            fun({OptToReport, DefaultValue}) ->
                    Value = proplists:get_value(OptToReport, Opts, DefaultValue),
                    {OptToReport, Value}
            end,OptsToReport)
    catch
        _:_ -> {none, none}
    end.
