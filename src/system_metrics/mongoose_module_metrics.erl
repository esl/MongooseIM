-module(mongoose_module_metrics).

-export([opts_for_module/3]).

-ignore_xref([behaviour_info/1]).

-callback config_metrics(mongooseim:host_type()) -> any().

-optional_callbacks([config_metrics/1]).

-spec opts_for_module(mongooseim:host_type(), module(), [gen_mod:opt_key()]) ->
          [{gen_mod:opt_key(), gen_mod:opt_value()}].
opts_for_module(HostType, Module, OptsToReport) ->
    try
        Opts = gen_mod:get_module_opts(HostType, Module),
        [get_opt(OptToReport, Opts) || OptToReport <- OptsToReport]
    catch
        _:_ -> [{none, none}]
    end.

get_opt(Opt, Opts) ->
    {Opt, gen_mod:get_opt(Opt, Opts)}.
