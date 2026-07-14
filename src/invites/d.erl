-module(d).

-compile(export_all).

trace(Mod) ->
    recon_trace:calls({Mod, '_', '_'}, 1000, [{scope, local}]).
