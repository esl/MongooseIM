-module(mongooseim_helper).
-compile([export_all, nowarn_export_all]).

start_link_loaded_hooks() ->
    Ret = gen_hook:start_link(),
    gen_hook:reload_hooks(),
    Ret.
