-module(hook_helper).
-moduledoc "Utilities for hook manipulation in big tests. They make sure hooks are reloaded.".

-compile([export_all, nowarn_export_all]).

-import(distributed_helper, [rpc/4, mim/0]).

add_handler(Handler) ->
    add_handlers([Handler]).

delete_handler(Handler) ->
    delete_handlers([Handler]).

add_handlers(Handlers) ->
    add_handlers(mim(), Handlers).

delete_handlers(Handlers) ->
    delete_handlers(mim(), Handlers).

add_handlers(_Node, []) ->
    ok;
add_handlers(Node, Handlers) ->
    ok = rpc(Node, gen_hook, add_handlers, [Handlers]),
    ok = rpc(Node, gen_hook, reload_hooks, []).

delete_handlers(_Node, []) ->
    ok;
delete_handlers(Node, Handlers) ->
    ok = rpc(Node, gen_hook, delete_handlers, [Handlers]),
    ok = rpc(Node, gen_hook, reload_hooks, []).
