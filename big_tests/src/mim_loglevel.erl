-module(mim_loglevel).
-export([enable_logging/2]).
-export([disable_logging/2]).

enable_logging(Hosts, Levels) ->
    [set_custom(Host, Module, Level) || Host <- Hosts, {Module, Level} <- Levels].

disable_logging(Hosts, Levels) ->
    [clear_custom(Host, Module, Level) || Host <- Hosts, {Module, Level} <- Levels].

set_custom(Host, Module, Level) ->
    Node = ct:get_config({hosts, Host, node}),
    mongoose_helper:successful_rpc(#{node => Node}, ejabberd_loglevel, set_custom, [Module, Level]).

clear_custom(Host, Module, _Level) ->
    Node = ct:get_config({hosts, Host, node}),
    mongoose_helper:successful_rpc(#{node => Node}, ejabberd_loglevel, clear_custom, [Module]).
