-module(mim_loglevel).
-export([enable_logging/2]).
-export([disable_logging/2]).
-export([save_log_level/1]).
-export([restore_log_level/1]).

enable_logging(Hosts, Levels) ->
    [set_custom(Host, Module, Level) || Host <- Hosts, {Module, Level} <- Levels].

disable_logging(Hosts, Levels) ->
    [clear_custom(Host, Module, Level) || Host <- Hosts, {Module, Level} <- Levels].

set_custom(Host, Module, Level) ->
    Node = ct:get_config({hosts, Host, node}),
    mongoose_helper:successful_rpc(#{node => Node}, mongoose_logs, set_module_loglevel, [Module, Level]).

clear_custom(Host, Module, _Level) ->
    Node = ct:get_config({hosts, Host, node}),
    mongoose_helper:successful_rpc(#{node => Node}, mongoose_logs, clear_module_loglevel, [Module]).

save_log_level(Config) ->
    Node = distributed_helper:mim(),
    OldLogLevel = distributed_helper:rpc(Node, mongoose_logs, get_global_loglevel, []),
    [{old_log_level, OldLogLevel} | Config].

restore_log_level(Config) ->
    Node = distributed_helper:mim(),
    {old_log_level, OldLogLevel} = lists:keyfind(old_log_level, 1, Config),
    ok = distributed_helper:rpc(Node, mongoose_logs, set_global_loglevel, [OldLogLevel]),
    ok.
