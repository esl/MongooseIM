-module(mod_mam_rdbms_arch_simulated_error).

%% ----------------------------------------------------------------------
%% Exports

%% gen_mod handlers
-export([start/2, stop/1, hooks/1, supported_features/0]).

-behaviour(gen_mod).

-export([archive_message/3]).

-type host_type() :: mongooseim:host_type().

%% ----------------------------------------------------------------------
%% gen_mod callbacks
%% Starting and stopping functions for users' archives

-spec start(host_type(), gen_mod:module_opts()) -> ok.
start(_HostType, Opts) -> ok.

-spec stop(host_type()) -> ok.
stop(_HostType) -> ok.

-spec supported_features() -> [atom()].
supported_features() ->
    [dynamic_domains].

%% ----------------------------------------------------------------------
%% Add hooks for mod_mam_pm

-spec hooks(mongooseim:host_type()) -> gen_hook:hook_list().
hooks(HostType) ->
    [{mam_archive_message, HostType, fun ?MODULE:archive_message/3, #{}, 49}].

archive_message(_Result, _Params, _) ->
    {stop, {error, simulated}}.
