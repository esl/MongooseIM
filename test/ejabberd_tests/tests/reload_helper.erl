%%==============================================================================
%% Copyright 2014 Erlang Solutions Ltd.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%==============================================================================

-module(reload_helper).

-include_lib("common_test/include/ct.hrl").

-export([modify_config_file/3,
         backup_ejabberd_config_file/2,
         restore_ejabberd_config_file/2,
         reload_through_ctl/2,
         restart_ejabberd_node/1]).

-import(distributed_helper, [rpc/4, rpc/5]).

-define(CTL_RELOAD_OUTPUT_PREFIX,
        "# Reloaded: " ++ atom_to_list(ct:get_config({hosts, mim, node}))).

backup_ejabberd_config_file(Node, Config) ->
    {ok, _} = rpc(Node, file, copy, [node_cfg(Node, current, Config),
                                     node_cfg(Node, backup, Config)]).

restore_ejabberd_config_file(Node, Config) ->
    ok = rpc(Node, file, rename, [node_cfg(Node, backup, Config),
                                  node_cfg(Node, current, Config)]).

restart_ejabberd_node(Node) ->
    %% Node restarts might take a long time -> long timeout.
    ok = rpc(Node, application, stop, [ejabberd], timer:seconds(10)),
    ok = rpc(Node, application, start, [ejabberd], timer:seconds(10)).

reload_through_ctl(Node, Config) ->
    ReloadCmd = node_ctl(Node, Config) ++ " reload_local",
    OutputStr = rpc(Node, os, cmd, [ReloadCmd]),
    ok = verify_reload_output(ReloadCmd, OutputStr).

verify_reload_output(ReloadCmd, OutputStr) ->
    ExpectedOutput = ?CTL_RELOAD_OUTPUT_PREFIX,
    case lists:sublist(OutputStr, length(ExpectedOutput)) of
        ExpectedOutput ->
            ok;
        _ ->
            ct:pal("ReloadCmd: ~p", [ReloadCmd]),
            ct:pal("OutputStr: ~ts", [OutputStr]),
            error(config_reload_failed, [OutputStr])
    end.

modify_config_file(Node, CfgVarsToChange, Config) ->
    CurrentCfgPath = node_cfg(Node, current, Config),
    {ok, CfgTemplate} = rpc(Node, file, read_file, [node_cfg(Node, template, Config)]),
    {ok, CfgVars} = rpc(Node, file, consult, [node_cfg(Node, vars, Config)]),
    UpdatedCfgVars = update_config_variables(CfgVarsToChange, CfgVars),
    CfgTemplateList = binary_to_list(CfgTemplate),
    UpdatedCfgFile = mustache:render(CfgTemplateList,
                                     dict:from_list(UpdatedCfgVars)),
    ok = rpc(Node, file, write_file, [CurrentCfgPath, UpdatedCfgFile]).

update_config_variables(CfgVarsToChange, CfgVars) ->
    lists:foldl(fun({Var, Val}, Acc) ->
                        lists:keystore(Var, 1, Acc,{Var, Val})
                end, CfgVars, CfgVarsToChange).

node_cfg(N, current, C)  -> flat([node_cwd(N, C), "etc", "ejabberd.cfg"]);
node_cfg(N, backup, C)   -> flat([node_cwd(N, C), "etc", "ejabberd.cfg.bak"]);
node_cfg(N, template, C) -> flat([node_cwd(N, C), "..", "..", "rel", "files", "ejabberd.cfg"]);
node_cfg(N, vars, C)     -> flat([node_cwd(N, C), "..", "..", "rel", "vars.config"]).

node_ctl(N, C) -> flat([node_cwd(N, C), "bin", "mongooseimctl"]).

flat(PathComponents) -> filename:join(PathComponents).

node_cwd(Node, Config) ->
    CWD = escalus_config:get_config({ejabberd_cwd, Node}, Config),
    CWD == undefined andalso error({{ejabberd_cwd, Node}, undefined}, [Node, Config]),
    CWD.
