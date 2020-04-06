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

-export([backup_ejabberd_config_file/2,
         restore_ejabberd_config_file/2,
         reload_through_ctl/2,
         restart_ejabberd_node/1]).

-import(distributed_helper, [rpc/4]).

-define(CTL_RELOAD_OUTPUT_PREFIX,
        "done").

backup_ejabberd_config_file(#{node := Node} = RPCSpec, Config) ->
    {ok, _} = rpc(RPCSpec, file, copy, [node_cfg(Node, current, Config),
                                        node_cfg(Node, backup, Config)]).

restore_ejabberd_config_file(#{node := Node} = RPCSpec, Config) ->
    ok = rpc(RPCSpec, file, rename, [node_cfg(Node, backup, Config),
                                     node_cfg(Node, current, Config)]).

restart_ejabberd_node(Node) ->
    %% Node restarts might take a long time -> long timeout.
    ok = rpc(Node#{timeout => timer:seconds(10)}, application, stop, [mongooseim]),
    ok = rpc(Node#{timeout => timer:seconds(10)}, application, start, [mongooseim]).

reload_through_ctl(#{node := Node} = RPCSpec, Config) ->
    ReloadCmd = node_ctl(Node, Config) ++ " reload_local",
    OutputStr = rpc(RPCSpec, os, cmd, [ReloadCmd]),
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

update_config_variables(CfgVarsToChange, CfgVars) ->
    lists:foldl(fun({Var, Val}, Acc) ->
                        lists:keystore(Var, 1, Acc,{Var, Val})
                end, CfgVars, CfgVarsToChange).

node_cfg(N, current, C) ->
    filename:join(ejabberd_node_utils:node_cwd(N, C), "etc/mongooseim.cfg");
node_cfg(N, backup, C)  ->
    filename:join(ejabberd_node_utils:node_cwd(N, C), "etc/mongooseim.cfg.bak").

node_ctl(N, C) ->
    filename:join(ejabberd_node_utils:node_cwd(N, C), "bin/mongooseimctl").
