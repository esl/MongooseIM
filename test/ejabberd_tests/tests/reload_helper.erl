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

-export([modify_config_file/2,
         bacup_ejabberd_config_file/1,
         restore_ejabberd_config_file/1,
         reload_through_ctl/1,
         restart_ejabberd_node/0,
         set_ejabberd_node_cwd/1]).

-define(CWD(Config), ?config(ejabberd_node_cwd, Config)).
-define(CURRENT_CFG_PATH(Config),
        filename:join([?CWD(Config), "etc", "ejabberd.cfg"])).
-define(BACKUP_CFG_PATH(Config),
        filename:join([?CWD(Config), "etc","ejabberd.cfg.bak"])).
-define(CFG_TEMPLATE_PATH(Config),
        filename:join([?CWD(Config), "..", "..", "rel", "files",
                       "ejabberd.cfg"])).
-define(CFG_VARS_PATH(Config),
        filename:join([?CWD(Config), "..", "..", "rel",
                       "vars.config"])).
-define(CTL_PATH(Config),
        filename:join([?CWD(Config), "bin", "mongooseimctl"])).

-define(CTL_RELOAD_OUTPUT_PREFIX,
        "# Reloaded: " ++ atom_to_list(ct:get_config(ejabberd_node))).


call_ejabberd(M, F, A) ->
    Node = ct:get_config(ejabberd_node),
    rpc:call(Node, M, F, A).

set_ejabberd_node_cwd(Config) ->
    {ok, Cwd} = call_ejabberd(file, get_cwd, []),
    [{ejabberd_node_cwd, Cwd} | Config].

bacup_ejabberd_config_file(Config) ->
    {ok, _} = call_ejabberd(file, copy, [?CURRENT_CFG_PATH(Config),
                                         ?BACKUP_CFG_PATH(Config)]).

restore_ejabberd_config_file(Config) ->
    ok = call_ejabberd(file, rename, [?BACKUP_CFG_PATH(Config),
                                      ?CURRENT_CFG_PATH(Config)]).

restart_ejabberd_node() ->
    ok = call_ejabberd(application, stop, [ejabberd]),
    ok = call_ejabberd(application, start, [ejabberd]).

reload_through_ctl(Config) ->
    ReloadCmd = ?CTL_PATH(Config) ++ " reload_local",
    OutputStr = call_ejabberd(os, cmd, [ReloadCmd]),
    ok = verify_reload_output(OutputStr).

verify_reload_output(OutputStr) ->
    ExpectedOutput = ?CTL_RELOAD_OUTPUT_PREFIX,
    case lists:sublist(OutputStr, length(ExpectedOutput)) of
        ExpectedOutput ->
            ok;
        _ ->
            ct:pal("~ts", [OutputStr]),
            error(config_reload_failed, [OutputStr])
    end.
    

modify_config_file(CfgVarsToChange, Config) ->
    CurrentCfgPath = ?CURRENT_CFG_PATH(Config),
    {ok, CfgTemplate} = call_ejabberd(file, read_file,
                                      [?CFG_TEMPLATE_PATH(Config)]),
    {ok, CfgVars} = call_ejabberd(file, consult,
                                  [?CFG_VARS_PATH(Config)]),
    UpdatedCfgVars = update_config_variables(CfgVarsToChange, CfgVars),
    CfgTemplateList = binary_to_list(CfgTemplate),
    UpdatedCfgFile = mustache:render(CfgTemplateList,
                                     dict:from_list(UpdatedCfgVars)),
    ok = call_ejabberd(file, write_file, [CurrentCfgPath,
                                          UpdatedCfgFile]).

update_config_variables(CfgVarsToChange, CfgVars) ->
    lists:foldl(fun({Var, Val}, Acc) ->
                        lists:keystore(Var, 1, Acc,{Var, Val})
                end, CfgVars, CfgVarsToChange).
