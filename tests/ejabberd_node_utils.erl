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

-module(ejabberd_node_utils).

-export([init/1, restart_application/1, call_fun/3, call_ctl/2, file_exists/1,
         backup_config_file/1, restore_config_file/1, modify_config_file/2]).

-include_lib("common_test/include/ct.hrl").

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

-type ct_config() :: list({Key :: term(), Value :: term()}).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------

-spec init(ct_config()) -> ct_config().
init(Config) ->
    set_ejabberd_node_cwd(Config).

-spec restart_application(atom()) -> ok.
restart_application(ApplicationName) ->
    ok = ejabberd_node_utils:call_fun(application, stop, [ApplicationName]),
    ok = ejabberd_node_utils:call_fun(application, start, [ApplicationName]).

-spec backup_config_file(ct_config()) -> ct_config().
backup_config_file(Config) ->
    {ok, _} = call_fun(file, copy, [?CURRENT_CFG_PATH(Config),
                                    ?BACKUP_CFG_PATH(Config)]).

-spec restore_config_file(ct_config()) -> ct_config().
restore_config_file(Config) ->
    ok = call_fun(file, rename, [?BACKUP_CFG_PATH(Config),
                                 ?CURRENT_CFG_PATH(Config)]).

-spec call_fun(module(), atom(), [term()]) -> term() | {badrpc, term()}.
call_fun(M, F, A) ->
    Node = ct:get_config(ejabberd_node),
    rpc:call(Node, M, F, A).

%% @doc Calls the mongooseimctl script with given command `Cmd'.
%%
%% For example to restart mongooseim call `call_ctl(restart, Config).'.
-spec call_ctl(atom(), ct_config()) -> term() | {badrpc, term()}.
call_ctl(Cmd, Config) ->
    OsCmd = io_lib:format("~p ~p", [?CTL_PATH(Config), atom_to_list(Cmd)]),
    call_fun(os, cmd, [OsCmd]).

-spec file_exists(file:name_all()) -> term() | {badrpc, term()}.
file_exists(Filename) ->
    call_fun(filelib, is_file, [Filename]).

%% @doc Modifies default ejabberd config file: `etc/ejabberd.cfg'.
%%
%% This function assumes that the config file was generated from template
%% file in `rel/files/ejabberd.cfg' using variables from `rel/vars.config'.
%% The modification procedure overrides given variables provided in
%% `rel/vars.config'.
%%
%% For example to change `hosts' value in the configuration file one
%% has to call the function as follows:
%% ```NewHosts = {hosts, "[\"" ++ binary_to_list(Domain) ++ "\"]"},
%%    modify_config_file([NewHosts], Config).'''
-spec modify_config_file([{ConfigVariable, Value}], ct_config()) -> ok when
      ConfigVariable :: atom(),
      Value :: string().
modify_config_file(CfgVarsToChange, Config) ->
    CurrentCfgPath = ?CURRENT_CFG_PATH(Config),
    {ok, CfgTemplate} = ejabberd_node_utils:call_fun(
                          file, read_file, [?CFG_TEMPLATE_PATH(Config)]),
    {ok, CfgVars} = ejabberd_node_utils:call_fun(file, consult,
                                                 [?CFG_VARS_PATH(Config)]),
    UpdatedCfgVars = update_config_variables(CfgVarsToChange, CfgVars),
    CfgTemplateList = binary_to_list(CfgTemplate),
    UpdatedCfgFile = mustache:render(CfgTemplateList,
                                     dict:from_list(UpdatedCfgVars)),
    ok = ejabberd_node_utils:call_fun(file, write_file, [CurrentCfgPath,
                                                         UpdatedCfgFile]).

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------

set_ejabberd_node_cwd(Config) ->
    {ok, Cwd} = call_fun(file, get_cwd, []),
    [{ejabberd_node_cwd, Cwd} | Config].

update_config_variables(CfgVarsToChange, CfgVars) ->
    lists:foldl(fun({Var, Val}, Acc) ->
                        lists:keystore(Var, 1, Acc,{Var, Val})
                end, CfgVars, CfgVarsToChange).




