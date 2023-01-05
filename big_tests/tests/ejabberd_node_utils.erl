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

-export([init/1, init/2,
         node_cwd/2,
         restart_application/1, restart_application/2,
         call_fun/3, call_fun/4,
         call_ctl/2, call_ctl/3,
         call_ctl_with_args/3,
         file_exists/1, file_exists/2,
         backup_config_file/1, backup_config_file/2,
         restore_config_file/1, restore_config_file/2,
         modify_config_file/2, modify_config_file/4,
         get_cwd/2]).

-include_lib("common_test/include/ct.hrl").

cwd(Node, Config) ->
    ?config({ejabberd_cwd, Node}, Config).

backup_config_path(Node, Config, Format) ->
    filename:join([cwd(Node, Config), "etc", config_file_name(Format) ++ ".bak"]).

config_template_path(Config, Format) ->
    filename:join([path_helper:repo_dir(Config), "rel", "files", config_file_name(Format)]).

config_vars_path(File, Config) ->
    filename:join([path_helper:repo_dir(Config), "rel", File]).

ctl_path(Node, Config) ->
    filename:join([cwd(Node, Config), "bin", "mongooseimctl"]).

config_file_name(toml) -> "mongooseim.toml".

-type ct_config() :: list({Key :: term(), Value :: term()}).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------

-spec init(ct_config()) -> ct_config().
init(Config) ->
    Node = distributed_helper:mim(),
    init(Node, Config).

init(Node, Config) ->
    set_ejabberd_node_cwd(Node, Config).

node_cwd(Node, Config) ->
    CWD = escalus_config:get_config({ejabberd_cwd, Node}, Config),
    CWD == undefined andalso error({{ejabberd_cwd, Node}, undefined}, [Node, Config]),
    CWD.

-spec restart_application(atom()) -> ok.
restart_application(ApplicationName) ->
    Node = distributed_helper:mim(),
    restart_application(Node#{timeout => timer:seconds(30)}, ApplicationName).

-spec restart_application(node(), atom()) -> ok.
restart_application(Node, ApplicationName) ->
    ok = ejabberd_node_utils:call_fun(Node, application, stop, [ApplicationName]),
    ok = ejabberd_node_utils:call_fun(Node, application, start, [ApplicationName]).


-spec backup_config_file(ct_config()) -> ct_config().
backup_config_file(Config) ->
    Node = distributed_helper:mim(),
    backup_config_file(Node, Config).

-spec backup_config_file(distributed_helper:rpc_spec(), ct_config()) -> ct_config().
backup_config_file(#{node := Node} = RPCSpec, Config) ->
    {ok, _} = call_fun(RPCSpec, file, copy, [get_config_path(RPCSpec),
                                             backup_config_path(Node, Config, toml)]).

-spec restore_config_file(ct_config()) -> 'ok'.
restore_config_file(Config) ->
    Node = distributed_helper:mim(),
    restore_config_file(Node, Config).

-spec restore_config_file(distributed_helper:rpc_spec(), ct_config()) -> 'ok'.
restore_config_file(#{node := Node} = RPCSpec, Config) ->
    ok = call_fun(RPCSpec, file, rename, [backup_config_path(Node, Config, toml),
                                          update_config_path(RPCSpec, toml)]).

-spec call_fun(module(), atom(), []) -> term() | {badrpc, term()}.
call_fun(M, F, A) ->
    Node = distributed_helper:mim(),
    call_fun(Node, M, F, A).

-spec call_fun(distributed_helper:rpc_spec(), module(), atom(), []) ->
    term() | {badrpc, term()}.
call_fun(Node, M, F, A) ->
    distributed_helper:rpc(Node, M, F, A).

%% @doc Calls the mongooseimctl script with given command `Cmd'.
%%
%% For example to restart mongooseim call `call_ctl(restart, Config).'.
-spec call_ctl(atom(), ct_config()) -> term() | term().
call_ctl(Cmd, Config) ->
    Node = ct:get_config({hosts, mim, node}),
    call_ctl(Node, Cmd, Config).

-spec call_ctl(node(), atom(), ct_config()) -> term() | term().
call_ctl(Node, Cmd, Config) ->
    call_ctl_with_args(Node, [atom_to_list(Cmd)], Config).

-spec call_ctl_with_args(node(), [string()], ct_config()) -> term() | term().
call_ctl_with_args(Node, CmdAndArgs, Config) ->
    OsCmd = string:join([ctl_path(Node, Config) | CmdAndArgs], " "),
    os:cmd(OsCmd).

-spec file_exists(file:name_all()) -> term() | {badrpc, term()}.
file_exists(Filename) ->
    call_fun(filelib, is_file, [Filename]).

-spec file_exists(node(), file:name_all()) -> term() | {badrpc, term()}.
file_exists(Node, Filename) ->
    call_fun(Node, filelib, is_file, [Filename]).

%% @doc Modifies default ejabberd config file: `etc/mongooseim.toml'.
%%
%% This function assumes that the config file was generated from template
%% file in `rel/files/mongooseim.toml' using variables from `rel/vars-toml.config'.
%% The modification procedure overrides given variables provided in
%% `rel/vars-toml.config'.
%%
%% For example to change `hosts' value in the configuration file one
%% has to call the function as follows:
%% ```NewHosts = {hosts, "[\"" ++ binary_to_list(Domain) ++ "\"]"},
%%    modify_config_file([NewHosts], Config).'''
-spec modify_config_file([{ConfigVariable, Value}], ct_config()) -> ok when
      ConfigVariable :: atom(),
      Value :: string().
modify_config_file(CfgVarsToChange, Config) ->
    modify_config_file(mim, CfgVarsToChange, Config, toml).

-spec modify_config_file(Host, [{ConfigVariable, Value}], ct_config(), toml) -> ok when
      Host :: atom(),
      ConfigVariable :: atom(),
      Value :: string().
modify_config_file(Host, VarsToChange, Config, Format) ->
    NodeVarsFile = ct:get_config({hosts, Host, vars}, Config) ++ "." ++ vars_file(Format),
    TemplatePath = config_template_path(Config, Format),
    NodeVarsPath = config_vars_path(NodeVarsFile, Config),

    {ok, Template} = file:read_file(TemplatePath),
    NodeVars = read_vars(NodeVarsPath),
    PresetVars = preset_vars(Config, Format),

    TemplatedConfig = template_config(Template, NodeVars ++ PresetVars ++ VarsToChange),

    RPCSpec = distributed_helper:Host(),
    NewCfgPath = update_config_path(RPCSpec, Format),
    ok = ejabberd_node_utils:call_fun(RPCSpec, file, write_file, [NewCfgPath, TemplatedConfig]).

read_vars(File) ->
    {ok, Terms} = file:consult(File),
    lists:flatmap(fun({Key, Val}) ->
                          [{Key, Val}];
                     (IncludedFile) when is_list(IncludedFile) ->
                          Path = filename:join(filename:dirname(File), IncludedFile),
                          read_vars(Path)
                  end, Terms).

template_config(Template, Vars) ->
    MergedVars = ensure_binary_strings(maps:from_list(Vars)),
    %% Render twice to replace variables in variables
    Tmp = bbmustache:render(Template, MergedVars, [{key_type, atom}]),
    bbmustache:render(Tmp, MergedVars, [{key_type, atom}]).

%% bbmustache tries to iterate over lists, so we need to make them binaries
ensure_binary_strings(Vars) ->
    maps:map(fun(dbs, V) -> V;
                (_K, []) -> <<"\n">>; % empty binary is considered falsey in conditions
                (_K, V) when is_list(V) -> list_to_binary(V);
                (_K, V) -> V
              end, Vars).

update_config_path(RPCSpec, Format) ->
    CurrentCfgPath = get_config_path(RPCSpec),
    CurrentConfigFileName = filename:basename(CurrentCfgPath),
    case config_file_name(Format) of
        CurrentConfigFileName ->
            CurrentCfgPath;
        NewConfigFileName ->
            Path = filename:join(filename:dirname(CurrentCfgPath), NewConfigFileName),
            set_config_path(RPCSpec, Path),
            Path
    end.

get_config_path(RPCSpec) ->
    ejabberd_node_utils:call_fun(RPCSpec, os, getenv, ["EJABBERD_CONFIG_PATH"]).

set_config_path(RPCSpec, Path) ->
    ejabberd_node_utils:call_fun(RPCSpec, os, putenv, ["EJABBERD_CONFIG_PATH", Path]).

vars_file(toml) -> "vars-toml.config".

preset_vars(Config, Format) ->
    case proplists:get_value(preset, Config) of
        undefined -> [];
        Name -> ct:get_config({presets, Format, list_to_existing_atom(Name)})
    end.

-spec get_cwd(node(), ct_config()) -> string().
get_cwd(Node, Config) ->
    cwd(Node, Config).

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------

set_ejabberd_node_cwd(#{node := Node} = RPCSpec, Config) ->
    {ok, Cwd} = call_fun(RPCSpec, file, get_cwd, []),
    [{{ejabberd_cwd, Node}, Cwd} | Config].
