-module(ct_mim_config_hook).

%% @doc Add the following line in your *.spec file to enable
%% fixes for Common Tests for your common tests:
%% {ct_hooks, [ct_mim_config_hook]}.

%% Callbacks
-export([id/1]).
-export([init/2]).
-export([pre_init_per_suite/3]).
-record(state, { }).

%% @doc Return a unique id for this CTH.
id(_Opts) ->
    ?MODULE_STRING ++ "_001".

%% @doc Always called before any other callback function. Use this to initiate
%% any common state.
init(_Id, _Opts) ->
    {ok, #state{  }}.

%% Fix data_dir for init_per_suite function
pre_init_per_suite(SuiteName, Config, State) ->
    {fix_data_dir(SuiteName, Config), State}.

%% Common tests set data dir based on beam source file attribute.
%% Which makes us care, where the file has been compiled and makes
%% us less portable.
%% Lets set out own data_dir.
%% Be aware, CT would try to set data_dir over and over again,
%% so to be safe we use new mim_data_dir option instead.
fix_data_dir(SuiteName, Config) ->
    DataDir = path_helper:data_dir(SuiteName, Config),
    [{mim_data_dir, DataDir}|Config].
