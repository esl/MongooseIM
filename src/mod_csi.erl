%% @doc Client State Indication.
%%
%% Includes support for XEP-0352: Client State Indication.
%%
-module(mod_csi).
-xep([{xep, 352}, {version, "0.2"}]).
-behaviour(gen_mod).
-behaviour(mongoose_module_metrics).

-export([start/2]).
-export([stop/1]).
-export([config_spec/0]).
-export([add_csi_feature/2]).

-include("jlib.hrl").
-include("ejabberd_config.hrl").

-type state() :: active | inactive.

-export_type([state/0]).

start(Host, _Opts) ->
    [ejabberd_hooks:add(Name, Host, Module, Function, Priority) ||
     {Name, Module, Function, Priority} <- hooks()],
    ok.

stop(Host) ->
    [ejabberd_hooks:delete(Name, Host, Module, Function, Priority) ||
     {Name, Module, Function, Priority} <- hooks()],
    ok.

-spec config_spec() -> mongoose_config_spec:config_section().
config_spec() ->
    #section{
       items = #{<<"buffer_max">> => #option{type = int_or_infinity,
                                             validate = non_negative}}
      }.

hooks() ->
    [{c2s_stream_features, ?MODULE, add_csi_feature, 60}].

add_csi_feature(Acc, _Host) ->
    lists:keystore(<<"csi">>, #xmlel.name, Acc, csi()).

csi() ->
    #xmlel{name = <<"csi">>,
           attrs = [{<<"xmlns">>, ?NS_CSI}]}.
