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
-export([c2s_stream_features/3]).

-include("jlib.hrl").
-include("mongoose_config_spec.hrl").

-type state() :: active | inactive.

-export_type([state/0]).

start(Host, _Opts) ->
    [ejabberd_hooks:add(Name, Host, Module, Function, Priority) ||
     {Name, Module, Function, Priority} <- hooks()],
    ensure_metrics(Host),
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
    [{c2s_stream_features, ?MODULE, c2s_stream_features, 60}].

ensure_metrics(Host) ->
    mongoose_metrics:ensure_metric(Host, [Host, modCSIInactive], spiral),
    mongoose_metrics:ensure_metric(Host, [Host, modCSIInactive], spiral).

-spec c2s_stream_features([exml:element()], mongooseim:host_type(), jid:lserver()) ->
          [exml:element()].
c2s_stream_features(Acc, _HostType, _Lserver) ->
    lists:keystore(<<"csi">>, #xmlel.name, Acc, csi()).

csi() ->
    #xmlel{name = <<"csi">>,
           attrs = [{<<"xmlns">>, ?NS_CSI}]}.
