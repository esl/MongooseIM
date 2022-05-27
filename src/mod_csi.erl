%% @doc Client State Indication.
%%
%% Includes support for XEP-0352: Client State Indication.
%%
-module(mod_csi).
-xep([{xep, 352}, {version, "0.2"}]).
-behaviour(gen_mod).
-behaviour(mongoose_module_metrics).

%% gen_mod callbacks
-export([start/2,
         stop/1,
         config_spec/0,
         supported_features/0]).

%% Hook handlers
-export([c2s_stream_features/3]).

-ignore_xref([c2s_stream_features/3]).

-include("jlib.hrl").
-include("mongoose_config_spec.hrl").

-type state() :: active | inactive.

-export_type([state/0]).

-spec start(mongooseim:host_type(), gen_mod:module_opts()) -> ok.
start(HostType, _Opts) ->
    ejabberd_hooks:add(hooks(HostType)),
    ensure_metrics(HostType),
    ok.

-spec stop(mongooseim:host_type()) -> ok.
stop(HostType) ->
    ejabberd_hooks:delete(hooks(HostType)),
    ok.

hooks(HostType) ->
    [{c2s_stream_features, HostType, ?MODULE, c2s_stream_features, 60}].

ensure_metrics(HostType) ->
    mongoose_metrics:ensure_metric(HostType, [HostType, modCSIInactive], spiral),
    mongoose_metrics:ensure_metric(HostType, [HostType, modCSIInactive], spiral).

%%%
%%% config_spec
%%%

-spec config_spec() -> mongoose_config_spec:config_section().
config_spec() ->
    #section{
       items = #{<<"buffer_max">> => #option{type = int_or_infinity,
                                             validate = non_negative}},
       defaults = #{<<"buffer_max">> => 20}
    }.

-spec supported_features() -> [atom()].
supported_features() ->
    [dynamic_domains].

%%%
%%% Hook handlers
%%%

-spec c2s_stream_features([exml:element()], mongooseim:host_type(), jid:lserver()) ->
          [exml:element()].
c2s_stream_features(Acc, _HostType, _Lserver) ->
    lists:keystore(<<"csi">>, #xmlel.name, Acc, csi()).

csi() ->
    #xmlel{name = <<"csi">>,
           attrs = [{<<"xmlns">>, ?NS_CSI}]}.
