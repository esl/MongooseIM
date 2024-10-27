%%%-------------------------------------------------------------------
%%% @author jaspreet.chhabra
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 27. Oct 2024 10:00 AM
%%%-------------------------------------------------------------------
-module(mod_muc_light_multiple_admin).
-author("jaspreet.chhabra").

-behaviour(gen_mod).

-include("mongoose.hrl").
-include("mongoose_config_spec.hrl").
%% ------------------------------------------------------------------
%% gen_mod
%% ------------------------------------------------------------------
-export([start/2, stop/1, hooks/1, config_spec/0]).
%% ------------------------------------------------------------------
%% Hook handlers
%% ------------------------------------------------------------------
-export([should_allow_multiple_admin/3]).

%% ------------------------------------------------------------------
%% Macros
%%--------------------------------------------------------------------
-define(DEFAULT_ALLOW_MULTIPLE_ADMINS, true).

-spec start(mongooseim:host_type(), gen_mod:module_opts()) -> ok.
start(_HostType, _Opts) ->
  ok.

-spec stop(mongooseim:host_type()) -> ok.
stop(_HostType) ->
  ok.

-spec hooks(mongooseim:host_type()) -> gen_hook:hook_list().
hooks(HostType) ->
  [{should_allow_multiple_admin, HostType, fun ?MODULE:should_allow_multiple_admin/3, #{}, 98}].

-spec config_spec() -> mongoose_config_spec:config_section().
config_spec() ->
  #section{
    items = #{<<"allow_multiple_admin">> => #option{type = boolean}},
    defaults = #{<<"allow_multiple_admin">> => ?DEFAULT_ALLOW_MULTIPLE_ADMINS}
  }.

%%====================================================================
%% Hook handlers
%%====================================================================
-spec should_allow_multiple_admin(Acc, Params, Extra) -> {ok | stop, Acc} when
  Acc :: boolean(),
  Params :: #{},
  Extra :: gen_hook:extra().
should_allow_multiple_admin(_InAcc, #{}, #{host_type := HostType}) ->
  AllowMultipleAdmin = gen_mod:get_module_opt(HostType, ?MODULE, allow_multiple_admin),
  {ok, AllowMultipleAdmin}.


