%%%----------------------------------------------------------------------
%%% File    : mod_adhoc.erl
%%% Author  : Magnus Henoch <henoch@dtek.chalmers.se>
%%% Purpose : Handle incoming ad-doc requests (XEP-0050)
%%% Created : 15 Nov 2005 by Magnus Henoch <henoch@dtek.chalmers.se>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2011   ProcessOne
%%%
%%% This program is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation; either version 2 of the
%%% License, or (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with this program; if not, write to the Free Software
%%% Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
%%%
%%%----------------------------------------------------------------------
-module(mod_adhoc).
-author('henoch@dtek.chalmers.se').

-behaviour(gen_mod).
-behaviour(mongoose_module_metrics).

-type command_hook_acc() :: {error, exml:element()} | exml:element() | ignore | empty.
-export_type([command_hook_acc/0]).

%% Gen_mod callbacks
-export([start/2,
         stop/1,
         hooks/1,
         config_spec/0,
         supported_features/0]).

%% IQ and hook handlers
-export([process_local_iq/5,
         process_sm_iq/5,
         disco_local_items/3,
         disco_local_identity/3,
         disco_local_features/3,
         disco_sm_items/3,
         disco_sm_identity/3,
         disco_sm_features/3,
         ping_command/3]).

-include("mongoose.hrl").
-include("jlib.hrl").
-include("adhoc.hrl").
-include("mongoose_config_spec.hrl").

-spec start(mongooseim:host_type(), gen_mod:module_opts()) -> ok.
start(HostType, #{iqdisc := IQDisc}) ->
    [gen_iq_handler:add_iq_handler_for_domain(HostType, ?NS_COMMANDS, Component, Fn, #{}, IQDisc) ||
        {Component, Fn} <- iq_handlers()],
    ok.

-spec stop(mongooseim:host_type()) -> ok.
stop(HostType) ->
    [gen_iq_handler:remove_iq_handler_for_domain(HostType, ?NS_COMMANDS, Component) ||
        {Component, _Fn} <- iq_handlers()],
    ok.

iq_handlers() ->
    [{ejabberd_local, fun ?MODULE:process_local_iq/5},
     {ejabberd_sm, fun ?MODULE:process_sm_iq/5}].

hooks(HostType) ->
    [{disco_local_features, HostType, fun ?MODULE:disco_local_features/3, #{}, 99},
     {disco_local_identity, HostType, fun ?MODULE:disco_local_identity/3, #{}, 99},
     {disco_local_items, HostType, fun ?MODULE:disco_local_items/3, #{}, 99},
     {disco_sm_identity, HostType, fun ?MODULE:disco_sm_identity/3, #{}, 99},
     {disco_sm_features, HostType, fun ?MODULE:disco_sm_features/3, #{}, 99},
     {disco_sm_items, HostType, fun ?MODULE:disco_sm_items/3, #{}, 99},
     {adhoc_local_commands, HostType, fun ?MODULE:ping_command/3, #{}, 100}].

%%%
%%% config_spec
%%%

-spec config_spec() -> mongoose_config_spec:config_section().
config_spec() ->
    #section{
       items = #{<<"report_commands_node">> => #option{type = boolean},
                 <<"iqdisc">> => mongoose_config_spec:iqdisc()},
       defaults = #{<<"report_commands_node">> => false,
                    <<"iqdisc">> => one_queue}
      }.

-spec supported_features() -> [atom()].
supported_features() -> [dynamic_domains].

%%%
%%% IQ handlers
%%%

-spec process_local_iq(mongoose_acc:t(), jid:jid(), jid:jid(), jlib:iq(), map())
        -> {mongoose_acc:t(), ignore | jlib:iq()}.
process_local_iq(Acc, From, To, IQ, _Extra) ->
    {Acc, process_adhoc_request(Acc, From, To, IQ, adhoc_local_commands)}.

-spec process_sm_iq(mongoose_acc:t(), jid:jid(), jid:jid(), jlib:iq(), map()) ->
    {mongoose_acc:t(), ignore | jlib:iq()}.
process_sm_iq(Acc, From, To, IQ, _Extra) ->
    {Acc, process_adhoc_request(Acc, From, To, IQ, adhoc_sm_commands)}.

-spec process_adhoc_request(mongoose_acc:t(), jid:jid(), jid:jid(), jlib:iq(),
        Hook :: atom()) -> ignore | jlib:iq().
process_adhoc_request(Acc, From, To, #iq{sub_el = SubEl} = IQ, Hook) ->
    ?LOG_DEBUG(#{what => adhoc_parse_request, iq => IQ, hook => Hook}),
    case adhoc:parse_request(IQ) of
        {error, Error} ->
            IQ#iq{type = error, sub_el = [SubEl, Error]};
        #adhoc_request{} = AdhocRequest ->
            HostType = mongoose_acc:host_type(Acc),
            case run_request_hook(Hook, HostType, From, To, AdhocRequest) of
                ignore ->
                    ignore;
                empty ->
                    IQ#iq{type = error, sub_el = [SubEl, mongoose_xmpp_errors:item_not_found()]};
                {error, Error} ->
                    IQ#iq{type = error, sub_el = [SubEl, Error]};
                Command ->
                    IQ#iq{type = result, sub_el = [Command]}
            end
    end.

run_request_hook(adhoc_local_commands, HostType, From, To, AdhocRequest) ->
    mongoose_hooks:adhoc_local_commands(HostType, From, To, AdhocRequest);
run_request_hook(adhoc_sm_commands, HostType, From, To, AdhocRequest) ->
    mongoose_hooks:adhoc_sm_commands(HostType, From, To, AdhocRequest).

%%%
%%% Hooks handlers
%%%

-spec disco_local_items(Acc, Params, Extra) -> {ok, Acc} when
      Acc :: mongoose_disco:item_acc(),
      Params :: map(),
      Extra :: #{host_type := mongooseim:host_type()}.
disco_local_items(Acc = #{to_jid := #jid{lserver = LServer}, node := <<>>, lang := Lang}, _, #{host_type := HostType}) ->
    Items = case are_commands_visible(HostType) of
                false ->
                    [];
                _ ->
                    [item(LServer, ?NS_COMMANDS, <<"Commands">>, Lang)]
            end,
    {ok, mongoose_disco:add_items(Items, Acc)};
disco_local_items(Acc = #{to_jid := #jid{lserver = LServer}, node := ?NS_COMMANDS, lang := Lang}, _, _) ->
    Items = [item(LServer, <<"ping">>, <<"Ping">>, Lang)],
    {ok, mongoose_disco:add_items(Items, Acc)};
disco_local_items(Acc = #{node := <<"ping">>}, _, _) ->
    {ok, Acc#{result := []}}; % override the result
disco_local_items(Acc, _, _) ->
    {ok, Acc}.

%%-------------------------------------------------------------------------

-spec disco_sm_items(Acc, Params, Extra) -> {ok, Acc} when
      Acc :: mongoose_disco:item_acc(),
      Params :: map(),
      Extra :: #{host_type := mongooseim:host_type()}.
disco_sm_items(Acc = #{to_jid := To, node := <<>>, lang := Lang}, _, #{host_type := HostType}) ->
    Items = case are_commands_visible(HostType) of
                false ->
                    [];
                _ ->
                    [item(jid:to_binary(To), ?NS_COMMANDS, <<"Commands">>, Lang)]
            end,
    {ok, mongoose_disco:add_items(Items, Acc)};
disco_sm_items(Acc, _, _) ->
    {ok, Acc}.

are_commands_visible(HostType) ->
    gen_mod:get_module_opt(HostType, ?MODULE, report_commands_node).

item(LServer, Node, Name, Lang) ->
    #{jid => LServer, node => Node, name => service_translations:do(Lang, Name)}.

%%-------------------------------------------------------------------------

%% @doc On disco info request to the ad-hoc node, return automation/command-list.
-spec disco_local_identity(Acc, Params, Extra) -> {ok, Acc} when
      Acc :: mongoose_disco:identity_acc(),
      Params :: map(),
      Extra :: gen_hook:extra().
disco_local_identity(Acc = #{node := ?NS_COMMANDS, lang := Lang}, _, _) ->
    {ok, mongoose_disco:add_identities([command_list_identity(Lang)], Acc)};
disco_local_identity(Acc = #{node := <<"ping">>, lang := Lang}, _, _) ->
    {ok, mongoose_disco:add_identities([ping_identity(Lang)], Acc)};
disco_local_identity(Acc, _, _) ->
    {ok, Acc}.

%%-------------------------------------------------------------------------

%% @doc On disco info request to the ad-hoc node, return automation/command-list.
-spec disco_sm_identity(Acc, Params, Extra) -> {ok, Acc} when
      Acc :: mongoose_disco:identity_acc(),
      Params :: map(),
      Extra :: gen_hook:extra().
disco_sm_identity(Acc = #{node := ?NS_COMMANDS, lang := Lang}, _, _) ->
    {ok, mongoose_disco:add_identities([command_list_identity(Lang)], Acc)};
disco_sm_identity(Acc, _, _) ->
    {ok, Acc}.

ping_identity(Lang) ->
    #{category => <<"automation">>,
      type => <<"command-node">>,
      name => service_translations:do(Lang, <<"Ping">>)}.

command_list_identity(Lang) ->
    #{category => <<"automation">>,
      type => <<"command-list">>,
      name => service_translations:do(Lang, <<"Commands">>)}.

%%-------------------------------------------------------------------------

-spec disco_local_features(Acc, Params, Extra) -> {ok, Acc} when
    Acc :: mongoose_disco:feature_acc(),
    Params :: map(),
    Extra :: gen_hook:extra().
disco_local_features(Acc = #{node := <<>>}, _, _) ->
    {ok, mongoose_disco:add_features([?NS_COMMANDS], Acc)};
disco_local_features(Acc = #{node := ?NS_COMMANDS}, _, _) ->
    %% override all lesser features...
    {ok, Acc#{result := []}};
disco_local_features(Acc = #{node := <<"ping">>}, _, _) ->
    %% override all lesser features...
    {ok, Acc#{result := [?NS_COMMANDS]}};
disco_local_features(Acc, _, _) ->
    {ok, Acc}.

%%-------------------------------------------------------------------------

-spec disco_sm_features(Acc, Params, Extra) -> {ok, Acc} when
    Acc :: mongoose_disco:feature_acc(),
    Params :: map(),
    Extra :: gen_hook:extra().
disco_sm_features(Acc = #{node := <<>>}, _, _) ->
    {ok, mongoose_disco:add_features([?NS_COMMANDS], Acc)};
disco_sm_features(Acc = #{node := ?NS_COMMANDS}, _, _) ->
    %% override all lesser features...
    {ok, Acc#{result := []}};
disco_sm_features(Acc, _, _) ->
    {ok, Acc}.

%%-------------------------------------------------------------------------

-spec ping_command(Acc, Params, Extra) -> {ok, Acc} when
    Acc :: command_hook_acc(),
    Params :: #{adhoc_request := adhoc:request()},
    Extra :: gen_hook:extra().
ping_command(empty,
             #{adhoc_request := #adhoc_request{lang = Lang, node = <<"ping">> = Node,
                                               session_id = SessionID, action = Action}},
             _) ->
    NewAcc = case Action == <<"">> orelse Action == <<"execute">> of
        true ->
            adhoc:produce_response(
              #adhoc_response{lang = Lang,
                              node = Node,
                              session_id = SessionID,
                              status = completed,
                              notes = [{<<"info">>, service_translations:do(Lang, <<"Pong">>)}]});
        false ->
            {error, mongoose_xmpp_errors:bad_request()}
    end,
    {ok, NewAcc};
ping_command(Acc, _, _) ->
    {ok, Acc}.
