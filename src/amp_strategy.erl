%% @doc This module is responsible for determining the server's strategy for a
%% particular message. (See XEP section 2.2.2 "Determine Default Action")
%% This work was sponsored by Grindr LLC
%% @reference <a href="http://xmpp.org/extensions/xep-0079.html">XEP-0079</a>
%% @author <mongooseim@erlang-solutions.com>
%% @copyright 2014 Erlang Solutions, Ltd.
-module(amp_strategy).

-export([determine_strategy/3,
         null_strategy/0]).

-include("amp.hrl").
-include("jlib.hrl").

-spec determine_strategy(StrategyAcc, Params, Extra) -> {ok, StrategyAcc} when
      StrategyAcc :: mod_amp:amp_strategy(),
      Params :: #{to := jid:jid() | undefined, event := mod_amp:amp_event()},
      Extra :: map().
determine_strategy(_, #{to := undefined}, _) -> {ok, null_strategy()};
determine_strategy(_, #{to := To, event := Event}, _) ->
    TargetResources = get_target_resources(To),
    Deliver = deliver_strategy(TargetResources, Event),
    MatchResource = match_resource_strategy(TargetResources),

    {ok, #amp_strategy{deliver = Deliver,
                       'match-resource' = MatchResource,
                       'expire-at' = undefined}}.

%% @doc This strategy will never be matched by any amp_rules.
%% Use it as a seed parameter to `mongoose_hooks'.
-spec null_strategy() -> amp_strategy().
null_strategy() ->
    #amp_strategy{deliver = undefined,
                  'match-resource' = undefined,
                  'expire-at' = undefined}.

%% Internals
get_target_resources(MessageTarget = #jid{lresource = TargetRes}) ->
    Resources = ejabberd_sm:get_user_resources(MessageTarget),
    ResStatus = case lists:member(TargetRes, Resources) of
                    true -> online;
                    false -> offline
                end,
    {ResStatus, Resources}.

deliver_strategy({offline, []}, initial_check) -> [none];
deliver_strategy({_Status, _}, initial_check) -> [direct, none];
deliver_strategy({offline, []}, archived) -> [stored];
deliver_strategy({_Status, _}, archived) -> [direct, stored];
deliver_strategy(_, delivery_failed) -> [stored, none];
deliver_strategy({offline, []}, mam_failed) -> [none];
deliver_strategy({_Status, _}, mam_failed) -> [direct, none];
deliver_strategy(_, offline_failed) -> [none];
deliver_strategy(_, delivered) -> [direct].

%% @doc Notes on matching
%%
%% The undefined value in match-resource signifies that no resource could be matched,
%% and therefore no rules with match-resource set could possibly yield true.
%% Conversely, the 'any' strategy is not a valid server-side strategy:
%% the server will either match the exact resource, or not. (See match_res_any CT test)
%% in test/amp_resolver_SUITE.erl
%%
match_resource_strategy({offline, []}) -> undefined;
match_resource_strategy({offline, [_ | _ManyRes]}) -> other;
match_resource_strategy({online, [_ | _ManyRes]}) -> exact.
