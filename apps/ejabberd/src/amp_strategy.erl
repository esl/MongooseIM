-module(amp_strategy).
%% @doc This module is responsible for determining the server's strategy for a
%% particular message. (See XEP section 2.2.2 "Determine Default Action")
%% @reference <a href="http://xmpp.org/extensions/xep-0079.html">XEP-0079</a>
%% @author <mongooseim@erlang-solutions.com>
%% @copyright 2014 Erlang Solutions, Ltd.
%% This work was sponsored by Grindr LLC
-export([determine_strategy/2,
         null_strategy/0]).

-include_lib("ejabberd/include/amp.hrl").
-include_lib("ejabberd/include/ejabberd.hrl").
-include_lib("ejabberd/include/jlib.hrl").

-spec determine_strategy(any(), {jid(), jid(), exml:xmlel()})
                        -> amp_strategy().
determine_strategy(_,{_From,To,_Packet} = HookData) ->
    TargetResources = get_target_resources(To),
    Deliver = deliver_strategy(HookData, TargetResources),
    MatchResource = match_resource_strategy(HookData, TargetResources),

    #amp_strategy{deliver = Deliver,
                  'match-resource' = MatchResource,
                  'expire-at' = undefined}.

%% @doc This strategy will never be matched by any amp_rules.
%% Use it as a seed parameter to ejaberd_hooks:run_fold
-spec null_strategy() -> amp_strategy().
null_strategy() ->
    #amp_strategy{deliver = undefined,
                  'match-resource' = undefined,
                  'expire-at' = undefined}.

%% Internals
get_target_resources(MessageTarget) ->
    {User,Server,Resource} = jlib:jid_to_lower(MessageTarget),
    ResourceSession = ejabberd_sm:get_session(User, Server, Resource),
    UserResources = ejabberd_sm:get_user_resources(User, Server),
    {ResourceSession, UserResources}.

deliver_strategy(_, {offline, []}) -> 'none';
deliver_strategy(_, {offline,  _}) -> 'forward';
deliver_strategy(_, {_Session, _}) -> 'direct'.

%% @doc Notes on matching
%%
%% The undefined value in match-resource signifies that no resource could be matched,
%% and therefore no rules with match-resource set could possibly yield true.
%% Conversely, the 'any' strategy is not a valid server-side strategy:
%% the server will either match the exact resource, or not. (See match_res_any CT test)
%% in apps/ejabberd/test/amp_resolver_SUITE.erl
%%
match_resource_strategy(_, {offline, []})            -> undefined;
match_resource_strategy(_, {offline, [_|_ManyRes]})  -> 'other';
match_resource_strategy(_, {_Session, [_|_ManyRes]}) -> 'exact'.
