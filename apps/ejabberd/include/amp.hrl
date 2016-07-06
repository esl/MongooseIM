%% @doc XML Namespaces, records, types and legal value lists for XEP-0079
%% @reference <a href="http://xmpp.org/extensions/xep-0079.html">XEP-0079</a>
%% @author <mongooseim@erlang-solutions.com>
%% @copyright 2014 Erlang Solutions, Ltd.
%% This work was sponsored by Grindr LLC

-define(NS_AMP, <<"http://jabber.org/protocol/amp">>).
-define(NS_AMP_FEATURE, <<"http://jabber.org/feature/amp">>).

%% @doc This represents well-formed and legal rules
-record(amp_rule, {
          condition :: amp_condition(),
          value     :: amp_value(),
          action    :: amp_action()
         }).

%% @doc This includes all well-formed but nonsensical rules, like:
%% <rule action='explode' condition='fly' value='xyzzy'/>
%% It is used for returing the rules in <error> elements.
-record(amp_invalid_rule, {
          condition :: binary(),
          value     :: binary(),
          action    :: binary()
         }).

-record(amp_strategy, {
          deliver          :: [amp_value()] | undefined,
          'match-resource' :: amp_value() | undefined,
          'expire-at'      :: amp_value() | undefined
         }).

-type amp_condition() :: 'deliver' | 'expire-at' | 'match-resource'.
-type amp_value() :: 'direct' | 'forward' | 'gateway' | 'none' | 'stored' %% deliver
                   | 'any' | 'exact' | 'other'                            %% match-resource
                   | binary().                                            %% expire-at
-type amp_action() :: 'notify' | 'drop' | 'alert' | 'error'.

-type amp_error() :: 'unsupported-actions' | 'unsupported-conditions'
                   | 'not-acceptable' | 'undefined-condition'.

-type amp_rule_support() :: {'supported', amp_rule()}
                          | {error, amp_error(), amp_rule()}.

-type amp_rule_match() ::{match, amp_rule()}
                       | {error, amp_error(), amp_rule()}
                       | 'no_match'.

-type amp_rules() :: [amp_rule()].
-type amp_rule() :: #amp_rule{}.
-type amp_invalid_rule() :: #amp_invalid_rule{}.
-type amp_any_rule() :: amp_rule() | amp_invalid_rule().
-type amp_strategy() :: #amp_strategy{}.

-type amp_event() :: initial_check | archived | delivered | mam_failed | offline_failed | delivery_failed.
-type amp_match_result() :: match | no_match | undecided.

-define(AMP_LEGAL_CONDITIONS,
        [<<"deliver">>, <<"match-resource">>, <<"expire-at">>]).

-define(AMP_LEGAL_DELIVER_VALUES,
        [<<"direct">>, <<"forward">>, <<"gateway">>, <<"none">>, <<"stored">>]).

-define(AMP_LEGAL_MATCH_RESOURCE_VALUES,
        [<<"any">>, <<"exact">>, <<"other">>]).

-define(AMP_LEGAL_ACTIONS,
        [<<"alert">>, <<"drop">>, <<"error">>, <<"notify">>]).
