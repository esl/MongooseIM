-module(amp).
%% @doc Models and business logic for  XEP-0079: Advanced Message Processing
%% @reference <a href="http://xmpp.org/extensions/xep-0079.html">XEP-0079</a>
%% @author <mongooseim@erlang-solutions.com>
%% @copyright 2014 Erlang Solutions, Ltd.
%% This work was sponsored by Grindr LLC

-include_lib("ejabberd/include/amp.hrl").
-include_lib("ejabberd/include/ejabberd.hrl").
-include_lib("ejabberd/include/jlib.hrl").

-export([extract_requested_rules/1,
         make_response/3,
         make_error_response/4,
         rule_to_xmlel/1,
         strip_amp_el/1,

         binaries_to_rule/3,
         is_amp_request/1
        ]).

-export_type([amp_rule/0,
              amp_rules/0]).


-spec binaries_to_rule(binary(), binary(), binary()) -> amp_rule() | amp_invalid_rule().
binaries_to_rule(<<"deliver">> = Condition, Value, Action) ->
    case are_valid_deliver_params(Value, Action) of
        true -> mk_amp_rule('deliver', from_bin_(Value), from_bin_(Action));
        false -> mk_amp_invalid_rule(Condition, Value, Action)
    end;
binaries_to_rule(<<"match-resource">> = Condition, Value, Action) ->
    case are_valid_match_resource_params(Value, Action) of
        true -> mk_amp_rule('match-resource', from_bin_(Value), from_bin_(Action));
        false -> mk_amp_invalid_rule(Condition, Value, Action)
    end;
binaries_to_rule(<<"expire-at">> = Condition, Value, Action) ->
    case are_valid_expire_at_params(Value, Action) of
        true -> mk_amp_rule('expire-at', Value, from_bin_(Action)); %% Value is binary here!
        false -> mk_amp_invalid_rule(Condition, Value, Action)
    end;
binaries_to_rule(Condition, Value, Action) ->
    mk_amp_invalid_rule(Condition, Value, Action).


-spec extract_requested_rules(#xmlel{}) -> 'none'
                                             | {rules, amp_rules()}
                                             | {errors, [{amp_error(), amp_invalid_rule()}]}.
extract_requested_rules(#xmlel{} = Stanza) ->
    case is_amp_request(Stanza) of
        true -> parse_rules(Stanza);
        _    -> none
    end.

-spec make_response(amp_rule(), jid(), #xmlel{}) -> #xmlel{}.
make_response(Rule, User, Packet) ->
    OriginalId = exml_query:attr(Packet, <<"id">>, <<"original-id-missing">>),
    OriginalSender = jid:to_binary(User),
    OriginalRecipient = exml_query:attr(Packet, <<"to">>),

    Amp = #xmlel{name = <<"amp">>,
                 attrs = [{<<"xmlns">>, ?NS_AMP},
                          {<<"status">>, to_bin_(Rule#amp_rule.action)},
                          {<<"to">>, OriginalRecipient},
                          {<<"from">>, OriginalSender}],
                 children = [rule_to_xmlel(Rule)]},
    #xmlel{name = <<"message">>,
           attrs = [{<<"id">>, OriginalId}],
           children = [Amp]}.


-spec make_error_response([amp_error()], [amp_any_rule()], jid(), #xmlel{})
                         -> #xmlel{}.
make_error_response([E|_] = Errors, [_|_] = Rules, User, Packet) ->
    OriginalId = exml_query:attr(Packet, <<"id">>, <<"original-id-missing">>),
    Error = make_error_el(Errors, Rules),
    Amp = #xmlel{name = <<"amp">>,
                 attrs = [{<<"xmlns">>, ?NS_AMP} |
                          error_amp_attrs(E, User, Packet)],
                 children = [rule_to_xmlel(R) || R <- Rules]},
    #xmlel{name = <<"message">>,
           attrs = [{<<"id">>, OriginalId},
                    {<<"type">>, <<"error">>}],
           children = [Error, Amp]};
make_error_response(Errors, Rules, User, Packet) ->
    ?ERROR_MSG("amp:make_error_response/4 got invalid data: ~p",
               [Errors, Rules, User, Packet]),
    error(invalid_data).

error_amp_attrs('undefined-condition', User, Packet) ->
    OriginalSender = jid:to_binary(User),
    OriginalRecipient = exml_query:attr(Packet, <<"to">>),
    [{<<"status">>, <<"error">>},
     {<<"to">>, OriginalRecipient},
     {<<"from">>, OriginalSender}];
error_amp_attrs(_, _, _) -> [].


%% The lists are guaranteed to be non-empty and of equal
%% length by make_error_message/4
-spec make_error_el([amp_error()], [amp_any_rule()]) -> #xmlel{}.
make_error_el(Errors, Rules) ->
    ErrorMarker = #xmlel{name = error_marker_name(hd(Errors)),
                         attrs = [{<<"xmlns">>, ?NS_STANZAS}]},
    RuleContainer = #xmlel{name = rule_container_name(hd(Errors)),
                          attrs = [{<<"xmlns">>, ?NS_AMP}],
                          children = [ rule_to_xmlel(R) || R <- Rules ]},
    #xmlel{name = <<"error">>,
           attrs = [{<<"type">>, <<"modify">>},
                    {<<"code">>, error_code(hd(Errors))}],
           children = [ErrorMarker, RuleContainer]}.

-spec rule_to_xmlel(amp_any_rule()) -> #xmlel{}.
rule_to_xmlel(#amp_rule{condition=C, value=V, action=A}) ->
    #xmlel{name = <<"rule">>,
           attrs = [{<<"condition">>, to_bin_(C)},
                    {<<"value">>, to_bin_(V)},
                    {<<"action">>, to_bin_(A)}]};
rule_to_xmlel(#amp_invalid_rule{condition=C, value=V, action=A}) ->
    #xmlel{name = <<"rule">>,
           attrs = [{<<"condition">>, C},
                    {<<"value">>, V},
                    {<<"action">>, A}]}.

-spec strip_amp_el(#xmlel{}) -> #xmlel{}.
strip_amp_el(#xmlel{children = Children} = Elem) ->
    NewChildren = [ C || C <- Children, not is_amp_el(C) ],
    Elem#xmlel{children = NewChildren}.


%% Internal
%% @doc We want to keep client->server AMPed messages,
%%      but filter out server->client AMPed responses.
%%      We can distinguish them by the fact that s2c messages MUST have
%%      a 'status' attr on the <amp> element.
-spec is_amp_request(xmlel()) -> boolean().
is_amp_request(Stanza) ->
    Amp = exml_query:subelement(Stanza, <<"amp">>),
    (undefined =/= Amp)
    andalso
        (undefined == exml_query:attr(Amp, <<"status">>))
         andalso
         (undefined == exml_query:subelement(Stanza, <<"error">>)).

-spec is_amp_el(#xmlel{}) -> boolean().
is_amp_el(#xmlel{name = <<"amp">>}) -> true;
is_amp_el(_)                        -> false.

-spec parse_rules(#xmlel{}) -> none
                                 | {rules, amp_rules()}
                                 | {errors, [{amp_error(), amp_rule()}]}.
parse_rules(Stanza) ->
    Amp = exml_query:subelement(Stanza, <<"amp">>),
    RuleElems = exml_query:subelements(Amp, <<"rule">>),
    MaybeRules = [ parse_rule(R) || R <- RuleElems ],
    case lists:partition(fun is_valid_rule/1, MaybeRules) of
        {[], []}     -> none;
        {Valid, []}  -> {rules, Valid};
        {_, Invalid} -> {errors, [ {'not-acceptable', R} || R <- Invalid ]}
    end.

-spec parse_rule(#xmlel{}) -> amp_rule() | amp_invalid_rule().
parse_rule(#xmlel{attrs = Attrs}) ->
    GetF = fun(Value) -> proplists:get_value(Value, Attrs, <<"attribute-missing">>) end,
    {C, V, A} = {GetF(<<"condition">>),
               GetF(<<"value">>),
               GetF(<<"action">>)},
    binaries_to_rule(C, V, A).

-spec is_valid_rule(amp_rule() | amp_invalid_rule()) -> boolean().
is_valid_rule(#amp_rule{}) -> true;
is_valid_rule(#amp_invalid_rule{}) -> false.

is_valid_action(Action) ->
    lists:member(Action, ?AMP_LEGAL_ACTIONS).

are_valid_deliver_params(Value, Action) ->
    lists:member(Value, ?AMP_LEGAL_DELIVER_VALUES) andalso
        is_valid_action(Action).

are_valid_match_resource_params(Value, Action) ->
    lists:member(Value, ?AMP_LEGAL_MATCH_RESOURCE_VALUES) andalso
        is_valid_action(Action).

are_valid_expire_at_params(_Value, Action) ->
    %% We may check the value with a regexp for a proper date in the future.
    is_valid_action(Action).

mk_amp_rule(C, V, A) ->
    #amp_rule{condition = C, value = V, action = A}.
mk_amp_invalid_rule(C, V, A) ->
    #amp_invalid_rule{condition = C, value = V, action = A}.

error_code('not-acceptable')      -> <<"405">>;
error_code('undefined-condition') -> <<"500">>;
error_code(_)                     -> <<"400">>.

error_marker_name('not-acceptable') -> <<"not-acceptable">>;
error_marker_name('unsupported-actions') -> <<"bad-request">>;
error_marker_name('unsupported-conditions') -> <<"bad-request">>;
error_marker_name('undefined-condition') -> <<"undefined-condition">>.

rule_container_name('not-acceptable') -> <<"invalid-rules">>;
rule_container_name('unsupported-actions') -> <<"unsupported-actions">>;
rule_container_name('unsupported-conditions') -> <<"unsupported-conditions">>;
rule_container_name('undefined-condition') -> <<"failed-rules">>.

-spec to_bin_(amp_action() | amp_condition() | amp_value() | amp_error()) ->
                     binary().
to_bin_(A) when is_atom(A) -> atom_to_binary(A, utf8);
to_bin_(X) when is_binary(X) -> X.

-spec from_bin_(binary()) -> amp_action() | amp_condition() |
                             amp_value() | amp_error().
%% @doc WARNING! This is a partial function. Only values that have been
%% verified as legal in binaries_to_rule/3 should be passed here.
%% DO NOT EXPORT!!!
%% conditons
from_bin_(<<"deliver">>) -> 'deliver';
from_bin_(<<"expire-at">>) -> 'expire-at';
from_bin_(<<"match-resource">>) -> 'match-resource';
%% non-date values
from_bin_(<<"direct">>) -> 'direct';
from_bin_(<<"forward">>) -> 'forward';
from_bin_(<<"gateway">>) -> 'gateway';
from_bin_(<<"none">>) -> 'none';
from_bin_(<<"stored">>) -> 'stored';
from_bin_(<<"any">>) -> 'any';
from_bin_(<<"exact">>) -> 'exact';
from_bin_(<<"other">>) -> 'other';
%% actions
from_bin_(<<"alert">>) -> 'alert';
from_bin_(<<"drop">>) -> 'drop';
from_bin_(<<"error">>) -> 'error';
from_bin_(<<"notify">>) -> 'notify';
%% amp error types
from_bin_(<<"unsupported-actions">>) -> 'unsupported-actions';
from_bin_(<<"unsupported-conditions">>) -> 'unsupported-conditions'.
