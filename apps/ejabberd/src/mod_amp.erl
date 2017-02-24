-module(mod_amp).
%% @doc MongooseIM/Ejabberd module for (a subset of) XEP-0079 support.
%% @reference <a href="http://xmpp.org/extensions/xep-0079.html">XEP-0079</a>
%% @author <mongooseim@erlang-solutions.com>
%% @copyright 2014 Erlang Solutions, Ltd.
%% This work was sponsored by Grindr LLC

-behavior(gen_mod).
-xep([{xep, 79}, {version, "1.2"}, {comment, "partially implemented."}]).
-export([start/2, stop/1]).
-export([check_packet/2,
         check_packet/3,
         add_local_features/5,
         add_stream_feature/2,
         amp_check_packet/3,
         strip_amp_el_from_request/1
        ]).

-include_lib("ejabberd/include/amp.hrl").
-include_lib("ejabberd/include/ejabberd.hrl").
-include_lib("ejabberd/include/jlib.hrl").

-define(AMP_FEATURE,
        #xmlel{name = <<"amp">>, attrs = [{<<"xmlns">>, ?NS_AMP_FEATURE}]}).
-define(AMP_RESOLVER, amp_resolver).
-define(AMP_STRATEGY, amp_strategy).

start(Host, _Opts) ->
    mod_disco:register_feature(Host, ?NS_AMP),
    ejabberd_hooks:add(c2s_stream_features, Host, ?MODULE, add_stream_feature, 50),
    ejabberd_hooks:add(disco_local_features, Host, ?MODULE, add_local_features, 99),
    ejabberd_hooks:add(amp_check_packet, Host, ?MODULE, amp_check_packet, 10),
    ejabberd_hooks:add(amp_verify_support, Host, ?AMP_RESOLVER, verify_support, 10),
    ejabberd_hooks:add(amp_check_condition, Host, ?AMP_RESOLVER, check_condition, 10),
    ejabberd_hooks:add(amp_determine_strategy, Host, ?AMP_STRATEGY, determine_strategy, 10).

stop(Host) ->
    ejabberd_hooks:delete(amp_determine_strategy, Host, ?AMP_STRATEGY, determine_strategy, 10),
    ejabberd_hooks:delete(amp_check_condition, Host, ?AMP_RESOLVER, check_condition, 10),
    ejabberd_hooks:delete(amp_verify_support, Host, ?AMP_RESOLVER, verify_support, 10),
    ejabberd_hooks:delete(amp_check_packet, Host, ?MODULE, amp_check_packet, 10),
    ejabberd_hooks:delete(disco_local_features, Host, ?MODULE, add_local_features, 99),
    ejabberd_hooks:delete(c2s_stream_features, Host, ?MODULE, add_stream_feature, 50),
    mod_disco:unregister_feature(Host, ?NS_AMP).

%% Business API

-spec check_packet(mongoose_acc:t() | exml:element(), amp_event()) ->
    mongoose_acc:t() | exml:element() | drop.
check_packet(Packet = #xmlel{attrs = Attrs}, Event) ->
    % it is called this way only from ejabberd_c2s:send_and_maybe_buffer_stanza/3, line 1666
    % maybe Paweł Chrząszcz knows why and can advise what to do about it
    ?DEPRECATED,
    case xml:get_attr(<<"from">>, Attrs) of
        {value, From} ->
            check_packet(Packet, jid:from_binary(From), Event);
        _ ->
            Packet
    end;
check_packet(Acc, Event) ->
    check_packet(Acc, mongoose_acc:get(from_jid, Acc), Event).

-spec check_packet(exml:element()|mongoose_acc:t(), jid(), amp_event()) ->
    exml:element() | mongoose_acc:t() | drop.
check_packet(Packet = #xmlel{name = <<"message">>}, From, Event) ->
    ?DEPRECATED,
    mongoose_acc:get(element, check_packet(mongoose_acc:from_element(Packet), From, Event));
check_packet(Packet = #xmlel{}, _, _) ->
    Packet;
check_packet(Acc, #jid{lserver = Host} = From, Event) ->
    case mongoose_acc:get(name, Acc) of
        <<"message">> ->
            % this hook replaces original element with something modified by amp
            % which is a hack, but since we have accumulator here we have a chance
            % to fix implementation
            ejabberd_hooks:run_fold(amp_check_packet, Host, Acc, [From, Event]);
        _ ->
            Acc
    end.

add_local_features(Acc, _From, _To, ?NS_AMP, _Lang) ->
    Features = result_or(Acc, []) ++ amp_features(),
    {result, Features};
add_local_features(Acc, _From, _To, _NS, _Lang) ->
    Acc.

add_stream_feature(Acc, _Host) ->
    lists:keystore(<<"amp">>, #xmlel.name, Acc, ?AMP_FEATURE).

-spec amp_check_packet(mongoose_acc:t(), jid(), amp_event()) -> mongoose_acc:t().
amp_check_packet(Acc, From, Event) ->
    Res = mongoose_acc:get(amp_check_result, Acc, ok),
    Name = mongoose_acc:get(name, Acc),
    amp_check_packet(Res, Name, Acc, From, Event).

amp_check_packet(drop, _, Acc, _, _) ->
    Acc;
amp_check_packet(_, <<"message">>, Acc, From, Event) ->
    Packet = mongoose_acc:get(element, Acc),
    Res = case amp:extract_requested_rules(Packet) of
              none                    -> Packet;
              {rules, Rules}          -> process_amp_rules(Packet, From, Event, Rules);
              {errors, Errors}        -> send_errors_and_drop(Packet, From, Errors)
          end,
    case Res of
        drop ->
            mongoose_acc:put(amp_check_result, drop, Acc);
        NewElem ->
            mongoose_acc:put(element, NewElem, Acc)
    end;
amp_check_packet(_, _, Acc, _, _) ->
    Acc.

strip_amp_el_from_request(Packet) ->
    case amp:is_amp_request(Packet) of
        true -> amp:strip_amp_el(Packet);
        false -> Packet
    end.

%% @doc This may eventually be configurable, but for now we return a constant list.
amp_features() ->
    [<<"http://jabber.org/protocol/amp">>
   , <<"http://jabber.org/protocol/amp?action=notify">>
   , <<"http://jabber.org/protocol/amp?action=error">>
   , <<"http://jabber.org/protocol/amp?condition=deliver">>
   , <<"http://jabber.org/protocol/amp?condition=match-resource">>
    ].

-spec process_amp_rules(exml:element(), jid(), amp_event(), amp_rules()) -> exml:element() | drop.
process_amp_rules(Packet, From, Event, Rules) ->
    VerifiedRules = verify_support(host(From), Rules),
    {Good, Bad} = lists:partition(fun is_supported_rule/1, VerifiedRules),
    ValidRules = [ Rule || {supported, Rule} <- Good ],
    case Bad of
        [{error, ValidationError, InvalidRule} | _] ->
            send_error_and_drop(Packet, From, ValidationError, InvalidRule);
        [] ->
            Strategy = determine_strategy(Packet, From, Event),
            ApplyResult = fold_apply_rules(Packet, From, Strategy, ValidRules),
            take_action(Packet, From, Event, ApplyResult)
    end.

%% @doc ejabberd_hooks helpers
-spec verify_support(binary(), amp_rules()) -> [amp_rule_support()].
verify_support(Host, Rules) ->
    ejabberd_hooks:run_fold(amp_verify_support, Host, [], [Rules]).

-spec determine_strategy(exml:element(), jid(), amp_event()) -> amp_strategy().
determine_strategy(Packet, From, Event) ->
    To = message_target(Packet),
    ejabberd_hooks:run_fold(amp_determine_strategy, host(From),
                            amp_strategy:null_strategy(), [From, To, Packet, Event]).

-spec fold_apply_rules(exml:element(), jid(), amp_strategy(), [amp_rule()]) ->
                              no_match | {matched | undecided, amp_rule()}.
fold_apply_rules(_, _, _, []) -> 'no_match';
fold_apply_rules(Packet, From, Strategy, [Rule|Rest]) ->
    case resolve_condition(From, Strategy, Rule) of
        no_match -> fold_apply_rules(Packet, From, Strategy, Rest);
        Status -> {Status, Rule}
    end.

-spec resolve_condition(jid(), amp_strategy(), amp_rule()) -> amp_match_result().
resolve_condition(From, Strategy, Rule) ->
    ejabberd_hooks:run_fold
      (amp_check_condition, host(From), no_match,
       [Strategy, Rule]).

-spec take_action(exml:element(),
                  jid(),
                  amp_event(),
                  no_match | {matched | undecided, amp_rule()}) ->
    exml:element() | drop.
take_action(Packet, _From, initial_check, no_match) ->
    amp:strip_amp_el(Packet);
take_action(Packet, From, _, {match, Rule}) ->
    take_action_for_matched_rule(Packet, From, Rule);
take_action(Packet, _From, _, _) ->
    Packet.

-spec take_action_for_matched_rule(exml:element(), jid(), amp_rule()) -> exml:element() | drop.
take_action_for_matched_rule(Packet, From, #amp_rule{action = notify} = Rule) ->
    Host = host(From),
    reply_to_sender(Rule, server_jid(From), From, Packet),
    ejabberd_hooks:run(amp_notify_action_triggered, Host, [Host]),
    amp:strip_amp_el(Packet);
take_action_for_matched_rule(Packet, From, #amp_rule{action = error} = Rule) ->
    send_error_and_drop(Packet, From, 'undefined-condition', Rule);
take_action_for_matched_rule(Packet, From, #amp_rule{action = drop}) ->
    update_metric_and_drop(Packet, From).

-spec reply_to_sender(amp_rule(), jid(), jid(), exml:element()) -> ok.
reply_to_sender(MatchedRule, ServerJid, OriginalSender, OriginalPacket) ->
    Response = amp:make_response(MatchedRule, OriginalSender, OriginalPacket),
    ejabberd_router:route(ServerJid, OriginalSender, Response).

-spec send_error_and_drop(exml:element(), jid(), amp_error(), amp_rule()) -> drop.
send_error_and_drop(Packet, From, AmpError, MatchedRule) ->
    send_errors_and_drop(Packet, From, [{AmpError, MatchedRule}]).

-spec send_errors_and_drop(exml:element(), jid(), [{amp_error(), amp_rule()}]) -> drop.
send_errors_and_drop(Packet, From, []) ->
    ?ERROR_MSG("~p from ~p generated an empty list of errors. This shouldn't happen!",
               [Packet, From]),
    update_metric_and_drop(Packet, From);
send_errors_and_drop(Packet, From, ErrorRules) ->
    Host = host(From),
    {Errors, Rules} = lists:unzip(ErrorRules),
    ErrorResponse = amp:make_error_response(Errors, Rules, From, Packet),
    ejabberd_router:route(server_jid(From), From, ErrorResponse),
    ejabberd_hooks:run(amp_error_action_triggered, Host, [Host]),
    update_metric_and_drop(Packet, From).

-spec update_metric_and_drop(exml:element(), jid()) -> drop.
update_metric_and_drop(Packet, From) ->
    ejabberd_hooks:run(xmpp_stanza_dropped, host(From),
                       [From, message_target(Packet), Packet]),
    drop.

%% Internal
result_or({result, I}, _) -> I;
result_or(_, Or)         -> Or.

-spec is_supported_rule(amp_rule_support()) -> boolean().
is_supported_rule({supported, _}) -> true;
is_supported_rule(_)              -> false.

-spec host(jid()) -> binary().
host(#jid{lserver=Host}) -> Host.

server_jid(#jid{lserver = Host}) ->
    jid:from_binary(Host).

-spec message_target(exml:element()) -> jid() | undefined.
message_target(El) ->
    case exml_query:attr(El, <<"to">>) of
        undefined -> undefined;
        J -> jid:from_binary(J)
    end.
