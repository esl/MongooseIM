%% @doc MongooseIM/Ejabberd module for (a subset of) XEP-0079 support.
%% This work was sponsored by Grindr LLC
%% @reference <a href="http://xmpp.org/extensions/xep-0079.html">XEP-0079</a>
%% @author <mongooseim@erlang-solutions.com>
%% @copyright 2014 Erlang Solutions, Ltd.
-module(mod_amp).

-behavior(gen_mod).
-behaviour(mongoose_module_metrics).
-xep([{xep, 79}, {version, "1.2"}, {comment, "partially implemented."}]).
-export([start/2, stop/1]).
-export([check_packet/2,
         check_packet/3,
         add_local_features/5,
         add_stream_feature/2,
         run_initial_check/2,
         amp_check_packet/3,
         strip_amp_el_from_request/1
        ]).

-include("amp.hrl").
-include("mongoose.hrl").
-include("jlib.hrl").
-include("ejabberd_c2s.hrl").

-define(AMP_FEATURE,
        #xmlel{name = <<"amp">>, attrs = [{<<"xmlns">>, ?NS_AMP_FEATURE}]}).
-define(AMP_RESOLVER, amp_resolver).
-define(AMP_STRATEGY, amp_strategy).

start(Host, _Opts) ->
    mod_disco:register_feature(Host, ?NS_AMP),
    ejabberd_hooks:add(hooks(Host)).

stop(Host) ->
    ejabberd_hooks:delete(hooks(Host)),
    mod_disco:unregister_feature(Host, ?NS_AMP).

hooks(Host) ->
    [{c2s_stream_features, Host, ?MODULE, add_stream_feature, 50},
     {disco_local_features, Host, ?MODULE, add_local_features, 99},
     {c2s_preprocessing_hook, Host, ?MODULE, run_initial_check, 10},
     {amp_check_packet, Host, ?MODULE, amp_check_packet, 10},
     {amp_verify_support, Host, ?AMP_RESOLVER, verify_support, 10},
     {amp_check_condition, Host, ?AMP_RESOLVER, check_condition, 10},
     {amp_determine_strategy, Host, ?AMP_STRATEGY, determine_strategy, 10}].
%% Business API


-spec run_initial_check(mongoose_acc:t(), ejabberd_c2s:state()) -> mongoose_acc:t().
run_initial_check(#{result := drop} = Acc, _C2SState) ->
    Acc;
run_initial_check(Acc, _C2SState) ->
    Acc1 = mod_amp:check_packet(Acc, mongoose_acc:from_jid(Acc), initial_check),
    case mongoose_acc:get(amp, check_result, ok, Acc1) of
        drop -> mongoose_acc:set(hook, result, drop, Acc1);
        _ -> Acc1
    end.


-spec check_packet(mongoose_acc:t() | exml:element(), amp_event()) ->
    mongoose_acc:t() | exml:element().
check_packet(Packet = #xmlel{attrs = Attrs}, Event) ->
    % it is called this way only from ejabberd_c2s:send_and_maybe_buffer_stanza/3, line 1666
    % maybe Paweł Chrząszcz knows why and can advise what to do about it
    case xml:get_attr(<<"from">>, Attrs) of
        {value, From} ->
            check_packet(Packet, jid:from_binary(From), Event);
        _ ->
            Packet
    end;
check_packet(Acc, Event) ->
    check_packet(Acc, mongoose_acc:from_jid(Acc), Event).

-spec check_packet(exml:element()|mongoose_acc:t(), jid:jid(), amp_event()) ->
    exml:element() | mongoose_acc:t().
check_packet(Packet = #xmlel{name = <<"message">>}, From, Event) ->
    Acc = mongoose_acc:new(#{ location => ?LOCATION,
                              lserver => From#jid.lserver,
                              from_jid => From,
                              element => Packet }),
    mongoose_acc:element(check_packet(Acc, From, Event));
check_packet(Packet = #xmlel{}, _, _) ->
    Packet;
check_packet(Acc, #jid{lserver = Host} = From, Event) ->
    case mongoose_acc:stanza_name(Acc) of
        <<"message">> ->
            % this hook replaces original element with something modified by amp
            % which is a hack, but since we have accumulator here we have a chance
            % to fix implementation
            mongoose_hooks:amp_check_packet(Host, Acc, From, Event);
        _ ->
            Acc
    end.

-spec add_local_features(Acc :: {result, [exml:element()]} | empty | {error, any()},
                         From :: jid:jid(),
                         To :: jid:jid(),
                         NS :: binary(),
                         ejabberd:lang()) -> {result, [exml:element()]} | {error, any()}.
add_local_features(Acc, _From, _To, ?NS_AMP, _Lang) ->
    Features = result_or(Acc, []) ++ amp_features(),
    {result, Features};
add_local_features(Acc, _From, _To, _NS, _Lang) ->
    Acc.

add_stream_feature(Acc, _Host) ->
    lists:keystore(<<"amp">>, #xmlel.name, Acc, ?AMP_FEATURE).

-spec amp_check_packet(mongoose_acc:t(), jid:jid(), amp_event()) -> mongoose_acc:t().
amp_check_packet(Acc, From, Event) ->
    Res = mongoose_acc:get(amp, check_result, ok, Acc),
    amp_check_packet(Res, mongoose_acc:stanza_name(Acc), Acc, From, Event).

amp_check_packet(drop, _, Acc, _, _) ->
    Acc;
amp_check_packet(_, <<"message">>, Acc, From, Event) ->
    Packet = mongoose_acc:element(Acc),
    Res = case amp:extract_requested_rules(Packet) of
              none                    -> Packet;
              {rules, Rules}          -> process_amp_rules(Packet, From, Event, Rules);
              {errors, Errors}        -> send_errors_and_drop(Packet, From, Errors)
          end,
    case Res of
        drop ->
            mongoose_acc:set(amp, check_result, drop, Acc);
        NewElem ->
            mongoose_acc:update_stanza(#{ element => NewElem,
                                          from_jid => From,
                                          to_jid => mongoose_acc:to_jid(Acc) }, Acc)
    end;
amp_check_packet(_, _, Acc, _, _) ->
    Acc.

strip_amp_el_from_request(Packet) ->
    % this will probably be removed - we have accumulator so we won't need anymore to store amp
    % markers in stanza
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

-spec process_amp_rules(exml:element(), jid:jid(), amp_event(), amp_rules()) -> exml:element() | drop.
process_amp_rules(Packet, From, Event, Rules) ->
    VerifiedRules = verify_support(host(From), Rules),
    {Good, Bad} = lists:partition(fun is_supported_rule/1, VerifiedRules),
    ValidRules = [ Rule || {supported, Rule} <- Good ],
    case Bad of
        [{error, ValidationError, InvalidRule} | _] ->
            send_error_and_drop(Packet, From, ValidationError, InvalidRule);
        [] ->
            Strategy = determine_strategy(Packet, From, Event),
            RulesWithResults = apply_rules(fun(Rule) ->
                                                   resolve_condition(From, Strategy, Event, Rule)
                                           end, ValidRules),
            PacketResult = take_action(Packet, From, RulesWithResults),
            return_result(PacketResult, Packet, RulesWithResults)
    end.

%% @doc ejabberd_hooks helpers
-spec verify_support(binary(), amp_rules()) -> [amp_rule_support()].
verify_support(Host, Rules) ->
    mongoose_hooks:amp_verify_support(Host, [], Rules).

-spec determine_strategy(exml:element(), jid:jid(), amp_event()) -> amp_strategy().
determine_strategy(Packet, From, Event) ->
    To = message_target(Packet),
    mongoose_hooks:amp_determine_strategy(host(From),
                                          amp_strategy:null_strategy(),
                                          From, To, Packet, Event).

apply_rules(F, Rules) ->
    [Rule#amp_rule{result = F(Rule)} || Rule <- Rules].

-spec resolve_condition(jid:jid(), amp_strategy(), amp_event(), amp_rule()) -> amp_match_result().
resolve_condition(From, Strategy, Event, Rule) ->
    Result = mongoose_hooks:amp_check_condition(host(From), no_match, Strategy, Rule),
    match_undecided_for_final_event(Rule, Event, Result).

match_undecided_for_final_event(#amp_rule{condition = deliver}, Event, undecided)
  when Event =:= delivered;
       Event =:= delivery_failed -> match;
match_undecided_for_final_event(_, _, Result) -> Result.

take_action(Packet, From, Rules) ->
    case find(fun(#amp_rule{result = Result}) -> Result =:= match end, Rules) of
        not_found -> pass;
        {found, Rule} -> take_action_for_matched_rule(Packet, From, Rule)
    end.

return_result(drop, _Packet, _Rules) -> drop;
return_result(pass, Packet, Rules) ->
    update_rules(Packet, lists:filter(fun(#amp_rule{result = Result}) ->
                                              Result =:= undecided
                                      end, Rules)).

update_rules(Packet, []) -> amp:strip_amp_el(Packet);
update_rules(Packet, Rules) -> amp:update_rules(Packet, Rules).

-spec take_action_for_matched_rule(exml:element(), jid:jid(), amp_rule()) -> pass | drop.
take_action_for_matched_rule(Packet, From, #amp_rule{action = notify} = Rule) ->
    Host = host(From),
    reply_to_sender(Rule, server_jid(From), From, Packet),
    mongoose_hooks:amp_notify_action_triggered(Host, ok),
    pass;
take_action_for_matched_rule(Packet, From, #amp_rule{action = error} = Rule) ->
    send_error_and_drop(Packet, From, 'undefined-condition', Rule);
take_action_for_matched_rule(Packet, From, #amp_rule{action = drop}) ->
    update_metric_and_drop(Packet, From).

-spec reply_to_sender(amp_rule(), jid:jid(), jid:jid(), exml:element()) -> mongoose_acc:t().
reply_to_sender(MatchedRule, ServerJid, OriginalSender, OriginalPacket) ->
    Response = amp:make_response(MatchedRule, OriginalSender, OriginalPacket),
    ejabberd_router:route(ServerJid, OriginalSender, Response).

-spec send_error_and_drop(exml:element(), jid:jid(), amp_error(), amp_rule()) -> drop.
send_error_and_drop(Packet, From, AmpError, MatchedRule) ->
    send_errors_and_drop(Packet, From, [{AmpError, MatchedRule}]).

-spec send_errors_and_drop(exml:element(), jid:jid(), [{amp_error(), amp_rule()}]) -> drop.
send_errors_and_drop(Packet, From, []) ->
    ?ERROR_MSG("~p from ~p generated an empty list of errors. This shouldn't happen!",
               [Packet, From]),
    update_metric_and_drop(Packet, From);
send_errors_and_drop(Packet, From, ErrorRules) ->
    Host = host(From),
    {Errors, Rules} = lists:unzip(ErrorRules),
    ErrorResponse = amp:make_error_response(Errors, Rules, From, Packet),
    ejabberd_router:route(server_jid(From), From, ErrorResponse),
    mongoose_hooks:amp_error_action_triggered(Host, ok),
    update_metric_and_drop(Packet, From).

-spec update_metric_and_drop(exml:element(), jid:jid()) -> drop.
update_metric_and_drop(Packet, From) ->
    mongoose_hooks:xmpp_stanza_dropped(host(From),
                                       ok,
                                       From, message_target(Packet), Packet),
    drop.

%% Internal
result_or({result, I}, _) -> I;
result_or(_, Or)         -> Or.

-spec is_supported_rule(amp_rule_support()) -> boolean().
is_supported_rule({supported, _}) -> true;
is_supported_rule(_)              -> false.

-spec host(jid:jid()) -> binary().
host(#jid{lserver=Host}) -> Host.

server_jid(#jid{lserver = Host}) ->
    jid:from_binary(Host).

-spec message_target(exml:element()) -> jid:jid() | undefined.
message_target(El) ->
    case exml_query:attr(El, <<"to">>) of
        undefined -> undefined;
        J -> jid:from_binary(J)
    end.

find(_Pred, []) -> not_found;
find(Pred, [H|T]) ->
    case Pred(H) of
        true -> {found, H};
        false -> find(Pred, T)
    end.
