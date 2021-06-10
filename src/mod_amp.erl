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
-export([run_initial_check/2,
         check_packet/2,
         disco_local_features/1,
         add_stream_feature/2
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
    ejabberd_hooks:add(hooks(Host)).

stop(Host) ->
    ejabberd_hooks:delete(hooks(Host)).

hooks(Host) ->
    [{c2s_stream_features, Host, ?MODULE, add_stream_feature, 50},
     {disco_local_features, Host, ?MODULE, disco_local_features, 99},
     {c2s_preprocessing_hook, Host, ?MODULE, run_initial_check, 10},
     {amp_verify_support, Host, ?AMP_RESOLVER, verify_support, 10},
     {amp_check_condition, Host, ?AMP_RESOLVER, check_condition, 10},
     {amp_determine_strategy, Host, ?AMP_STRATEGY, determine_strategy, 10}].

%% API

-spec run_initial_check(mongoose_acc:t(), ejabberd_c2s:state()) -> mongoose_acc:t().
run_initial_check(#{result := drop} = Acc, _C2SState) ->
    Acc;
run_initial_check(Acc, _C2SState) ->
    case mongoose_acc:stanza_name(Acc) of
        <<"message">> -> run_initial_check(Acc);
        _ -> Acc
    end.

-spec check_packet(mongoose_acc:t(), amp_event()) -> mongoose_acc:t().
check_packet(Acc, Event) ->
    case mongoose_acc:get(amp, rules, none, Acc) of
        none -> Acc;
        Rules -> process_event(Acc, Rules, Event)
    end.

-spec disco_local_features(mongoose_disco:feature_acc()) -> mongoose_disco:feature_acc().
disco_local_features(Acc = #{node := Node}) ->
    case amp_features(Node) of
        [] -> Acc;
        Features -> mongoose_disco:add_features(Features, Acc)
    end.

add_stream_feature(Acc, _Host) ->
    lists:keystore(<<"amp">>, #xmlel.name, Acc, ?AMP_FEATURE).

%% Internal

-spec run_initial_check(mongoose_acc:t()) -> mongoose_acc:t().
run_initial_check(Acc) ->
    Packet = mongoose_acc:element(Acc),
    From = mongoose_acc:from_jid(Acc),
    To = mongoose_acc:to_jid(Acc),
    Result = case amp:extract_requested_rules(Packet) of
                 none -> nothing_to_do;
                 {rules, Rules} -> validate_and_process_rules(Packet, From, Rules);
                 {errors, Errors} -> send_errors_and_drop(Packet, From, Errors)
             end,
    case Result of
        nothing_to_do ->
            Acc;
        drop ->
            mongoose_acc:set(hook, result, drop, Acc);
        NewRules ->
            Acc1 = mongoose_acc:set_permanent(amp, rules, NewRules, Acc),
            mongoose_acc:update_stanza(#{element => amp:strip_amp_el(Packet),
                                         from_jid => From,
                                         to_jid => To}, Acc1)
    end.

-spec validate_and_process_rules(exml:element(), jid:jid(), amp_rules()) -> amp_rules() | drop.
validate_and_process_rules(Packet, From, Rules) ->
    VerifiedRules = verify_support(host(From), Rules),
    {Good, Bad} = lists:partition(fun is_supported_rule/1, VerifiedRules),
    ValidRules = [ Rule || {supported, Rule} <- Good ],
    case Bad of
        [{error, ValidationError, InvalidRule} | _] ->
            send_error_and_drop(Packet, From, ValidationError, InvalidRule);
        [] ->
            process_rules(Packet, From, initial_check, ValidRules)
    end.

-spec process_event(mongoose_acc:t(), amp_rules(), amp_event()) -> mongoose_acc:t().
process_event(Acc, Rules, Event) when Event =/= initial_check ->
    Packet = mongoose_acc:element(Acc),
    From = mongoose_acc:from_jid(Acc),
    NewRules = process_rules(Packet, From, Event, Rules),
    mongoose_acc:set_permanent(amp, rules, NewRules, Acc).

-spec amp_features(binary()) -> [mongoose_disco:feature()].
amp_features(?NS_AMP) ->
    [<<?NS_AMP/binary, Suffix/binary>> || Suffix <- amp_feature_suffixes()];
amp_features(<<>>) ->
    [?NS_AMP];
amp_features(_) ->
    [].

%% @doc This may eventually be configurable, but for now we return a constant list.
amp_feature_suffixes() ->
    [<<>>,
     <<"?action=drop">>,
     <<"?action=notify">>,
     <<"?action=error">>,
     <<"?condition=deliver">>,
     <<"?condition=match-resource">>].

-spec process_rules(exml:element(), jid:jid(), amp_event(), amp_rules()) -> amp_rules() | drop.
process_rules(Packet, From, Event, Rules) ->
    Strategy = determine_strategy(Packet, From, Event),
    RulesWithResults = apply_rules(fun(Rule) ->
                                           resolve_condition(From, Strategy, Event, Rule)
                                   end, Rules),
    PacketResult = take_action(Packet, From, RulesWithResults),
    return_result(PacketResult, Event, RulesWithResults).

%% @doc hooks helpers
-spec verify_support(binary(), amp_rules()) -> [amp_rule_support()].
verify_support(Host, Rules) ->
    mongoose_hooks:amp_verify_support(Host, Rules).

-spec determine_strategy(exml:element(), jid:jid(), amp_event()) -> amp_strategy().
determine_strategy(Packet, From, Event) ->
    To = message_target(Packet),
    mongoose_hooks:amp_determine_strategy(host(From), From, To, Packet, Event).

apply_rules(F, Rules) ->
    [Rule#amp_rule{result = F(Rule)} || Rule <- Rules].

-spec resolve_condition(jid:jid(), amp_strategy(), amp_event(), amp_rule()) -> amp_match_result().
resolve_condition(From, Strategy, Event, Rule) ->
    Result = mongoose_hooks:amp_check_condition(host(From), Strategy, Rule),
    match_undecided_for_final_event(Rule, Event, Result).

match_undecided_for_final_event(#amp_rule{condition = deliver}, Event, undecided)
  when Event =:= delivered;
       Event =:= delivery_failed -> match;
match_undecided_for_final_event(_, _, Result) -> Result.

-spec take_action(exml:element(), jid:jid(), amp_rules()) -> pass | drop.
take_action(Packet, From, Rules) ->
    case find(fun(#amp_rule{result = Result}) -> Result =:= match end, Rules) of
        not_found -> pass;
        {found, Rule} -> take_action_for_matched_rule(Packet, From, Rule)
    end.

return_result(drop, initial_check, _Rules) -> drop;
return_result(pass, _Event, Rules) ->
    lists:filter(fun(#amp_rule{result = Result}) ->
                         Result =:= undecided
                 end, Rules).

-spec take_action_for_matched_rule(exml:element(), jid:jid(), amp_rule()) -> pass | drop.
take_action_for_matched_rule(Packet, From, #amp_rule{action = notify} = Rule) ->
    Host = host(From),
    reply_to_sender(Rule, server_jid(From), From, Packet),
    mongoose_hooks:amp_notify_action_triggered(Host),
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
    ?LOG_ERROR(#{what => empty_list_of_errors_generated,
                 text => <<"This shouldn't happen!">>,
                 exml_packet => Packet, from => From}),
    update_metric_and_drop(Packet, From);
send_errors_and_drop(Packet, From, ErrorRules) ->
    Host = host(From),
    {Errors, Rules} = lists:unzip(ErrorRules),
    ErrorResponse = amp:make_error_response(Errors, Rules, From, Packet),
    ejabberd_router:route(server_jid(From), From, ErrorResponse),
    mongoose_hooks:amp_error_action_triggered(Host),
    update_metric_and_drop(Packet, From).

-spec update_metric_and_drop(exml:element(), jid:jid()) -> drop.
update_metric_and_drop(Packet, From) ->
    mongoose_hooks:xmpp_stanza_dropped(host(From), From,
                                       message_target(Packet),
                                       Packet),
    drop.

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
