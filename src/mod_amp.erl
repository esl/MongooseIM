%% @doc MongooseIM/Ejabberd module for (a subset of) XEP-0079 support.
%% This work was sponsored by Grindr LLC
%% @reference <a href="http://xmpp.org/extensions/xep-0079.html">XEP-0079</a>
%% @author <mongooseim@erlang-solutions.com>
%% @copyright 2014 Erlang Solutions, Ltd.
-module(mod_amp).

-behavior(gen_mod).
-behaviour(mongoose_module_metrics).
-xep([{xep, 79}, {version, "1.2"}, {comment, "partially implemented."}]).
-export([start/2, stop/1, supported_features/0]).
-export([run_initial_check/3,
         check_packet/2,
         disco_local_features/3,
         c2s_stream_features/3,
         xmpp_send_element/2]).

-ignore_xref([c2s_stream_features/3, xmpp_send_element/2]).

-include("amp.hrl").
-include("mongoose.hrl").
-include("jlib.hrl").

-define(AMP_FEATURE,
        #xmlel{name = <<"amp">>, attrs = [{<<"xmlns">>, ?NS_AMP_FEATURE}]}).
-define(AMP_RESOLVER, amp_resolver).
-define(AMP_STRATEGY, amp_strategy).

-spec start(mongooseim:host_type(), gen_mod:module_opts()) -> ok.
start(HostType, _Opts) ->
    ejabberd_hooks:add(legacy_hooks(HostType)),
    gen_hook:add_handlers(hooks(HostType)).

-spec stop(mongooseim:host_type()) -> ok.
stop(HostType) ->
    gen_hook:delete_handlers(hooks(HostType)),
    ejabberd_hooks:delete(legacy_hooks(HostType)).

-spec supported_features() -> [atom()].
supported_features() -> [dynamic_domains].

-spec legacy_hooks(mongooseim:host_type()) -> [ejabberd_hooks:hook()].
legacy_hooks(HostType) ->
    [{c2s_stream_features, HostType, ?MODULE, c2s_stream_features, 50},
     {amp_verify_support, HostType, ?AMP_RESOLVER, verify_support, 10},
     {amp_check_condition, HostType, ?AMP_RESOLVER, check_condition, 10},
     {amp_determine_strategy, HostType, ?AMP_STRATEGY, determine_strategy, 10},
     {xmpp_send_element, HostType, ?MODULE, xmpp_send_element, 10}].

-spec hooks(mongooseim:host_type()) -> gen_hook:hook_list().
hooks(HostType) ->
    [{disco_local_features, HostType, fun ?MODULE:disco_local_features/3, #{}, 99}
     | c2s_hooks(HostType)].

-spec c2s_hooks(mongooseim:host_type()) -> gen_hook:hook_list(mongoose_c2s_hooks:hook_fn()).
c2s_hooks(HostType) ->
    [{c2s_preprocessing_hook, HostType, fun ?MODULE:run_initial_check/3, #{}, 10}].

%% API

-spec run_initial_check(mongoose_acc:t(), mongoose_c2s:hook_params(), gen_hook:extra()) ->
    mongoose_c2s_hooks:hook_result().
run_initial_check(#{result := drop} = Acc, _Params, _Extra) ->
    {ok, Acc};
run_initial_check(Acc, _Params, _Extra) ->
    case mongoose_acc:stanza_name(Acc) of
        <<"message">> -> {ok, run_initial_check(Acc)};
        _ -> {ok, Acc}
    end.

-spec check_packet(mongoose_acc:t(), amp_event()) -> mongoose_acc:t().
check_packet(Acc, Event) ->
    case mongoose_acc:get(amp, rules, none, Acc) of
        none -> Acc;
        Rules -> process_event(Acc, Rules, Event)
    end.

-spec disco_local_features(mongoose_disco:feature_acc(),
                           map(),
                           map()) -> {ok, mongoose_disco:feature_acc()}.
disco_local_features(Acc = #{node := Node}, _, _) ->
    NewAcc = case amp_features(Node) of
        [] -> Acc;
        Features -> mongoose_disco:add_features(Features, Acc)
    end,
    {ok, NewAcc}.

-spec c2s_stream_features([exml:element()], mongooseim:host_type(), jid:lserver()) ->
          [exml:element()].
c2s_stream_features(Acc, _HostType, _Lserver) ->
    lists:keystore(<<"amp">>, #xmlel.name, Acc, ?AMP_FEATURE).

%% Internal

-spec run_initial_check(mongoose_acc:t()) -> mongoose_acc:t().
run_initial_check(Acc) ->
    Packet = mongoose_acc:element(Acc),
    From = mongoose_acc:from_jid(Acc),
    To = mongoose_acc:to_jid(Acc),
    Result = case amp:extract_requested_rules(Packet) of
                 none -> nothing_to_do;
                 {rules, Rules} -> validate_and_process_rules(Packet, From, Rules, Acc);
                 {errors, Errors} -> send_errors_and_drop(Packet, From, Errors, Acc)
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

-spec validate_and_process_rules(exml:element(), jid:jid(), amp_rules(), mongoose_acc:t()) ->
          amp_rules() | drop.
validate_and_process_rules(Packet, From, Rules, Acc) ->
    VerifiedRules = verify_support(mongoose_acc:host_type(Acc), Rules),
    {Good, Bad} = lists:partition(fun is_supported_rule/1, VerifiedRules),
    ValidRules = [ Rule || {supported, Rule} <- Good ],
    case Bad of
        [{error, ValidationError, InvalidRule} | _] ->
            send_error_and_drop(Packet, From, ValidationError, InvalidRule, Acc);
        [] ->
            process_rules(Packet, From, initial_check, ValidRules, Acc)
    end.

-spec process_event(mongoose_acc:t(), amp_rules(), amp_event()) -> mongoose_acc:t().
process_event(Acc, Rules, Event) when Event =/= initial_check ->
    Packet = mongoose_acc:element(Acc),
    From = mongoose_acc:from_jid(Acc),
    NewRules = process_rules(Packet, From, Event, Rules, Acc),
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

-spec process_rules(exml:element(), jid:jid(), amp_event(), amp_rules(), mongoose_acc:t()) ->
          amp_rules() | drop.
process_rules(Packet, From, Event, Rules, Acc) ->
    HostType = mongoose_acc:host_type(Acc),
    To = mongoose_acc:to_jid(Acc),
    Strategy = determine_strategy(HostType, Packet, From, To, Event),
    RulesWithResults = apply_rules(fun(Rule) ->
                                           resolve_condition(HostType, Strategy, Event, Rule)
                                   end, Rules),
    PacketResult = take_action(Packet, From, RulesWithResults, Acc),
    return_result(PacketResult, Event, RulesWithResults).

%% @doc hooks helpers
-spec verify_support(mongooseim:host_type(), amp_rules()) -> [amp_rule_support()].
verify_support(HostType, Rules) ->
    mongoose_hooks:amp_verify_support(HostType, Rules).

-spec determine_strategy(mongooseim:host_type(), exml:element(), jid:jid(), jid:jid(),
                         amp_event()) ->
          amp_strategy().
determine_strategy(HostType, Packet, From, To, Event) ->
    mongoose_hooks:amp_determine_strategy(HostType, From, To, Packet, Event).

apply_rules(F, Rules) ->
    [Rule#amp_rule{result = F(Rule)} || Rule <- Rules].

-spec resolve_condition(mongooseim:host_type(), amp_strategy(), amp_event(), amp_rule()) ->
          amp_match_result().
resolve_condition(HostType, Strategy, Event, Rule) ->
    Result = mongoose_hooks:amp_check_condition(HostType, Strategy, Rule),
    match_undecided_for_final_event(Rule, Event, Result).

match_undecided_for_final_event(#amp_rule{condition = deliver}, Event, undecided)
  when Event =:= delivered;
       Event =:= delivery_failed -> match;
match_undecided_for_final_event(_, _, Result) -> Result.

-spec take_action(exml:element(), jid:jid(), amp_rules(), mongoose_acc:t()) -> pass | drop.
take_action(Packet, From, Rules, Acc) ->
    case find(fun(#amp_rule{result = Result}) -> Result =:= match end, Rules) of
        not_found -> pass;
        {found, Rule} -> take_action_for_matched_rule(Packet, From, Rule, Acc)
    end.

return_result(drop, initial_check, _Rules) -> drop;
return_result(pass, _Event, Rules) ->
    lists:filter(fun(#amp_rule{result = Result}) ->
                         Result =:= undecided
                 end, Rules).

-spec take_action_for_matched_rule(exml:element(), jid:jid(), amp_rule(), mongoose_acc:t()) ->
          pass | drop.
take_action_for_matched_rule(Packet, From, #amp_rule{action = notify} = Rule, _Acc) ->
    reply_to_sender(Rule, server_jid(From), From, Packet),
    pass;
take_action_for_matched_rule(Packet, From, #amp_rule{action = error} = Rule, Acc) ->
    send_error_and_drop(Packet, From, 'undefined-condition', Rule, Acc);
take_action_for_matched_rule(Packet, From, #amp_rule{action = drop}, Acc) ->
    update_metric_and_drop(Packet, From, Acc).

-spec reply_to_sender(amp_rule(), jid:jid(), jid:jid(), exml:element()) -> mongoose_acc:t().
reply_to_sender(MatchedRule, ServerJid, OriginalSender, OriginalPacket) ->
    Response = amp:make_response(MatchedRule, OriginalSender, OriginalPacket),
    ejabberd_router:route(ServerJid, OriginalSender, Response).

-spec send_error_and_drop(exml:element(), jid:jid(), amp_error(), amp_rule(), mongoose_acc:t()) -> drop.
send_error_and_drop(Packet, From, AmpError, MatchedRule, Acc) ->
    send_errors_and_drop(Packet, From, [{AmpError, MatchedRule}], Acc).

-spec send_errors_and_drop(exml:element(), jid:jid(), [{amp_error(), amp_rule()}], mongoose_acc:t()) -> drop.
send_errors_and_drop(Packet, From, [], Acc) ->
    ?LOG_ERROR(#{what => empty_list_of_errors_generated,
                 text => <<"This shouldn't happen!">>,
                 exml_packet => Packet, from => From}),
    update_metric_and_drop(Packet, From, Acc);
send_errors_and_drop(Packet, From, ErrorRules, Acc) ->
    {Errors, Rules} = lists:unzip(ErrorRules),
    ErrorResponse = amp:make_error_response(Errors, Rules, From, Packet),
    ejabberd_router:route(server_jid(From), From, ErrorResponse),
    update_metric_and_drop(Packet, From, Acc).

-spec update_metric_and_drop(exml:element(), jid:jid(), mongoose_acc:t()) -> drop.
update_metric_and_drop(Packet, From, Acc) ->
    mongoose_hooks:xmpp_stanza_dropped(Acc, From, mongoose_acc:to_jid(Acc), Packet),
    drop.

-spec is_supported_rule(amp_rule_support()) -> boolean().
is_supported_rule({supported, _}) -> true;
is_supported_rule(_)              -> false.

server_jid(#jid{lserver = Host}) ->
    jid:from_binary(Host).

find(_Pred, []) -> not_found;
find(Pred, [H|T]) ->
    case Pred(H) of
        true -> {found, H};
        false -> find(Pred, T)
    end.

-spec xmpp_send_element(mongoose_acc:t(), exml:element()) -> mongoose_acc:t().
xmpp_send_element(Acc, _El) ->
    Event = case mongoose_acc:get(c2s, send_result, undefined, Acc) of
                ok -> delivered;
                _ -> delivery_failed
            end,
    check_packet(Acc, Event).
