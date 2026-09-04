-module(mod_event_pusher_push_rules).
-moduledoc "Matches and executes configurable push notification rules".

-include("mongoose.hrl").
-include("mongoose_config_spec.hrl").
-include("mod_event_pusher_events.hrl").

-export([config_spec/0, push_event/4]).

-export([process_rule/1]).

-type event() :: #msg_event{} | #unack_msg_event{}.

-spec config_spec() -> mongoose_config_spec:config_list().
config_spec() ->
    #list{items = rule_config_spec(), validate = unique}.

-spec push_event(mongoose_acc:t(), event(), jid:jid(), [map()]) ->
          mongoose_acc:t().
push_event(Acc, Event, BareRecipient, Rules) ->
    perform_action(Acc, Event, BareRecipient, match_rule(Rules, Event)).

perform_action(Acc, _Event, _BareRecipient, #{action := skip}) ->
    Acc;
perform_action(Acc, Event, BareRecipient, #{action := push, content := ContentSpec} = Rule) ->
    case mod_event_pusher_push_content:build(ContentSpec, Acc) of
        {ok, Content} ->
            publish_content(Acc, BareRecipient, Content);
        {error, Reason} ->
            ?LOG_WARNING(#{what => mod_event_pusher_push_failed_to_build_content,
                           reason => Reason,
                           rule => Rule,
                           content_spec => ContentSpec,
                           event => Event}),
            Acc
    end.

-spec rule_config_spec() -> mongoose_config_spec:config_section().
rule_config_spec() ->
    #section{items = #{~"conditions" => conditions_config_spec(),
                       ~"action" => #option{type = atom,
                                            validate = {enum, [push, skip]}},
                       ~"content" => #option{type = atom,
                                             validate = {enum, [message, jingle]}}},
             defaults = #{~"conditions" => [#{}]},
             required = [~"action"],
             process = fun ?MODULE:process_rule/1}.

-spec conditions_config_spec() -> mongoose_config_spec:config_list().
conditions_config_spec() ->
    #list{items = condition_config_spec(), validate = unique_non_empty}.

-spec condition_config_spec() -> mongoose_config_spec:config_section().
condition_config_spec() ->
    #section{items = #{~"event" => #option{type = atom,
                                           validate = {enum, [msg, unack_msg]}},
                       ~"type" => #option{type = atom,
                                          validate = {enum, [chat, groupchat]}},
                       ~"body" => #option{type = atom,
                                          validate = {enum, [absent, empty, non_empty]}},
                       ~"hint" => #option{type = atom,
                                          validate = {enum, [no_store, store]}},
                       ~"jingle" => #option{type = boolean},
                       ~"user_status" => #option{type = atom,
                                                  validate = {enum, [online, offline]}},
                       ~"client_state" => #option{type = atom,
                                                   validate = {enum, [active, inactive]}}},
             validate = non_empty}.

-spec process_rule(map()) -> map().
process_rule(Rule = #{action := push}) when not is_map_key(content, Rule) ->
    error(#{what => missing_push_rule_content,
            text => ~"The 'content' option is required when 'action' is 'push'"});
process_rule(#{action := Action, content := _}) when Action =/= push ->
    error(#{what => unsupported_push_rule_content,
            text => ~"The 'content' option is supported only when 'action' is 'push'"});
process_rule(Rule) ->
    Rule.

-spec match_rule([map()], event()) -> map().
match_rule([#{conditions := Conditions} = Rule | RemainingRules], Event) ->
    case check_conditions(Conditions, Event) of
        true -> Rule;
        false -> match_rule(RemainingRules, Event)
    end;
match_rule([], _Event) ->
    #{conditions => [], action => skip}.

-spec check_conditions([map()], event()) -> boolean().
check_conditions(Conditions, Event) ->
    lists:any(fun(Condition) -> check_condition_map(Condition, Event) end, Conditions).

-spec check_condition_map(map(), event()) -> boolean().
check_condition_map(Condition, Event) ->
    lists:all(fun({Key, Value}) -> check_condition(Key, Value, Event) end,
              maps:to_list(Condition)).

-spec check_condition(atom(), term(), event()) -> boolean().
check_condition(event, msg, #msg_event{}) ->
    true;
check_condition(event, unack_msg, #unack_msg_event{}) ->
    true;
check_condition(type, Expected, #msg_event{type = Expected}) ->
    true;
check_condition(type, Expected, #unack_msg_event{type = Expected}) ->
    true;
check_condition(user_status, offline, #msg_event{user_status = offline}) ->
    true;
check_condition(user_status, online, #msg_event{user_status = {online, _}}) ->
    true;
check_condition(client_state, Expected,
                #msg_event{user_status = {online, #{client_state := Expected}}}) ->
    true;
check_condition(body, Expected, Event) ->
    Expected =:= body_state(packet(Event));
check_condition(hint, no_store, Event) ->
    Packet = packet(Event),
    exml_query:subelement_with_name_and_ns(Packet, ~"no-store", ?NS_HINTS) =/= undefined;
check_condition(hint, store, Event) ->
    Packet = packet(Event),
    exml_query:subelement_with_name_and_ns(Packet, ~"store", ?NS_HINTS) =/= undefined;
check_condition(jingle, Expected, Event) ->
    Packet = packet(Event),
    Expected =:= (exml_query:subelement_with_ns(Packet, ?JINGLE_MSG_NS) =/= undefined);
check_condition(_Condition, _Expected, _Event) ->
    false.

-spec packet(event()) -> exml:element().
packet(#msg_event{packet = Packet}) -> Packet;
packet(#unack_msg_event{packet = Packet}) -> Packet.

-spec body_state(exml:element()) -> absent | empty | non_empty.
body_state(Packet) ->
    case exml_query:subelement(Packet, ~"body") of
        undefined -> absent;
        Body ->
            case exml_query:cdata(Body) of
                ~"" -> empty;
                _ -> non_empty
            end
    end.

-spec publish_content(mongoose_acc:t(), jid:jid(), mod_event_pusher_push_content:content()) ->
          mongoose_acc:t().
publish_content(Acc, BareRecipient, Content) ->
    HostType = mongoose_acc:host_type(Acc),
    {ok, Services} = mod_event_pusher_push_backend:get_publish_services(HostType, BareRecipient),
    PublishedServices = mongoose_acc:get(event_pusher, published_services, [], Acc),
    ServicesToPublish = Services -- PublishedServices,
    mod_event_pusher_push_publisher:publish_notification(Acc, Content, ServicesToPublish).
