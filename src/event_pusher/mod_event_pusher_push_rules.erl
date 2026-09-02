-module(mod_event_pusher_push_rules).
-moduledoc "Matches and executes configurable push notification rules".

-include("mongoose_config_spec.hrl").
-include("mod_event_pusher_events.hrl").

-export([config_spec/0, push_event/4]).

-export([process_rule/1]).

-spec config_spec() -> mongoose_config_spec:config_list().
config_spec() ->
    #list{items = rule_config_spec(), validate = unique}.

-spec push_event(mongoose_acc:t(), mod_event_pusher:event(), jid:jid(), [map()]) ->
          mongoose_acc:t().
push_event(Acc, Event, BareRecipient, Rules) ->
    case match_rule(Rules, Event) of
        #{action := skip} ->
            Acc;
        #{action := push, content := ContentSpec} ->
            case mod_event_pusher_push_content:build(ContentSpec, Acc) of
                skip -> Acc;
                Content -> publish_content(Acc, BareRecipient, Content)
            end
    end.

-spec rule_config_spec() -> mongoose_config_spec:config_section().
rule_config_spec() ->
    #section{items = #{~"conditions" => conditions_config_spec(),
                       ~"action" => #option{type = atom,
                                            validate = {enum, [push, skip]}},
                       ~"content" => #option{type = atom,
                                             validate = {enum, [message, jingle]}}},
             defaults = #{~"conditions" => #{}},
             required = [~"action"],
             process = fun ?MODULE:process_rule/1}.

-spec conditions_config_spec() -> mongoose_config_spec:config_section().
conditions_config_spec() ->
    #section{items = #{~"event" => #option{type = atom,
                                           validate = {enum, [chat, unack_msg]}},
                       ~"has_body" => #option{type = boolean},
                       ~"hint" => #option{type = atom,
                                          validate = {enum, [no_store, store]}},
                       ~"jingle" => #option{type = boolean},
                       ~"user_status" => #option{type = atom,
                                                  validate = {enum, [online, offline]}},
                       ~"client_state" => #option{type = atom,
                                                   validate = {enum, [active, inactive]}}}}.

-spec process_rule(map()) -> map().
process_rule(Rule = #{action := push}) when not is_map_key(content, Rule) ->
    error(#{what => missing_push_rule_content,
            text => ~"The 'content' option is required when 'action' is 'push'"});
process_rule(#{action := Action, content := _}) when Action =/= push ->
    error(#{what => unsupported_push_rule_content,
            text => ~"The 'content' option is supported only when 'action' is 'push'"});
process_rule(Rule) ->
    Rule.

-spec match_rule([map()], mod_event_pusher:event()) -> map().
match_rule([#{conditions := Conditions} = Rule | RemainingRules], Event) ->
    case check_conditions(Conditions, Event) of
        true -> Rule;
        false -> match_rule(RemainingRules, Event)
    end;
match_rule([], _Event) ->
    #{conditions => #{}, action => skip}.

-spec check_conditions(map(), mod_event_pusher:event()) -> boolean().
check_conditions(Conditions, Event) ->
    lists:all(fun({Key, Value}) -> check_condition(Key, Value, Event) end,
              maps:to_list(Conditions)).

-spec check_condition(atom(), term(), mod_event_pusher:event()) -> boolean().
check_condition(event, chat, #chat_event{}) ->
    true;
check_condition(event, unack_msg, #unack_msg_event{}) ->
    true;
check_condition(user_status, offline, #chat_event{user_status = offline}) ->
    true;
check_condition(user_status, online, #chat_event{user_status = {online, _}}) ->
    true;
check_condition(client_state, Expected,
                #chat_event{user_status = {online, #{client_state := Expected}}}) ->
    true;
check_condition(has_body, Expected, #chat_event{packet = Packet}) ->
    Expected =:= (exml_query:subelement(Packet, ~"body") =/= undefined);
check_condition(hint, no_store, #chat_event{packet = Packet}) ->
    exml_query:subelement_with_name_and_ns(Packet, ~"no-store", ?NS_HINTS) =/= undefined;
check_condition(hint, store, #chat_event{packet = Packet}) ->
    exml_query:subelement_with_name_and_ns(Packet, ~"store", ?NS_HINTS) =/= undefined;
check_condition(jingle, Expected, #chat_event{packet = Packet}) ->
    Expected =:= (exml_query:subelement_with_ns(Packet, ?JINGLE_MSG_NS) =/= undefined);
check_condition(_Condition, _Expected, _Event) ->
    false.

-spec publish_content(mongoose_acc:t(), jid:jid(), mod_event_pusher_push_content:content()) ->
          mongoose_acc:t().
publish_content(Acc, BareRecipient, Content) ->
    HostType = mongoose_acc:host_type(Acc),
    {ok, Services} = mod_event_pusher_push_backend:get_publish_services(HostType, BareRecipient),
    PublishedServices = mongoose_acc:get(event_pusher, published_services, [], Acc),
    ServicesToPublish = Services -- PublishedServices,
    mod_event_pusher_push_publisher:publish_notification(Acc, Content, ServicesToPublish).
