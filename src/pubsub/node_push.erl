%%%-------------------------------------------------------------------
%%% @author Rafal Slota
%%% @copyright (C) 2017 Erlang Solutions Ltd.
%%% This software is released under the Apache License, Version 2.0
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Node implementation that proxies all published items to `push_notification' hook.
%%% @end
%%%-------------------------------------------------------------------
-module(node_push).
-author('rafal.slota@erlang-solutions.com').
-behaviour(gen_pubsub_node).

-include("mongoose.hrl").
-include("jlib.hrl").
-include("pubsub.hrl").

-export([based_on/0, init/3, terminate/2, options/0, features/0,
         publish_item/9, node_to_path/1, should_delete_when_owner_removed/0]).

based_on() ->  node_flat.

init(Host, ServerHost, Opts) ->
    node_flat:init(Host, ServerHost, Opts),
    ok.

terminate(Host, ServerHost) ->
    node_flat:terminate(Host, ServerHost),
    ok.

options() ->
    [{deliver_payloads, true},
     {notify_config, false},
     {notify_delete, false},
     {notify_retract, false},
     {purge_offline, false},
     {persist_items, false},
     {max_items, 1},
     {subscribe, true},
     {access_model, whitelist},
     {roster_groups_allowed, []},
     {publish_model, open},
     {notification_type, headline},
     {max_payload_size, ?MAX_PAYLOAD_SIZE},
     {send_last_published_item, on_sub_and_presence},
     {deliver_notifications, true},
     {presence_based_delivery, true}].

features() ->
    [
        <<"create-nodes">>,
        <<"delete-nodes">>,
        <<"modify-affiliations">>,
        <<"publish">>,
        <<"publish-options">>,
        <<"publish-only-affiliation">>,
        <<"purge-nodes">>,
        <<"retrieve-affiliations">>
    ].

publish_item(ServerHost, Nidx, Publisher, Model, _MaxItems, _ItemId, _ItemPublisher, Payload,
             PublishOptions) ->
    {ok, Affiliation} = mod_pubsub_db_backend:get_affiliation(Nidx, jid:to_lower(Publisher)),
    ElPayload = [El || #xmlel{} = El <- Payload],

    case is_allowed_to_publish(Model, Affiliation) of
        true ->
            do_publish_item(ServerHost, PublishOptions, ElPayload);
        false ->
            {error, mongoose_xmpp_errors:forbidden()}
    end.

do_publish_item(ServerHost, PublishOptions,
                [#xmlel{name = <<"notification">>} | _] = Notifications) ->
    case catch parse_form(PublishOptions) of
        #{<<"device_id">> := _, <<"service">> := _} = OptionMap ->
            NotificationRawForms = [exml_query:subelement(El, <<"x">>) || El <- Notifications],
            NotificationForms = [parse_form(Form) || Form <- NotificationRawForms],
            Result = ejabberd_hooks:run_fold(push_notifications, ServerHost, ok,
                                             [ServerHost, NotificationForms, OptionMap]),
            handle_push_hook_result(Result);
        _ ->
            {error, mod_pubsub:extended_error(mongoose_xmpp_errors:conflict(), <<"precondition-not-met">>)}
    end;
do_publish_item(_ServerHost, _PublishOptions, _Payload) ->
    {error, mongoose_xmpp_errors:bad_request()}.

handle_push_hook_result(ok) ->
    {result, default};
handle_push_hook_result({error, device_not_registered}) ->
    {error, mod_pubsub:extended_error(mongoose_xmpp_errors:not_acceptable_cancel(), <<"device-not-registered">>)};
handle_push_hook_result({error, _}) ->
    {error, mod_pubsub:extended_error(mongoose_xmpp_errors:bad_request(), <<"faild-to-submit-push-notification">>)}.

node_to_path(Node) ->
    node_flat:node_to_path(Node).

should_delete_when_owner_removed() -> true.

%%%
%%% Internal
%%%

is_allowed_to_publish(PublishModel, Affiliation) ->
    (PublishModel == open)
    or (PublishModel == publishers)
        and ((Affiliation == owner)
              or (Affiliation == publisher)
              or (Affiliation == publish_only)).


-spec parse_form(undefined | exml:element()) -> invalid_form | #{atom() => binary()}.
parse_form(undefined) ->
    #{};
parse_form(Form) ->
    IsForm = ?NS_XDATA == exml_query:attr(Form, <<"xmlns">>),
    IsSubmit = <<"submit">> == exml_query:attr(Form, <<"type">>, <<"submit">>),

    FieldsXML = exml_query:subelements(Form, <<"field">>),
    Fields = [{exml_query:attr(Field, <<"var">>),
               exml_query:path(Field, [{element, <<"value">>}, cdata])} || Field <- FieldsXML],
    {_, CustomFields} = lists:partition(
        fun({Name, _}) ->
            Name == <<"FORM_TYPE">>
        end, Fields),

    case IsForm andalso IsSubmit of
        true ->
            maps:from_list(CustomFields);
        false ->
            invalid_form
    end.
