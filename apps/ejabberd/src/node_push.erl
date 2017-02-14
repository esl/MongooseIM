%%%-------------------------------------------------------------------
%%% @author Rafal Slota
%%% @copyright (C) 2017 Erlang Solutions Ltd.
%%% This software is released under the Apache License, Version 2.0
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% @todo: Write me!
%%% @end
%%%-------------------------------------------------------------------
-module(node_push).
-author('rafal.slota@erlang-solutions.com').
-behaviour(gen_pubsub_node).

-include_lib("ejabberd/include/ejabberd.hrl").
-include_lib("ejabberd/include/jlib.hrl").
-include_lib("ejabberd/include/pubsub.hrl").
-include_lib("mixer/include/mixer.hrl").

-mixin([{node_flat, except, [options/0, features/0, publish_item/8]}]).

-export([options/0, features/0, publish_item/8]).

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

publish_item(Nidx, Publisher, Model, MaxItems, ItemId, ItemPublisher, Payload, PublishOptions) ->
    SubKey = jid:to_lower(Publisher),
    GenKey = jid:to_bare(SubKey),
    GenState = get_state(Nidx, GenKey),
    SubState = case SubKey of
                   GenKey -> GenState;
                   _ -> get_state(Nidx, SubKey)
               end,
    Affiliation = SubState#pubsub_state.affiliation,

    case is_allowed_to_publish(Model, Affiliation) of
        true ->
            do_publish_item(Nidx, Publisher, Model, MaxItems, ItemId, ItemPublisher,
                            Payload, PublishOptions);
        false ->
            {error, ?ERR_FORBIDDEN}
    end.

do_publish_item(_Nidx, _Publisher, _Model, _MaxItems, _ItemId, _ItemPublisher,
                [#xmlel{name = <<"notification">>} | _] = Notifications, PublishOptions) ->
    Host = ?MYNAME,
    case catch parse_form(PublishOptions) of
        #{<<"device_id">> := _, <<"service">> := _} = OptionMap ->
            NotificationRawForms = [exml_query:subelement(El, <<"x">>) || El <- Notifications],
            NotificationForms = [parse_form(Form) || Form <- NotificationRawForms],
            ejabberd_hooks:run(push_notifications, Host, [Host, NotificationForms, OptionMap]),
            {result, default};
        _ ->
            {error, mod_pubsub:extended_error(?ERR_CONFLICT, <<"precondition-not-met">>)}
    end;
do_publish_item(_Nidx, _Publisher, _Model, _MaxItems, _ItemId, _ItemPublisher,
                _Payload, _PublishOptions) ->
    {error, ?ERR_BAD_REQUEST}.

%%%
%%% Internal
%%%

is_allowed_to_publish(PublishModel, Affiliation) ->
    (PublishModel == open)
    or (PublishModel == publishers)
        and ((Affiliation == owner)
              or (Affiliation == publisher)
              or (Affiliation == publish_only)).


-spec parse_form(undefined | jlib:xmlel()) -> invalid_form | #{atom() => binary()}.
parse_form(undefined) ->
    #{};
parse_form(Form) ->
    IsForm = ?NS_XDATA == exml_query:attr(Form, <<"xmlns">>),
    IsSubmit = <<"submit">> == exml_query:attr(Form, <<"type">>),

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
