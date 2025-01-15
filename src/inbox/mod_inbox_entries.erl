-module(mod_inbox_entries).

-include("mongoose_ns.hrl").
-include("jlib.hrl").
-include("mod_inbox.hrl").

% Inbox extensions
-export([process_iq_conversation/5]).
-export([should_be_stored_in_inbox/1]).

-type entry_query() :: mod_inbox:entry_properties().

-type get_entry_type() :: full_entry | only_properties.
-export_type([get_entry_type/0]).

-spec process_iq_conversation(Acc :: mongoose_acc:t(),
                              From :: jid:jid(),
                              To :: jid:jid(),
                              IQ :: jlib:iq(),
                              Extra :: map()) ->
    {mongoose_acc:t(), jlib:iq()}.
process_iq_conversation(Acc, From, _To, #iq{type = get, sub_el = SubEl} = IQ, _Extra) ->
    process_iq_conversation_get(Acc, IQ, From, SubEl);
process_iq_conversation(Acc, From, _To, #iq{type = set,
                                            sub_el = #xmlel{name = <<"reset">>} = ResetStanza} = IQ,
                       _Extra) ->
    maybe_process_reset_stanza(Acc, From, IQ, ResetStanza);
process_iq_conversation(Acc, From, _To, #iq{type = set,
                                            sub_el = #xmlel{name = <<"query">>} = Query} = IQ,
                       _Extra) ->
    process_iq_conversation_set(Acc, IQ, From, Query).

-spec process_iq_conversation_get(mongoose_acc:t(), jlib:iq(), jid:jid(), exml:element()) ->
    {mongoose_acc:t(), jlib:iq()}.
process_iq_conversation_get(Acc, IQ, From, SubEl) ->
    case mod_inbox_utils:extract_attr_jid(SubEl) of
        {error, _} ->
            HostType = mongoose_acc:host_type(Acc),
            Form = build_inbox_entry_form(HostType),
            SubElWithForm = SubEl#xmlel{children = [Form]},
            {Acc, IQ#iq{type = result, sub_el = SubElWithForm}};
        EntryJID ->
            FullEntry = maybe_get_full_entry(SubEl),
            get_properties_for_jid(Acc, IQ, From, EntryJID, FullEntry)
    end.

-spec maybe_get_full_entry(exml:element()) -> get_entry_type().
maybe_get_full_entry(SubEl) ->
    case exml_query:attr(SubEl, <<"complete">>) of
        <<"true">> -> full_entry;
        _ -> only_properties
    end.

-spec build_inbox_entry_form(mongooseim:host_type()) -> exml:element().
build_inbox_entry_form(HostType) ->
    AllBoxes = mod_inbox_utils:all_valid_boxes_for_query(HostType),
    Fields =
        [#{var => <<"box">>, type => <<"list-single">>, values => [<<"all">>], options => AllBoxes},
         #{var => <<"archive">>, type => <<"boolean">>, values => [<<"false">>]},
         #{var => <<"read">>, type => <<"boolean">>, values => [<<"false">>]},
         #{var => <<"mute">>, type => <<"text-single">>, values => [<<"0">>]}],
    mongoose_data_forms:form(#{ns => ?NS_ESL_INBOX_CONVERSATION, fields => Fields}).

fetch_right_query(HostType, InboxEntryKey, only_properties) ->
    mod_inbox_backend:get_entry_properties(HostType, InboxEntryKey);
fetch_right_query(HostType, InboxEntryKey, full_entry) ->
    mod_inbox_backend:get_full_entry(HostType, InboxEntryKey).

-spec get_properties_for_jid(mongoose_acc:t(), jlib:iq(), jid:jid(), jid:jid(), get_entry_type()) ->
    {mongoose_acc:t(), jlib:iq()}.
get_properties_for_jid(Acc, IQ, From, EntryJID, QueryType) ->
    HostType = mongoose_acc:host_type(Acc),
    {_, _, BinEntryJID} = InboxEntryKey = mod_inbox_utils:build_inbox_entry_key(From, EntryJID),
    case fetch_right_query(HostType, InboxEntryKey, QueryType) of
        [] -> return_error(Acc, IQ, item_not_found, <<"Entry not found">>);
        Result ->
            Children = build_query_children(Acc, Result, IQ, QueryType),
            X = [#xmlel{name = <<"query">>,
                        attrs = #{<<"xmlns">> => ?NS_ESL_INBOX_CONVERSATION,
                                  <<"jid">> => BinEntryJID},
                        children = Children}],
            {Acc, IQ#iq{type = result, sub_el = X}}
    end.

-spec process_iq_conversation_set(mongoose_acc:t(), jlib:iq(), jid:jid(), exml:element()) ->
    {mongoose_acc:t(), jlib:iq()}.
process_iq_conversation_set(Acc, IQ, From, #xmlel{children = Requests} = Query) ->
    case mod_inbox_utils:extract_attr_jid(Query) of
        {error, Msg} ->
            return_error(Acc, IQ, bad_request, Msg);
        EntryJID ->
            extract_requests(Acc, IQ, From, EntryJID, Requests)
    end.

-spec extract_requests(mongoose_acc:t(), jlib:iq(), jid:jid(), jid:jid(), [exml:element()]) ->
    {mongoose_acc:t(), jlib:iq()}.
extract_requests(Acc, IQ, From, EntryJID, Requests) ->
    case form_to_query(Acc, Requests, #{}) of
        {error, Msg} ->
            return_error(Acc, IQ, bad_request, Msg);
        Params ->
            process_requests(Acc, IQ, From, EntryJID, Params)
    end.

-spec process_requests(mongoose_acc:t(), jlib:iq(), jid:jid(), jid:jid(), entry_query()) ->
    {mongoose_acc:t(), jlib:iq()}.
process_requests(Acc, IQ, From, EntryJID, Params) ->
    HostType = mongoose_acc:host_type(Acc),
    InboxEntryKey = mod_inbox_utils:build_inbox_entry_key(From, EntryJID),
    case mod_inbox_backend:set_entry_properties(HostType, InboxEntryKey, Params) of
        {error, Msg} ->
            return_error(Acc, IQ, bad_request, Msg);
        Result ->
            forward_result(Acc, IQ, From, InboxEntryKey, Result)
    end.

-spec forward_result(mongoose_acc:t(), jlib:iq(), jid:jid(), mod_inbox:entry_key(), entry_properties()) ->
    {mongoose_acc:t(), jlib:iq()}.
forward_result(Acc, IQ = #iq{id = IqId}, From, {_, _, ToBareJidBin}, Result) ->
    Children0 = build_query_children(Acc, Result, IQ, only_properties),
    Children1 = iq_x_wrapper(IQ, ToBareJidBin, Children0),
    Msg = #xmlel{name = <<"message">>, attrs = #{<<"id">> => IqId}, children = Children1},
    Acc1 = ejabberd_router:route(From, jid:to_bare(From), Acc, Msg),
    Res = IQ#iq{type = result, sub_el = []},
    {Acc1, Res}.

-spec iq_x_wrapper(jlib:iq(), binary(), [exml:element()]) -> [exml:element()].
iq_x_wrapper(#iq{id = IqId, sub_el = Query}, Jid, Properties) ->
    [#xmlel{name = <<"x">>,
            attrs = #{<<"xmlns">> => ?NS_ESL_INBOX_CONVERSATION,
                      <<"jid">> => Jid,
                      <<"queryid">> => exml_query:attr(Query, <<"queryid">>, IqId)},
            children = Properties}].

maybe_process_reset_stanza(Acc, From, IQ, ResetStanza) ->
    case mod_inbox_utils:extract_attr_jid(ResetStanza) of
        {error, Msg} ->
            return_error(Acc, IQ, bad_request, Msg);
        InterlocutorJID ->
            process_reset_stanza(Acc, From, IQ, ResetStanza, InterlocutorJID)
    end.

process_reset_stanza(Acc, From, IQ, _ResetStanza, InterlocutorJID) ->
    ok = mod_inbox_utils:reset_unread_count_to_zero(Acc, From, InterlocutorJID),
    Res = IQ#iq{type = result,
                sub_el = [#xmlel{name = <<"reset">>,
                                 attrs = #{<<"xmlns">> => ?NS_ESL_INBOX_CONVERSATION},
                                 children = []}]},
    {Acc, Res}.

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

-spec build_query_children(mongoose_acc:t(), inbox_res() | entry_properties(), jlib:iq(), get_entry_type()) ->
    [exml:element()].
build_query_children(Acc, Entry0, IQ, full_entry) ->
    CurrentTS = mongoose_acc:timestamp(Acc),
    [Entry1] = mongoose_hooks:extend_inbox_result(Acc, [Entry0], IQ),
    mod_inbox_utils:build_inbox_result_elements(Entry1, CurrentTS);
build_query_children(Acc, Entry, _IQ, only_properties) ->
    CurrentTS = mongoose_acc:timestamp(Acc),
    mod_inbox_utils:build_entry_result_elements(Entry, CurrentTS).

-spec return_error(mongoose_acc:t(), jlib:iq(), bad_request | item_not_found, binary()) ->
    {mongoose_acc:t(), jlib:iq()}.
return_error(Acc, IQ, Type, Msg) when is_binary(Msg) ->
    {Acc, IQ#iq{type = error, sub_el = [mongoose_xmpp_errors:Type(<<"en">>, Msg)]}}.

-spec form_to_query(mongoose_acc:t(), [exml:element()], map()) -> entry_query() | {error, binary()}.
form_to_query(_, [], Acc) when map_size(Acc) == 0 ->
    {error, <<"no-property">>};
form_to_query(_, [], Acc) ->
    Acc;
form_to_query(MAcc, [#xmlel{name = <<"box">>, children = [#xmlcdata{content = BoxName}]} | Rest], Acc) ->
    case is_box_accepted(MAcc, BoxName) of
        true -> form_to_query(MAcc, Rest, Acc#{box => BoxName});
        false -> {error, <<"invalid-box">>}
    end;
form_to_query(MAcc, [#xmlel{name = <<"archive">>, children = [#xmlcdata{content = <<"true">>}]} | Rest], Acc) ->
    form_to_query(MAcc, Rest, Acc#{box => maps:get(box, Acc, <<"archive">>)});
form_to_query(MAcc, [#xmlel{name = <<"archive">>, children = [#xmlcdata{content = <<"false">>}]} | Rest], Acc) ->
    form_to_query(MAcc, Rest, Acc#{box => maps:get(box, Acc, <<"inbox">>)});
form_to_query(MAcc, [#xmlel{name = <<"read">>, children = [#xmlcdata{content = <<"true">>}]} | Rest], Acc) ->
    form_to_query(MAcc, Rest, Acc#{unread_count => 0});
form_to_query(MAcc, [#xmlel{name = <<"read">>, children = [#xmlcdata{content = <<"false">>}]} | Rest], Acc) ->
    form_to_query(MAcc, Rest, Acc#{unread_count => 1});
form_to_query(MAcc, [#xmlel{name = <<"mute">>,
                            children = [#xmlcdata{content = Value}]} | Rest], Acc) ->
    case mod_inbox_utils:maybe_binary_to_positive_integer(Value) of
        {error, _} -> {error, <<"bad-request">>};
        0 ->
            form_to_query(MAcc, Rest, Acc#{muted_until => 0});
        Num when Num > 0 ->
            TS = mongoose_acc:timestamp(MAcc),
            MutedUntilMicroSec = TS + erlang:convert_time_unit(Num, second, microsecond),
            form_to_query(MAcc, Rest, Acc#{muted_until => MutedUntilMicroSec})
    end;
form_to_query(_, _, _) ->
    {error, <<"bad-request">>}.

is_box_accepted(MAcc, Box) ->
    HostType = mongoose_acc:host_type(MAcc),
    BoxOptions = gen_mod:get_module_opt(HostType, mod_inbox, boxes),
    lists:member(Box, BoxOptions).

-spec should_be_stored_in_inbox(exml:element()) -> boolean().
should_be_stored_in_inbox(Msg) ->
    not is_inbox_update(Msg).

-spec is_inbox_update(exml:element()) -> boolean().
is_inbox_update(Msg) ->
    case exml_query:subelement_with_ns(Msg, ?NS_ESL_INBOX_CONVERSATION, undefined) of
        undefined -> false;
        _ -> true
    end.
