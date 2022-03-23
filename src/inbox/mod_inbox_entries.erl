-module(mod_inbox_entries).

-include("mongoose_ns.hrl").
-include("jlib.hrl").
-include("mod_inbox.hrl").

% Inbox extensions
-export([process_iq_conversation/5]).
-export([should_be_stored_in_inbox/1]).
-export([extensions_result/3]).

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
            Form = build_inbox_entry_form(),
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

-spec build_inbox_entry_form() -> exml:element().
build_inbox_entry_form() ->
    #xmlel{name = <<"x">>,
           attrs = [{<<"xmlns">>, ?NS_XDATA},
                    {<<"type">>, <<"form">>}],
           children = [jlib:form_field({<<"FORM_TYPE">>, <<"hidden">>, ?NS_ESL_INBOX_CONVERSATION}),
                       jlib:form_field({<<"box">>, <<"list-single">>, <<"all">>}),
                       jlib:form_field({<<"archive">>, <<"boolean">>, <<"false">>}),
                       jlib:form_field({<<"read">>, <<"boolean">>, <<"false">>}),
                       jlib:form_field({<<"mute">>, <<"text-single">>, <<"0">>})]}.

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
        [] -> return_error(Acc, IQ, <<"Entry not found">>);
        Result ->
            Properties = build_result(Acc, Result, IQ),
            X = [#xmlel{name = <<"query">>,
                        attrs = [{<<"xmlns">>, ?NS_ESL_INBOX_CONVERSATION},
                                 {<<"jid">>, BinEntryJID}],
                        children = Properties}],
            {Acc, IQ#iq{type = result, sub_el = X}}
    end.

-spec process_iq_conversation_set(mongoose_acc:t(), jlib:iq(), jid:jid(), exml:element()) ->
    {mongoose_acc:t(), jlib:iq()}.
process_iq_conversation_set(
  Acc, #iq{id = IqId} = IQ, From, #xmlel{children = Requests} = Query) ->
    case mod_inbox_utils:extract_attr_jid(Query) of
        {error, Msg} ->
            return_error(Acc, IQ, Msg);
        EntryJID ->
            extract_requests(Acc, IQ, From, EntryJID, Requests, exml_query:attr(Query, <<"queryid">>, IqId))
    end.

-spec extract_requests(mongoose_acc:t(), jlib:iq(), jid:jid(), jid:jid(), [exml:element()], binary() | undefined) ->
    {mongoose_acc:t(), jlib:iq()}.
extract_requests(Acc, IQ, From, EntryJID, Requests, QueryId) ->
    CurrentTS = mongoose_acc:timestamp(Acc),
    case form_to_query(CurrentTS, Requests, #{}) of
        {error, Msg} ->
            return_error(Acc, IQ, Msg);
        Params ->
            process_requests(Acc, IQ, From, EntryJID, Params, QueryId)
    end.

-spec process_requests(mongoose_acc:t(), jlib:iq(), jid:jid(), jid:jid(), map(), binary() | undefined) ->
    {mongoose_acc:t(), jlib:iq()}.
process_requests(Acc, IQ, From, EntryJID, Params, QueryId) ->
    HostType = mongoose_acc:host_type(Acc),
    InboxEntryKey = mod_inbox_utils:build_inbox_entry_key(From, EntryJID),
    case mod_inbox_backend:set_entry_properties(HostType, InboxEntryKey, Params) of
        {error, Msg} ->
            return_error(Acc, IQ, Msg);
        Result ->
            forward_result(Acc, IQ, From, InboxEntryKey, Result, QueryId)
    end.

-spec forward_result(mongoose_acc:t(), jlib:iq(), jid:jid(), mod_inbox:entry_key(), entry_properties(), binary() | undefined) ->
    {mongoose_acc:t(), jlib:iq()}.
forward_result(Acc, IQ, From, {_, _, ToBareJidBin}, Result, QueryId) ->
    Properties = build_result(Acc, Result, IQ),
    Children = prepare_children(ToBareJidBin, Properties, QueryId),
    Msg = #xmlel{name = <<"message">>,
                 attrs = [{<<"id">>, IQ#iq.id}],
                 children = Children},
    Acc1 = ejabberd_router:route(From, jid:to_bare(From), Acc, Msg),
    Res = IQ#iq{type = result, sub_el = []},
    {Acc1, Res}.

-spec prepare_children(binary(), [exml:element()], undefined | binary()) -> [exml:element()].
prepare_children(Jid, Properties, undefined) ->
    [#xmlel{name = <<"x">>,
            attrs = [{<<"xmlns">>, ?NS_ESL_INBOX_CONVERSATION},
                     {<<"jid">>, Jid}],
            children = Properties}];
prepare_children(Jid, Properties, QueryId) ->
    [#xmlel{name = <<"x">>,
            attrs = [{<<"xmlns">>, ?NS_ESL_INBOX_CONVERSATION},
                     {<<"jid">>, Jid},
                     {<<"queryid">>, QueryId}],
            children = Properties}].

maybe_process_reset_stanza(Acc, From, IQ, ResetStanza) ->
    case mod_inbox_utils:extract_attr_jid(ResetStanza) of
        {error, Msg} ->
            {Acc, IQ#iq{type = error, sub_el = [mongoose_xmpp_errors:bad_request(<<"en">>, Msg)]}};
        InterlocutorJID ->
            process_reset_stanza(Acc, From, IQ, ResetStanza, InterlocutorJID)
    end.

process_reset_stanza(Acc, From, IQ, _ResetStanza, InterlocutorJID) ->
    HostType = mongoose_acc:host_type(Acc),
    ok = mod_inbox_utils:reset_unread_count_to_zero(HostType, From, InterlocutorJID),
    Res = IQ#iq{type = result,
                sub_el = [#xmlel{name = <<"reset">>,
                                 attrs = [{<<"xmlns">>, ?NS_ESL_INBOX_CONVERSATION}],
                                 children = []}]},
    {Acc, Res}.

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

-spec build_result(mongoose_acc:t(), inbox_res() | entry_properties(), jlib:iq()) -> [exml:element()].
build_result(Acc, Entry = #{archive := Archived,
                            unread_count := UnreadCount,
                            muted_until := MutedUntil}, IQ) ->
    CurrentTS = mongoose_acc:timestamp(Acc),
    maybe_full_entry(Acc, Entry, IQ) ++
    [
     kv_to_el(<<"archive">>, mod_inbox_utils:bool_to_binary(Archived)),
     kv_to_el(<<"read">>, mod_inbox_utils:bool_to_binary(0 =:= UnreadCount)),
     kv_to_el(<<"mute">>, mod_inbox_utils:maybe_muted_until(MutedUntil, CurrentTS))
    ].

maybe_full_entry(Acc, Entry = #{msg := Msg, timestamp := Timestamp}, IQ) ->
    Extensions = mongoose_hooks:extend_inbox_message(Acc, Entry, IQ),
    [ mod_inbox_utils:build_forward_el(Msg, Timestamp) | Extensions];
maybe_full_entry(_, _, _) ->
    [].

-spec kv_to_el(binary(), binary()) -> exml:element().
kv_to_el(Key, Value) ->
    #xmlel{name = Key, children = [#xmlcdata{content = Value}]}.

-spec return_error(mongoose_acc:t(), jlib:iq(), any()) ->
    {mongoose_acc:t(), jlib:iq()}.
return_error(Acc, IQ, Msg) when is_binary(Msg) ->
    {Acc, IQ#iq{type = error, sub_el = [mongoose_xmpp_errors:bad_request(<<"en">>, Msg)]}}.

-spec form_to_query(integer(), [exml:element()], map()) -> map() | {error, binary()}.
form_to_query(_, [], Acc) when map_size(Acc) == 0 ->
    {error, <<"no-property">>};
form_to_query(_, [], Acc) ->
    Acc;
form_to_query(TS, [#xmlel{name = <<"archive">>,
                          children = [#xmlcdata{content = <<"true">>}]} | Rest], Acc) ->
    form_to_query(TS, Rest, Acc#{archive => 1});
form_to_query(TS, [#xmlel{name = <<"archive">>,
                          children = [#xmlcdata{content = <<"false">>}]} | Rest], Acc) ->
    form_to_query(TS, Rest, Acc#{archive => 0});
form_to_query(TS, [#xmlel{name = <<"read">>,
                          children = [#xmlcdata{content = <<"true">>}]} | Rest], Acc) ->
    form_to_query(TS, Rest, Acc#{unread_count => 0});
form_to_query(TS, [#xmlel{name = <<"read">>,
                          children = [#xmlcdata{content = <<"false">>}]} | Rest], Acc) ->
    form_to_query(TS, Rest, Acc#{unread_count => 1});
form_to_query(TS, [#xmlel{name = <<"mute">>,
                          children = [#xmlcdata{content = Value}]} | Rest], Acc) ->
    case mod_inbox_utils:maybe_binary_to_positive_integer(Value) of
        {error, _} -> {error, <<"bad-request">>};
        0 ->
            form_to_query(TS, Rest, Acc#{muted_until => 0});
        Num when Num > 0 ->
            MutedUntilMicroSec = TS + erlang:convert_time_unit(Num, second, microsecond),
            form_to_query(TS, Rest, Acc#{muted_until => MutedUntilMicroSec})
    end;
form_to_query(_, _, _) ->
    {error, <<"bad-request">>}.

-spec should_be_stored_in_inbox(exml:element()) -> boolean().
should_be_stored_in_inbox(Msg) ->
    not is_inbox_update(Msg).

-spec is_inbox_update(exml:element()) -> boolean().
is_inbox_update(Msg) ->
    case exml_query:subelement_with_ns(Msg, ?NS_ESL_INBOX_CONVERSATION, undefined) of
        undefined -> false;
        _ -> true
    end.

-spec extensions_result(boolean(), integer(), integer()) -> [exml:element()].
extensions_result(Archive, MutedUntil, AccTS) ->
    [#xmlel{name = <<"archive">>,
            children = [#xmlcdata{content = mod_inbox_utils:bool_to_binary(Archive)}]},
     #xmlel{name = <<"mute">>,
            children = [#xmlcdata{content = maybe_muted_until(MutedUntil, AccTS)}]}].

-spec maybe_muted_until(integer(), integer()) -> binary().
maybe_muted_until(0, _) -> <<"0">>;
maybe_muted_until(MutedUntil, CurrentTS) ->
    case CurrentTS =< MutedUntil of
        true -> list_to_binary(calendar:system_time_to_rfc3339(MutedUntil, [{offset, "Z"}, {unit, microsecond}]));
        false -> <<"0">>
    end.
