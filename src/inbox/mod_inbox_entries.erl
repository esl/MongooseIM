-module(mod_inbox_entries).

-include("mongoose_ns.hrl").
-include("jlib.hrl").
-include("mod_inbox.hrl").

% Inbox extensions
-export([process_iq_conversation/4]).
-export([should_be_stored_in_inbox/1]).
-export([extensions_result/2]).

-spec process_iq_conversation(jid:jid(), jid:jid(), mongoose_acc:t(), jlib:iq()) ->
    {mongoose_acc:t(), jlib:iq()}.
process_iq_conversation(From, _To, Acc, #iq{type = get, sub_el = SubEl} = IQ) ->
    process_iq_conversation_get(Acc, IQ, From, SubEl);
process_iq_conversation(From, _To, Acc, #iq{type = set,
                                            sub_el = #xmlel{name = <<"reset">>} = ResetStanza} = IQ) ->
    maybe_process_reset_stanza(From, Acc, IQ, ResetStanza);
process_iq_conversation(From, _To, Acc, #iq{type = set, sub_el = Query} = IQ) ->
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
            get_properties_for_jid(Acc, IQ, From, EntryJID)
    end.

-spec build_inbox_entry_form() -> exml:element().
build_inbox_entry_form() ->
    #xmlel{name = <<"x">>,
           attrs = [{<<"xmlns">>, ?NS_XDATA},
                    {<<"type">>, <<"form">>}],
           children = [jlib:form_field({<<"FORM_TYPE">>, <<"hidden">>, ?NS_ESL_INBOX_CONVERSATION}),
                       jlib:form_field({<<"archive">>, <<"boolean">>, <<"false">>}),
                       jlib:form_field({<<"read">>, <<"boolean">>, <<"false">>}),
                       jlib:form_field({<<"mute">>, <<"text-single">>, <<"0">>})]}.

-spec get_properties_for_jid(mongoose_acc:t(), jlib:iq(), jid:jid(), jid:jid()) ->
    {mongoose_acc:t(), jlib:iq()}.
get_properties_for_jid(Acc, IQ, From, EntryJID) ->
    BinEntryJID = jid:to_binary(jid:to_lus(EntryJID)),
    case mod_inbox_backend:get_entry_properties(From, BinEntryJID) of
        [] -> return_error(Acc, IQ, <<"Entry not found">>);
        Result ->
            CurrentTS = mongoose_acc:timestamp(Acc),
            Properties = build_result(Result, CurrentTS),
            X = [#xmlel{name = <<"query">>,
                        attrs = [{<<"xmlns">>, ?NS_ESL_INBOX_CONVERSATION},
                                 {<<"jid">>, BinEntryJID}],
                        children = Properties}],
            {Acc, IQ#iq{type = result, sub_el = X}}
    end.

-spec process_iq_conversation_set(mongoose_acc:t(), jlib:iq(), jid:jid(), exml:element()) ->
    {mongoose_acc:t(), jlib:iq()}.
process_iq_conversation_set(
  Acc, IQ, From, #xmlel{name = <<"query">>, children = Requests0} = Query) ->
    case mod_inbox_utils:extract_attr_jid(Query) of
        {error, Msg} ->
            return_error(Acc, IQ, Msg);
        EntryJID ->
            extract_requests(Acc, IQ, From, EntryJID, Requests0)
    end.

-spec extract_requests(mongoose_acc:t(), jlib:iq(), jid:jid(), jid:jid(), [exml:element()]) ->
    {mongoose_acc:t(), jlib:iq()}.
extract_requests(Acc, IQ, From, EntryJID, Requests0) ->
    CurrentTS = mongoose_acc:timestamp(Acc),
    case form_to_query(CurrentTS, Requests0, #{}) of
        {error, Msg} ->
            return_error(Acc, IQ, Msg);
        Params ->
            process_requests(Acc, IQ, From, EntryJID, CurrentTS, Params)
    end.

-spec process_requests(mongoose_acc:t(), jlib:iq(), jid:jid(), jid:jid(), integer(), map()) ->
    {mongoose_acc:t(), jlib:iq()}.
process_requests(Acc, IQ, From, EntryJID, CurrentTS, Params) ->
    BinEntryJID = jid:to_binary(jid:to_lus(EntryJID)),
    case mod_inbox_backend:set_entry_properties(From, BinEntryJID, Params) of
        {error, Msg} ->
            return_error(Acc, IQ, Msg);
        Result ->
            forward_request(Acc, IQ, From, BinEntryJID, Result, CurrentTS)
    end.

-spec forward_request(mongoose_acc:t(), jlib:iq(), jid:jid(), binary(), {_,_,_}, integer()) ->
    {mongoose_acc:t(), jlib:iq()}.
forward_request(Acc, IQ, From, ToBareJidBin, Result, CurrentTS) ->
    Properties = build_result(Result, CurrentTS),
    X = [#xmlel{name = <<"x">>,
                attrs = [{<<"xmlns">>, ?NS_ESL_INBOX_CONVERSATION},
                         {<<"jid">>, ToBareJidBin}],
                children = Properties}],
    Msg = #xmlel{name = <<"message">>,
                 attrs = [{<<"id">>, IQ#iq.id}],
                 children = X},
    Acc1 = ejabberd_router:route(From, jid:to_bare(From), Acc, Msg),
    {Acc1, IQ#iq{type = result, sub_el = []}}.

maybe_process_reset_stanza(From, Acc, IQ, ResetStanza) ->
    case mod_inbox_utils:extract_attr_jid(ResetStanza) of
        {error, Msg} ->
            {Acc, IQ#iq{type = error, sub_el = [mongoose_xmpp_errors:bad_request(<<"en">>, Msg)]}};
        InterlocutorJID ->
            process_reset_stanza(From, Acc, IQ, ResetStanza, InterlocutorJID)
    end.

process_reset_stanza(From, Acc, IQ, _ResetStanza, InterlocutorJID) ->
    ok = mod_inbox_utils:reset_unread_count_to_zero(From, InterlocutorJID),
    {Acc, IQ#iq{type = result,
                sub_el = [#xmlel{name = <<"reset">>,
                                 attrs = [{<<"xmlns">>, ?NS_ESL_INBOX_CONVERSATION}],
                                 children = []}]}}.

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

-spec build_result({binary(), binary(), binary()}, integer()) -> [exml:element()].
build_result({Archived, UnreadCount, MutedUntil}, CurrentTS) ->
    NumericMutedUntil = mongoose_rdbms:result_to_integer(MutedUntil),
    [
     kv_to_el(<<"archive">>, mod_inbox_utils:expand_bin_bool(Archived)),
     kv_to_el(<<"read">>, read_or_not(UnreadCount)),
     kv_to_el(<<"mute">>, mod_inbox_utils:maybe_muted_until(NumericMutedUntil, CurrentTS))
    ].

-spec kv_to_el(binary(), binary()) -> exml:element().
kv_to_el(Key, Value) ->
    #xmlel{name = Key, children = [#xmlcdata{content = Value}]}.

-spec read_or_not(binary()) -> binary().
read_or_not(<<"0">>) -> <<"true">>;
read_or_not(0) -> <<"true">>;
read_or_not(_) -> <<"false">>.

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
    form_to_query(TS, Rest, Acc#{archive => true});
form_to_query(TS, [#xmlel{name = <<"archive">>,
                          children = [#xmlcdata{content = <<"false">>}]} | Rest], Acc) ->
    form_to_query(TS, Rest, Acc#{archive => false});
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
        N when N > 0 ->
            MutedUntilSec = erlang:convert_time_unit(TS, microsecond, second) + N,
            MutedUntilMicroSec = erlang:convert_time_unit(MutedUntilSec, second, microsecond),
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

-spec extensions_result(binary(), binary()) -> [exml:element()].
extensions_result(Archive, MutedUntil) ->
    [#xmlel{name = <<"archive">>, children = [#xmlcdata{content = Archive}]},
     #xmlel{name = <<"mute">>, children = [#xmlcdata{content = MutedUntil}]}].
