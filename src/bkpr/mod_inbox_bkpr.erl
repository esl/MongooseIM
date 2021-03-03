-module(mod_inbox_bkpr).

-include("mongoose_ns.hrl").
-include("jlib.hrl").

-import(mod_inbox_rdbms, [esc_string/1, esc_int/1]).

% SQL extensions
-export([sql_selection/0]).
-export([sql_filters/1]).
-export([sql_process/2]).
% Inbox extensions
-export([process_iq_conversation/4]).
-export([should_be_stored_in_inbox/1]).
-export([extensions_result/2]).
-export([fields_to_params/3]).

-spec process_iq_conversation(jid:jid(), jid:jid(), mongoose_acc:t(), jlib:iq()) ->
    {mongoose_acc:t(), jlib:iq()}.
process_iq_conversation(From, _To, Acc, #iq{type = get, sub_el = SubEl} = IQ) ->
    process_iq_conversation_get(Acc, IQ, From, SubEl);
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
    {LUser, LServer} = jid:to_lus(From),
    Query = ["SELECT archive, unread_count, muted_until ",
             "FROM inbox "
             "WHERE luser = ", esc_string(LUser), " AND "
                   "lserver = ", esc_string(LServer), " AND "
                   "remote_bare_jid = ", esc_string(BinEntryJID)],
    case execute_requests(LServer, Query) of
        {error, Msg} ->
            return_error(Acc, IQ, Msg);
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
    case form_to_query(CurrentTS, Requests0, []) of
        {error, Msg} ->
            return_error(Acc, IQ, Msg);
        Requests ->
            process_requests(Acc, IQ, From, EntryJID, CurrentTS, Requests)
    end.

-spec process_requests(mongoose_acc:t(), jlib:iq(), jid:jid(), jid:jid(), integer(), iolist()) ->
    {mongoose_acc:t(), jlib:iq()}.
process_requests(Acc, IQ, From, EntryJID, CurrentTS, Requests) ->
    {LUser, LServer} = jid:to_lus(From),
    BinEntryJID = jid:to_binary(jid:to_lus(EntryJID)),
    Query = build_sql_query(LUser, LServer, BinEntryJID, CurrentTS, Requests),
    case execute_requests(LServer, Query) of
        {error, Msg} ->
            return_error(Acc, IQ, Msg);
        Result ->
            forward_request(Acc, IQ, From, BinEntryJID, Result, CurrentTS)
    end.

-spec build_sql_query(jid:luser(), jid:lserver(), binary(), integer(), iolist()) -> iolist().
build_sql_query(LUser, LServer, BinEntryJID, CurrentTS, Requests) ->
    ["UPDATE inbox ",
     "SET ", Requests, "timestamp=", esc_int(CurrentTS),
     "WHERE "
         "luser=", esc_string(LUser), " AND "
         "lserver=", esc_string(LServer), " AND "
         "remote_bare_jid=", esc_string(BinEntryJID),
     "RETURNING archive, unread_count, muted_until;"].

-spec execute_requests(jid:lserver(), iolist()) ->
    {_,_,_} | {error, binary()}.
execute_requests(LServer, Query) ->
    case mongoose_rdbms:sql_query(LServer, Query) of
        {error, Msg} ->
            {error, Msg};
        {updated, 0, []} ->
            {error, <<"item-not-found">>};
        {updated, 1, [Result]} ->
            Result;
        {selected, [Selected]} ->
            Selected
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

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

-spec build_result({binary(), binary(), binary()}, integer()) -> [exml:element()].
build_result({Archived, UnreadCount, MutedUntil}, CurrentTS) ->
    NumericMutedUntil = mongoose_rdbms:result_to_integer(MutedUntil),
    [
     kv_to_el(<<"archive">>, expand_bin_bool(Archived)),
     kv_to_el(<<"read">>, read_or_not(UnreadCount)),
     kv_to_el(<<"mute">>, maybe_muted_until(NumericMutedUntil, CurrentTS))
    ].

-spec kv_to_el(binary(), binary()) -> exml:element().
kv_to_el(Key, Value) ->
    #xmlel{name = Key, children = [#xmlcdata{content = Value}]}.

-spec read_or_not(binary()) -> binary().
read_or_not(<<"0">>) -> <<"true">>;
read_or_not(_) -> <<"false">>.

-spec expand_bin_bool(binary()) -> binary().
expand_bin_bool(<<"t">>) -> <<"true">>;
expand_bin_bool(<<"f">>) -> <<"false">>.

-spec maybe_muted_until(integer()) -> binary().
maybe_muted_until(Val) ->
    CurrentTS = os:system_time(microsecond),
    maybe_muted_until(Val, CurrentTS).

-spec maybe_muted_until(integer(), integer()) -> binary().
maybe_muted_until(0, _) -> <<"0">>;
maybe_muted_until(MutedUntil, CurrentTS) ->
    case CurrentTS =< MutedUntil of
        true -> list_to_binary(calendar:system_time_to_rfc3339(MutedUntil, [{offset, "Z"}, {unit, microsecond}]));
        false -> <<"0">>
    end.

-spec sql_selection() -> iolist().
sql_selection() ->
    [", archive, muted_until "].

-spec sql_filters(map()) -> iolist().
sql_filters(Params) ->
    sql_and_where_archive(maps:get(archive, Params, undefined)).

-spec sql_and_where_archive(boolean() | undefined) -> iolist().
sql_and_where_archive(true) -> [" AND archive = true "];
sql_and_where_archive(false) -> [" AND archive = false "];
sql_and_where_archive(undefined) -> [].

-spec sql_process(binary(), binary() | integer()) -> {binary(), binary()}.
sql_process(Archive, MutedUntil) ->
    Extra1 = expand_bin_bool(Archive),
    Extra2 = maybe_muted_until(mongoose_rdbms:result_to_integer(MutedUntil)),
    {Extra1, Extra2}.

-spec return_error(mongoose_acc:t(), jlib:iq(), any()) ->
    {mongoose_acc:t(), jlib:iq()}.
return_error(Acc, IQ, Msg) when is_binary(Msg) ->
    {Acc, IQ#iq{type = error, sub_el = [mongoose_xmpp_errors:bad_request(<<"en">>, Msg)]}}.

-spec form_to_query(integer(), [exml:element()], iolist()) -> iolist() | {error, binary()}.
form_to_query(_, [], []) ->
    {error, <<"no-property">>};
form_to_query(_, [], Acc) ->
    Acc;
form_to_query(TS, [#xmlel{name = <<"archive">>,
                          children = [#xmlcdata{content = <<"true">>}]} | Rest], Acc) ->
    form_to_query(TS, Rest, ["archive=true," | Acc]);
form_to_query(TS, [#xmlel{name = <<"archive">>,
                          children = [#xmlcdata{content = <<"false">>}]} | Rest], Acc) ->
    form_to_query(TS, Rest, ["archive=false," | Acc]);
form_to_query(TS, [#xmlel{name = <<"read">>,
                          children = [#xmlcdata{content = <<"true">>}]} | Rest], Acc) ->
    form_to_query(TS, Rest, ["unread_count=0," | Acc]);
form_to_query(TS, [#xmlel{name = <<"read">>,
                          children = [#xmlcdata{content = <<"false">>}]} | Rest], Acc) ->
    form_to_query(TS, Rest, ["unread_count = CASE unread_count WHEN 0 THEN 1 ELSE unread_count END," | Acc]);
form_to_query(TS, [#xmlel{name = <<"mute">>,
                          children = [#xmlcdata{content = Value}]} | Rest], Acc) ->
    try erlang:binary_to_integer(Value) of
        N when N >= 0 ->
            MutedUntilSec = erlang:convert_time_unit(TS, microsecond, second) + N,
            MutedUntilMicroSec = erlang:convert_time_unit(MutedUntilSec, second, microsecond),
            form_to_query(TS, Rest, ["muted_until=", esc_int(MutedUntilMicroSec), "," | Acc]);
        _ -> {error, <<"bad-request">>}
    catch error:badarg -> {error, <<"bad-request">>}
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

-spec fields_to_params(binary(), binary(), map()) -> map() | {error, bad_request | unknown_field}.
fields_to_params(<<"archive">>, <<"true">>, Acc) ->
    Acc#{archive => true};
fields_to_params(<<"archive">>, <<"false">>, Acc) ->
    Acc#{archive => false};
fields_to_params(<<"archive">>, _, _) ->
    {error, bad_request};
fields_to_params(_, _, _) ->
    {error, unknown_field}.
