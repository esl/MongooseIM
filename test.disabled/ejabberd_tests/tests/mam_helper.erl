%%==============================================================================
%% Copyright 2016 Erlang Solutions Ltd.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%==============================================================================
-module(mam_helper).

-compile(export_all).

-include("mam_helper.hrl").
-include_lib("escalus/include/escalus.hrl").
-include_lib("escalus/include/escalus_xmlns.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("exml/include/exml_stream.hrl").

-import(muc_helper,
        [muc_host/0,
         room_address/1, room_address/2,
         stanza_muc_enter_room/2,
         stanza_to_room/2,
         start_room/5]).

rpc_apply(M, F, Args) ->
    case rpc_call(M, F, Args) of
    {badrpc, Reason} ->
        ct:fail("~p:~p/~p with arguments ~w fails with reason ~p.",
                [M, F, length(Args), Args, Reason]);
    Result ->
        Result
    end.

rpc_call(M, F, A) ->
    Node = ct:get_config({hosts, mim, node}),
    Cookie = escalus_ct:get_config(ejabberd_cookie),
    escalus_ct:rpc_call(Node, M, F, A, 10000, Cookie).

mam03_props() ->
    [{data_form, true},                 %% send data forms
     {final_message, true},             %% expect final message with <fin/> inside
     {result_format, mess_fin},         %% RSM is inside final message
     {mam_ns, mam_ns_binary_v03()}].    %% v0.3 namespace

mam04_props() ->
    [{data_form, true},                 %% send data forms
     {result_format, iq_fin},           %% RSM is inside iq with <fin/> inside
     {mam_ns, mam_ns_binary_v04()}].

mam06_props() ->
     [{data_form, true},                 %% send data forms
     {result_format, iq_fin},           %% RSM is inside iq with <fin/> inside
     {mam_ns, mam_ns_binary_v06()}].   

respond_messages(#mam_archive_respond{respond_messages=Messages}) ->
    Messages.

respond_iq(#mam_archive_respond{respond_iq=IQ}) ->
    IQ.

respond_fin(#mam_archive_respond{respond_fin=Fin}) ->
    Fin.

get_prop(Key, undefined) ->
    get_prop(Key, []);
get_prop(final_message, P) ->
    proplists:get_bool(final_message, P);
get_prop(result_format, P) ->
    proplists:get_value(result_format, P, iq_query);
get_prop(mam_ns, P) ->
    proplists:get_value(mam_ns, P, mam_ns_binary());
get_prop(data_form, P) ->
    proplists:get_bool(data_form, P).

wait_archive_respond(P, User) ->
    case get_prop(final_message, P) of
        true ->
            wait_archive_respond_fin(User);
        false ->
            wait_archive_respond_nofin(User)
    end.

wait_archive_respond_fin(User) ->
    %% rot1
    [IQ|MessagesAndFin] = wait_archive_respond_v03(User),
    [Fin|Messages] = lists:reverse(MessagesAndFin),
    #mam_archive_respond{
       respond_iq=IQ,
       respond_fin=Fin,
       respond_messages=lists:reverse(Messages)}.

wait_archive_respond_nofin(User) ->
    %% rot1
    [IQ|Messages] = lists:reverse(wait_archive_respond_v02(User)),
    #mam_archive_respond{
       respond_iq=IQ,
       respond_messages=lists:reverse(Messages)}.

%% MAM v0.2 respond
wait_archive_respond_v02(User) ->
    S = escalus:wait_for_stanza(User, 5000),
    case escalus_pred:is_iq_error(S) of
        true ->
            ct:pal("Stanza ~p", [S]),
            ct:fail("Unexpected error IQ.", []);
        false -> ok
    end,
    case escalus_pred:is_iq_result(S) of
        true  -> [S];
        false -> [S|wait_archive_respond_v02(User)]
    end.

%% MAM v0.3 respond
wait_archive_respond_v03(User) ->
    IQ = escalus:wait_for_stanza(User, 5000),
    case escalus_pred:is_iq_error(IQ) of
        true ->
            ct:pal("Stanza ~p", [IQ]),
            ct:fail("Unexpected error IQ.", []);
        false -> ok
    end,
    escalus:assert(is_iq_result, IQ),
    [IQ|wait_archive_respond_v03_part2(User)].

wait_archive_respond_v03_part2(User) ->
    M = escalus:wait_for_stanza(User, 5000),
    escalus:assert(is_message, M),
    case is_final_message(M) of
        true ->
            [M];
        false ->
            [M|wait_archive_respond_v03_part2(User)]
    end.

is_final_message(M) ->
    undefined =/= exml_query:subelement(M, <<"fin">>).

assert_respond_size(Size, Respond=#mam_archive_respond{respond_messages=Messages})
      when length(Messages) =:= Size ->
    Respond;
assert_respond_size(ExpectedSize, #mam_archive_respond{respond_messages=Messages}) ->
    RespondSize = length(Messages),
    ct:fail("Respond size is ~p, ~p is expected.", [RespondSize, ExpectedSize]).
    %% void()

assert_respond_query_id(_P, _ExpectedQueryId, #result_iq{query_id=not_supported}) ->
    ok;
assert_respond_query_id(_P, ExpectedQueryId, #result_iq{query_id=QueryId}) ->
    ?assert_equal(ExpectedQueryId, QueryId).

make_iso_time(Micro) ->
    Now = usec:to_now(Micro),
    DateTime = calendar:now_to_datetime(Now),
    {Time, TimeZone} = rpc_apply(jlib, timestamp_to_iso, [DateTime, utc]),
    Time ++ TimeZone.

generate_message_text(N) when is_integer(N) ->
    <<"Message #", (list_to_binary(integer_to_list(N)))/binary>>.

rsm_send(Config, User, Packet) ->
    case ?config(with_rsm, Config) of
        true ->
            BWithJID = nick_to_jid(bob, Config),
            rsm_send1(Config, User, add_with_jid(BWithJID, Packet));
        _ ->
            rsm_send1(Config, User, Packet)
    end.

rsm_send1(Config, User, Packet) ->
    case ?config(muc_rsm, Config) of
        true ->
            Room = ?config(room, Config),
            escalus:send(User, stanza_to_room(Packet, Room));
        _ ->
            escalus:send(User, Packet)
    end.

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

nick(User) -> escalus_utils:get_username(User).

mam_ns_binary() -> <<"urn:xmpp:mam:tmp">>.
mam_ns_binary_v03() -> <<"urn:xmpp:mam:0">>.
mam_ns_binary_v04() -> <<"urn:xmpp:mam:1">>.
mam_ns_binary_v06() -> <<"urn:xmpp:mam:2">>.
namespaces() -> [mam_ns_binary(), mam_ns_binary_v03(), mam_ns_binary_v04(), mam_ns_binary_v06()].
muc_ns_binary() -> <<"http://jabber.org/protocol/muc">>.

stanza_purge_single_message(MessId) ->
    escalus_stanza:iq(<<"set">>, [#xmlel{
       name = <<"purge">>,
       attrs = [{<<"xmlns">>, mam_ns_binary()}, {<<"id">>, MessId}]
    }]).

stanza_purge_multiple_messages(BStart, BEnd, BWithJID) ->
    escalus_stanza:iq(<<"set">>, [#xmlel{
       name = <<"purge">>,
       attrs = [{<<"xmlns">>, mam_ns_binary()}],
       children = skip_undefined([
           maybe_start_elem(BStart),
           maybe_end_elem(BEnd),
           maybe_with_elem(BWithJID)])
    }]).


skip_undefined(Xs) ->
    [X || X <- Xs, X =/= undefined].

maybe_attr(_, undefined) ->
    [];
maybe_attr(K, V) ->
    [{K, V}].

mam_ns_attr(P) ->
    [{<<"xmlns">>, get_prop(mam_ns, P)}].

maybe_start_elem(undefined) ->
    undefined;
maybe_start_elem(BStart) ->
    #xmlel{
        name = <<"start">>,
        children = [#xmlcdata{content = BStart}]}.

maybe_end_elem(undefined) ->
    undefined;
maybe_end_elem(BEnd) ->
    #xmlel{
        name = <<"end">>,
        children = [#xmlcdata{content = BEnd}]}.

maybe_with_elem(undefined) ->
    undefined;
maybe_with_elem(BWithJID) ->
    #xmlel{
        name = <<"with">>,
        children = [#xmlcdata{content = BWithJID}]}.

%% An optional 'queryid' attribute allows the client to match results to
%% a certain query.
stanza_archive_request(P, QueryId) ->
    stanza_lookup_messages_iq(P, QueryId,
                              undefined, undefined,
                              undefined, undefined, undefined).

stanza_date_range_archive_request(P) ->
    stanza_lookup_messages_iq(P, undefined,
                              "2010-06-07T00:00:00Z", "2010-07-07T13:23:54Z",
                              undefined, undefined, undefined).

stanza_date_range_archive_request_not_empty(P, Start, Stop) ->
    stanza_lookup_messages_iq(P, undefined,
                              Start, Stop,
                              undefined, undefined, undefined).

stanza_limit_archive_request(P) ->
    stanza_lookup_messages_iq(P, undefined, "2010-08-07T00:00:00Z",
                              undefined, undefined, #rsm_in{max=10}, undefined).

stanza_page_archive_request(P, QueryId, RSM) ->
    stanza_lookup_messages_iq(P, QueryId, undefined, undefined, undefined, RSM, undefined).

stanza_filtered_by_jid_request(P, BWithJID) ->
    stanza_lookup_messages_iq(P, undefined, undefined,
                              undefined, BWithJID, undefined, undefined).

stanza_text_search_archive_request(P, QueryId, TextSearch) ->
    stanza_lookup_messages_iq(P, QueryId,
                              undefined, undefined,
                              undefined, undefined, TextSearch).

stanza_lookup_messages_iq(P, QueryId, BStart, BEnd, BWithJID, RSM, TextSearch) ->
    case get_prop(data_form, P) of
        false ->
            stanza_lookup_messages_iq_v02(P, QueryId, BStart, BEnd, BWithJID, RSM);
        true ->
            stanza_lookup_messages_iq_v03(P, QueryId, BStart, BEnd, BWithJID, RSM, TextSearch)
    end.

stanza_lookup_messages_iq_v03(P, QueryId, BStart, BEnd, BWithJID, RSM, TextSearch) ->
    escalus_stanza:iq(<<"set">>, [#xmlel{
       name = <<"query">>,
       attrs = mam_ns_attr(P)
            ++ maybe_attr(<<"queryid">>, QueryId),
       children = skip_undefined([
           form_x(BStart, BEnd, BWithJID, RSM, TextSearch),
           maybe_rsm_elem(RSM)])
    }]).


form_x(BStart, BEnd, BWithJID, RSM) ->
    form_x(BStart, BEnd, BWithJID, RSM, undefined).
form_x(BStart, BEnd, BWithJID, RSM, TextSearch) ->
    #xmlel{name = <<"x">>,
           attrs = [{<<"xmlns">>, <<"jabber:x:data">>}],
           children = skip_undefined([
                form_field(<<"start">>, BStart),
                form_field(<<"end">>, BEnd),
                form_field(<<"with">>, BWithJID),
                form_field(<<"full-text-search">>, TextSearch)]
                ++ form_extra_fields(RSM)
                ++ form_border_fields(RSM))}.

form_extra_fields(undefined) ->
    [];
form_extra_fields(#rsm_in{simple=Simple, opt_count=OptCount}) ->
    [form_bool_field(<<"simple">>, Simple),
     form_bool_field(<<"opt_count">>, OptCount)].

form_border_fields(undefined) ->
    [];
form_border_fields(#rsm_in{
        before_id=BeforeId, after_id=AfterId, from_id=FromId, to_id=ToId}) ->
    [form_field(<<"before_id">>, BeforeId),
     form_field(<<"after_id">>, AfterId),
     form_field(<<"from_id">>, FromId),
     form_field(<<"to_id">>, ToId)].

form_type_field(MamNs) ->
    form_field(<<"FORM_TYPE">>, MamNs).

form_field(_VarName, undefined) ->
    undefined;
form_field(VarName, VarValue) ->
    #xmlel{name = <<"field">>, attrs = [{<<"var">>, VarName}],
           children = [#xmlel{name = <<"value">>,
                              children = [#xmlcdata{content = VarValue}]}]}.

form_bool_field(Name, true) ->
    form_field(Name, <<"true">>);
form_bool_field(_Name, _) ->
    undefined.

stanza_lookup_messages_iq_v02(P, QueryId, BStart, BEnd, BWithJID, RSM) ->
    escalus_stanza:iq(<<"get">>, [#xmlel{
       name = <<"query">>,
       attrs = mam_ns_attr(P)
            ++ maybe_attr(<<"queryid">>, QueryId)
            ++ border_attributes(RSM),
       children = skip_undefined([
           maybe_simple_elem(RSM),
           maybe_opt_count_elem(RSM),
           maybe_start_elem(BStart),
           maybe_end_elem(BEnd),
           maybe_with_elem(BWithJID),
           maybe_rsm_elem(RSM)])
    }]).

stanza_retrieve_form_fields(QueryId, NS) ->
    escalus_stanza:iq(<<"get">>, [#xmlel{
        name = <<"query">>,
        attrs =     [{<<"xmlns">>, NS}]
        ++ maybe_attr(<<"queryid">>, QueryId),
        children = []
    }]).

maybe_simple_elem(#rsm_in{simple=true}) ->
    #xmlel{name = <<"simple">>};
maybe_simple_elem(_) ->
    undefined.

maybe_opt_count_elem(#rsm_in{opt_count=true}) ->
    #xmlel{name = <<"opt_count">>};
maybe_opt_count_elem(_) ->
    undefined.

border_attributes(undefined) ->
    [];
border_attributes(#rsm_in{
        before_id=BeforeId, after_id=AfterId, from_id=FromId, to_id=ToId}) ->
    maybe_attr(<<"before_id">>, BeforeId)
    ++ maybe_attr(<<"after_id">>, AfterId)
    ++ maybe_attr(<<"from_id">>, FromId)
    ++ maybe_attr(<<"to_id">>, ToId).

maybe_rsm_elem(undefined) ->
    undefined;
maybe_rsm_elem(#rsm_in{max=Max, direction=Direction, id=Id, index=Index}) ->
    #xmlel{name = <<"set">>,
           children = skip_undefined([
                maybe_rsm_max(Max),
                maybe_rsm_index(Index),
                maybe_rsm_direction(Direction, Id)])}.

rsm_id_children(undefined) -> [];
rsm_id_children(Id) -> [#xmlcdata{content = Id}].

maybe_rsm_direction(undefined, undefined) ->
    undefined;
maybe_rsm_direction(Direction, Id) ->
    #xmlel{
        name = atom_to_binary(Direction, latin1),
        children = rsm_id_children(Id)}.

maybe_rsm_index(undefined) ->
    undefined;
maybe_rsm_index(Index) when is_integer(Index) ->
    #xmlel{
        name = <<"index">>,
        children = [#xmlcdata{content = integer_to_list(Index)}]}.

maybe_rsm_max(undefined) ->
    undefined;
maybe_rsm_max(Max) when is_integer(Max) ->
    #xmlel{
        name = <<"max">>,
        children = [#xmlcdata{content = integer_to_list(Max)}]}.

add_with_jid(BWithJID,
    IQ=#xmlel{children=[
        Query=#xmlel{children=QueryChildren}]}) ->
    IQ#xmlel{children=[
        Query#xmlel{children=[maybe_with_elem(BWithJID) | QueryChildren]}]}.

assert_only_one_of_many_is_equal(Archived, Sent) ->
    Scanned = lists:map(fun parse_forwarded_message/1, Archived),
    Same = lists:filter(fun (Stanza) -> is_same_message_text(Stanza, Sent) end, Scanned),
    ?assert_equal(1, erlang:length(Same)).

assert_not_stored(Archived, Sent) ->
    Scanned = lists:map(fun parse_forwarded_message/1, Archived),
    Same = lists:filter(fun (Stanza) -> is_same_message_text(Stanza, Sent) end, Scanned),
    ?assert_equal(0, erlang:length(Same)).

is_same_message_text(Stanza, Raw) ->
    #forwarded_message{message_body = A} = Stanza,
    A =:= Raw.

-spec verify_archived_muc_light_aff_msg(
        Msg :: #forwarded_message{}, AffUsersChanges :: [{escalus:client(), binary()}],
        IsCreate :: boolean()) -> [].
verify_archived_muc_light_aff_msg(Msg, AffUsersChanges, IsCreate) ->
    BinAffUsersChanges = muc_light_helper:bin_aff_users(AffUsersChanges),
    [X] = Msg#forwarded_message.message_xs,
    ProperNS = muc_light_helper:ns_muc_light_affiliations(),
    ProperNS = exml_query:attr(X, <<"xmlns">>),
    undefined = exml_query:subelement(X, <<"prev-version">>),
    Version = exml_query:path(X, [{element, <<"version">>}, cdata]),
    true = IsCreate orelse is_binary(Version),
    Items = exml_query:subelements(X, <<"user">>),
    muc_light_helper:verify_aff_users(Items, BinAffUsersChanges).

%% ----------------------------------------------------------------------
%% PREFERENCE QUERIES

stanza_prefs_set_request(DefaultMode, AlwaysJIDs, NeverJIDs, Namespace) ->
    AlwaysEl = #xmlel{name = <<"always">>,
                      children = encode_jids(AlwaysJIDs)},
    NeverEl  = #xmlel{name = <<"never">>,
                      children = encode_jids(NeverJIDs)},
    escalus_stanza:iq(<<"set">>, [#xmlel{
       name = <<"prefs">>,
       attrs = [{<<"xmlns">>, Namespace}]
               ++ [{<<"default">>, DefaultMode} || is_def(DefaultMode)],
       children = [AlwaysEl, NeverEl]
    }]).

stanza_prefs_get_request(Namespace) ->
    escalus_stanza:iq(<<"get">>, [#xmlel{
       name = <<"prefs">>,
       attrs = [{<<"xmlns">>, Namespace}]
    }]).

stanza_query_get_request(Namespace) ->
    escalus_stanza:iq(<<"get">>, [#xmlel{
        name = <<"query">>,
        attrs = [{<<"xmlns">>, Namespace}]
    }]).

%% Allows to cdata to be put as it is
encode_jids(JIDs) ->
    [encode_jid_or_cdata(JID) || JID <- JIDs].

encode_jid_or_cdata({xmlcdata, Text}) ->
    {xmlcdata, Text};
encode_jid_or_cdata(JID) ->
    #xmlel{name = <<"jid">>,
           children = [#xmlcdata{content = JID}]}.

%% ----------------------------------------------------------------------
%% PARSING RESPONDS

parse_forwarded_message(#xmlel{name = <<"message">>,
                               attrs = Attrs, children = Children}) ->
    M = #forwarded_message{
        from = proplists:get_value(<<"from">>, Attrs),
        to   = proplists:get_value(<<"to">>, Attrs),
        has_x_user_element = false},
    lists:foldl(fun parse_children_message/2, M, Children).

parse_children_message(#xmlel{name = <<"result">>,
                              attrs = Attrs,
                              children = Children}, M) ->
    M1 = M#forwarded_message{
        result_queryid = proplists:get_value(<<"queryid">>, Attrs),
        result_id      = proplists:get_value(<<"id">>, Attrs)},
    lists:foldl(fun parse_children_message_result/2, M1, Children).

parse_children_message_result(#xmlel{name = <<"forwarded">>,
                                     children = Children}, M) ->
    lists:foldl(fun parse_children_message_result_forwarded/2, M, Children).


parse_children_message_result_forwarded(#xmlel{name = <<"delay">>,
                                               attrs = Attrs}, M) ->
    M#forwarded_message{
        delay_from  = proplists:get_value(<<"from">>, Attrs),
        delay_stamp = proplists:get_value(<<"stamp">>, Attrs)};
parse_children_message_result_forwarded(#xmlel{name = <<"message">>,
                                               attrs = Attrs,
                                               children = Children}, M) ->
    M1 = M#forwarded_message{
        message_to   = proplists:get_value(<<"to">>, Attrs),
        message_from = proplists:get_value(<<"from">>, Attrs),
        message_type = proplists:get_value(<<"type">>, Attrs)},
    lists:foldl(fun parse_children_message_result_forwarded_message/2,
                M1, Children).

parse_children_message_result_forwarded_message(#xmlel{name = <<"body">>,
                                                       children = [{xmlcdata, Body}]}, M) ->
    M#forwarded_message{message_body = Body};
parse_children_message_result_forwarded_message(#xmlel{name = <<"x">>,
                                                       attrs = Attrs} = XEl, M) ->
    IsUser = lists:member({<<"xmlns">>, <<"http://jabber.org/protocol/muc#user">>}, Attrs),
    M#forwarded_message{has_x_user_element = IsUser,
                        message_xs = [XEl | M#forwarded_message.message_xs]};
%% Parse `<archived />' here.
parse_children_message_result_forwarded_message(_, M) ->
    M.

%% Num is 1-based.
message_id(Num, Config) ->
    AllMessages = proplists:get_value(all_messages, Config),
    #forwarded_message{result_id=Id} = lists:nth(Num, AllMessages),
    Id.

%% @doc Result query iq.
%%
%% [{xmlel,<<"iq">>,
%%     [{<<"from">>,<<"alice@localhost">>},
%%      {<<"to">>,<<"alice@localhost/res1">>},
%%      {<<"id">>,<<"387862024ce65379b049e19751e4309e">>},
%%      {<<"type">>,<<"result">>}],
%%     []}]
%%
%%
%%  [{xmlel,<<"iq">>,
%%       [{<<"from">>,<<"alice@localhost">>},
%%        {<<"to">>,<<"alice@localhost/res1">>},
%%        {<<"id">>,<<"c256a18c4b720465e215a81362d41eb7">>},
%%        {<<"type">>,<<"result">>}],
%%       [{xmlel,<<"query">>,
%%            [{<<"xmlns">>,<<"urn:xmpp:mam:tmp">>}],
%%            [{xmlel,<<"set">>,
%%                 [{<<"xmlns">>,<<"http://jabber.org/protocol/rsm">>}],
%%                 [{xmlel,<<"first">>,
%%                      [{<<"index">>,<<"10">>}],
%%                      [{xmlcdata,<<"103439">>}]},
%%                  {xmlel,<<"last">>,[],[{xmlcdata,<<"103447">>}]},
%%                  {xmlel,<<"count">>,[],[{xmlcdata,<<"15">>}]}]}]}]}]
parse_result_iq(P, Result) ->
    case get_prop(result_format, P) of
        mess_fin ->
            parse_fin_and_iq(Result);
        iq_query ->
            parse_legacy_iq(respond_iq(Result));
        iq_fin ->
            parse_fin_iq(Result)
    end.

%% MAM v0.3
parse_fin_and_iq(#mam_archive_respond{respond_iq=IQ, respond_fin=FinMsg}) ->
    Fin = exml_query:subelement(FinMsg, <<"fin">>),
    Set = exml_query:subelement(Fin, <<"set">>),
    QueryId = exml_query:attr(Fin, <<"queryid">>),
    parse_set_and_iq(IQ, Set, QueryId).

%% MAM v0.4
parse_fin_iq(#mam_archive_respond{respond_iq=IQ, respond_fin=undefined}) ->
    Fin = exml_query:subelement(IQ, <<"fin">>),
    Set = exml_query:subelement(Fin, <<"set">>),
    parse_set_and_iq(IQ, Set, not_supported).

%% MAM v0.2
parse_legacy_iq(IQ) ->
    Fin = exml_query:subelement(IQ, <<"query">>),
    Set = exml_query:subelement(Fin, <<"set">>),
    parse_set_and_iq(IQ, Set, not_supported).

parse_set_and_iq(IQ, Set, QueryId) ->
    #result_iq{
        query_id    = QueryId,
        from        = exml_query:attr(IQ, <<"from">>),
        to          = exml_query:attr(IQ, <<"to">>),
        id          = exml_query:attr(IQ, <<"id">>),
        first       = exml_query:path(Set, [{element, <<"first">>}, cdata]),
        first_index = maybe_binary_to_integer(exml_query:path(Set, [{element, <<"first">>},
                                                                    {attr, <<"index">>}])),
        last        = exml_query:path(Set, [{element, <<"last">>}, cdata]),
        count       = maybe_binary_to_integer(exml_query:path(Set, [{element, <<"count">>},
                                                                    cdata]))}.


is_def(X) -> X =/= undefined.



parse_prefs_result_iq(#xmlel{name = <<"iq">>, children = Children}) ->
    IQ = #prefs_result_iq{},
    lists:foldl(fun parse_children_prefs_iq/2, IQ, Children).

parse_children_prefs_iq(#xmlel{name = <<"prefs">>,
                               attrs = Attrs, children = Children},
                        IQ) ->
    DefaultMode = proplists:get_value(<<"default">>, Attrs),
    IQ1 = IQ#prefs_result_iq{default_mode = DefaultMode},
    lists:foldl(fun parse_children_prefs_iq_prefs/2, IQ1, Children).


parse_children_prefs_iq_prefs(#xmlel{name = <<"always">>,
                                     children = Children},
                              IQ) ->
    IQ#prefs_result_iq{always_jids = lists:sort(parse_jids(Children))};
parse_children_prefs_iq_prefs(#xmlel{name = <<"never">>,
                                     children = Children},
                              IQ) ->
    IQ#prefs_result_iq{never_jids = lists:sort(parse_jids(Children))}.


parse_jids(Els) ->
    [escalus_utils:jid_to_lower(JID) %% MongooseIM normalizes JIDs
     || #xmlel{name = <<"jid">>, children = [{xmlcdata, JID}]} <- Els].

%% <iq type='error' id='q29302'>
%%   <error type='modify'>
%%     <policy-violation xmlns='urn:ietf:params:xml:ns:xmpp-stanzas'/>
%%     <text xmlns='urn:ietf:params:xml:ns:xmpp-stanzas'>Too many results</text>
%%   </error>
%% </iq>
parse_error_iq(#xmlel{name = <<"iq">>,
                      attrs = Attrs, children = Children}) ->

    IQ = #error_iq{
        type = proplists:get_value(<<"type">>, Attrs),
        id   = proplists:get_value(<<"id">>, Attrs)},
    lists:foldl(fun parse_children_error_iq/2, IQ, Children).


parse_children_error_iq(#xmlel{name = <<"error">>,
                               attrs = Attrs, children = Children}, IQ) ->
    IQ1 = IQ#error_iq{
        error_type = proplists:get_value(<<"type">>, Attrs)},
    lists:foldl(fun parse_children_error_iq_error/2, IQ1, Children);
parse_children_error_iq(_, IQ) ->
    IQ.

parse_children_error_iq_error(#xmlel{name = <<"text">>,
                                     children = [{xmlcdata, Text}]}, IQ) ->
    IQ#error_iq{text = Text};
parse_children_error_iq_error(#xmlel{name = Condition}, IQ) ->
    IQ#error_iq{condition = Condition};
parse_children_error_iq_error(_, IQ) ->
    IQ.

%%--------------------------------------------------------------------
%% Helpers (muc)
%%--------------------------------------------------------------------

generate_rpc_jid({_, User}) ->
    {username, Username} = lists:keyfind(username, 1, User),
    {server, Server} = lists:keyfind(server, 1, User),
    LUsername = escalus_utils:jid_to_lower(Username),
    LServer = escalus_utils:jid_to_lower(Server),
    {jid, Username, Server, <<"rpc">>, LUsername, LServer, <<"rpc">>}.

start_alice_room(Config) ->
    %% TODO: ensure, that the room's archive is empty
    escalus_users:get_username(Config, alice),
    RoomName = room_name(Config),
    RoomNick = <<"alicesnick">>,
    [Alice | _] = ?config(escalus_users, Config),
    start_room(Config, Alice, RoomName, RoomNick,
               [{persistent, true},
                {anonymous, false}]).

start_alice_protected_room(Config) ->
    RoomName = room_name(Config),
    RoomNick = <<"alicesnick">>,
    [Alice | _] = ?config(escalus_users, Config),
    start_room(Config, Alice, RoomName, RoomNick,
               [{persistent, true},
                {password_protected, true},
                {password, <<"secret">>}]).

start_alice_anonymous_room(Config) ->
    RoomName = room_name(Config),
    RoomNick = <<"alicesnick">>,
    [Alice | _] = ?config(escalus_users, Config),
    start_room(Config, Alice, RoomName, RoomNick, [{anonymous, true}]).

send_muc_rsm_messages(Config) ->
    Pid = self(),
    Room = ?config(room, Config),
    RoomAddr = room_address(Room),
    P = ?config(props, Config),
    F = fun(Alice, Bob) ->
        escalus:send(Alice, stanza_muc_enter_room(Room, nick(Alice))),
        escalus:send(Bob, stanza_muc_enter_room(Room, nick(Bob))),

        escalus:wait_for_stanzas(Bob, 3),
        escalus:wait_for_stanzas(Alice, 3),

        %% Alice sends messages to Bob.
        lists:foreach(fun(N) ->
                              escalus:send(Alice, escalus_stanza:groupchat_to(
                                                    RoomAddr, generate_message_text(N)))
                      end, lists:seq(1, 15)),
        %% Bob is waiting for 15 messages for 5 seconds.
        escalus:wait_for_stanzas(Bob, 15, 5000),
        escalus:wait_for_stanzas(Alice, 15, 5000),

        maybe_wait_for_archive(Config),

        %% Get whole history.
        escalus:send(Alice,
            stanza_to_room(stanza_archive_request(P, <<"all_room_messages">>), Room)),
        AllMessages =
            respond_messages(assert_respond_size(15, wait_archive_respond(P, Alice))),
        ParsedMessages = [parse_forwarded_message(M) || M <- AllMessages],
        Pid ! {parsed_messages, ParsedMessages},
        ok
        end,
    Config1 = escalus:init_per_testcase(pre_rsm, Config),
    escalus:story(Config1, [{alice, 1}, {bob, 1}], F),
    ParsedMessages = receive {parsed_messages, PM} -> PM
                     after 5000 -> error(receive_timeout) end,

    escalus:end_per_testcase(pre_rsm, Config1),
    [{all_messages, ParsedMessages}|Config].

send_rsm_messages(Config) ->
    Pid = self(),
%%    Room = ?config(room, Config),
    P = ?config(props, Config),
    F = fun(Alice, Bob) ->
        %% Alice sends messages to Bob.
        lists:foreach(fun(N) ->
                              escalus:send(Alice,
                                           escalus_stanza:chat_to(Bob, generate_message_text(N))),
                              timer:sleep(5)
                      end, lists:seq(1, 15)),
        %% Bob is waiting for 15 messages for 5 seconds.
        escalus:wait_for_stanzas(Bob, 15, 5000),
        maybe_wait_for_archive(Config),
        %% Get whole history.
        rsm_send(Config, Alice, stanza_archive_request(P, <<"all_messages">>)),
        AllMessages =
            respond_messages(assert_respond_size(15, wait_archive_respond(P, Alice))),
        ParsedMessages = [parse_forwarded_message(M) || M <- AllMessages],
        Pid ! {parsed_messages, ParsedMessages},
        ok
        end,
    Config1 = escalus:init_per_testcase(pre_rsm, Config),
    escalus:story(Config1, [{alice, 1}, {bob, 1}], F),
    ParsedMessages = receive {parsed_messages, PM} -> PM
                     after 5000 -> error(receive_timeout) end,

    escalus:end_per_testcase(pre_rsm, Config1),
    [{all_messages, ParsedMessages}|Config].

append_subelem(Elem=#xmlel{children=Cs}, SubElem) ->
    Elem#xmlel{children=Cs ++ [SubElem]}.

archived_elem(By, Id) ->
    #xmlel{name = <<"archived">>,
           attrs = [{<<"by">>, By}, {<<"id">>, Id}]}.

clean_archives(Config) ->
    SUs = serv_users(Config),
    %% It is not the best place to delete these messages.
    [ok = delete_offline_messages(S, U) || {S, U} <- SUs],
    [ok = delete_archive(S, U) || {S, U} <- SUs],
    %% Retry 10 times if not empty
    [assert_empty_archive(S, U, 10) || {S, U} <- SUs],
    Config.

destroy_room(Config) ->
    clean_room_archive(Config),
    muc_helper:destroy_room(Config).

clean_room_archive(Config) ->
    Room = ?config(room, Config),
    delete_room_archive(muc_host(), Room),
    %% Retry 10 times if not empty
    assert_empty_room_archive(muc_host(), Room, 10),
    Config.

serv_users(Config) ->
    [serv_user(Config, UserSpec)
     || {_, UserSpec} <- escalus_users:get_users(all)].

serv_user(Config, UserSpec) ->
    [Username, Server, _Pass] = escalus_users:get_usp(Config, UserSpec),
    {Server, Username}.

%% @doc Check, that the archive is empty.
assert_empty_archive(Server, Username, RetryTimes) when is_integer(RetryTimes) ->
    %% Wait for zero messages in archive
    case wait_for_archive_size(Server, Username, RetryTimes, 0) of
       0 -> ok;
       X -> ct:fail({not_empty, Server, Username, {actual_size, X}})
    end.

wait_for_archive_size(Server, Username, _RetryTimes=0, _ExpectedSize) ->
    archive_size(Server, Username);
wait_for_archive_size(Server, Username, RetryTimes, ExpectedSize) when RetryTimes > 0 ->
    case archive_size(Server, Username) of
        ExpectedSize ->
            ExpectedSize;
        _ActualSize ->
            %% Wait and retry
            timer:sleep(100),
            wait_for_archive_size(Server, Username, RetryTimes-1, ExpectedSize)
    end.

wait_for_archive_size_or_warning(Server, Username, RetryTimes, ExpectedSize) ->
    case wait_for_archive_size(Server, Username, RetryTimes, ExpectedSize) of
        ExpectedSize -> ok;
        ActualSize ->
            ct:pal("issue=wait_for_archive_size_or_warning, expected_size=~p, actual_size=~p",
                   [ExpectedSize, ActualSize])
    end.

%% @doc Check, that the archive is empty.
assert_empty_room_archive(Server, Username, RetryTimes) ->
    %% Wait for zero messages in archive
    case wait_for_room_archive_size(Server, Username, RetryTimes, 0) of
       0 -> ok;
       X -> ct:fail({room_not_empty, Server, Username, {actual_size, X}})
    end.

wait_for_room_archive_size(Server, Username, _RetryTimes=0, _ExpectedSize) ->
    room_archive_size(Server, Username);
wait_for_room_archive_size(Server, Username, RetryTimes, ExpectedSize) when RetryTimes > 0 ->
    case room_archive_size(Server, Username) of
        ExpectedSize ->
            ExpectedSize;
        _ActualSize ->
            %% Wait and retry
            timer:sleep(100),
            wait_for_room_archive_size(Server, Username, RetryTimes-1, ExpectedSize)
    end.


archive_size(Server, Username) ->
    rpc_apply(mod_mam, archive_size, [Server, Username]).

room_archive_size(Server, Username) ->
    rpc_apply(mod_mam_muc, archive_size, [Server, Username]).

delete_archive(Server, Username) ->
    rpc_apply(mod_mam, delete_archive, [Server, Username]).

delete_room_archive(Server, Username) ->
    rpc_apply(mod_mam_muc, delete_archive, [Server, Username]).

delete_offline_messages(Server, Username) ->
    %% Do not care
    catch rpc_apply(mod_offline, remove_user, [Username, Server]),
    ok.

wait_message_range(P, Client, FromN, ToN) ->
    wait_message_range(P, Client, 15, FromN-1, FromN, ToN).

wait_message_range(P, Client, TotalCount, Offset, FromN, ToN) ->
    Result = wait_archive_respond(P, Client),
    Messages = respond_messages(Result),
    IQ = respond_iq(Result),
    Fin = respond_fin(Result),
    ParsedMessages = parse_messages(Messages),
    ParsedIQ = parse_result_iq(P, Result),
    try
        ?assert_equal(TotalCount, ParsedIQ#result_iq.count),
        ?assert_equal(Offset, ParsedIQ#result_iq.first_index),
        %% Compare body of the messages.
        ?assert_equal([generate_message_text(N) || N <- lists:seq(FromN, ToN)],
                      [B || #forwarded_message{message_body=B} <- ParsedMessages]),
        ok
    catch Class:Reason ->
        Stacktrace = erlang:get_stacktrace(),
        ct:pal("IQ: ~p~n"
               "Fin: ~p~n"
               "Messages: ~p~n"
               "Parsed messages: ~p~n",
               [IQ, Fin, Messages, ParsedMessages]),
        erlang:raise(Class, Reason, Stacktrace)
    end.


wait_empty_rset(P, Alice, TotalCount) ->
    Result = wait_archive_respond(P, Alice),
    IQ = respond_iq(Result),
    ?assert_equal([], respond_messages(Result)),
    ParsedIQ = parse_result_iq(P, Result),
    try
        ?assert_equal(TotalCount, ParsedIQ#result_iq.count),
        ok
    catch Class:Reason ->
        Stacktrace = erlang:get_stacktrace(),
        ct:pal("IQ: ~p~n", [IQ]),
        erlang:raise(Class, Reason, Stacktrace)
    end.

parse_messages(Messages) ->
    try [parse_forwarded_message(M) || M <- Messages]
    catch Class:Reason ->
        Stacktrace = erlang:get_stacktrace(),
        ct:pal("Messages: ~p~n", [Messages]),
        erlang:raise(Class, Reason, Stacktrace)
    end.

bootstrap_archive(Config) ->
    random:seed(os:timestamp()),
    Users = escalus_ct:get_config(escalus_users),
    AliceJID    = escalus_users:get_jid(Config, alice),
    BobJID      = escalus_users:get_jid(Config, bob),
    CarolJID    = escalus_users:get_jid(Config, carol),
    AliceName   = escalus_users:get_username(Config, alice),
    BobName     = escalus_users:get_username(Config, bob),
    CarolName   = escalus_users:get_username(Config, carol),
    AliceServer = escalus_users:get_server(Config, alice),
    BobServer   = escalus_users:get_server(Config, bob),
    CarolServer = escalus_users:get_server(Config, carol),
    ArcJID = {AliceJID, make_jid(AliceName, AliceServer, <<>>),
              rpc_apply(mod_mam, archive_id, [AliceServer, AliceName])},
    OtherUsers = [{BobJID, make_jid(BobName, BobServer, <<>>),
                   rpc_apply(mod_mam, archive_id, [BobServer, BobName])},
                  {CarolJID, make_jid(CarolName, CarolServer, <<>>),
                   rpc_apply(mod_mam, archive_id, [CarolServer, CarolName])}],
    Msgs = generate_msgs_for_days(ArcJID, OtherUsers, 16),
    put_msgs(Msgs),
    AllUsers = [{AliceServer, AliceName},
                {BobServer, BobName},
                {CarolServer, CarolName}],
    wait_for_msgs(Msgs, AllUsers),

    [{pre_generated_msgs, sort_msgs(Msgs)} | Config].

%% Wait for messages to be written
wait_for_msgs(Msgs, Users) ->
    UsersCnt = [{S, U, count_msgs(Msgs, S, U)} || {S, U} <- Users],
    [wait_for_archive_size_or_warning(S, U, 10, C) || {S, U, C} <- UsersCnt],
    ok.

count_msgs(Msgs, S, U) ->
    Bin = <<U/binary, "@", S/binary>>,
    length([1 ||
            {_,
             {FromBin, _FromJID, _FromArcID},
             {ToBin, _ToJID, _ToArcID}, _, _Packet} <- Msgs,
           FromBin =:= Bin orelse ToBin =:= Bin]).

sort_msgs(Msgs) ->
    SortFun = fun({{ID1, _}, _, _, _, _}, {{ID2, _}, _, _, _, _}) ->
        ID1 =< ID2
    end,
    lists:sort(SortFun, Msgs).

generate_msgs_for_days(OwnerJID, OtherUsers, Days) ->
    {TodayDate, _} = calendar:local_time(),
    Today = calendar:date_to_gregorian_days(TodayDate),
    StartDay = Today - Days,
    lists:flatten([generate_msgs_for_day(Day, OwnerJID, OtherUsers)
                   || Day <- lists:seq(StartDay, Today)]).

generate_msgs_for_day(Day, OwnerJID, OtherUsers) ->
    Date = calendar:gregorian_days_to_date(Day),

    [generate_msg_for_date_user(OwnerJID, RemoteJID, {Date, random_time()})
     || RemoteJID <- OtherUsers].

generate_msg_for_date_user(Owner, Remote, DateTime) ->
    generate_msg_for_date_user(Owner, Remote, DateTime, base16:encode(crypto:strong_rand_bytes(4))).

generate_msg_for_date_user(Owner, {RemoteBin, _, _} = Remote, DateTime, Content) ->
    MicrosecDateTime = datetime_to_microseconds(DateTime),
    NowMicro = rpc_apply(mod_mam_utils, now_to_microseconds, [rpc_apply(erlang, now, [])]),
    Microsec = min(NowMicro, MicrosecDateTime),
    MsgIdOwner = rpc_apply(mod_mam_utils, encode_compact_uuid, [Microsec, random:uniform(20)]),
    MsgIdRemote = rpc_apply(mod_mam_utils, encode_compact_uuid, [Microsec+1, random:uniform(20)]),
    Packet = escalus_stanza:chat_to(RemoteBin, Content),
    {{MsgIdOwner, MsgIdRemote}, Owner, Remote, Owner, Packet}.

random_time() ->
    MaxSecondsInDay = 86399,
    RandSeconds = random:uniform(MaxSecondsInDay),
    calendar:seconds_to_time(RandSeconds).

datetime_to_microseconds({{_, _, _}, {_, _, _}} = DateTime) ->
    S1 = calendar:datetime_to_gregorian_seconds(DateTime),
    S0 = calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}),
    Seconds = S1 - S0,
    Seconds * 1000000.


put_msgs(Msgs) ->
    [put_msg(Msg) || Msg <- Msgs].

put_msg({{MsgIdOwner, MsgIdRemote},
         {_FromBin, FromJID, FromArcID},
         {_ToBin, ToJID, ToArcID},
         {_, Source, _}, Packet}) ->
    Host = ct:get_config({hosts, mim, domain}),
    archive_message([Host, MsgIdOwner, FromArcID, FromJID, ToJID, Source, outgoing, Packet]),
    archive_message([Host, MsgIdRemote, ToArcID, ToJID, FromJID, Source, incoming, Packet]).

archive_message(Args) ->
    rpc_apply(mod_mam, archive_message, Args).



muc_bootstrap_archive(Config) ->
    Room = ?config(room, Config),

    A = room_address(Room, nick(alice)),
    B = room_address(Room, nick(bob)),
    R = room_address(Room),

    AliceName   = escalus_users:get_username(Config, alice),
    BobName     = escalus_users:get_username(Config, bob),
    AliceServer = escalus_users:get_server(Config, alice),
    BobServer   = escalus_users:get_server(Config, bob),

    Domain = muc_host(),
    Host = host(),
    RoomJid = make_jid(Room, Domain, <<>>),
    ArcJID = {R, RoomJid,
              rpc_apply(mod_mam_muc, archive_id, [Domain, Room])},
    Msgs = generate_msgs_for_days(ArcJID,
                                 [{B, make_jid(BobName, BobServer, <<"res1">>),
                                  rpc_apply(jid, replace_resource, [RoomJid, nick(bob)])},
                                  {A, make_jid(AliceName, AliceServer, <<"res1">>),
                                   rpc_apply(jid, replace_resource, [RoomJid, nick(alice)])}], 16),

    put_muc_msgs(Msgs),

    maybe_wait_for_archive(Config),
    ?assert_equal(length(Msgs),
                  wait_for_room_archive_size(Domain, Room, 10, length(Msgs))),

    [{pre_generated_muc_msgs, sort_msgs(Msgs)} | Config].

put_muc_msgs(Msgs) ->
    Host = host(),
    [archive_muc_msg(Host, Msg) || Msg <- Msgs].

archive_muc_msg(Host, {{MsgID, _},
                {_RoomBin, RoomJID, RoomArcID},
                {_FromBin, _FromJID, SrcJID}, _, Packet}) ->
    rpc_apply(mod_mam_muc, archive_message, [Host, MsgID, RoomArcID, RoomJID,
                                             SrcJID, SrcJID, incoming, Packet]).

%% @doc Get a binary jid of the user, that tagged with `UserName' in the config.
nick_to_jid(UserName, Config) when is_atom(UserName) ->
    UserSpec = escalus_users:get_userspec(Config, UserName),
    escalus_utils:jid_to_lower(escalus_users:get_jid(Config, UserSpec)).

make_jid(U, S, R) ->
    rpc_apply(jid, make, [U, S, R]).

-spec backend() -> odbc | riak | cassandra | false.
backend() ->
    Funs = [fun maybe_odbc/1, fun maybe_riak/1, fun maybe_cassandra/1],
    determine_backend(host(), Funs).

determine_backend(_, []) ->
    disabled;
determine_backend(Host, [F | Rest]) ->
    case F(Host) of
        false ->
            determine_backend(Host, Rest);
        Result ->
            Result
    end.

maybe_odbc(Host) ->
    case mongoose_helper:is_odbc_enabled(Host) of
        true ->
            odbc;
        _ ->
            false
    end.

maybe_riak(Host) ->
    case is_riak_enabled(Host) of
        true ->
            riak;
        _ ->
            false
    end.

maybe_cassandra(Host) ->
    case is_cassandra_enabled(Host) of
        true ->
            cassandra;
        _ ->
            false
    end.

is_mam_possible(Host) ->
    mongoose_helper:is_odbc_enabled(Host) orelse is_riak_enabled(Host) orelse
    is_cassandra_enabled(Host).

is_riak_enabled(_Host) ->
    case escalus_ejabberd:rpc(mongoose_riak, get_worker, []) of
        Pid when is_pid(Pid) ->
            true;
        _ ->
            false
    end.

is_cassandra_enabled(_) ->
    case escalus_ejabberd:rpc(mongoose_cassandra_sup, get_all_workers, []) of
        [_|_]=_Pools ->
            true;
        _ ->
            false
    end.

sql_transaction(Host, F) ->
    escalus_ejabberd:rpc(mongoose_rdbms, sql_transaction, [Host, F]).

login_send_presence(Config, User) ->
    Spec = escalus_users:get_userspec(Config, User),
    {ok, Client} = escalus_client:start(Config, Spec, <<"dummy">>),
    escalus:send(Client, escalus_stanza:presence(<<"available">>)),
    Client.

maybe_wait_for_archive(Config) ->
    case ?config(archive_wait, Config) of
        undefined ->
            ok;
        Value ->
            timer:sleep(Value)
    end.

%% Bob and Alice are friends.
%% Kate and Alice are not friends.
%%
%% Messages:
%% 1. Bob sends a message to Alice
%% 2. Alice sends a message to Bob
%% 3. Kate sends a message to Alice
%% 4. Alice sends a message to Kate
%%
%% Each tuple is
%% {{Default, Always, Never},
%%  [Message1Archived, Message2Archived, Message3Archied, Message4Archived]}
prefs_cases2() ->
    [
     {{roster, [], []},              [true, true, false, false]},
     {{roster, [bob], []},           [true, true, false, false]},
     {{roster, [kate], []},          [true, true, true, true]},
     {{roster, [kate, bob], []},     [true, true, true, true]},

     {{roster, [], [bob]},           [false, false, false, false]},
     {{roster, [], [kate]},          [true, true, false, false]},
     {{roster, [], [bob, kate]},     [false, false, false, false]},

     {{never, [], []},              [false, false, false, false]},
     {{never, [bob], []},           [true, true, false, false]},
     {{never, [kate], []},          [false, false, true, true]},
     {{never, [kate, bob], []},     [true, true, true, true]},

     {{never, [], [bob]},           [false, false, false, false]},
     {{never, [], [kate]},          [false, false, false, false]},
     {{never, [], [bob, kate]},     [false, false, false, false]},

     {{always, [], []},              [true, true, true, true]},
     {{always, [bob], []},           [true, true, true, true]},
     {{always, [kate], []},          [true, true, true, true]},
     {{always, [kate, bob], []},     [true, true, true, true]},

     {{always, [], [bob]},           [false, false, true, true]},
     {{always, [], [kate]},          [true, true, false, false]},
     {{always, [], [bob, kate]},     [false, false, false, false]}
    ].

default_policy({{Default, _, _}, _}) -> Default.

make_alice_and_bob_friends(Alice, Bob) ->
        escalus_client:send(Alice, escalus_stanza:presence_direct(escalus_client:short_jid(Bob),
                                                                  <<"subscribe">>)),
        escalus:wait_for_stanzas(Alice, 1, 5000), % iq set
        escalus:wait_for_stanzas(Bob, 1, 5000), % presence subscribe

        escalus_client:send(Bob, escalus_stanza:presence_direct(escalus_client:short_jid(Alice),
                                                                <<"subscribed">>)),
        escalus:wait_for_stanzas(Alice, 3, 5000), % iq set, presence subscribed, presence
        escalus:wait_for_stanzas(Bob, 1, 5000), % iq set subscription=from

        escalus_client:send(Bob, escalus_stanza:presence_direct(escalus_client:short_jid(Alice),
                                                                <<"subscribe">>)),
        escalus:wait_for_stanzas(Alice, 2, 5000), % iq set subscription=to, presence subscribe
        escalus:wait_for_stanzas(Bob, 1, 5000), % iq set subscription=from

        escalus_client:send(Alice, escalus_stanza:presence_direct(escalus_client:short_jid(Bob),
                                                                  <<"subscribed">>)),
        escalus:wait_for_stanzas(Alice, 1, 5000), % iq set subscription=both
        escalus:wait_for_stanzas(Bob, 3, 5000), % iq set subscription=both, presence subscribed,
                                                % presence
        ok.

run_prefs_case({PrefsState, ExpectedMessageStates}, Namespace, Alice, Bob, Kate, Config) ->
    {DefaultMode, AlwaysUsers, NeverUsers} = PrefsState,
    IqSet = stanza_prefs_set_request({DefaultMode, AlwaysUsers, NeverUsers, Namespace}, Config),
    escalus:send(Alice, IqSet),
    _ReplySet = escalus:wait_for_stanza(Alice),
    Messages = [iolist_to_binary(io_lib:format("n=~p, prefs=~p, now=~p",
                                               [N, PrefsState, os:timestamp()]))
                || N <- [1, 2, 3, 4]],
    %% Messages:
    %% 1. Bob sends a message to Alice
    %% 2. Alice sends a message to Bob
    %% 3. Kate sends a message to Alice
    %% 4. Alice sends a message to Kate
    escalus:send(Bob, escalus_stanza:chat_to(Alice, lists:nth(1, Messages))),
    escalus:send(Alice, escalus_stanza:chat_to(Bob, lists:nth(2, Messages))),
    escalus:send(Kate, escalus_stanza:chat_to(Alice, lists:nth(3, Messages))),
    escalus:send(Alice, escalus_stanza:chat_to(Kate, lists:nth(4, Messages))),
    escalus:wait_for_stanzas(Bob, 1, 5000),
    escalus:wait_for_stanzas(Kate, 1, 5000),
    escalus:wait_for_stanzas(Alice, 2, 5000),
    %% Delay check
    fun(Bodies) ->
        ActualMessageStates = [lists:member(M, Bodies) || M <- Messages],
        ?_assert_equal_extra(ExpectedMessageStates, ActualMessageStates,
                             [{prefs_state, PrefsState}])
    end.

get_last_four_messages(P, Alice) ->
    RSM = #rsm_in{max=4, direction='before'},
    escalus:send(Alice, stanza_page_archive_request(P, <<"last4_rsm">>, RSM)),
    respond_messages(wait_archive_respond(P, Alice)).

get_all_messages(P, Alice) ->
    get_all_messages(P, Alice, undefined).

get_all_messages(P, Alice, Id) ->
    RSM = #rsm_in{max=50, direction='after', id=Id},
    escalus:send(Alice, stanza_page_archive_request(P, <<"page_rsm">>, RSM)),
    Result = wait_archive_respond(P, Alice),
    PageMessages = respond_messages(Result),
    ParsedIQ = parse_result_iq(P, Result),
    #result_iq{last=LastId} = ParsedIQ,
    case PageMessages of
        [] ->
            [];
        [_|_] ->
            PageMessages ++ get_all_messages(P, Alice, LastId)
    end.

stanza_prefs_set_request({DefaultMode, AlwaysUsers, NeverUsers, Namespace}, Config) ->
    DefaultModeBin = atom_to_binary(DefaultMode, utf8),
    AlwaysJIDs = users_to_jids(AlwaysUsers, Config),
    NeverJIDs  = users_to_jids(NeverUsers, Config),
    stanza_prefs_set_request(DefaultModeBin, AlwaysJIDs, NeverJIDs, Namespace).

users_to_jids(Users, Config) ->
    [escalus_users:get_jid(Config, User) || User <- Users].

print_configuration_not_supported(C, B) ->
    I = io_lib:format("issue=configuration_not_supported, "
                       "configuration=~p, basic_group=~p", [C, B]),
     binary_to_list(iolist_to_binary(I)).

%% Alice sets and gets her preferences
run_set_and_get_prefs_case({PrefsState, _ExpectedMessageStates}, Namespace, Alice, Config) ->
    {DefaultMode, AlwaysUsers, NeverUsers} = PrefsState,
    IqSet = stanza_prefs_set_request({DefaultMode, AlwaysUsers, NeverUsers, Namespace}, Config),
    escalus:send(Alice, IqSet),
    ReplySet = escalus:wait_for_stanza(Alice, 5000),
    ReplySetNS = exml_query:path(ReplySet, [{element, <<"prefs">>}, {attr, <<"xmlns">>}]),
    ?assert_equal(ReplySetNS, Namespace),
    escalus:send(Alice, stanza_prefs_get_request(Namespace)),
    ReplyGet = escalus:wait_for_stanza(Alice),
    ReplyGetNS = exml_query:path(ReplyGet, [{element, <<"prefs">>}, {attr, <<"xmlns">>}]),
    ?assert_equal(ReplyGetNS, Namespace),
    ResultIQ1 = parse_prefs_result_iq(ReplySet),
    ResultIQ2 = parse_prefs_result_iq(ReplyGet),
    ?assert_equal(ResultIQ1, ResultIQ2),
    ok.

maybe_binary_to_integer(B) when is_binary(B) ->
    list_to_integer(binary_to_list(B));
maybe_binary_to_integer(undefined) ->
    undefined.

add_nostore_hint(#xmlel{children=Children}=Elem) ->
    Elem#xmlel{children=Children ++ [nostore_hint_elem()]}.

nostore_hint_elem() ->
    #xmlel{name = <<"no-store">>, attrs = [{<<"xmlns">>, <<"urn:xmpp:hints">>}]}.

has_x_user_element(ArcMsg) ->
    ParsedMess = parse_forwarded_message(ArcMsg),
    ParsedMess#forwarded_message.has_x_user_element.

muc_light_host() ->
    <<"muclight.localhost">>.

host() ->
    ct:get_config({hosts, mim, domain}).

room_name(Config) ->
    AliceName   = escalus_users:get_username(Config, alice),
    StoryPidBin = to_nodename(list_to_binary(pid_to_list(self()))),
    <<AliceName/binary, "room", StoryPidBin/binary>>.

%% Strip dissallowed characters
to_nodename(Binary) ->
    << << (rewrite_nodename(X))/binary >> || <<X>> <= Binary >>.

%% This function is only for pid characters
rewrite_nodename($<) -> <<>>;
rewrite_nodename($>) -> <<>>;
rewrite_nodename($.) -> <<"-">>;
rewrite_nodename(X)  -> <<X>>.
