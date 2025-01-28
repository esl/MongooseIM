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

-include("mam_helper.hrl").
-include_lib("escalus/include/escalus_xmlns.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("exml/include/exml_stream.hrl").

-compile([export_all, nowarn_export_all]).

-import(distributed_helper, [mim/0,
                             rpc/4]).

-import(muc_helper,
        [muc_host/0,
         room_address/1, room_address/2,
         stanza_muc_enter_room/2,
         stanza_to_room/2,
         start_room/5]).

-import(config_parser_helper, [config/2, mod_config/2]).

config_opts(ExtraOpts) ->
    lists:foldl(fun set_opts/2, ExtraOpts, [defaults, backend, pm, muc, async_writer]).

set_opts(defaults, Opts) ->
    mod_config(mod_mam, Opts);
set_opts(pm, #{pm := PMExtra} = Opts) ->
    Opts#{pm := config([modules, mod_mam, pm], PMExtra)};
set_opts(muc, #{muc := MUCExtra} = Opts) ->
    Opts#{muc := config([modules, mod_mam, muc], MUCExtra)};
set_opts(async_writer, #{async_writer := AsyncExtra} = Opts) ->
    Opts#{async_writer := config([modules, mod_mam, async_writer], AsyncExtra)};
set_opts(_, Opts) ->
    Opts.

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
    escalus_rpc:call(Node, M, F, A, 10000, Cookie).

mam04_props() ->
    [{mam_ns, mam_ns_binary_v04()}].

mam06_props() ->
    [{mam_ns, mam_ns_binary_v06()}].

respond_messages(#mam_archive_respond{respond_messages=Messages}) ->
    Messages.

respond_iq(#mam_archive_respond{respond_iq=IQ}) ->
    IQ.

respond_fin(#mam_archive_respond{respond_fin=Fin}) ->
    Fin.

get_prop(Key, undefined) ->
    get_prop(Key, []);
get_prop(mam_ns, P) ->
    proplists:get_value(mam_ns, P, mam_ns_binary_v04()).

wait_archive_respond(User) ->
    [IQ|Messages] = lists:reverse(wait_archive_respond_v04plus(User)),
    #mam_archive_respond{
       respond_iq=IQ,
       respond_messages=lists:reverse(Messages)}.

%% MAM v0.4+ respond
wait_archive_respond_v04plus(User) ->
    S = escalus:wait_for_stanza(User, 5000),
    case escalus_pred:is_iq_error(S) of
        true ->
            ct:pal("Stanza ~p", [S]),
            ct:fail("Unexpected error IQ.", []);
        false -> ok
    end,
    case escalus_pred:is_iq_result(S) of
        true  -> [S];
        false -> [S|wait_archive_respond_v04plus(User)]
    end.

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

%% @doc Calls wait_archive_respond and checks "complete" field value
%%
%% Common ExpectedCompleteValue values are <<"true">> or <<"false">>.
%% Be aware, that if the attribute is missing in respond XML,
%% than ExpectedCompleteValue should be <<"false">>.
wait_for_complete_archive_response(P, Alice, ExpectedCompleteValue)
      when is_binary(ExpectedCompleteValue) ->
    Result = wait_archive_respond(Alice),
    ParsedIQ = parse_result_iq(Result),
    ?assert_equal_extra(ExpectedCompleteValue,
                        ParsedIQ#result_iq.complete,
                        #{mam_props => P, parsed_iq => ParsedIQ}).

make_iso_time(Micro) ->
    calendar:system_time_to_rfc3339(Micro, [{offset, "Z"}, {unit, microsecond}]).

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

nick(User) ->
    Name = escalus_utils:get_username(User),
    <<"unique_", Name/binary, "_nickname">>.

namespaces() ->
    [mam_ns_binary_v04(),
     mam_ns_binary_v06(),
     mam_ns_binary_extended(),
     retract_ns(),
     retract_esl_ns(),
     retract_tombstone_ns()].

mam_ns_binary() -> mam_ns_binary_v04().
mam_ns_binary_v04() -> <<"urn:xmpp:mam:1">>.
mam_ns_binary_v06() -> <<"urn:xmpp:mam:2">>.
mam_ns_binary_extended() -> <<"urn:xmpp:mam:2#extended">>.
retract_ns() -> <<"urn:xmpp:message-retract:0">>.
retract_esl_ns() -> <<"urn:esl:message-retract-by-stanza-id:0">>.
retract_tombstone_ns() -> <<"urn:xmpp:message-retract:0#tombstone">>.
groupchat_field_ns() -> <<"urn:xmpp:mam:2#groupchat-field">>.
groupchat_available_ns() -> <<"urn:xmpp:mam:2#groupchat-available">>.
data_validate_ns() -> <<"http://jabber.org/protocol/xdata-validate">>.
stanzas_ns() -> <<"urn:ietf:params:xml:ns:xmpp-stanzas">>.

skip_undefined(Xs) ->
    [X || X <- Xs, X =/= undefined].

maybe_attr(K, V) ->
    maybe_attr(K, V, #{}).

maybe_attr(_, undefined, Attrs) ->
    Attrs;
maybe_attr(K, V, Attrs) ->
    Attrs#{K => V}.

mam_ns_attr(P) ->
    #{<<"xmlns">> => get_prop(mam_ns, P)}.

maybe_with_elem(undefined) ->
    undefined;
maybe_with_elem(BWithJID) ->
    #xmlel{
        name = <<"with">>,
        children = [#xmlcdata{content = BWithJID}]}.

stanza_metadata_request() ->
    escalus_stanza:iq(<<"get">>,
                      [#xmlel{name = <<"metadata">>,
                              attrs = #{<<"xmlns">> => mam_ns_binary_v06()}}]).

%% An optional 'queryid' attribute allows the client to match results to
%% a certain query.
stanza_archive_request(P, QueryId) ->
    Params = #{query_id => QueryId},
    stanza_lookup_messages_iq(P, Params).

stanza_date_range_archive_request(P) ->
    Params = #{
        start => <<"2010-06-07T00:00:00Z">>,
        stop => <<"2010-07-07T13:23:54Z">>
    },
    stanza_lookup_messages_iq(P, Params).

stanza_date_range_archive_request_not_empty(P, Start, Stop) ->
    Params = #{
        start => list_to_binary(Start),
        stop => list_to_binary(Stop)
    },
    stanza_lookup_messages_iq(P, Params).

stanza_limit_archive_request(P) ->
    Params = #{
        start => <<"2010-08-07T00:00:00Z">>,
        rsm => #rsm_in{max=10}
    },
    stanza_lookup_messages_iq(P, Params).

stanza_page_archive_request(P, QueryId, RSM) ->
    Params = #{
        query_id => QueryId,
        rsm => RSM
    },
    stanza_lookup_messages_iq(P, Params).

stanza_flip_page_archive_request(P, QueryId, RSM) ->
    Params = #{
        query_id => QueryId,
        rsm => RSM,
        flip_page => true
    },
    stanza_lookup_messages_iq(P, Params).

stanza_filtered_by_jid_request(P, BWithJID) ->
    Params = #{with_jid => BWithJID},
    stanza_lookup_messages_iq(P, Params).

stanza_text_search_archive_request(P, QueryId, TextSearch) ->
    Params = #{
        query_id => QueryId,
        text_search => TextSearch
    },
    stanza_lookup_messages_iq(P, Params).

stanza_include_groupchat_request(P, QueryId, IncludeGroupChat) ->
    Params = #{
        query_id => QueryId,
        include_group_chat => IncludeGroupChat
    },
    stanza_lookup_messages_iq(P, Params).

stanza_fetch_by_id_request(P, QueryId, IDs) ->
    stanza_fetch_by_id_request(P, QueryId, IDs, undefined).

stanza_fetch_by_id_request(P, QueryId, IDs, RSM) ->
    Params = #{
        query_id => QueryId,
        messages_ids => IDs,
        rsm => RSM
    },
    stanza_lookup_messages_iq(P, Params).

stanza_lookup_messages_iq(P, Params) ->
    QueryId = maps:get(query_id, Params, undefined),
    BStart = maps:get(start, Params, undefined),
    BEnd = maps:get(stop, Params, undefined),
    BWithJID = maps:get(with_jid, Params, undefined),
    RSM = maps:get(rsm, Params, undefined),
    TextSearch = maps:get(text_search, Params, undefined),
    FlipPage = maps:get(flip_page, Params, undefined),
    IncludeGroupChat = maps:get(include_group_chat, Params, undefined),
    MessagesIDs = maps:get(messages_ids, Params, undefined),

    Attrs = mam_ns_attr(P),
    escalus_stanza:iq(<<"set">>, [#xmlel{
       name = <<"query">>,
       attrs = maybe_attr(<<"queryid">>, QueryId, Attrs),
       children = skip_undefined([
           form_x(BStart, BEnd, BWithJID, RSM, TextSearch, IncludeGroupChat, MessagesIDs),
           maybe_rsm_elem(RSM),
           maybe_flip_page(FlipPage)])
    }]).

form_x(undefined, undefined, undefined, undefined, undefined, undefined, undefined) ->
    undefined;
form_x(BStart, BEnd, BWithJID, RSM, TextSearch, IncludeGroupChat, MessagesIDs) ->
    Fields = skip_undefined([form_field(<<"start">>, BStart),
                             form_field(<<"end">>, BEnd),
                             form_field(<<"with">>, BWithJID),
                             form_field(<<"full-text-search">>, TextSearch),
                             form_field(<<"include-groupchat">>, IncludeGroupChat),
                             form_field(<<"ids">>, MessagesIDs)]
                            ++ form_extra_fields(RSM)
                            ++ form_border_fields(RSM)),
    form_helper:form(#{fields => Fields}).

form_extra_fields(undefined) ->
    [];
form_extra_fields(#rsm_in{simple = Simple}) ->
    [form_bool_field(<<"simple">>, Simple)].

form_border_fields(undefined) ->
    [];
form_border_fields(#rsm_in{
        before_id=BeforeId, after_id=AfterId, from_id=FromId, to_id=ToId}) ->
    [form_field(<<"before-id">>, BeforeId),
     form_field(<<"after-id">>, AfterId),
     form_field(<<"from-id">>, FromId),
     form_field(<<"to-id">>, ToId)].

form_field(_VarName, undefined) ->
    undefined;
form_field(VarName, VarValues) when is_list(VarValues) ->
    #{var => VarName, values => VarValues};
form_field(VarName, VarValue) ->
    #{var => VarName, values => [VarValue]}.

form_bool_field(Name, true) ->
    form_field(Name, <<"true">>);
form_bool_field(_Name, _) ->
    undefined.

stanza_retrieve_form_fields(QueryId, NS) ->
    Attrs = #{<<"xmlns">> => NS},
    escalus_stanza:iq(<<"get">>, [#xmlel{
        name = <<"query">>,
        attrs = maybe_attr(<<"queryid">>, QueryId, Attrs),
        children = []
    }]).

maybe_rsm_elem(undefined) ->
    undefined;
maybe_rsm_elem(#rsm_in{max=Max, direction=Direction, id=Id, index=Index}) ->
    #xmlel{name = <<"set">>,
           children = skip_undefined([
                maybe_rsm_max(Max),
                maybe_rsm_index(Index),
                maybe_rsm_direction(Direction, Id)])}.

maybe_flip_page(undefined) ->
    undefined;
maybe_flip_page(_) ->
    #xmlel{name = <<"flip-page">>}.

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

verify_id_error_text_msg(Condition, Stanza) ->
    Text = exml_query:path(Stanza, [{element, <<"error">>},
                                    {element_with_ns, <<"text">>, stanzas_ns()}, cdata]),
    case Condition of
        [<<"modify">>, <<"not-acceptable">>] ->
            ?assert_equal(<<"Invalid stanza ID provided">>, Text);
        [<<"cancel">>, <<"item-not-found">>] ->
            ?assert_equal(<<"Message with specified ID is not found">>, Text)
    end.

%% ----------------------------------------------------------------------
%% PREFERENCE QUERIES

stanza_prefs_set_request(DefaultMode, AlwaysJIDs, NeverJIDs, Namespace) ->
    AlwaysEl = #xmlel{name = <<"always">>,
                      children = encode_jids(AlwaysJIDs)},
    NeverEl  = #xmlel{name = <<"never">>,
                      children = encode_jids(NeverJIDs)},
    Attrs = #{<<"xmlns">> => Namespace},
    escalus_stanza:iq(<<"set">>, [#xmlel{
       name = <<"prefs">>,
       attrs = maybe_attr(<<"default">>, DefaultMode, Attrs),
       children = [AlwaysEl, NeverEl]
    }]).

stanza_prefs_get_request(Namespace) ->
    escalus_stanza:iq(<<"get">>, [#xmlel{
       name = <<"prefs">>,
       attrs = #{<<"xmlns">> => Namespace}
    }]).

stanza_query_get_request(Namespace) ->
    escalus_stanza:iq(<<"get">>, [#xmlel{
        name = <<"query">>,
        attrs = #{<<"xmlns">> => Namespace}
    }]).

%% Allows to cdata to be put as it is
encode_jids(JIDs) ->
    [encode_jid_or_cdata(JID) || JID <- JIDs].

encode_jid_or_cdata(#xmlcdata{content = Text}) ->
    #xmlcdata{content = Text};
encode_jid_or_cdata(JID) ->
    #xmlel{name = <<"jid">>,
           children = [#xmlcdata{content = JID}]}.

%% ----------------------------------------------------------------------
%% PARSING RESPONDS

parse_forwarded_message(#xmlel{name = <<"message">>,
                               attrs = Attrs, children = Children}) ->
    M = #forwarded_message{
        from = get_attr(<<"from">>, Attrs),
        to   = get_attr(<<"to">>, Attrs),
        has_x_user_element = false},
    lists:foldl(fun parse_children_message/2, M, Children).

parse_children_message(#xmlel{name = <<"result">>,
                              attrs = Attrs,
                              children = Children}, M) ->
    M1 = M#forwarded_message{
        result_queryid = get_attr(<<"queryid">>, Attrs),
        result_id      = get_attr(<<"id">>, Attrs)},
    lists:foldl(fun parse_children_message_result/2, M1, Children).

parse_children_message_result(#xmlel{name = <<"forwarded">>,
                                     children = Children}, M) ->
    lists:foldl(fun parse_children_message_result_forwarded/2, M, Children).


parse_children_message_result_forwarded(#xmlel{name = <<"delay">>,
                                               attrs = Attrs}, M) ->
    M#forwarded_message{
        delay_from  = get_attr(<<"from">>, Attrs),
        delay_stamp = get_attr(<<"stamp">>, Attrs)};
parse_children_message_result_forwarded(#xmlel{name = <<"message">>,
                                               attrs = Attrs,
                                               children = Children}, M) ->
    M1 = M#forwarded_message{
        message_to   = get_attr(<<"to">>, Attrs),
        message_from = get_attr(<<"from">>, Attrs),
        message_type = get_attr(<<"type">>, Attrs)},
    lists:foldl(fun parse_children_message_result_forwarded_message/2,
                M1, Children).

parse_children_message_result_forwarded_message(#xmlel{name = <<"body">>,
                                                       children = [#xmlcdata{content =  Body}]}, M) ->
    M#forwarded_message{message_body = Body};
parse_children_message_result_forwarded_message(#xmlel{name = <<"x">>,
                                                       attrs = Attrs} = XEl, M) ->
    IsUser = get_attr(<<"xmlns">>, Attrs) =:= <<"http://jabber.org/protocol/muc#user">>,
    M#forwarded_message{has_x_user_element = IsUser,
                        message_xs = [XEl | M#forwarded_message.message_xs]};
%% Parse `<archived />' or chat markers.
parse_children_message_result_forwarded_message(Elem, M) ->
    case exml_query:attr(Elem, <<"xmlns">>) of
        ?NS_CHAT_MARKERS ->
            M#forwarded_message{ chat_marker = Elem#xmlel.name };
        _ ->
            % Not relevant
            M#forwarded_message{ message_children = [Elem | M#forwarded_message.message_children] }
    end.

%% Num is 1-based.
message_id(Num, Config) ->
    AllMessages = proplists:get_value(all_messages, Config),
    #forwarded_message{result_id=Id} = lists:nth(Num, AllMessages),
    Id.

get_pre_generated_msgs_ids(Msgs, Nums) ->
    lists:map(fun(N) ->
                 Msg = lists:nth(N, Msgs),
                 {{MsgID, _}, _, _, _, _} = Msg,
                 rpc_apply(mod_mam_utils, mess_id_to_external_binary, [MsgID])
              end, Nums).

get_received_msgs_ids(Response) ->
    Msgs = respond_messages(Response),
    lists:map(fun(M) ->
                 Parsed = parse_forwarded_message(M),
                 Parsed#forwarded_message.result_id
              end, Msgs).

parse_result_iq(#mam_archive_respond{respond_iq = IQ, respond_fin = undefined}) ->
    Fin = exml_query:subelement(IQ, <<"fin">>),
    Set = exml_query:subelement(Fin, <<"set">>),
    %% MongooseIM does not add complete attribute, if complete is false
    Complete = exml_query:attr(Fin, <<"complete">>, <<"false">>),
    parse_set_and_iq(IQ, Set, not_supported, Complete).

parse_set_and_iq(IQ, Set, QueryId, Complete) ->
    #result_iq{
        query_id    = QueryId,
        complete    = Complete,
        from        = exml_query:attr(IQ, <<"from">>),
        to          = exml_query:attr(IQ, <<"to">>),
        id          = exml_query:attr(IQ, <<"id">>),
        first       = exml_query:path(Set, [{element, <<"first">>}, cdata]),
        first_index = maybe_binary_to_integer(exml_query:path(Set, [{element, <<"first">>},
                                                                    {attr, <<"index">>}])),
        last        = exml_query:path(Set, [{element, <<"last">>}, cdata]),
        count       = maybe_binary_to_integer(exml_query:path(Set, [{element, <<"count">>},
                                                                    cdata]))}.

get_attr(Key, Attrs) ->
    maps:get(Key, Attrs, undefined).

parse_prefs_result_iq(#xmlel{name = <<"iq">>, children = Children}) ->
    IQ = #prefs_result_iq{},
    lists:foldl(fun parse_children_prefs_iq/2, IQ, Children).

parse_children_prefs_iq(#xmlel{name = <<"prefs">>,
                               attrs = Attrs, children = Children},
                        IQ) ->
    DefaultMode = get_attr(<<"default">>, Attrs),
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
     || #xmlel{name = <<"jid">>, children = [#xmlcdata{content = JID}]} <- Els].

%%--------------------------------------------------------------------
%% Helpers (muc)
%%--------------------------------------------------------------------

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
    N = 15,
    F = fun(Alice, Bob) ->
        escalus:send(Alice, stanza_muc_enter_room(Room, nick(Alice))),
        escalus:send(Bob, stanza_muc_enter_room(Room, nick(Bob))),

        escalus:wait_for_stanzas(Bob, 3),
        escalus:wait_for_stanzas(Alice, 3),

        %% Alice sends messages to Bob.
        lists:foreach(fun(NN) ->
                              escalus:send(Alice, escalus_stanza:groupchat_to(
                                                    RoomAddr, generate_message_text(NN)))
                      end, lists:seq(1, N)),
        assert_list_size(N, escalus:wait_for_stanzas(Bob, N)),
        assert_list_size(N, escalus:wait_for_stanzas(Alice, N)),
        wait_for_room_archive_size(muc_host(), Room, N),

        %% Get the whole history.
        escalus:send(Alice,
            stanza_to_room(stanza_archive_request(P, <<"all_room_messages">>), Room)),
        AllMessages =
            respond_messages(assert_respond_size(N, wait_archive_respond(Alice))),
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
    N = 15,
    Pid = self(),
    P = ?config(props, Config),
    F = fun(Alice, Bob) ->
                %% Alice sends messages to Bob.
                [escalus:send(Alice,
                              escalus_stanza:chat_to(Bob, generate_message_text(K)))
                 || K <- lists:seq(1, N)],
                assert_list_size(N, escalus:wait_for_stanzas(Bob, N)),
                wait_for_archive_size(Alice, N),

                %% Get the whole history.
                rsm_send(Config, Alice, stanza_archive_request(P, <<"all_messages">>)),
                AllMessages = respond_messages(
                                assert_respond_size(N, wait_archive_respond(Alice))),
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

prepare_for_suite(Config) ->
    Loaded = is_module_loaded(mod_offline),
    [{mod_offline_loaded, Loaded}|Config].

is_module_loaded(Mod) ->
    rpc(mim(), gen_mod, is_loaded, [host_type(), Mod]).

clean_archives(Config) ->
    SUs = serv_users(Config),
    %% It is not the best place to delete these messages.
    case ?config(mod_offline_loaded, Config) of
        true ->
            [ok = delete_offline_messages(S, U) || {S, U} <- SUs];
        false ->
            ok
    end,
    %% Wait until messages are flushed before removing them
    wait_for_parallel_writer(Config),
    [ok = delete_archive(S, U) || {S, U} <- SUs],
    %% Wait for archive to be empty
    [wait_for_archive_size(S, U, 0) || {S, U} <- SUs],
    Config.

destroy_room(Config) ->
    %% Wait until messages are flushed before removing them
    wait_for_parallel_writer(Config),
    clean_room_archive(Config),
    muc_helper:destroy_room(Config).

clean_room_archive(Config) ->
    Room = ?config(room, Config),
    delete_room_archive(muc_host(), Room),
    %% Retry 10 times if not empty
    wait_for_room_archive_size(muc_host(), Room, 0),
    Config.

serv_users(Config) ->
    Users = ?config(escalus_users, Config),
    [serv_user(Config, UserSpec) || {_Tag, UserSpec} <- Users].

serv_user(Config, UserSpec) ->
    [Username, Server, _Pass] = escalus_users:get_usp(Config, UserSpec),
    {Server, Username}.

wait_for_archive_size(User, ExpectedSize) ->
    wait_for_archive_size(
      escalus_utils:get_server(User),
      escalus_utils:jid_to_lower(escalus_utils:get_username(User)),
      ExpectedSize).

wait_for_archive_size(Server, Username, ExpectedSize) ->
    wait_helper:wait_until(fun() -> archive_size(Server, Username) end,
                           ExpectedSize,
                           #{
                             time_left => timer:seconds(20),
                             name => archive_size
                            }).

wait_for_archive_size_with_host_type(HostType, User, ExpectedSize) ->
    wait_for_archive_size_with_host_type(
      HostType,
      escalus_utils:get_server(User),
      escalus_utils:jid_to_lower(escalus_utils:get_username(User)),
      ExpectedSize).

wait_for_archive_size_with_host_type(HostType, Server, Username, ExpectedSize) ->
    F = fun() -> archive_size_with_host_type(HostType, Server, Username) end,
    wait_helper:wait_until(F, ExpectedSize,
                           #{
                             time_left => timer:seconds(20),
                             name => archive_size_with_host_type
                            }).

wait_for_archive_size_or_warning(Server, Username, ExpectedSize) ->
    try wait_helper:wait_until(fun() -> archive_size(Server, Username) end,
                               ExpectedSize,
                               #{
                                 time_left => timer:seconds(20),
                                 name => archive_size
                                }) of
        {ok, ExpectedSize} ->
            ok
    catch
        _Error:Reason ->
            ct:pal("issue=wait_for_archive_size_or_warning, expected_size=~p, log=~p",
                   [ExpectedSize, Reason])
    end.

wait_for_room_archive_size(Server, Username, ExpectedSize) ->
    {ok, ExpectedSize} = wait_helper:wait_until(fun() -> room_archive_size(Server, Username) end,
                                                ExpectedSize,
                                                #{
                                                  time_left => timer:seconds(20),
                                                  name => room_archive_size
                                                 }).


archive_size(Server, Username) ->
    rpc_apply(mod_mam_pm, archive_size, [Server, Username]).

archive_size_with_host_type(HostType, Server, Username) ->
    rpc_apply(mod_mam_pm, archive_size_with_host_type, [HostType, Server, Username]).

room_archive_size(Server, Username) ->
    rpc_apply(mod_mam_muc, archive_size, [Server, Username]).

delete_archive(Server, Username) ->
    ArcJID = jid:make_bare(Username, Server),
    rpc_apply(mod_mam_pm, delete_archive, [ArcJID]).

delete_room_archive(Server, Username) ->
    rpc_apply(mod_mam_muc, delete_archive, [Server, Username]).

delete_offline_messages(Server, Username) ->
    HostType = domain_helper:host_type(),
    rpc_apply(mod_offline_backend, remove_user, [HostType, Username, Server]).

wait_message_range(Client, FromN, ToN) when FromN =< ToN ->
    wait_message_range(Client, 15, FromN-1, FromN, ToN, 1);
wait_message_range(Client, FromN, ToN) when FromN > ToN ->
    wait_message_range(Client, 15, FromN-1, FromN, ToN, -1).

wait_message_range(Client, TotalCount, Offset, FromN, ToN) when FromN =< ToN ->
    wait_message_range(Client, TotalCount, Offset, FromN, ToN, 1);
wait_message_range(Client, TotalCount, Offset, FromN, ToN) when FromN > ToN ->
    wait_message_range(Client, TotalCount, Offset, FromN, ToN, -1).

wait_message_range(Client, TotalCount, Offset, FromN, ToN, Step) ->
    wait_message_range(Client, #{total_count => TotalCount, offset => Offset,
                                 from => FromN, to => ToN, step => Step}).

wait_message_range(Client, Params = #{total_count := TotalCount, offset := Offset,
                                      from := FromN, to := ToN, step := Step}) ->
    IsComplete = maps:get(is_complete, Params, undefined),
    Result = wait_archive_respond(Client),
    Messages = respond_messages(Result),
    IQ = respond_iq(Result),
    Fin = respond_fin(Result),
    ParsedMessages = parse_messages(Messages),
    ParsedIQ = parse_result_iq(Result),
    try
        ?assert_equal(TotalCount, ParsedIQ#result_iq.count),
        case Step of
            -1 -> ok;
            _ -> ?assert_equal(Offset, ParsedIQ#result_iq.first_index)
        end,
        %% Compare body of the messages.
        ?assert_equal([generate_message_text(N) || N <- maybe_seq(FromN, ToN, Step)],
                      [B || #forwarded_message{message_body=B} <- ParsedMessages]),
        case IsComplete of
            true      -> ?assert_equal(<<"true">>, ParsedIQ#result_iq.complete);
            false     -> ?assert_equal(<<"false">>, ParsedIQ#result_iq.complete);
            undefined -> ok
        end,
        ok
    catch Class:Reason:StackTrace ->
        ct:pal("IQ: ~p~n"
               "Fin: ~p~n"
               "Messages: ~p~n"
               "Parsed messages: ~p~n",
               [IQ, Fin, Messages, ParsedMessages]),
        erlang:raise(Class, Reason, StackTrace)
    end.

maybe_seq(undefined, undefined, _) -> [];
maybe_seq(A, B, Step) -> lists:seq(A, B, Step).

wait_empty_rset(Alice, TotalCount) ->
    Result = wait_archive_respond(Alice),
    IQ = respond_iq(Result),
    ?assert_equal([], respond_messages(Result)),
    ParsedIQ = parse_result_iq(Result),
    try
        ?assert_equal(TotalCount, ParsedIQ#result_iq.count),
        ok
    catch Class:Reason:StackTrace ->
        ct:pal("IQ: ~p~n", [IQ]),
        erlang:raise(Class, Reason, StackTrace)
    end.

parse_messages(Messages) ->
    try [parse_forwarded_message(M) || M <- Messages]
    catch Class:Reason:StackTrace ->
        ct:pal("Messages: ~p~n", [Messages]),
        erlang:raise(Class, Reason, StackTrace)
    end.

bootstrap_archive(Config) ->
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
              get_archive_id(AliceServer, AliceName)},
    OtherUsers = [{BobJID, make_jid(BobName, BobServer, <<>>),
                   get_archive_id(BobServer, BobName)},
                  {CarolJID, make_jid(CarolName, CarolServer, <<>>),
                   get_archive_id(CarolServer, CarolName)}],
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
    [wait_for_archive_size_or_warning(S, U, C) || {S, U, C} <- UsersCnt],
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
    generate_msg_for_date_user(Owner, Remote, DateTime, random_text()).

random_text() ->
    binary:encode_hex(crypto:strong_rand_bytes(4)).

generate_msg_for_date_user(Owner, {RemoteBin, _, _} = Remote, DateTime, Content) ->
    Microsec = datetime_to_microseconds(DateTime),
    MsgIdOwner = rpc_apply(mod_mam_utils, encode_compact_uuid, [Microsec, rand:uniform(20)]),
    MsgIdRemote = rpc_apply(mod_mam_utils, encode_compact_uuid, [Microsec+1, rand:uniform(20)]),
    Packet = escalus_stanza:chat_to(RemoteBin, Content),
    {{MsgIdOwner, MsgIdRemote}, Owner, Remote, Owner, Packet}.

random_time() ->
    MaxSecondsInDay = 86399,
    RandSeconds = rand:uniform(MaxSecondsInDay),
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
    Map1 = #{message_id => MsgIdOwner, archive_id => FromArcID,
             local_jid => FromJID, remote_jid => ToJID, source_jid => Source,
             origin_id => none, direction => outgoing, packet => Packet, is_groupchat => false},
    Map2 = #{message_id => MsgIdRemote, archive_id => ToArcID,
             local_jid => ToJID, remote_jid => FromJID, source_jid => Source,
             origin_id => none, direction => incoming, packet => Packet, is_groupchat => false},
    archive_message(Map1),
    archive_message(Map2).

archive_message(#{} = Map) ->
    ok = rpc_apply(mod_mam_pm, archive_message_from_ct, [Map]).

muc_bootstrap_archive(Config) ->
    Room = ?config(room, Config),

    AliceNick = nick(alice),
    BobNick = nick(bob),
    A = room_address(Room, AliceNick),
    B = room_address(Room, BobNick),
    R = room_address(Room),

    AliceName   = escalus_users:get_username(Config, alice),
    BobName     = escalus_users:get_username(Config, bob),
    AliceServer = escalus_users:get_server(Config, alice),
    BobServer   = escalus_users:get_server(Config, bob),

    Domain = muc_host(),
    RoomJid = make_jid(Room, Domain, <<>>),
    ArcJID = {R, RoomJid, get_muc_archive_id(Domain, Room)},
    Msgs = generate_msgs_for_days(ArcJID,
                                 [{B, make_jid(BobName, BobServer, <<"res1">>),
                                  rpc_apply(jid, replace_resource, [RoomJid, BobNick])},
                                  {A, make_jid(AliceName, AliceServer, <<"res1">>),
                                   rpc_apply(jid, replace_resource, [RoomJid, AliceNick])}], 16),

    put_muc_msgs(Msgs),

    maybe_wait_for_archive(Config),
    wait_for_room_archive_size(Domain, Room, length(Msgs)),

    [{pre_generated_muc_msgs, sort_msgs(Msgs)},
     {alice_nickname, AliceNick},
     {bob_nickname, BobNick} | Config].

put_muc_msgs(Msgs) ->
    [archive_muc_msg(Msg) || Msg <- Msgs].

archive_muc_msg({{MsgID, _},
                {_RoomBin, RoomJID, RoomArcID},
                {_FromBin, FromJID, SrcJID}, _, Packet}) ->
    rpc_apply(mod_mam_muc, archive_message_for_ct, [#{message_id => MsgID,
                                                     archive_id => RoomArcID,
                                                     local_jid => RoomJID,
                                                     remote_jid => FromJID,
                                                     source_jid => SrcJID,
                                                     origin_id => none,
                                                     direction => incoming,
                                                     packet => Packet}]).

get_archive_id(Server, User) ->
    rpc_apply(mod_mam_pm, archive_id, [Server, User]).

get_muc_archive_id(MucHost, Room) ->
    rpc_apply(mod_mam_muc, archive_id, [MucHost, Room]).

%% @doc Get a binary jid of the user, that tagged with `UserName' in the config.
nick_to_jid(UserName, Config) when is_atom(UserName) ->
    UserSpec = escalus_users:get_userspec(Config, UserName),
    escalus_utils:jid_to_lower(escalus_users:get_jid(Config, UserSpec)).

make_jid(U, S, R) ->
    mongoose_helper:make_jid(U, S, R).

-spec backend() -> rdbms | cassandra | disabled.
backend() ->
    Funs = [fun maybe_rdbms/1, fun maybe_cassandra/1],
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

maybe_rdbms(Host) ->
    case mongoose_helper:is_rdbms_enabled(Host) of
        true ->
            rdbms;
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
    mongoose_helper:is_rdbms_enabled(Host) orelse
    is_cassandra_enabled(Host) orelse
    is_elasticsearch_enabled(Host).

is_cassandra_enabled(_) ->
    is_cassandra_enabled().

is_cassandra_enabled() ->
    rpc(mim(), mongoose_wpool, is_configured, [cassandra]).

is_elasticsearch_enabled(_Host) ->
    case rpc(mim(), mongoose_elasticsearch, health, []) of
        {ok, _} ->
            true;
        {error, _} ->
            false
    end.

login_send_presence(Config, User) ->
    Spec = escalus_users:get_userspec(Config, User),
    {ok, Client} = escalus_client:start(Config, Spec, <<"dummy">>),
    escalus:send(Client, escalus_stanza:presence(<<"available">>)),
    Client.

maybe_wait_for_archive(Config) ->
    wait_for_parallel_writer(Config),
    case ?config(archive_wait, Config) of
        undefined ->
            ok;
        Value ->
            timer:sleep(Value)
    end.

wait_for_parallel_writer(Config) ->
    case ?config(wait_for_parallel_writer, Config) of
        undefined ->
            ok;
        Types ->
            HostType = domain_helper:host_type(),
            [wait_for_parallel_writer(Type, HostType) || Type <- Types]
    end.

wait_for_parallel_writer(pm, HostType) ->
    rpc(mim(), mongoose_hooks, mam_archive_sync, [HostType]);
wait_for_parallel_writer(muc, HostType) ->
    rpc(mim(), mongoose_hooks, mam_muc_archive_sync, [HostType]).

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

prefs_cases2_muc() ->
    [
     {{roster, [], []},              [true, true, true, true]},
     {{roster, [bob], []},           [true, true, true, true]},
     {{roster, [kate], []},          [true, true, true, true]},
     {{roster, [kate, bob], []},     [true, true, true, true]},

     {{roster, [], [bob]},           [false, true, true, true]},
     {{roster, [], [kate]},          [true, true, false, true]},
     {{roster, [], [bob, kate]},     [false, true, false, true]},

     {{never, [], []},              [false, false, false, false]},
     {{never, [bob], []},           [true, false, false, false]},
     {{never, [kate], []},          [false, false, true, false]},
     {{never, [kate, bob], []},     [true, false, true, false]},

     {{never, [], [bob]},           [false, false, false, false]},
     {{never, [], [kate]},          [false, false, false, false]},
     {{never, [], [bob, kate]},     [false, false, false, false]},

     {{always, [], []},              [true, true, true, true]},
     {{always, [bob], []},           [true, true, true, true]},
     {{always, [kate], []},          [true, true, true, true]},
     {{always, [kate, bob], []},     [true, true, true, true]},

     {{always, [], [bob]},           [false, true, true, true]},
     {{always, [], [kate]},          [true, true, false, true]},
     {{always, [], [bob, kate]},     [false, true, false, true]}
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

%% Alice and Bob are friends.
%% Alice and Kate are not friends.
run_prefs_case({PrefsState, ExpectedMessageStates}, Namespace, Alice, Bob, Kate, Config) ->
    {DefaultMode, AlwaysUsers, NeverUsers} = PrefsState,
    IqSet = stanza_prefs_set_request({DefaultMode, AlwaysUsers, NeverUsers, Namespace}, Config),
    _ReplySet = escalus:send_iq_and_wait_for_result(Alice, IqSet),
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
    [M1] = escalus:wait_for_stanzas(Bob, 1, 5000),
    [M2] = escalus:wait_for_stanzas(Kate, 1, 5000),
    [M3, M4] = escalus:wait_for_stanzas(Alice, 2, 5000),
    [escalus:assert(is_message, Message) || Message <- [M1, M2, M3, M4]],
    %% Delay check
    fun(Bodies) ->
        ActualMessageStates = [lists:member(M, Bodies) || M <- Messages],
        Debug = make_debug_prefs(ExpectedMessageStates, ActualMessageStates),
        ?_assert_equal_extra(ExpectedMessageStates, ActualMessageStates,
                             #{prefs_state => PrefsState,
                               debug => Debug,
                               bob_receives => [M1],
                               kate_receives => [M2],
                               alice_receives => [M3, M4]})
    end.

muc_run_prefs_case({PrefsState, ExpectedMessageStates}, Namespace, Alice, Bob, Kate, Config) ->
    Room = ?config(room, Config),
    RoomAddr = room_address(Room),

    {DefaultMode, AlwaysUsers, NeverUsers} = PrefsState,
    IqSet = stanza_prefs_set_request_muc({DefaultMode, AlwaysUsers, NeverUsers, Namespace}, Config),
    escalus:send(Alice, stanza_to_room(IqSet, Room)),
    _ReplySet = escalus:wait_for_stanza(Alice),

    Messages = [iolist_to_binary(io_lib:format("n=~p, prefs=~p, now=~p",
                                               [N, PrefsState, os:timestamp()]))
                || N <- [1, 2, 3, 4]],

    escalus:send(Bob, escalus_stanza:groupchat_to(RoomAddr, lists:nth(1,Messages))),
    escalus:wait_for_stanza(Alice),

    escalus:send(Alice, escalus_stanza:groupchat_to(RoomAddr, lists:nth(2,Messages))),
    escalus:wait_for_stanza(Alice),

    escalus:send(Kate, escalus_stanza:groupchat_to(RoomAddr, lists:nth(3,Messages))),
    escalus:wait_for_stanza(Alice),

    escalus:send(Alice, escalus_stanza:groupchat_to(RoomAddr, lists:nth(4,Messages))),
    escalus:wait_for_stanza(Alice),

    %% Delay check
    fun(Bodies) ->
        ActualMessageStates = [lists:member(M, Bodies) || M <- Messages],
        Debug = make_debug_prefs(ExpectedMessageStates, ActualMessageStates),
        ?_assert_equal_extra(ExpectedMessageStates, ActualMessageStates,
                             #{prefs_state => PrefsState,
                               debug => Debug})
    end.

make_debug_prefs(ExpectedMessageStates, ActualMessageStates) ->
    Zipped = lists:zip(prefs_checks_descriptions(),
                       lists:zip(ExpectedMessageStates, ActualMessageStates)),
    [#{case_description => Description,
       problem_found => describe_problem(Expected, Actual)}
     || {Description, {Expected, Actual}} <- Zipped, Expected =/= Actual].

describe_problem(_Expected=true, _Actual=false) -> "Ignored, but should be archived";
describe_problem(_Expected=false, _Actual=true) -> "Archived, but should be ignored".

prefs_checks_descriptions() ->
    ["1. Bob sends a message to Alice",
     "2. Alice sends a message to Bob",
     "3. Kate sends a message to Alice",
     "4. Alice sends a message to Kate"].

get_all_messages(P, Alice) ->
    get_all_messages(P, Alice, undefined).

get_all_messages(P, Alice, Id) ->
    RSM = #rsm_in{max=50, direction='after', id=Id},
    escalus:send(Alice, stanza_page_archive_request(P, <<"page_rsm">>, RSM)),
    Result = wait_archive_respond(Alice),
    PageMessages = respond_messages(Result),
    ParsedIQ = parse_result_iq(Result),
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

stanza_prefs_set_request_muc({DefaultMode, AlwaysUsers, NeverUsers, Namespace}, Config) ->
    DefaultModeBin = atom_to_binary(DefaultMode, utf8),
    AlwaysJIDs = users_to_nick(AlwaysUsers, Config),
    NeverJIDs  = users_to_nick(NeverUsers, Config),
    stanza_prefs_set_request(DefaultModeBin, AlwaysJIDs, NeverJIDs, Namespace).

users_to_nick(Users, Config) ->
    Room = ?config(room, Config),
    [muc_helper:room_address(Room, string:lowercase(nick(escalus_users:get_jid(Config, User)))) || User <- Users].

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
    ReplySet = escalus:send_iq_and_wait_for_result(Alice, IqSet),
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

add_store_hint(#xmlel{children=Children}=Elem) ->
    Elem#xmlel{children=Children ++ [hint_elem(store)]}.

add_nostore_hint(#xmlel{children=Children}=Elem) ->
    Elem#xmlel{children=Children ++ [hint_elem(no_store)]}.

hint_elem(store) ->
    #xmlel{name = <<"store">>, attrs = #{<<"xmlns">> => <<"urn:xmpp:hints">>}};
hint_elem(no_store) ->
    #xmlel{name = <<"no-store">>, attrs = #{<<"xmlns">> => <<"urn:xmpp:hints">>}}.

has_x_user_element(ArcMsg) ->
    ParsedMess = parse_forwarded_message(ArcMsg),
    ParsedMess#forwarded_message.has_x_user_element.

muc_light_host() ->
    muc_light_helper:muc_host().

host() ->
    ct:get_config({hosts, mim, domain}).

host_type() ->
    domain_helper:host_type().

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

assert_list_size(N, List) when N =:= length(List) -> List.

%% Assertions for instrumentation events

assert_archive_message_event(EventName, BinJid) ->
    instrument_helper:assert_one(
      EventName, labels(),
      fun(#{count := 1, time := T, params := #{local_jid := LocalJid}}) ->
              eq_bjid(LocalJid, BinJid) andalso pos_int(T)
      end).

assert_lookup_event(EventName, BinJid) ->
    instrument_helper:assert_one(
      EventName, labels(),
      fun(#{count := 1, size := 1, time := T, params := #{caller_jid := CallerJid}}) ->
              eq_bjid(CallerJid, BinJid) andalso pos_int(T)
      end).

%% The event might originate from a different test case running in parallel,
%% but there is no easy way around it other than adding all flushed messages to measurements.
assert_flushed_event_if_async(EventName, Config) ->
    case ?config(configuration, Config) of
        C when C =:= rdbms_async_pool;
               C =:= rdbms_async_cache ->
            instrument_helper:assert(
              EventName, labels(),
              fun(#{count := Count, time := T, time_per_message := T1}) ->
                      pos_int(Count) andalso pos_int(T) andalso pos_int(T1) andalso T >= T1
              end);
        _ ->
            ok
    end.

assert_async_batch_flush_event(TS, ExpectedCount, PoolId) ->
    instrument_helper:assert(
        async_pool_flush,
        #{host_type => host_type(), pool_id => PoolId},
        fun(#{batch := 1}) -> true end,
        #{min_timestamp => TS, expected_count => ExpectedCount}).

assert_async_timed_flush_event(Config, TS, PoolId) ->
    case ?config(configuration, Config) of
        rdbms_async_pool ->
            instrument_helper:assert(
                async_pool_flush,
                #{host_type => host_type(), pool_id => PoolId},
                fun(#{timed := 1}) -> true end,
                #{min_timestamp => TS});
        _ ->
            ok
    end.

assert_dropped_iq_event(Config, BinJid) ->
    EventName = case ?config(room, Config) of
                    undefined -> mod_mam_pm_dropped_iq;
                    _ -> mod_mam_muc_dropped_iq
                end,
    instrument_helper:assert_one(
      EventName, labels(),
      fun(#{acc := #{stanza := #{from_jid := FromJid}}}) -> eq_bjid(FromJid, BinJid) end).

assert_dropped_msg_event(EventName, TS) ->
    instrument_helper:assert(
      EventName, labels(), fun(#{count := 1}) -> true end, #{min_timestamp => TS}).

assert_event_with_jid(EventName, BinJid) ->
    instrument_helper:assert_one(
      EventName, labels(), fun(#{count := 1, jid := Jid}) -> eq_bjid(Jid, BinJid) end).

assert_no_event_with_jid(EventName, BinJid) ->
    instrument_helper:assert_not_emitted(
      EventName, labels(), fun(#{count := 1, jid := Jid}) -> eq_bjid(Jid, BinJid) end).

labels() -> #{host_type => host_type()}.

pos_int(T) -> is_integer(T) andalso T > 0.

eq_bjid(Jid, BinJid) -> Jid =:= jid:from_binary(BinJid).
