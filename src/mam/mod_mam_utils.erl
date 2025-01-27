%%%-------------------------------------------------------------------
%%% @author Uvarov Michael <arcusfelis@gmail.com>
%%% @copyright (C) 2013, Uvarov Michael
%%% @doc General functions for MAM.
%%% @end
%%%-------------------------------------------------------------------
-module(mod_mam_utils).
%% Time
-export([maybe_microseconds/1]).

%% UID
-export([get_or_generate_mam_id/1,
         generate_message_id/1,
         encode_compact_uuid/2,
         decode_compact_uuid/1,
         mess_id_to_external_binary/1,
         external_binary_to_mess_id/1,
         wrapper_id/0]).

%% XML
-export([maybe_add_arcid_elems/4,
         maybe_log_deprecation/1,
         is_arcid_elem_for/3,
         replace_arcid_elem/4,
         replace_x_user_element/4,
         append_arcid_elem/4,
         delete_arcid_elem/3,
         delete_x_user_element/1,
         packet_to_x_user_jid/1,
         get_one_of_path/2,
         get_one_of_path/3,
         is_archivable_message/4,
         has_message_retraction/2,
         get_retract_id/2,
         get_origin_id/1,
         is_groupchat/1,
         should_page_be_flipped/1,
         tombstone/2,
         wrap_message/6,
         wrap_message/7,
         result_set/4,
         result_query/2,
         result_prefs/4,
         make_fin_element/7,
         parse_prefs/1,
         form_borders_decode/1,
         form_decode_optimizations/1,
         is_mam_result_message/1,
         make_metadata_element/0,
         make_metadata_element/4,
         features/2]).

%% Forms
-export([
    message_form/3,
    form_to_text/1
]).

%% Text search
-export([
    normalize_search_text/1,
    normalize_search_text/2,
    packet_to_search_body/2,
    has_full_text_search/2
]).

%% JID serialization
-export([jid_to_opt_binary/2,
         expand_minified_jid/2]).

%% Other
-export([maybe_integer/2,
         maybe_min/2,
         maybe_max/2,
         maybe_last/1,
         apply_start_border/2,
         apply_end_border/2,
         bare_jid/1,
         full_jid/1,
         calculate_msg_id_borders/3,
         calculate_msg_id_borders/4,
         maybe_encode_compact_uuid/2,
         wait_shaper/4,
         check_for_item_not_found/3,
         maybe_reverse_messages/2,
         get_msg_id_and_timestamp/1,
         lookup_specific_messages/4,
         is_mam_muc_enabled/2]).

%% Ejabberd
-export([send_message/4,
         maybe_set_client_xmlns/2,
         is_jid_in_user_roster/3]).

%% Shared logic
-export([check_result_for_policy_violation/2,
         lookup/3,
         lookup_first_and_last_messages/4,
         lookup_first_and_last_messages/5,
         incremental_delete_domain/5,
         db_message_codec/2, db_jid_codec/2]).

-callback extra_fin_element(mongooseim:host_type(),
                            mam_iq:lookup_params(),
                            exml:element()) -> exml:element().

-ignore_xref([behaviour_info/1, append_arcid_elem/4, delete_arcid_elem/3,
              get_one_of_path/3, is_arcid_elem_for/3, maybe_encode_compact_uuid/2,
              maybe_last/1, result_query/2, send_message/4, wrap_message/7, wrapper_id/0]).

%-define(MAM_INLINE_UTILS, true).

-ifdef(MAM_INLINE_UTILS).
-compile({inline, [
                   is_valid_message/4,
                   is_valid_message_type/3,
                   encode_compact_uuid/2,
                   get_one_of_path/3,
                   delay/2,
                   forwarded/3,
                   result/4,
                   valid_behavior/1]}).
-endif.

-include("jlib.hrl").
-include_lib("exml/include/exml.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-export([is_valid_message/4]).
-endif.

-include("mod_mam.hrl").
-include("mongoose_rsm.hrl").
-include("mongoose_ns.hrl").

-define(MAYBE_BIN(X), (is_binary(X) orelse (X) =:= undefined)).
-define(BIGINT_MAX, 16#7fffffffffffffff).  % 9223372036854775807, (2^63 - 1)

-export_type([direction/0, retraction_id/0, retraction_info/0]).

%% ----------------------------------------------------------------------
%% Datetime types
-type ne_binary() :: <<_:8, _:_*8>>.
-type iso8601_datetime_binary() :: ne_binary().
%% Microseconds from 01.01.1970
-type unix_timestamp() :: mod_mam:unix_timestamp().

-type archive_behaviour() :: mod_mam:archive_behaviour().
-type archive_behaviour_bin() :: binary(). % `<<"roster">> | <<"always">> | <<"never">>'.

-type direction() :: incoming | outgoing.
-type retraction_id() :: {origin_id | stanza_id, binary()}.
-type retraction_info() :: #{retract_on := origin_id | stanza_id,
                             packet := exml:element(),
                             message_id := mod_mam:message_id(),
                             origin_id := null | binary()}.

%% -----------------------------------------------------------------------
%% Time

%% @doc Return a unix timestamp in microseconds.
%%
%% "maybe" means, that the function may return `undefined'.
%% @end
-spec maybe_microseconds(iso8601_datetime_binary()) -> unix_timestamp();
                          (<<>>) -> undefined.
maybe_microseconds(<<>>) -> undefined;
maybe_microseconds(ISODateTime) ->
    try calendar:rfc3339_to_system_time(binary_to_list(ISODateTime), [{unit, microsecond}])
    catch error:_Error -> undefined
    end.

%% -----------------------------------------------------------------------
%% UID

-spec get_or_generate_mam_id(mongoose_acc:t()) -> integer().
get_or_generate_mam_id(Acc) ->
    case mongoose_acc:get(mam, mam_id, undefined, Acc) of
        undefined ->
            CandidateStamp = mongoose_acc:timestamp(Acc),
            generate_message_id(CandidateStamp);
        ExtMessId ->
            mod_mam_utils:external_binary_to_mess_id(ExtMessId)
    end.

-spec generate_message_id(integer()) -> integer().
generate_message_id(CandidateStamp) ->
    NodeNum = mongoose_node_num:node_num(),
    UniqueStamp = mongoose_mam_id:next_unique(CandidateStamp),
    encode_compact_uuid(UniqueStamp, NodeNum).

%% @doc Create a message ID (UID).
%%
%% It removes a leading 0 from 64-bit binary representation.
%% It puts node id as a last byte.
%% The maximum date, that can be encoded is `{{4253, 5, 31}, {22, 20, 37}}'.
-spec encode_compact_uuid(integer(), integer()) -> integer().
encode_compact_uuid(Microseconds, NodeNum)
    when is_integer(Microseconds), is_integer(NodeNum) ->
    (Microseconds bsl 8) + NodeNum.


%% @doc Extract date and node id from a message id.
-spec decode_compact_uuid(integer()) -> {integer(), byte()}.
decode_compact_uuid(Id) ->
    Microseconds = Id bsr 8,
    NodeNum = Id band 255,
    {Microseconds, NodeNum}.


%% @doc Encode a message ID to pass it to the user.
-spec mess_id_to_external_binary(integer()) -> binary().
mess_id_to_external_binary(MessID) when is_integer(MessID) ->
    integer_to_binary(MessID, 32).

%% @doc Decode a message ID received from the user.
-spec external_binary_to_mess_id(binary()) -> integer().
external_binary_to_mess_id(BExtMessID) when is_binary(BExtMessID) ->
    try binary_to_integer(BExtMessID, 32) of
        MessId when is_integer(MessId), MessId =< ?BIGINT_MAX -> MessId;
        _ -> throw(invalid_stanza_id)
    catch error:badarg -> throw(invalid_stanza_id)
    end.

%% -----------------------------------------------------------------------
%% XML

-spec maybe_add_arcid_elems(To :: jid:simple_jid()  | jid:jid(),
                            MessID :: binary(), Packet :: exml:element(),
                            AddStanzaid :: boolean()) ->
          AlteredPacket :: exml:element().
maybe_add_arcid_elems(To, MessID, Packet, AddStanzaid) ->
    BareTo = jid:to_bare_binary(To),
    case AddStanzaid of
        true ->
            replace_arcid_elem(<<"stanza-id">>, BareTo, MessID, Packet);
        _ -> Packet
    end.

maybe_log_deprecation(_IQ) ->
    ok. %% May be reused for future MAM versions.

%% @doc Return true, if the first element points on `By'.
-spec is_arcid_elem_for(ElemName :: binary(), exml:element(), By :: binary()) -> boolean().
is_arcid_elem_for(<<"archived">>, #xmlel{name = <<"archived">>,
                                        attrs = #{<<"by">> := By}}, By) ->
    true;
is_arcid_elem_for(<<"stanza-id">>,
                  #xmlel{name = <<"stanza-id">>,
                         attrs = #{<<"by">> := By,
                                   <<"xmlns">> := ?NS_STANZAID}},
                  By) ->
    true;
is_arcid_elem_for(_, _, _) ->
    false.

-spec replace_arcid_elem(ElemName :: binary(), By :: binary(), Id :: binary(),
                         Packet :: exml:element()) -> exml:element().
replace_arcid_elem(ElemName, By, Id, Packet) ->
    append_arcid_elem(ElemName, By, Id,
                       delete_arcid_elem(ElemName, By, Packet)).

-spec append_arcid_elem(ElemName :: binary(), By :: binary(), Id :: binary(),
                        Packet :: exml:element()) ->exml:element().
append_arcid_elem(<<"stanza-id">>, By, Id, Packet) ->
    Archived = #xmlel{
                  name = <<"stanza-id">>,
                  attrs=#{<<"by">> => By,
                          <<"id">> => Id,
                          <<"xmlns">> => ?NS_STANZAID}},
    jlib:append_subtags(Packet, [Archived]);
append_arcid_elem(ElemName, By, Id, Packet) ->
    Archived = #xmlel{
                  name = ElemName,
                  attrs=#{<<"by">> => By,
                          <<"id">> => Id}},
    jlib:append_subtags(Packet, [Archived]).

-spec delete_arcid_elem(ElemName :: binary(), By :: binary(), exml:element()) -> exml:element().
delete_arcid_elem(ElemName, By, Packet=#xmlel{children=Cs}) ->
    Packet#xmlel{children=[C || C <- Cs, not is_arcid_elem_for(ElemName, C, By)]}.


is_x_user_element(#xmlel{name = <<"x">>, attrs = #{<<"xmlns">> := ?NS_MUC_USER}}) ->
    true;
is_x_user_element(_) ->
    false.

-spec replace_x_user_element(FromJID :: jid:jid(), Role :: mod_muc:role(),
                             Affiliation :: mod_muc:affiliation(), exml:element()) -> exml:element().
replace_x_user_element(FromJID, Role, Affiliation, Packet) ->
    append_x_user_element(FromJID, Role, Affiliation,
                          delete_x_user_element(Packet)).

append_x_user_element(FromJID, Role, Affiliation, Packet) ->
    ItemElem = x_user_item(FromJID, Role, Affiliation),
    X = #xmlel{
        name = <<"x">>,
        attrs = #{<<"xmlns">> => ?NS_MUC_USER},
        children = [ItemElem]},
    jlib:append_subtags(Packet, [X]).

x_user_item(FromJID, Role, Affiliation) ->
    #xmlel{
       name = <<"item">>,
       attrs = #{<<"affiliation">> => atom_to_binary(Affiliation, latin1),
                 <<"jid">> => jid:to_binary(FromJID),
                 <<"role">> => atom_to_binary(Role, latin1)}}.

-spec delete_x_user_element(exml:element()) -> exml:element().
delete_x_user_element(Packet=#xmlel{children=Cs}) ->
    Packet#xmlel{children=[C || C <- Cs, not is_x_user_element(C)]}.

-spec packet_to_x_user_jid(exml:element()) -> jid:jid() | error | undefined.
packet_to_x_user_jid(#xmlel{children=Cs}) ->
    case [C || C <- Cs, is_x_user_element(C)] of
        [] -> undefined;
        [X|_] ->
            case exml_query:path(X, [{element, <<"item">>}, {attr, <<"jid">>}]) of
                undefined -> undefined;
                BinaryJid -> jid:from_binary(BinaryJid)
            end
    end.

-spec get_one_of_path(_, list(T)) -> T when T :: any().
get_one_of_path(Elem, List) ->
    get_one_of_path(Elem, List, <<>>).


-spec get_one_of_path(_, list(T), T) -> T when T :: any().
get_one_of_path(Elem, [H|T], Def) ->
    case exml_query:path(Elem, H) of
        undefined -> get_one_of_path(Elem, T, Def);
        Val  -> Val
    end;
get_one_of_path(_Elem, [], Def) ->
    Def.


%% @doc In order to be archived, the message must be of type "normal", "chat" or "groupchat".
%% It also must include a body or chat marker, as long as it doesn't include
%% "result", "delay" or "no-store" elements.
%% @end
-spec is_archivable_message(module(), direction(), exml:element(), boolean()) -> boolean().
is_archivable_message(Mod, Dir, Packet=#xmlel{name = <<"message">>}, ArchiveChatMarkers) ->
    Type = exml_query:attr(Packet, <<"type">>, <<"normal">>),
    is_valid_message_type(Mod, Dir, Type) andalso
        is_valid_message(Mod, Dir, Packet, ArchiveChatMarkers);
is_archivable_message(_, _, _, _) ->
    false.

is_valid_message_type(_, _, <<"normal">>) -> true;
is_valid_message_type(_, _, <<"chat">>) -> true;
is_valid_message_type(mod_inbox, _, <<"groupchat">>) -> true;
is_valid_message_type(_, incoming, <<"groupchat">>) -> true;
is_valid_message_type(_, _, _) -> false.

is_valid_message(_Mod, _Dir, Packet, ArchiveChatMarkers) ->
    Body       = exml_query:subelement(Packet, <<"body">>, false),
    ChatMarker = ArchiveChatMarkers
                 andalso has_chat_marker(Packet),
    Retract    = get_retract_id(Packet) =/= none,
    %% Used in MAM
    Result     = exml_query:subelement(Packet, <<"result">>, false),
    %% Used in mod_offline
    Delay      = exml_query:subelement(Packet, <<"delay">>, false),
    %% Message Processing Hints (XEP-0334)
    NoStore    = exml_query:path(Packet, [{element_with_ns, <<"no-store">>, ?NS_HINTS}], false),
    %% Message Processing Hints (XEP-0334)
    Store      = exml_query:path(Packet, [{element_with_ns, <<"store">>, ?NS_HINTS}], false),

    has_any([Store, Body, ChatMarker, Retract]) andalso not has_any([Result, Delay, NoStore]).

has_any(Elements) ->
    lists:any(fun(El) -> El =/= false end, Elements).

has_chat_marker(Packet) ->
    mongoose_chat_markers:has_chat_markers(Packet).

-spec get_retract_id(false, exml:element()) -> none;
                    (true, exml:element()) -> none | retraction_id().
get_retract_id(true = _Enabled, Packet) ->
    get_retract_id(Packet);
get_retract_id(false, _Packet) ->
    none.

-spec get_retract_id(exml:element()) -> none | retraction_id().
get_retract_id(Packet) ->
    case exml_query:path(Packet, [{element_with_ns, <<"apply-to">>, ?NS_FASTEN}], none) of
        none -> none;
        Fasten ->
            case {exml_query:path(Fasten, [{element, <<"retract">>}, {attr, <<"xmlns">>}], none),
                  exml_query:path(Fasten, [{attr, <<"id">>}], none)} of
                {none, _} -> none;
                {_, none} -> none;
                {?NS_RETRACT, OriginId} -> {origin_id, OriginId};
                {?NS_ESL_RETRACT, StanzaId} -> {stanza_id, StanzaId}
            end
    end.

get_origin_id(Packet) ->
    exml_query:path(Packet, [{element_with_ns, <<"origin-id">>, ?NS_STANZAID},
                             {attr, <<"id">>}], none).

is_groupchat(<<"groupchat">>) ->
    true;
is_groupchat(_) ->
    false.

-spec should_page_be_flipped(exml:element()) -> boolean().
should_page_be_flipped(Packet) ->
    case exml_query:path(Packet, [{element, <<"flip-page">>}], none) of
        none -> false;
        _ -> true
    end.

-spec maybe_reverse_messages(mam_iq:lookup_params(), [mod_mam:message_row()]) ->
    [mod_mam:message_row()].
maybe_reverse_messages(#{flip_page := true}, Messages) -> lists:reverse(Messages);
maybe_reverse_messages(#{flip_page := false}, Messages) -> Messages.

-spec get_msg_id_and_timestamp(mod_mam:message_row()) -> {binary(), binary()}.
get_msg_id_and_timestamp(#{id := MsgID}) ->
    {Microseconds, _NodeMessID} = decode_compact_uuid(MsgID),
    TS = calendar:system_time_to_rfc3339(Microseconds, [{offset, "Z"}, {unit, microsecond}]),
    ExtID = mess_id_to_external_binary(MsgID),
    {ExtID, list_to_binary(TS)}.

-spec lookup_specific_messages(mongooseim:host_type(),
                               mam_iq:lookup_params(),
                               [mod_mam:message_id()],
                               fun()) -> [mod_mam:message_row()] | {error, item_not_found}.
lookup_specific_messages(HostType, Params, IDs, FetchFun) ->
    {FinalOffset, AccumulatedMessages} = lists:foldl(
        fun(ID, {_AccOffset, AccMsgs}) ->
            {ok, {_, OffsetForID, MessagesForID}} = FetchFun(HostType, Params#{message_id => ID}),
            {OffsetForID, AccMsgs ++ MessagesForID}
        end,
        {0, []}, IDs),

    Result = determine_result(Params, FinalOffset, AccumulatedMessages),
    case length(IDs) == length(AccumulatedMessages) of
        true -> Result;
        false -> {error, item_not_found}
    end.

determine_result(#{is_simple := true}, _Offset, Messages) ->
    {ok, {undefined, undefined, Messages}};
determine_result(#{}, Offset, Messages) ->
    {ok, {length(Messages), Offset, Messages}}.

tombstone(RetractionInfo = #{packet := Packet}, LocJid) ->
    Packet#xmlel{children = [retracted_element(RetractionInfo, LocJid)]}.

-spec retracted_element(retraction_info(), jid:jid()) -> exml:element().
retracted_element(#{retract_on := origin_id,
                    origin_id := OriginID}, _LocJid) ->
    Timestamp = calendar:system_time_to_rfc3339(erlang:system_time(second), [{offset, "Z"}]),
    #xmlel{name = <<"retracted">>,
           attrs = #{<<"xmlns">> => ?NS_RETRACT,
                     <<"stamp">> => list_to_binary(Timestamp)},
           children = [#xmlel{name = <<"origin-id">>,
                              attrs = #{<<"xmlns">> => ?NS_STANZAID,
                                        <<"id">> => OriginID}}
                      ]};
retracted_element(#{retract_on := stanza_id,
                    message_id := MessID} = Env, LocJid) ->
    Timestamp = calendar:system_time_to_rfc3339(erlang:system_time(second), [{offset, "Z"}]),
    StanzaID = mod_mam_utils:mess_id_to_external_binary(MessID),
    MaybeOriginId = maybe_append_origin_id(Env),
    #xmlel{name = <<"retracted">>,
           attrs = #{<<"xmlns">> => ?NS_ESL_RETRACT,
                     <<"stamp">> => list_to_binary(Timestamp)},
           children = [#xmlel{name = <<"stanza-id">>,
                              attrs = #{<<"xmlns">> => ?NS_STANZAID,
                                        <<"id">> => StanzaID,
                                        <<"by">> => jid:to_bare_binary(LocJid)}} |
                       MaybeOriginId
                      ]}.

-spec maybe_append_origin_id(retraction_info()) -> [exml:element()].
maybe_append_origin_id(#{origin_id := OriginID}) when is_binary(OriginID), <<>> =/= OriginID ->
    [#xmlel{name = <<"origin-id">>, attrs = #{<<"xmlns">> => ?NS_STANZAID, <<"id">> => OriginID}}];
maybe_append_origin_id(_) ->
    [].

%% @doc Forms `<forwarded/>' element, according to the XEP.
-spec wrap_message(MamNs :: binary(), Packet :: exml:element(), QueryID :: binary(),
                   MessageUID :: term(), TS :: jlib:rfc3339_string(),
                   SrcJID :: jid:jid()) -> Wrapper :: exml:element().
wrap_message(MamNs, Packet, QueryID, MessageUID, TS, SrcJID) ->
    wrap_message(MamNs, Packet, QueryID, MessageUID, wrapper_id(), TS, SrcJID).

-spec wrap_message(MamNs :: binary(), Packet :: exml:element(), QueryID :: binary(),
                   MessageUID :: term(), WrapperI :: binary(),
                   TS :: jlib:rfc3339_string(),
                   SrcJID :: jid:jid()) -> Wrapper :: exml:element().
wrap_message(MamNs, Packet, QueryID, MessageUID, WrapperID, TS, SrcJID) ->
    #xmlel{ name = <<"message">>,
            attrs = #{<<"id">> => WrapperID},
            children = [result(MamNs, QueryID, MessageUID,
                               [forwarded(Packet, TS, SrcJID)])] }.

-spec forwarded(exml:element(), jlib:rfc3339_string(), jid:jid())
               -> exml:element().
forwarded(Packet, TS, SrcJID) ->
    #xmlel{
       name = <<"forwarded">>,
       attrs = #{<<"xmlns">> => ?NS_FORWARD},
       %% Two places to include SrcJID:
       %% - delay.from - optional XEP-0297 (TODO: depricate adding it?)
       %% - message.from - required XEP-0313
       %% Also, mod_mam_muc will replace it again with SrcJID
       children = [delay(TS, SrcJID), replace_from_attribute(SrcJID, Packet)]}.

-spec delay(jlib:rfc3339_string(), jid:jid()) -> exml:element().
delay(TS, SrcJID) ->
    jlib:timestamp_to_xml(TS, SrcJID, <<>>).

replace_from_attribute(From, Packet=#xmlel{attrs = Attrs}) ->
    Packet#xmlel{attrs = Attrs#{<<"from">> => jid:to_binary(From)}}.

%% @doc Generates tag `<result />'.
%% This element will be added in each forwarded message.
-spec result(binary(), _, MessageUID :: binary(), Children :: [exml:element(), ...])
            -> exml:element().
result(MamNs, QueryID, MessageUID, Children) when is_list(Children) ->
    %% <result xmlns='urn:xmpp:mam:tmp' queryid='f27' id='28482-98726-73623' />
    Attrs = case QueryID =/= undefined andalso QueryID =/= <<>>  of
              true->
                #{<<"queryid">> => QueryID};
              false ->
                #{}
            end,
    #xmlel{
       name = <<"result">>,
       attrs = Attrs#{<<"xmlns">> => MamNs, <<"id">> => MessageUID},
       children = Children}.


%% @doc Generates `<set />' tag.
%%
%% This element will be added into "iq/query".
%% @end
-spec result_set(FirstId :: binary() | undefined,
                 LastId :: binary() | undefined,
                 FirstIndexI :: non_neg_integer() | undefined,
                 CountI :: non_neg_integer() | undefined) -> exml:element().
result_set(FirstId, LastId, undefined, undefined)
  when ?MAYBE_BIN(FirstId), ?MAYBE_BIN(LastId) ->
    %% Simple response
    FirstEl = [#xmlel{name = <<"first">>,
                      children = [#xmlcdata{content = FirstId}]
                     }
               || FirstId =/= undefined],
    LastEl = [#xmlel{name = <<"last">>,
                     children = [#xmlcdata{content = LastId}]
                    }
              || LastId =/= undefined],
    #xmlel{
       name = <<"set">>,
       attrs = #{<<"xmlns">> => ?NS_RSM},
       children = FirstEl ++ LastEl};
result_set(FirstId, LastId, FirstIndexI, CountI)
  when ?MAYBE_BIN(FirstId), ?MAYBE_BIN(LastId) ->
    FirstEl = [#xmlel{name = <<"first">>,
                      attrs = #{<<"index">> => integer_to_binary(FirstIndexI)},
                      children = [#xmlcdata{content = FirstId}]
                     }
               || FirstId =/= undefined],
    LastEl = [#xmlel{name = <<"last">>,
                     children = [#xmlcdata{content = LastId}]
                    }
              || LastId =/= undefined],
    CountEl = #xmlel{
                 name = <<"count">>,
                 children = [#xmlcdata{content = integer_to_binary(CountI)}]},
    #xmlel{
       name = <<"set">>,
       attrs = #{<<"xmlns">> => ?NS_RSM},
       children = FirstEl ++ LastEl ++ [CountEl]}.


-spec result_query(exml:child(), binary()) -> exml:element().
result_query(SetEl, Namespace) ->
    #xmlel{
       name = <<"query">>,
       attrs = #{<<"xmlns">> => Namespace},
       children = [SetEl]}.

-spec result_prefs(DefaultMode :: archive_behaviour(),
                   AlwaysJIDs :: [jid:literal_jid()],
                   NeverJIDs :: [jid:literal_jid()],
                   Namespace :: binary()) -> exml:element().
result_prefs(DefaultMode, AlwaysJIDs, NeverJIDs, Namespace) ->
    AlwaysEl = #xmlel{name = <<"always">>,
                      children = encode_jids(AlwaysJIDs)},
    NeverEl  = #xmlel{name = <<"never">>,
                      children = encode_jids(NeverJIDs)},
    #xmlel{
       name = <<"prefs">>,
       attrs = #{<<"xmlns">> => Namespace,
                 <<"default">> => atom_to_binary(DefaultMode, utf8)},
       children = [AlwaysEl, NeverEl]
      }.


-spec encode_jids([binary() | string()]) -> [exml:element()].
encode_jids(JIDs) ->
    [#xmlel{name = <<"jid">>, children = [#xmlcdata{content = JID}]}
     || JID <- JIDs].


%% MAM v0.4.1 and above
-spec make_fin_element(mongooseim:host_type(),
                       mam_iq:lookup_params(),
                       binary(),
                       boolean(),
                       boolean(),
                       exml:element(),
                       module()) ->
    exml:element().
make_fin_element(HostType, Params, MamNs, IsComplete, IsStable, ResultSetEl, ExtFinMod) ->
    Attrs0 = if IsComplete -> #{<<"complete">> => <<"true">>};
                true -> #{}
             end,
    Attrs1 = if IsStable -> Attrs0;
                true -> Attrs0#{<<"stable">> => <<"false">>}
             end,
    FinEl = #xmlel{
               name = <<"fin">>,
               attrs = Attrs1#{<<"xmlns">> => MamNs},
               children = [ResultSetEl]},
    maybe_transform_fin_elem(ExtFinMod, HostType, Params, FinEl).

maybe_transform_fin_elem(undefined, _HostType, _Params, FinEl) ->
    FinEl;
maybe_transform_fin_elem(Module, HostType, Params, FinEl) ->
    Module:extra_fin_element(HostType, Params, FinEl).

-spec make_metadata_element() -> exml:element().
make_metadata_element() ->
    #xmlel{
        name = <<"metadata">>,
        attrs = #{<<"xmlns">> => ?NS_MAM_06}}.

-spec make_metadata_element(binary(), binary(), binary(), binary()) -> exml:element().
make_metadata_element(FirstMsgID, FirstMsgTS, LastMsgID, LastMsgTS) ->
    #xmlel{
        name = <<"metadata">>,
        attrs = #{<<"xmlns">> => ?NS_MAM_06},
        children = [#xmlel{name = <<"start">>,
                          attrs = #{<<"id">> => FirstMsgID, <<"timestamp">> => FirstMsgTS}},
                    #xmlel{name = <<"end">>,
                          attrs = #{<<"id">> => LastMsgID, <<"timestamp">> => LastMsgTS}}]
    }.

-spec parse_prefs(PrefsEl :: exml:element()) -> mod_mam:preference().
parse_prefs(El = #xmlel{ name = <<"prefs">> }) ->
    Default = exml_query:attr(El, <<"default">>),
    AlwaysJIDs = parse_jid_list(El, <<"always">>),
    NeverJIDs  = parse_jid_list(El, <<"never">>),
    {valid_behavior(Default), AlwaysJIDs, NeverJIDs}.


-spec valid_behavior(archive_behaviour_bin()) -> archive_behaviour().
valid_behavior(<<"always">>) -> always;
valid_behavior(<<"never">>)  -> never;
valid_behavior(<<"roster">>) -> roster.


-spec parse_jid_list(exml:element(), binary()) -> [jid:literal_jid()].
parse_jid_list(El, Name) ->
    case exml_query:subelement(El, Name) of
        undefined -> [];
        #xmlel{children = JIDEls} ->
            %% Ignore cdata between jid elements
            MaybeJids = [binary_jid_to_lower(exml_query:cdata(JIDEl))
                         || JIDEl <- JIDEls, is_jid_element(JIDEl)],
            skip_bad_jids(MaybeJids)
    end.

is_jid_element(#xmlel{name = <<"jid">>}) ->
    true;
is_jid_element(_) -> %% ignore cdata
    false.

%% @doc Normalize JID to be used when comparing JIDs in DB
binary_jid_to_lower(BinJid) when is_binary(BinJid) ->
    Jid = jid:from_binary(BinJid),
    case jid:to_lower(Jid) of
        error ->
            error;
        LowerJid ->
            jid:to_binary(LowerJid)
    end.

skip_bad_jids(MaybeJids) ->
    [Jid || Jid <- MaybeJids, is_binary(Jid)].

-spec form_borders_decode(mongoose_data_forms:kv_map()) -> 'undefined' | mod_mam:borders().
form_borders_decode(KVs) ->
    AfterID  = form_field_mess_id(KVs, <<"after-id">>),
    BeforeID = form_field_mess_id(KVs, <<"before-id">>),
    FromID   = form_field_mess_id(KVs, <<"from-id">>),
    ToID     = form_field_mess_id(KVs, <<"to-id">>),
    borders(AfterID, BeforeID, FromID, ToID).


-spec borders(AfterID :: 'undefined' | non_neg_integer(),
              BeforeID :: 'undefined' | non_neg_integer(),
              FromID :: 'undefined' | non_neg_integer(),
              ToID :: 'undefined' | non_neg_integer()
            ) -> 'undefined' | mod_mam:borders().
borders(undefined, undefined, undefined, undefined) ->
    undefined;
borders(AfterID, BeforeID, FromID, ToID) ->
    #mam_borders{
        after_id  = AfterID,
        before_id = BeforeID,
        from_id   = FromID,
        to_id     = ToID
    }.

-spec form_field_mess_id(mongoose_data_forms:kv_map(), binary()) -> 'undefined' | integer().
form_field_mess_id(KVs, Name) ->
    case KVs of
        #{Name := [BExtMessID]} -> external_binary_to_mess_id(BExtMessID);
        #{} -> undefined
    end.

-spec form_decode_optimizations(mongoose_data_forms:kv_map()) -> boolean().
form_decode_optimizations(#{<<"simple">> := [<<"true">>]}) ->
    true;
form_decode_optimizations(#{}) ->
    false.

is_mam_result_message(Packet = #xmlel{name = <<"message">>}) ->
    Ns = maybe_get_result_namespace(Packet),
    is_mam_namespace(Ns);
is_mam_result_message(_) ->
    false.

maybe_get_result_namespace(Packet) ->
    exml_query:path(Packet, [{element, <<"result">>}, {attr, <<"xmlns">>}], <<>>).

is_mam_namespace(NS) ->
    lists:member(NS, mam_features()).

features(Module, HostType) ->
    mam_features() ++ retraction_features(Module, HostType)
        ++ groupchat_features(Module, HostType).

mam_features() ->
    [?NS_MAM_04, ?NS_MAM_06, ?NS_MAM_EXTENDED].

retraction_features(Module, HostType) ->
    case has_message_retraction(Module, HostType) of
        true -> [?NS_RETRACT, ?NS_RETRACT_TOMBSTONE, ?NS_ESL_RETRACT];
        false -> [?NS_RETRACT]
    end.

groupchat_features(mod_mam_pm = Module, HostType) ->
    case gen_mod:get_module_opt(HostType, mod_mam, backend) of
        cassandra -> [];
        _ ->
            case gen_mod:get_module_opt(HostType, Module, archive_groupchats) of
                true -> [?NS_MAM_GC_FIELD, ?NS_MAM_GC_AVAILABLE];
                false -> [?NS_MAM_GC_FIELD]
            end
    end;
groupchat_features(_, _) ->
    [].

%% -----------------------------------------------------------------------
%% Forms

-spec message_form(Mod :: mod_mam_pm | mod_mam_muc,
                   HostType :: mongooseim:host_type(), binary()) ->
    exml:element().
message_form(Module, HostType, MamNs) ->
    Fields = message_form_fields(Module, HostType, MamNs),
    Form = mongoose_data_forms:form(#{ns => MamNs, fields => Fields}),
    result_query(Form, MamNs).

message_form_fields(Mod, HostType, <<"urn:xmpp:mam:1">>) ->
    TextSearch =
        case has_full_text_search(Mod, HostType) of
            true -> [#{type => <<"text-single">>,
                       var => <<"{https://erlang-solutions.com/}full-text-search">>}];
            false -> []
        end,
    [#{type => <<"jid-single">>, var => <<"with">>},
     #{type => <<"text-single">>, var => <<"start">>},
     #{type => <<"text-single">>, var => <<"end">>} | TextSearch];
message_form_fields(Mod, HostType, <<"urn:xmpp:mam:2">>) ->
    TextSearch =
        case has_full_text_search(Mod, HostType) of
            true -> [#{type => <<"text-single">>,
                       var => <<"{https://erlang-solutions.com/}full-text-search">>}];
            false -> []
        end,
    [#{type => <<"jid-single">>, var => <<"with">>},
     #{type => <<"text-single">>, var => <<"start">>},
     #{type => <<"text-single">>, var => <<"end">>},
     #{type => <<"text-single">>, var => <<"before-id">>},
     #{type => <<"text-single">>, var => <<"after-id">>},
     #{type => <<"boolean">>, var => <<"include-groupchat">>},
     #{type => <<"list-multi">>, var => <<"ids">>,
       validate => #{method => open, datatype => <<"xs:string">>}} | TextSearch].

-spec form_to_text(_) -> 'undefined' | binary().
form_to_text(#{<<"full-text-search">> := [Text]}) ->
    Text;
form_to_text(#{}) ->
    undefined.

%% -----------------------------------------------------------------------
%% Text search tokenization
%% -----------------------------------------------------------------------

%% -----------------------------------------------------------------------
%% @doc
%% Normalize given text to improve text search in some MAM backends.
%% This normalization involves making text all lowercase, replacing some word separators
%% ([, .:;-?!]) with given one (by default "%") and removing all unicode characters that are
%% considered non-alphanumerical.
%% For example, text: "My cat, was eaten by: my dog?!? Why...?!?" will be normalized as:
%% "my%cat%was%eaten%by%my%dog%why"
%% @end
%% -----------------------------------------------------------------------
-spec normalize_search_text(binary() | undefined) -> binary() | undefined.
normalize_search_text(Text) ->
    normalize_search_text(Text, <<"%">>).

-spec normalize_search_text(binary() | undefined, binary()) -> binary() | undefined.
normalize_search_text(undefined, _WordSeparator) ->
    undefined;
normalize_search_text(Text, WordSeparator) ->
    BodyString = unicode:characters_to_list(Text),
    LowerBody = string:lowercase(BodyString),
    ReOpts = [{return, list}, global, unicode, ucp],
    Re0 = re:replace(LowerBody, "[, .:;-?!]+", " ", ReOpts),
    Re1 = re:replace(Re0, "([^\\w ]+)|(^\\s+)|(\\s+$)", "", ReOpts),
    Re2 = re:replace(Re1, "\s+", unicode:characters_to_list(WordSeparator), ReOpts),
    unicode:characters_to_binary(Re2).

-spec packet_to_search_body(Enabled :: boolean(),
                            Packet :: exml:element()) -> binary().
packet_to_search_body(true, Packet) ->
    BodyValue = exml_query:path(Packet, [{element, <<"body">>}, cdata], <<>>),
    mod_mam_utils:normalize_search_text(BodyValue, <<" ">>);
packet_to_search_body(false, _Packet) ->
    <<>>.

-spec has_full_text_search(Module :: mod_mam_pm | mod_mam_muc,
                           HostType :: mongooseim:host_type()) -> boolean().
has_full_text_search(Module, HostType) ->
    gen_mod:get_module_opt(HostType, Module, full_text_search).

%% Message retraction

-spec has_message_retraction(Module :: mod_mam_pm | mod_mam_muc,
                             HostType :: mongooseim:host_type()) -> boolean().
has_message_retraction(Module, HostType) ->
    gen_mod:get_module_opt(HostType, Module, message_retraction).

%% -----------------------------------------------------------------------
%% JID serialization

-spec jid_to_opt_binary(UserJID :: jid:jid(), JID :: jid:jid()
                       ) -> jid:literal_jid().
jid_to_opt_binary(#jid{lserver = LServer},
                  #jid{lserver = LServer, luser = <<>>, lresource = <<>>}) ->
    <<$:>>;
jid_to_opt_binary(#jid{lserver = LServer, luser = LUser},
                  #jid{lserver = LServer, luser = LUser, lresource = <<>>}) ->
    <<>>;
jid_to_opt_binary(#jid{lserver = LServer, luser = LUser},
                  #jid{lserver = LServer, luser = LUser, lresource = LResource}) ->
    <<$/, LResource/binary>>;
jid_to_opt_binary(#jid{lserver = LServer},
                  #jid{lserver = LServer, luser = LUser, lresource = <<>>}) ->
    %% Both clients are on the same server.
    <<LUser/binary>>;
jid_to_opt_binary(#jid{lserver = LServer},
                  #jid{lserver = LServer, luser = <<>>, lresource = LResource}) ->
    %% Both clients are on the same server.
    <<$:, $/, LResource/binary>>;
jid_to_opt_binary(#jid{lserver = LServer},
                  #jid{lserver = LServer, luser = LUser, lresource = LResource}) ->
    %% Both clients are on the same server.
    <<LUser/binary, $/, LResource/binary>>;
jid_to_opt_binary(_,
                  #jid{lserver = LServer, luser = LUser, lresource = <<>>}) ->
    <<LServer/binary, $:, LUser/binary>>;
jid_to_opt_binary(_,
                  #jid{lserver = LServer, luser = LUser, lresource = LResource}) ->
    <<LServer/binary, $@, LUser/binary, $/, LResource/binary>>.


-spec expand_minified_jid(UserJID :: jid:jid(),
                          OptJID :: jid:literal_jid()) -> jid:literal_jid().
expand_minified_jid(#jid{lserver = LServer, luser = LUser}, <<>>) ->
    <<LUser/binary, $@, LServer/binary>>;
expand_minified_jid(#jid{lserver = LServer, luser = <<>>}, <<$/, LResource/binary>>) ->
    <<LServer/binary, $/, LResource/binary>>;
expand_minified_jid(#jid{lserver = LServer, luser = LUser}, <<$/, LResource/binary>>) ->
    <<LUser/binary, $@, LServer/binary, $/, LResource/binary>>;
expand_minified_jid(UserJID, Encoded) ->
    Part = binary:match(Encoded, [<<$@>>, <<$/>>, <<$:>>]),
    expand_minified_jid(Part, UserJID, Encoded).

-spec expand_minified_jid('nomatch' | {non_neg_integer(), 1}, jid:jid(),
                           Encoded :: jid:luser() | binary()) -> binary().
expand_minified_jid(nomatch, #jid{lserver = ThisServer}, LUser) ->
    <<LUser/binary, $@, ThisServer/binary>>;
expand_minified_jid({Pos, 1}, #jid{lserver = ThisServer}, Encoded) ->
    case Encoded of
        <<$:, $/, LResource/binary>> ->
            <<ThisServer/binary, $/, LResource/binary>>;
        <<$:>> ->
            ThisServer;
        <<LServer:Pos/binary, $:>> ->
            <<LServer/binary>>;
        <<LServer:Pos/binary, $:, LUser/binary>> ->
            <<LUser/binary, $@, LServer/binary>>;
        <<LServer:Pos/binary, $@, $/, LResource/binary>> ->
            <<LServer/binary, $/, LResource/binary>>;
        <<LServer:Pos/binary, $@, Tail/binary>> ->
            [LUser, LResource] = binary:split(Tail, <<$/>>),
            <<LUser/binary, $@, LServer/binary, $/, LResource/binary>>;
        <<LUser:Pos/binary, $/, LResource/binary>> ->
            <<LUser/binary, $@, ThisServer/binary, $/, LResource/binary>>
    end.

-ifdef(TEST).

jid_to_opt_binary_test_() ->
    check_stringprep(),
    UserJID = jid:from_binary(<<"alice@room">>),
    [?_assertEqual(JID,
        (expand_minified_jid(UserJID,
              jid_to_opt_binary(UserJID, jid:from_binary(JID)))))
     || JID <- test_jids()].

test_jids() ->
    [<<"alice@room">>,
     <<"alice@room/computer">>,
     <<"alice@street/mobile">>,
     <<"bob@room">>,
     <<"bob@room/mobile">>,
     <<"bob@street">>,
     <<"bob@street/mobile">>].

check_stringprep() ->
    is_loaded_application(jid) orelse start_stringprep().

start_stringprep() ->
    EJ = code:lib_dir(mongooseim),
    code:add_path(filename:join([EJ, "..", "..", "deps", "jid", "ebin"])),
    {ok, _} = application:ensure_all_started(jid).

is_loaded_application(AppName) when is_atom(AppName) ->
    lists:keymember(AppName, 1, application:loaded_applications()).

-endif.

%% -----------------------------------------------------------------------
%% Other
-spec bare_jid(undefined | jid:jid()) -> undefined | binary().
bare_jid(undefined) -> undefined;
bare_jid(JID) ->
    jid:to_bare_binary(jid:to_lower(JID)).

-spec full_jid(jid:jid()) -> binary().
full_jid(JID) ->
    jid:to_binary(jid:to_lower(JID)).

-spec maybe_integer(binary(), Default :: integer()) -> integer().
maybe_integer(<<>>, Def) -> Def;
maybe_integer(Bin, _Def) when is_binary(Bin) ->
    binary_to_integer(Bin).

-spec apply_start_border('undefined' | mod_mam:borders(), undefined | integer()) ->
                                undefined | integer().
apply_start_border(undefined, StartID) ->
    StartID;
apply_start_border(#mam_borders{after_id=AfterID, from_id=FromID}, StartID) ->
    maybe_max(maybe_next_id(AfterID), maybe_max(FromID, StartID)).


-spec apply_end_border('undefined' | mod_mam:borders(), undefined | integer()) ->
                              undefined | integer().
apply_end_border(undefined, EndID) ->
    EndID;
apply_end_border(#mam_borders{before_id=BeforeID, to_id=ToID}, EndID) ->
    maybe_min(maybe_previous_id(BeforeID), maybe_min(ToID, EndID)).

-spec calculate_msg_id_borders(mod_mam:borders() | undefined,
                               mod_mam:unix_timestamp() | undefined,
                               mod_mam:unix_timestamp() | undefined) -> R when
      R :: {integer() | undefined, integer() | undefined}.
calculate_msg_id_borders(Borders, Start, End) ->
    StartID = maybe_encode_compact_uuid(Start, 0),
    EndID = maybe_encode_compact_uuid(End, 255),
    {apply_start_border(Borders, StartID),
     apply_end_border(Borders, EndID)}.

-spec calculate_msg_id_borders(RSM, Borders, Start, End) -> R when
      RSM :: jlib:rsm_in() | undefined,
      Borders :: mod_mam:borders() | undefined,
      Start :: mod_mam:unix_timestamp() | undefined,
      End :: mod_mam:unix_timestamp() | undefined,
      R :: {integer() | undefined, integer() | undefined}.
calculate_msg_id_borders(undefined, Borders, Start, End) ->
    calculate_msg_id_borders(Borders, Start, End);
calculate_msg_id_borders(#rsm_in{id = undefined}, Borders, Start, End) ->
    calculate_msg_id_borders(Borders, Start, End);
calculate_msg_id_borders(#rsm_in{direction = aft, id = Id}, Borders, Start, End)
  when Id =/= undefined ->
    {StartId, EndId} = mod_mam_utils:calculate_msg_id_borders(Borders, Start, End),
    {mod_mam_utils:maybe_max(StartId, Id), EndId};
calculate_msg_id_borders(#rsm_in{direction = before, id = Id}, Borders, Start, End)
  when Id =/= undefined ->
    {StartId, EndId} = mod_mam_utils:calculate_msg_id_borders(Borders, Start, End),
    {StartId, mod_mam_utils:maybe_min(EndId, Id)}.

-spec maybe_encode_compact_uuid(mod_mam:unix_timestamp() | undefined, integer()) ->
    undefined | integer().
maybe_encode_compact_uuid(undefined, _) ->
    undefined;
maybe_encode_compact_uuid(Microseconds, NodeID) ->
    mod_mam_utils:encode_compact_uuid(Microseconds, NodeID).


-spec maybe_min('undefined' | integer(), undefined | integer()) -> integer().
maybe_min(undefined, Y) ->
    Y;
maybe_min(X, undefined) ->
    X;
maybe_min(X, Y) ->
    min(X, Y).


-spec maybe_max('undefined' | integer(), undefined | integer()) -> integer().
maybe_max(undefined, Y) ->
    Y;
maybe_max(X, undefined) ->
    X;
maybe_max(X, Y) ->
    max(X, Y).

-spec maybe_last([T]) -> undefined | {ok, T}.
maybe_last([]) -> undefined;
maybe_last([_|_] = L) -> {ok, lists:last(L)}.

-spec maybe_next_id('undefined' | non_neg_integer()) -> 'undefined' | pos_integer().
maybe_next_id(undefined) ->
    undefined;
maybe_next_id(X) ->
    X + 1.

-spec maybe_previous_id('undefined' | non_neg_integer()) -> 'undefined' | integer().
maybe_previous_id(undefined) ->
    undefined;
maybe_previous_id(X) ->
    X - 1.


%% @doc Returns true, if the current page is the final one in the result set.
%% If there are more pages with messages, than this function returns false.
%%
%% PageSize - maximum number of messages extracted in one lookup.
%% TotalCount - total number of messages in the Result Set.
%% Result Set - is subset of all messages in user's archive,
%% in a specified time period.
%% MessageRows - stuff we are about to send to the user.
%% Params - lookup parameters, coming from mam_iq module.
%%
%% TotalCount and Offset can be undefined, in case we use IsSimple=true.
%% IsSimple=true tells the server not to do heavy `SELECT COUNT(*)' queries.
%%
%% TODO It is possible to set complete flag WITH IsSimple=true,
%%      if we select one extra message from archive, but don't send it to the client.
%%      It's the most efficient way to query archive, if the client side does
%%      not care about the total number of messages and if it's stateless
%%      (i.e. web interface).
%% Handles case when we have TotalCount and Offset as integers
-spec is_complete_result_page_using_offset(Params, Result) ->
    boolean() when
    Params      :: mam_iq:lookup_params(),
    Result      :: mod_mam:lookup_result_map().
is_complete_result_page_using_offset(#{page_size := PageSize} = Params,
                                     #{total_count := TotalCount, offset := Offset,
                                       messages := MessageRows})
    when is_integer(TotalCount), is_integer(Offset) ->
    case maps:get(ordering_direction, Params, forward) of
        forward ->
            is_most_recent_page(PageSize, TotalCount, Offset, MessageRows);
        backward ->
            Offset =:= 0
   end.

%% @doc Returns true, if the current page contains the most recent messages.
%% If there are some more recent messages in archive, this function returns false.
-spec is_most_recent_page(PageSize, TotalCount, Offset, MessageRows) -> boolean() when
    PageSize    :: non_neg_integer(),
    TotalCount  :: non_neg_integer()|undefined,
    Offset      :: non_neg_integer()|undefined,
    MessageRows :: list().
is_most_recent_page(PageSize, _TotalCount, _Offset, MessageRows)
    when length(MessageRows) < PageSize ->
    true;
is_most_recent_page(PageSize, TotalCount, Offset, MessageRows)
  when is_integer(TotalCount), is_integer(Offset),
       length(MessageRows) =:= PageSize ->
    %% Number of messages on skipped pages from the beginning plus the current page
    PagedCount = Offset + PageSize,
    TotalCount =:= PagedCount; %% false means full page but not the last one in the result set
is_most_recent_page(_PageSize, _TotalCount, _Offset, _MessageRows) ->
    %% When is_integer(TotalCount), is_integer(Offset)
    %%     it's not possible case: the page is bigger than page size.
    %% Otherwise either TotalCount or Offset is undefined because of optimizations.
    false.

-spec maybe_set_client_xmlns(boolean(), exml:element()) -> exml:element().
maybe_set_client_xmlns(true, Packet) ->
    jlib:replace_tag_attr(<<"xmlns">>, <<"jabber:client">>, Packet);
maybe_set_client_xmlns(false, Packet) ->
    Packet.

-spec action_to_shaper_name(mam_iq:action()) -> atom().
action_to_shaper_name(Action) ->
    list_to_atom(atom_to_list(Action) ++ "_shaper").

-spec action_to_global_shaper_name(mam_iq:action()) -> atom().
action_to_global_shaper_name(Action) ->
    list_to_atom(atom_to_list(Action) ++ "_global_shaper").

-spec wait_shaper(mongooseim:host_type(), jid:server(), mam_iq:action(), jid:jid()) ->
    continue | {error, max_delay_reached}.
wait_shaper(HostType, Host, Action, From) ->
    case mongoose_shaper:wait(
           HostType, Host, action_to_shaper_name(Action), From, 1) of
        continue ->
            mongoose_shaper:wait(
              global, Host, action_to_global_shaper_name(Action), From, 1);
        {error, max_delay_reached} ->
            {error, max_delay_reached}
    end.

%% -----------------------------------------------------------------------
%% Ejabberd

-spec send_message(mod_mam:message_row(), jid:jid(), jid:jid(), exml:element()) -> mongoose_acc:t().
send_message(_Row, From, To, Mess) ->
    ejabberd_sm:route(From, To, Mess).

-spec is_jid_in_user_roster(mongooseim:host_type(), jid:jid(), jid:jid()) -> boolean().
is_jid_in_user_roster(HostType, #jid{} = ToJID, #jid{} = RemJID) ->
    RemBareJID = jid:to_bare(RemJID),
    {Subscription, _G} = mongoose_hooks:roster_get_jid_info(HostType, ToJID, RemBareJID),
    Subscription == from orelse Subscription == both.

%% @doc Returns a UUIDv4 canonical form binary.
-spec wrapper_id() -> binary().
wrapper_id() ->
    uuid:uuid_to_string(uuid:get_v4(), binary_standard).


-spec check_result_for_policy_violation(Params, Result) -> Result when
        Params :: mam_iq:lookup_params(),
        Result :: {ok, mod_mam:lookup_result()}
                | {error, 'policy-violation'}
                | {error, Reason :: term()}.
check_result_for_policy_violation(
         _Params = #{limit_passed := LimitPassed,
                     max_result_limit := MaxResultLimit},
         Result = {ok, {TotalCount, Offset, _MessageRows}})
        when is_integer(TotalCount), is_integer(Offset) ->
    case is_policy_violation(TotalCount, Offset, MaxResultLimit, LimitPassed) of
        true ->
            {error, 'policy-violation'};
        false ->
            Result
    end;
check_result_for_policy_violation(_Params, Result) ->
    Result.

is_policy_violation(TotalCount, Offset, MaxResultLimit, LimitPassed) ->
    TotalCount - Offset > MaxResultLimit andalso not LimitPassed.

%% @doc Check for XEP-313 `item-not-found' error condition,
%% that is if a message ID passed in a `before'/`after' query is actually present in the archive.
%% See https://xmpp.org/extensions/xep-0313.html#query-paging for details.
%%
%% In a backend it's reasonable to query for PageSize + 1 messages,
%% so that once the interval endpoint with requested ID is discarded we actually
%% return (up to) PageSize messages.
%% @end
-spec check_for_item_not_found(RSM, PageSize, LookupResult) -> R when
      RSM :: jlib:rsm_in() | undefined,
      PageSize :: non_neg_integer(),
      LookupResult :: mod_mam:lookup_result(),
      R :: {ok, mod_mam:lookup_result()} | {error, item_not_found}.
check_for_item_not_found(#rsm_in{direction = before, id = ID},
                         _PageSize, {TotalCount, Offset, MessageRows}) ->
    case maybe_last(MessageRows) of
        {ok, #{id := ID}} ->
            {ok, {TotalCount, Offset, lists:droplast(MessageRows)}};
        undefined ->
            {error, item_not_found}
    end;
check_for_item_not_found(#rsm_in{direction = aft, id = ID},
                         _PageSize, {TotalCount, Offset, MessageRows0}) ->
    case MessageRows0 of
        [#{id := ID} | MessageRows] ->
            {ok, {TotalCount, Offset, MessageRows}};
        _ ->
            {error, item_not_found}
    end.

-spec lookup(HostType :: mongooseim:host_type(),
             Params :: mam_iq:lookup_params(),
             F :: fun()) ->
    {ok, mod_mam:lookup_result_map()} | {error, Reason :: term()}.
lookup(HostType, Params, F) ->
    F1 = patch_fun_to_make_result_as_map(F),
    process_lookup_with_complete_check(HostType, Params, F1).

process_lookup_with_complete_check(HostType, Params = #{is_simple := true}, F) ->
    process_simple_lookup_with_complete_check(HostType, Params, F);
process_lookup_with_complete_check(HostType, Params, F) ->
    case F(HostType, Params) of
        {ok, Result} ->
            IsComplete = is_complete_result_page_using_offset(Params, Result),
            {ok, Result#{is_complete => IsComplete}};
        Other ->
            Other
    end.

-spec lookup_first_and_last_messages(mongooseim:host_type(), mod_mam:archive_id(),
                                     jid:jid(), fun()) ->
    {mod_mam:message_row(), mod_mam:message_row()} | {error, term()} | empty_archive.
lookup_first_and_last_messages(HostType, ArcID, ArcJID, F) ->
    lookup_first_and_last_messages(HostType, ArcID, ArcJID, ArcJID, F).

-spec lookup_first_and_last_messages(mongooseim:host_type(), mod_mam:archive_id(), jid:jid(),
                                     jid:jid(), fun()) ->
    {mod_mam:message_row(), mod_mam:message_row()} | {error, term()} | empty_archive.
lookup_first_and_last_messages(HostType, ArcID, CallerJID, OwnerJID, F) ->
    FirstMsgParams = create_lookup_params(undefined, forward, ArcID, CallerJID, OwnerJID),
    LastMsgParams = create_lookup_params(#rsm_in{direction = before},
                                         backward, ArcID, CallerJID, OwnerJID),
    case lookup(HostType, FirstMsgParams, F) of
        {ok, #{messages := [FirstMsg]}} ->
            case lookup(HostType, LastMsgParams, F) of
                {ok, #{messages := [LastMsg]}} -> {FirstMsg, LastMsg};
                ErrorLast -> ErrorLast
            end;
        {ok, #{messages := []}} -> empty_archive;
        ErrorFirst -> ErrorFirst
    end.

-spec create_lookup_params(jlib:rsm_in() | undefined,
                    backward | forward,
                    mod_mam:archive_id(),
                    jid:jid(),
                    jid:jid()) -> mam_iq:lookup_params().
create_lookup_params(RSM, Direction, ArcID, CallerJID, OwnerJID) ->
    #{now => erlang:system_time(microsecond),
      is_simple => true,
      rsm => RSM,
      max_result_limit => 1,
      archive_id => ArcID,
      owner_jid => OwnerJID,
      search_text => undefined,
      with_jid => undefined,
      start_ts => undefined,
      page_size => 1,
      end_ts => undefined,
      borders => undefined,
      flip_page => false,
      ordering_direction => Direction,
      limit_passed => true,
      caller_jid => CallerJID,
      message_ids => undefined}.

patch_fun_to_make_result_as_map(F) ->
    fun(HostType, Params) -> result_to_map(F(HostType, Params)) end.

result_to_map({ok, {TotalCount, Offset, MessageRows}}) ->
    {ok, #{total_count => TotalCount, offset => Offset, messages => MessageRows}};
result_to_map(Other) ->
    Other.

%% We query an extra message by changing page_size.
%% After that we remove this message from the result set when returning.
process_simple_lookup_with_complete_check(HostType, Params = #{page_size := PageSize}, F) ->
    Params2 = Params#{page_size => PageSize + 1},
    case F(HostType, Params2) of
        {ok, Result} ->
            {ok, set_complete_result_page_using_extra_message(PageSize, Params, Result)};
        Other ->
            Other
    end.

set_complete_result_page_using_extra_message(PageSize, Params, Result = #{messages := MessageRows}) ->
    case length(MessageRows) =:= (PageSize + 1) of
        true ->
            Result#{is_complete => false, messages => remove_extra_message(Params, MessageRows)};
        false ->
            Result#{is_complete => true}
    end.

remove_extra_message(Params, Messages) ->
    case maps:get(ordering_direction, Params, forward) of
        forward ->
            lists:droplast(Messages);
        backward ->
            tl(Messages)
    end.

-spec db_jid_codec(mongooseim:host_type(), module()) -> module().
db_jid_codec(HostType, Module) ->
    gen_mod:get_module_opt(HostType, Module, db_jid_format).

-spec db_message_codec(mongooseim:host_type(), module()) -> module().
db_message_codec(HostType, Module) ->
    gen_mod:get_module_opt(HostType, Module, db_message_format).

-spec incremental_delete_domain(
        mongooseim:host_type(), jid:lserver(), non_neg_integer(), [atom()], non_neg_integer()) ->
    non_neg_integer().
incremental_delete_domain(_HostType, _Domain, _Limit, [], TotalDeleted) ->
    TotalDeleted;
incremental_delete_domain(HostType, Domain, Limit, [Query | MoreQueries] = AllQueries, TotalDeleted) ->
    R1 = mongoose_rdbms:execute_successfully(HostType, Query, [Domain]),
    case is_removing_done(R1, Limit) of
        {done, N} ->
            incremental_delete_domain(HostType, Domain, Limit, MoreQueries, N + TotalDeleted);
        {remove_more, N} ->
            incremental_delete_domain(HostType, Domain, Limit, AllQueries, N + TotalDeleted)
    end.

-spec is_removing_done(LastResult :: {updated, non_neg_integer()}, Limit :: non_neg_integer()) ->
    {done | remove_more, non_neg_integer()}.
is_removing_done({updated, N}, Limit) when N < Limit ->
    {done, N};
is_removing_done({updated, N}, _)->
    {remove_more, N}.

-spec is_mam_muc_enabled(jid:lserver(), mongooseim:host_type()) -> boolean().
is_mam_muc_enabled(MucDomain, HostType) ->
    HostPattern = mongoose_config:get_opt([{modules, HostType}, mod_mam_muc, host]),
    {ok, #{subdomain_pattern := SubDomainPattern}} =
        mongoose_domain_api:get_subdomain_info(MucDomain),
    HostPattern =:= SubDomainPattern.
