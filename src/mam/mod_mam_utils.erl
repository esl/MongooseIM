%%%-------------------------------------------------------------------
%%% @author Uvarov Michael <arcusfelis@gmail.com>
%%% @copyright (C) 2013, Uvarov Michael
%%% @doc General functions for MAM.
%%% @end
%%%-------------------------------------------------------------------
-module(mod_mam_utils).
%% Time
-export([maybe_microseconds/1,
         now_to_microseconds/1,
         microseconds_to_now/1,
         datetime_to_microseconds/1,
         microseconds_to_datetime/1]).

%% UID
-export([generate_message_id/0,
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
         wrap_message/6,
         wrap_message/7,
         result_set/4,
         result_query/2,
         result_prefs/4,
         make_fin_element/4,
         parse_prefs/1,
         borders_decode/1,
         decode_optimizations/1,
         form_borders_decode/1,
         form_decode_optimizations/1,
         is_mam_result_message/1]).

%% Forms
-export([
    form_field_value_s/2,
    form_field_value/2,
    message_form/3,
    form_to_text/1
]).

%% Text search
-export([
    normalize_search_text/1,
    normalize_search_text/2,
    packet_to_search_body/3,
    has_full_text_search/2
]).

%% JID serialization
-export([jid_to_opt_binary/2,
         expand_minified_jid/2]).

%% SQL
-export([success_sql_query/2, success_sql_execute/3]).

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
         is_complete_result_page/4,
         wait_shaper/3,
         check_for_item_not_found/3]).

%% Ejabberd
-export([send_message/3,
         maybe_set_client_xmlns/2,
         is_jid_in_user_roster/2]).

%% Shared logic
-export([check_result_for_policy_violation/2]).

%-define(MAM_INLINE_UTILS, true).

-ifdef(MAM_INLINE_UTILS).
-compile({inline, [
                   rsm_ns_binary/0,
                   mam_ns_binary/0,
                   now_to_microseconds/1,
                   iso8601_datetime_binary_to_timestamp/1,
                   is_archived_elem_for/2,
                   is_valid_message/3,
                   is_valid_message_type/3,
                   is_valid_message_children/3,
                   encode_compact_uuid/2,
                   get_one_of_path/3,
                   delay/2,
                   forwarded/3,
                   result/4,
                   valid_behavior/1]}).
-endif.

-include("mongoose.hrl").
-include("jlib.hrl").
-include_lib("exml/include/exml.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("mod_mam.hrl").
-include("mongoose_rsm.hrl").

-define(MAYBE_BIN(X), (is_binary(X) orelse (X) =:= undefined)).

%% Constants
rsm_ns_binary() -> <<"http://jabber.org/protocol/rsm">>.


%% ----------------------------------------------------------------------
%% Datetime types
-type ne_binary() :: <<_:8, _:_*8>>.
-type iso8601_datetime_binary() :: ne_binary().
%% Microseconds from 01.01.1970
-type unix_timestamp() :: mod_mam:unix_timestamp().

-type archive_behaviour() :: mod_mam:archive_behaviour().
-type archive_behaviour_bin() :: binary(). % `<<"roster">> | <<"always">> | <<"never">>'.


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
    case iso8601_datetime_binary_to_timestamp(ISODateTime) of
        undefined -> undefined;
        Stamp -> now_to_microseconds(Stamp)
    end.


-spec now_to_microseconds(erlang:timestamp()) -> unix_timestamp().
now_to_microseconds({Mega, Secs, Micro}) ->
    (1000000 * Mega + Secs) * 1000000 + Micro.


-spec microseconds_to_now(unix_timestamp()) -> erlang:timestamp().
microseconds_to_now(MicroSeconds) when is_integer(MicroSeconds) ->
    Seconds = MicroSeconds div 1000000,
    {Seconds div 1000000, Seconds rem 1000000, MicroSeconds rem 1000000}.


%% @doc Returns time in `timestamp()' format.
-spec iso8601_datetime_binary_to_timestamp(iso8601_datetime_binary())
        -> erlang:timestamp() | undefined.
iso8601_datetime_binary_to_timestamp(DateTime) when is_binary(DateTime) ->
    jlib:datetime_binary_to_timestamp(DateTime).


-spec datetime_to_microseconds(calendar:datetime()) -> integer().
datetime_to_microseconds({{_, _, _}, {_, _, _}} = DateTime) ->
    S1 = calendar:datetime_to_gregorian_seconds(DateTime),
    S0 = calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}),
    Seconds = S1 - S0,
    Seconds * 1000000.


-spec microseconds_to_datetime(non_neg_integer()) -> calendar:datetime().
microseconds_to_datetime(MicroSeconds) when is_integer(MicroSeconds) ->
    calendar:now_to_datetime(mod_mam_utils:microseconds_to_now(MicroSeconds)).

%% -----------------------------------------------------------------------
%% UID

-spec generate_message_id() -> integer().
generate_message_id() ->
    {ok, NodeId} = ejabberd_node_id:node_id(),
    CandidateStamp = erlang:system_time(microsecond),
    UniqueStamp = mongoose_mam_id:next_unique(CandidateStamp),
    encode_compact_uuid(UniqueStamp, NodeId).


%% @doc Create a message ID (UID).
%%
%% It removes a leading 0 from 64-bit binary representation.
%% It puts node id as a last byte.
%% The maximum date, that can be encoded is `{{4253, 5, 31}, {22, 20, 37}}'.
-spec encode_compact_uuid(integer(), integer()) -> integer().
encode_compact_uuid(Microseconds, NodeId)
    when is_integer(Microseconds), is_integer(NodeId) ->
    (Microseconds bsl 8) + NodeId.


%% @doc Extract date and node id from a message id.
-spec decode_compact_uuid(integer()) -> {integer(), byte()}.
decode_compact_uuid(Id) ->
    Microseconds = Id bsr 8,
    NodeId = Id band 255,
    {Microseconds, NodeId}.


%% @doc Encode a message ID to pass it to the user.
-spec mess_id_to_external_binary(integer()) -> binary().
mess_id_to_external_binary(MessID) when is_integer(MessID) ->
    list_to_binary(integer_to_list(MessID, 32)).


-spec maybe_external_binary_to_mess_id(binary()) -> undefined | integer().
maybe_external_binary_to_mess_id(<<>>) ->
    undefined;
maybe_external_binary_to_mess_id(BExtMessID) ->
    external_binary_to_mess_id(BExtMessID).


%% @doc Decode a message ID received from the user.
-spec external_binary_to_mess_id(binary()) -> integer().
external_binary_to_mess_id(BExtMessID) when is_binary(BExtMessID) ->
    binary_to_integer(BExtMessID, 32).

%% -----------------------------------------------------------------------
%% XML

-spec maybe_add_arcid_elems(To :: jid:simple_jid()  | jid:jid(),
                            MessID :: binary(), Packet :: exml:element(),
                            AddStanzaid :: boolean()) ->
          AlteredPacket :: exml:element().
maybe_add_arcid_elems(To, MessID, Packet, AddStanzaid) ->
    BareTo = jid:to_binary(jid:to_bare(To)),
    case AddStanzaid of
        true ->
            replace_arcid_elem(<<"stanza-id">>, BareTo, MessID, Packet);
        _ -> Packet
    end.

maybe_log_deprecation(_IQ) ->
    ok. %% May be reused for future MAM versions.

%% @doc Return true, if the first element points on `By'.
-spec is_arcid_elem_for(ElemName :: binary(), exml:element(), By :: binary()) -> boolean().
is_arcid_elem_for(<<"archived">>, #xmlel{name = <<"archived">>, attrs=As}, By) ->
    lists:member({<<"by">>, By}, As);
is_arcid_elem_for(<<"stanza-id">>, #xmlel{name = <<"stanza-id">>, attrs=As}, By) ->
    lists:member({<<"by">>, By}, As) andalso
    lists:member({<<"xmlns">>, ?NS_STANZAID}, As);
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
                  attrs=[{<<"by">>, By}, {<<"id">>, Id}, {<<"xmlns">>, ?NS_STANZAID}]},
    xml:append_subtags(Packet, [Archived]);
append_arcid_elem(ElemName, By, Id, Packet) ->
    Archived = #xmlel{
                  name = ElemName,
                  attrs=[{<<"by">>, By}, {<<"id">>, Id}]},
    xml:append_subtags(Packet, [Archived]).

-spec delete_arcid_elem(ElemName :: binary(), By :: binary(), exml:element()) -> exml:element().
delete_arcid_elem(ElemName, By, Packet=#xmlel{children=Cs}) ->
    Packet#xmlel{children=[C || C <- Cs, not is_arcid_elem_for(ElemName, C, By)]}.


is_x_user_element(#xmlel{name = <<"x">>, attrs = As}) ->
    lists:member({<<"xmlns">>, ?NS_MUC_USER}, As);
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
        attrs = [{<<"xmlns">>, ?NS_MUC_USER}],
        children = [ItemElem]},
    xml:append_subtags(Packet, [X]).

x_user_item(FromJID, Role, Affiliation) ->
    #xmlel{
       name = <<"item">>,
       attrs = [{<<"affiliation">>, atom_to_binary(Affiliation, latin1)},
                {<<"jid">>, jid:to_binary(FromJID)},
                {<<"role">>, atom_to_binary(Role, latin1)}]}.

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
-spec is_archivable_message(Mod :: module(), Dir :: incoming | outgoing,
                            Packet :: exml:element(), boolean()) -> boolean().
is_archivable_message(Mod, Dir, Packet=#xmlel{name = <<"message">>}, ArchiveChatMarkers) ->
    Type = exml_query:attr(Packet, <<"type">>, <<"normal">>),
    is_valid_message_type(Mod, Dir, Type) andalso
        is_valid_message(Mod, Dir, Packet, ArchiveChatMarkers);
is_archivable_message(_, _, _, _) ->
    false.

is_valid_message_type(_, _, <<"normal">>) -> true;
is_valid_message_type(_, _, <<"chat">>) -> true;
is_valid_message_type(_, incoming, <<"groupchat">>) -> true;
is_valid_message_type(_, _, <<"error">>) -> false;
is_valid_message_type(_, _, _) -> false.

is_valid_message(_Mod, _Dir, Packet, ArchiveChatMarkers) ->
    Body       = exml_query:subelement(Packet, <<"body">>, false),
    ChatMarker = ArchiveChatMarkers
                 andalso has_chat_marker(Packet),
    %% Used in MAM
    Result     = exml_query:subelement(Packet, <<"result">>, false),
    %% Used in mod_offline
    Delay      = exml_query:subelement(Packet, <<"delay">>, false),
    %% Message Processing Hints (XEP-0334)
    NoStore    = exml_query:subelement(Packet, <<"no-store">>, false),
    is_valid_message_children(Body, ChatMarker, Result, Delay, NoStore).

%% Forwarded by MAM message, or just a message without body or chat marker
is_valid_message_children(false, false, _,     _,     _    ) -> false;
is_valid_message_children(_,     _,     false, false, false) -> true;
%% Forwarded by MAM message or delivered by mod_offline
%% See mam_SUITE:offline_message for a test case
is_valid_message_children(_,     _,     _,    _,     _    ) -> false.

has_chat_marker(Packet) ->
    case exml_query:subelement_with_ns(Packet, ?NS_CHAT_MARKERS) of
        #xmlel{name = <<"received">>}     -> true;
        #xmlel{name = <<"displayed">>}    -> true;
        #xmlel{name = <<"acknowledged">>} -> true;
        _                                 -> false
    end.

%% @doc Forms `<forwarded/>' element, according to the XEP.
-spec wrap_message(MamNs :: binary(), Packet :: exml:element(), QueryID :: binary(),
                   MessageUID :: term(), DateTime :: calendar:datetime(),
                   SrcJID :: jid:jid()) -> Wrapper :: exml:element().
wrap_message(MamNs, Packet, QueryID, MessageUID, DateTime, SrcJID) ->
    wrap_message(MamNs, Packet, QueryID, MessageUID, wrapper_id(), DateTime, SrcJID).

-spec wrap_message(MamNs :: binary(), Packet :: exml:element(), QueryID :: binary(),
                   MessageUID :: term(), WrapperI :: binary(), DateTime :: calendar:datetime(),
                   SrcJID :: jid:jid()) -> Wrapper :: exml:element().
wrap_message(MamNs, Packet, QueryID, MessageUID, WrapperID, DateTime, SrcJID) ->
    #xmlel{ name = <<"message">>,
            attrs = [{<<"id">>, WrapperID}],
            children = [result(MamNs, QueryID, MessageUID,
                               [forwarded(Packet, DateTime, SrcJID)])] }.

-spec forwarded(exml:element(), calendar:datetime(), jid:jid())
               -> exml:element().
forwarded(Packet, DateTime, SrcJID) ->
    #xmlel{
       name = <<"forwarded">>,
       attrs = [{<<"xmlns">>, ?NS_FORWARD}],
       %% Two places to include SrcJID:
       %% - delay.from - optional XEP-0297 (TODO: depricate adding it?)
       %% - message.from - required XEP-0313
       %% Also, mod_mam_muc will replace it again with SrcJID
       children = [delay(DateTime, SrcJID), replace_from_attribute(SrcJID, Packet)]}.

-spec delay(calendar:datetime(), jid:jid()) -> exml:element().
delay(DateTime, SrcJID) ->
    jlib:timestamp_to_xml(DateTime, utc, SrcJID, <<>>).

replace_from_attribute(From, Packet=#xmlel{attrs = Attrs}) ->
    Attrs1 = lists:keydelete(<<"from">>, 1, Attrs),
    Attrs2 = [{<<"from">>, jid:to_binary(From)} | Attrs1],
    Packet#xmlel{attrs = Attrs2}.

%% @doc Generates tag `<result />'.
%% This element will be added in each forwarded message.
-spec result(binary(), _, MessageUID :: binary(), Children :: [exml:element(), ...])
            -> exml:element().
result(MamNs, QueryID, MessageUID, Children) when is_list(Children) ->
    %% <result xmlns='urn:xmpp:mam:tmp' queryid='f27' id='28482-98726-73623' />
    #xmlel{
       name = <<"result">>,
       attrs = [{<<"queryid">>, QueryID} || QueryID =/= undefined, QueryID =/= <<>>] ++
           [{<<"xmlns">>, MamNs},
            {<<"id">>, MessageUID}],
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
       attrs = [{<<"xmlns">>, rsm_ns_binary()}],
       children = FirstEl ++ LastEl};
result_set(FirstId, LastId, FirstIndexI, CountI)
  when ?MAYBE_BIN(FirstId), ?MAYBE_BIN(LastId) ->
    FirstEl = [#xmlel{name = <<"first">>,
                      attrs = [{<<"index">>, integer_to_binary(FirstIndexI)}],
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
       attrs = [{<<"xmlns">>, rsm_ns_binary()}],
       children = FirstEl ++ LastEl ++ [CountEl]}.


-spec result_query(jlib:xmlcdata() | exml:element(), binary()) -> exml:element().
result_query(SetEl, Namespace) ->
    #xmlel{
       name = <<"query">>,
       attrs = [{<<"xmlns">>, Namespace}],
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
       attrs = [{<<"xmlns">>, Namespace},
                {<<"default">>, atom_to_binary(DefaultMode, utf8)}],
       children = [AlwaysEl, NeverEl]
      }.


-spec encode_jids([binary() | string()]) -> [exml:element()].
encode_jids(JIDs) ->
    [#xmlel{name = <<"jid">>, children = [#xmlcdata{content = JID}]}
     || JID <- JIDs].


%% MAM v0.4.1 and above
-spec make_fin_element(binary(), boolean(), boolean(), exml:element()) -> exml:element().
make_fin_element(MamNs, IsComplete, IsStable, ResultSetEl) ->
    #xmlel{
       name = <<"fin">>,
       attrs = [{<<"xmlns">>, MamNs}]
        ++ [{<<"complete">>, <<"true">>} || IsComplete]
        ++ [{<<"stable">>, <<"false">>} || not IsStable],
       children = [ResultSetEl]}.


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

-spec borders_decode(exml:element()) -> 'undefined' | mod_mam:borders().
borders_decode(QueryEl) ->
    AfterID  = tag_id(QueryEl, <<"after_id">>),
    BeforeID = tag_id(QueryEl, <<"before_id">>),
    FromID   = tag_id(QueryEl, <<"from_id">>),
    ToID     = tag_id(QueryEl, <<"to_id">>),
    borders(AfterID, BeforeID, FromID, ToID).

-spec form_borders_decode(exml:element()) -> 'undefined' | mod_mam:borders().
form_borders_decode(QueryEl) ->
    AfterID  = form_field_mess_id(QueryEl, <<"after_id">>),
    BeforeID = form_field_mess_id(QueryEl, <<"before_id">>),
    FromID   = form_field_mess_id(QueryEl, <<"from_id">>),
    ToID     = form_field_mess_id(QueryEl, <<"to_id">>),
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


-spec tag_id(exml:element(), binary()) -> 'undefined' | integer().
tag_id(QueryEl, Name) ->
    BExtMessID = exml_query:attr(QueryEl, Name, <<>>),
    maybe_external_binary_to_mess_id(BExtMessID).

-spec form_field_mess_id(exml:element(), binary()) -> 'undefined' | integer().
form_field_mess_id(QueryEl, Name) ->
    BExtMessID = form_field_value_s(QueryEl, Name),
    maybe_external_binary_to_mess_id(BExtMessID).

-spec decode_optimizations(exml:element()) -> 'false' | 'opt_count' | 'true'.
decode_optimizations(QueryEl) ->
    case {exml_query:subelement(QueryEl, <<"simple">>),
          exml_query:subelement(QueryEl, <<"opt_count">>)} of
        {undefined, undefined} -> false;
        {undefined, _}     -> opt_count;
        _              -> true
    end.

-spec form_decode_optimizations(exml:element()) -> false | opt_count | true.
form_decode_optimizations(QueryEl) ->
    case {form_field_value(QueryEl, <<"simple">>),
          form_field_value(QueryEl, <<"opt_count">>)} of
        {_, <<"true">>}     -> opt_count;
        {<<"true">>, _}     -> true;
        {_, _}              -> false
    end.


is_mam_result_message(Packet = #xmlel{name = <<"message">>}) ->
    Ns = maybe_get_result_namespace(Packet),
    is_mam_namespace(Ns);
is_mam_result_message(_) ->
    false.

maybe_get_result_namespace(Packet) ->
    exml_query:path(Packet, [{element, <<"result">>}, {attr, <<"xmlns">>}], <<>>).

is_mam_namespace(?NS_MAM_04) -> true;
is_mam_namespace(?NS_MAM_06) -> true;
is_mam_namespace(_)          -> false.


%% -----------------------------------------------------------------------
%% Forms

-spec form_field_value(exml:element(), binary()) -> undefined | binary().
form_field_value(QueryEl, Name) ->
    case exml_query:subelement(QueryEl, <<"x">>) of
        undefined ->
            undefined;
        #xmlel{children = Fields} -> %% <x xmlns='jabber:x:data'/>
            case find_field(Fields, Name) of
                undefined ->
                    undefined;
                Field ->
                    field_to_value(Field)
            end
    end.

form_field_value_s(QueryEl, Name) ->
    undefined_to_empty(form_field_value(QueryEl, Name)).

undefined_to_empty(undefined) -> <<>>;
undefined_to_empty(X)         -> X.

%% @doc Return first matched field
-spec find_field(list(exml:element()), binary()) -> undefined | exml:element().
find_field([#xmlel{ name = <<"field">> } = Field | Fields], Name) ->
    case exml_query:attr(Field, <<"var">>) of
        Name -> Field;
        _ -> find_field(Fields, Name)
    end;
find_field([_|Fields], Name) -> %% skip whitespaces
    find_field(Fields, Name);
find_field([], _Name) ->
    undefined.

-spec field_to_value(exml:element()) -> binary().
field_to_value(FieldEl) ->
    exml_query:path(FieldEl, [{element, <<"value">>}, cdata], <<>>).

-spec message_form(Mod :: mod_mam | mod_mam_muc, Host :: jid:lserver(), binary()) ->
    exml:element().
message_form(Module, Host, MamNs) ->
    SubEl = #xmlel{name = <<"x">>,
                   attrs = [{<<"xmlns">>, <<"jabber:x:data">>},
                            {<<"type">>, <<"form">>}],
                   children = message_form_fields(Module, Host, MamNs)},
    result_query(SubEl, MamNs).

message_form_fields(Mod, Host, MamNs) ->
    TextSearch =
        case has_full_text_search(Mod, Host) of
            true -> [form_field(<<"text-single">>, <<"full-text-search">>)];
            false -> []
        end,
    [form_type_field(MamNs),
     form_field(<<"jid-single">>, <<"with">>),
     form_field(<<"text-single">>, <<"start">>),
     form_field(<<"text-single">>, <<"end">>) | TextSearch].

form_type_field(MamNs) when is_binary(MamNs) ->
    #xmlel{name = <<"field">>,
           attrs = [{<<"type">>, <<"hidden">>},
                    {<<"var">>, <<"FORM_TYPE">>}],
           children = [#xmlel{name = <<"value">>,
                              children = [#xmlcdata{content = MamNs}]}]}.

form_field(Type, VarName) ->
    #xmlel{name = <<"field">>,
           attrs = [{<<"type">>, Type},
                    {<<"var">>, VarName}]}.

-spec form_to_text(_) -> 'undefined' | binary().
form_to_text(El) ->
    form_field_value(El, <<"full-text-search">>).

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
    LowerBody = string:to_lower(BodyString),
    ReOpts = [{return, list}, global, unicode, ucp],
    Re0 = re:replace(LowerBody, "[, .:;-?!]+", " ", ReOpts),
    Re1 = re:replace(Re0, "([^\\w ]+)|(^\\s+)|(\\s+$)", "", ReOpts),
    Re2 = re:replace(Re1, "\s+", unicode:characters_to_list(WordSeparator), ReOpts),
    unicode:characters_to_binary(Re2).

-spec packet_to_search_body(Module :: mod_mam | mod_mam_muc, Host :: jid:server(),
                            Packet :: exml:element()) -> binary().
packet_to_search_body(Module, Host, Packet) ->
    case has_full_text_search(Module, Host) of
        true ->
            BodyValue = exml_query:path(Packet, [{element, <<"body">>}, cdata], <<>>),
            mod_mam_utils:normalize_search_text(BodyValue, <<" ">>);
        false ->
            <<>>
    end.

-spec has_full_text_search(Module :: mod_mam | mod_mam_muc, Host :: jid:server()) -> boolean().
has_full_text_search(Module, Host) ->
    gen_mod:get_module_opt(Host, Module, full_text_search, true).

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
    is_loaded_application(stringprep) orelse start_stringprep().

start_stringprep() ->
    EJ = code:lib_dir(ejabberd),
    code:add_path(filename:join([EJ, "..", "..", "deps", "stringprep", "ebin"])),
    ok = application:start(stringprep).

is_loaded_application(AppName) when is_atom(AppName) ->
    lists:keymember(AppName, 1, application:loaded_applications()).

-endif.

%% -----------------------------------------------------------------------
%% Other
-spec bare_jid(undefined | jid:jid()) -> undefined | binary().
bare_jid(undefined) -> undefined;
bare_jid(JID) ->
    jid:to_binary(jid:to_bare(jid:to_lower(JID))).

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
-spec is_complete_result_page(TotalCount, Offset, MessageRows, Params) ->
    boolean() when
    TotalCount  :: non_neg_integer()|undefined,
    Offset      :: non_neg_integer()|undefined,
    MessageRows :: list(),
    Params      :: mam_iq:lookup_params().
is_complete_result_page(TotalCount, Offset, MessageRows,
                        #{page_size := PageSize} = Params) ->
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
    xml:replace_tag_attr(<<"xmlns">>, <<"jabber:client">>, Packet);
maybe_set_client_xmlns(false, Packet) ->
    Packet.


-spec action_to_shaper_name(mam_iq:action()) -> atom().
action_to_shaper_name(Action) ->
    list_to_atom(atom_to_list(Action) ++ "_shaper").

-spec action_to_global_shaper_name(mam_iq:action()) -> atom().
action_to_global_shaper_name(Action) -> list_to_atom(atom_to_list(Action) ++ "_global_shaper").


-spec wait_shaper(jid:server(), mam_iq:action(), jid:jid()) ->
    'ok' | {'error', 'max_delay_reached'}.
wait_shaper(Host, Action, From) ->
    case shaper_srv:wait(Host, action_to_shaper_name(Action), From, 1) of
        ok ->
            shaper_srv:wait(Host, action_to_global_shaper_name(Action), global, 1);
        Err ->
            Err
    end.

%% -----------------------------------------------------------------------
%% Ejabberd

-spec send_message(jid:jid(), jid:jid(), exml:element()
                  ) -> mongoose_acc:t().

-ifdef(MAM_COMPACT_FORWARDED).

send_message(_From, To, Mess) ->
    From = jid:from_binary(exml_query:attr(Mess, <<"from">>)),
    ejabberd_sm:route(From, To, Mess).

-else.

send_message(From, To, Mess) ->
    ejabberd_sm:route(From, To, Mess).

-endif.


-spec is_jid_in_user_roster(jid:jid(), jid:jid()) -> boolean().
is_jid_in_user_roster(#jid{lserver=LServer, luser=LUser},
                      #jid{} = RemJID) ->
    RemBareJID = jid:to_bare(RemJID),
    {Subscription, _Groups} =
        ejabberd_hooks:run_fold(
          roster_get_jid_info, LServer,
          {none, []}, [LUser, LServer, RemBareJID]),
    Subscription == from orelse Subscription == both.


-spec success_sql_query(atom() | jid:server(), mongoose_rdbms:sql_query()) -> any().
success_sql_query(HostOrConn, Query) ->
    Result = mongoose_rdbms:sql_query(HostOrConn, Query),
    error_on_sql_error(HostOrConn, Query, Result).

-spec success_sql_execute(atom() | jid:server(), atom(), [term()]) -> any().
success_sql_execute(HostOrConn, Name, Params) ->
    Result = mongoose_rdbms:execute(HostOrConn, Name, Params),
    error_on_sql_error(HostOrConn, Name, Result).

error_on_sql_error(HostOrConn, Query, {error, Reason}) ->
    ?ERROR_MSG("SQL-error on ~p.~nQuery ~p~nReason ~p", [HostOrConn, Query, Reason]),
            error({sql_error, Reason});
error_on_sql_error(_HostOrConn, _Query, Result) ->
    Result.

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
      RSM :: jlib:rsm_in(),
      PageSize :: non_neg_integer(),
      LookupResult :: mod_mam:lookup_result(),
      R :: {ok, mod_mam:lookup_result()} | {error, item_not_found}.
check_for_item_not_found(#rsm_in{direction = before, id = ID},
                         PageSize, {TotalCount, Offset, MessageRows}) when ID =/= undefined ->
    case maybe_last(MessageRows) of
        {ok, {ID, _, _}} = _IntervalEndpoint ->
            Page = lists:sublist(MessageRows, PageSize),
            {ok, {TotalCount, Offset, Page}};
        undefined ->
            {error, item_not_found}
    end;
check_for_item_not_found(#rsm_in{direction = aft, id = ID},
                         _PageSize, {TotalCount, Offset, MessageRows0}) when ID =/= undefined ->
    case MessageRows0 of
        [{ID, _, _} = _IntervalEndpoint | MessageRows] ->
            {ok, {TotalCount, Offset, MessageRows}};
        _ ->
            {error, item_not_found}
    end.
