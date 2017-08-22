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
-export([add_arcid_elems/3,
         is_arcid_elem_for/3,
         replace_arcid_elem/4,
         replace_x_user_element/4,
         append_arcid_elem/4,
         delete_arcid_elem/3,
         delete_x_user_element/1,
         packet_to_x_user_jid/1,
         get_one_of_path/2,
         get_one_of_path/3,
         is_archivable_message/3,
         wrap_message/6,
         wrap_message/7,
         result_set/4,
         result_query/2,
         result_prefs/4,
         make_fin_message/5,
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
         apply_start_border/2,
         apply_end_border/2,
         bare_jid/1,
         full_jid/1,
         calculate_msg_id_borders/3,
         calculate_msg_id_borders/4,
         maybe_encode_compact_uuid/2,
         is_last_page/4]).

%% Ejabberd
-export([send_message/3,
         is_jid_in_user_roster/2]).

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

-include_lib("ejabberd/include/ejabberd.hrl").
-include_lib("ejabberd/include/jlib.hrl").
-include_lib("exml/include/exml.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

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
    CandidateStamp = p1_time_compat:os_system_time(micro_seconds),
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

%% @doc Adds all arcid elements (for backward compatibility)
-spec add_arcid_elems(By :: binary(), Id :: binary(), jlib:xmlel()) -> jlib:xmlel().
add_arcid_elems(By, Id, Packet) ->
  WithArchived = replace_arcid_elem(<<"archived">>, By, Id, Packet),
  replace_arcid_elem(<<"stanza-id">>, By, Id, WithArchived).

%% @doc Return true, if the first element points on `By'.
-spec is_arcid_elem_for(ElemName :: binary(), jlib:xmlel(), By :: binary()) -> boolean().
is_arcid_elem_for(<<"archived">>, #xmlel{name = <<"archived">>, attrs=As}, By) ->
    lists:member({<<"by">>, By}, As);
is_arcid_elem_for(<<"stanza-id">>, #xmlel{name = <<"stanza-id">>, attrs=As}, By) ->
    lists:member({<<"by">>, By}, As) andalso
    lists:member({<<"xmlns">>, ?NS_STANZAID}, As);
is_arcid_elem_for(_, _, _) ->
    false.

-spec replace_arcid_elem(ElemName :: binary(), By :: binary(), Id :: binary(),
                         Packet :: jlib:xmlel()) -> jlin:xmlel().
replace_arcid_elem(ElemName, By, Id, Packet) ->
    append_arcid_elem(ElemName, By, Id,
                       delete_arcid_elem(ElemName, By, Packet)).

-spec append_arcid_elem(ElemName :: binary(), By :: binary(), Id :: binary(),
                        Packet :: jlib:xmlel()) ->jlib:xmlel().
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

-spec delete_arcid_elem(ElemName :: binary(), By :: binary(), jlib:xmlel()) -> jlib:xmlel().
delete_arcid_elem(ElemName, By, Packet=#xmlel{children=Cs}) ->
    Packet#xmlel{children=[C || C <- Cs, not is_arcid_elem_for(ElemName, C, By)]}.


is_x_user_element(#xmlel{name = <<"x">>, attrs = As}) ->
    lists:member({<<"xmlns">>, ?NS_MUC_USER}, As);
is_x_user_element(_) ->
    false.

-spec replace_x_user_element(FromJID :: ejabberd:jid(), Role :: mod_muc:role(),
                             Affiliation :: mod_muc:affiliation(), jlib:xmlel()) -> jlib:xmlel().
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

-spec delete_x_user_element(jlib:xmlel()) -> jlib:xmlel().
delete_x_user_element(Packet=#xmlel{children=Cs}) ->
    Packet#xmlel{children=[C || C <- Cs, not is_x_user_element(C)]}.

-spec packet_to_x_user_jid(jlib:xmlel()) -> ejabberd:jid() | error | undefined.
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


%% @doc Checks, that the stanza is a message with body.
%%
%% Servers SHOULD NOT archive messages that do not have a `<body/>' child tag.
%% Servers SHOULD NOT delayed messages.
%%
%% From v0.3: it is expected that all messages that hold meaningful content,
%% rather than state changes such as Chat State Notifications, would be archived.
%% @end
-spec is_archivable_message(Mod :: module(), Dir :: incoming | outgoing,
                            Packet :: jlib:xmlel()) -> boolean().
is_archivable_message(Mod, Dir, Packet=#xmlel{name = <<"message">>}) ->
    Type = exml_query:attr(Packet, <<"type">>, <<"normal">>),
    is_valid_message_type(Mod, Dir, Type) andalso
        is_valid_message(Mod, Dir, Packet);
is_archivable_message(_, _, _) ->
    false.

is_valid_message_type(_, _, <<"normal">>) -> true;
is_valid_message_type(_, _, <<"chat">>) -> true;
is_valid_message_type(_, incoming, <<"groupchat">>) -> true;
is_valid_message_type(_, _, <<"error">>) -> false;
is_valid_message_type(_, _, _) -> false.

is_valid_message(_Mod, _Dir, Packet) ->
    Body       = exml_query:subelement(Packet, <<"body">>, false),
    ChatMarker = should_check_chat_markers() andalso has_chat_marker(Packet),
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

should_check_chat_markers() ->
    mod_mam_params:archive_chat_markers().

has_chat_marker(Packet) ->
    case exml_query:subelement_with_ns(Packet, ?NS_CHAT_MARKERS) of
        #xmlel{name = <<"received">>}     -> true;
        #xmlel{name = <<"displayed">>}    -> true;
        #xmlel{name = <<"acknowledged">>} -> true;
        _                                 -> false
    end.

%% @doc Forms `<forwarded/>' element, according to the XEP.
-spec wrap_message(MamNs :: binary(), Packet :: jlib:xmlel(), QueryID :: binary(),
                   MessageUID :: term(), DateTime :: calendar:datetime(),
                   SrcJID :: ejabberd:jid()) -> Wrapper :: jlib:xmlel().
wrap_message(MamNs, Packet, QueryID, MessageUID, DateTime, SrcJID) ->
    wrap_message(MamNs, Packet, QueryID, MessageUID, wrapper_id(), DateTime, SrcJID).

-spec wrap_message(MamNs :: binary(), Packet :: jlib:xmlel(), QueryID :: binary(),
                   MessageUID :: term(), WrapperI :: binary(), DateTime :: calendar:datetime(),
                   SrcJID :: ejabberd:jid()) -> Wrapper :: jlib:xmlel().
wrap_message(MamNs, Packet, QueryID, MessageUID, WrapperID, DateTime, SrcJID) ->
    #xmlel{ name = <<"message">>,
            attrs = [{<<"id">>, WrapperID}],
            children = [result(MamNs, QueryID, MessageUID,
                               [forwarded(Packet, DateTime, SrcJID)])] }.

-spec forwarded(jlib:xmlel(), calendar:datetime(), ejabberd:jid())
               -> jlib:xmlel().
forwarded(Packet, DateTime, SrcJID) ->
    #xmlel{
       name = <<"forwarded">>,
       attrs = [{<<"xmlns">>, <<"urn:xmpp:forward:0">>}],
       %% Two places to include SrcJID:
       %% - delay.from - optional XEP-0297 (TODO: depricate adding it?)
       %% - message.from - required XEP-0313
       %% Also, mod_mam_muc will replace it again with SrcJID
       children = [delay(DateTime, SrcJID), replace_from_attribute(SrcJID, Packet)]}.

-spec delay(calendar:datetime(), ejabberd:jid()) -> jlib:xmlel().
delay(DateTime, SrcJID) ->
    jlib:timestamp_to_xml(DateTime, utc, SrcJID, <<>>).

replace_from_attribute(From, Packet=#xmlel{attrs = Attrs}) ->
    Attrs1 = lists:keydelete(<<"from">>, 1, Attrs),
    Attrs2 = [{<<"from">>, jid:to_binary(From)} | Attrs1],
    Packet#xmlel{attrs = Attrs2}.

%% @doc Generates tag `<result />'.
%% This element will be added in each forwarded message.
-spec result(binary(), _, MessageUID :: binary(), Children :: [jlib:xmlel(), ...])
            -> jlib:xmlel().
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
                 CountI :: non_neg_integer() | undefined) -> jlib:xmlel().
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


-spec result_query(jlib:xmlcdata() | jlib:xmlel(), binary()) -> jlib:xmlel().
result_query(SetEl, Namespace) ->
    #xmlel{
       name = <<"query">>,
       attrs = [{<<"xmlns">>, Namespace}],
       children = [SetEl]}.

-spec result_prefs(DefaultMode :: archive_behaviour(),
                   AlwaysJIDs :: [ejabberd:literal_jid()],
                   NeverJIDs :: [ejabberd:literal_jid()],
                   Namespace :: binary()) -> jlib:xmlel().
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


-spec encode_jids([binary() | string()]) -> [jlib:xmlel()].
encode_jids(JIDs) ->
    [#xmlel{name = <<"jid">>, children = [#xmlcdata{content = JID}]}
     || JID <- JIDs].


%% Make fin message introduced in MAM 0.3
-spec make_fin_message(binary(), boolean(), boolean(), jlib:xmlel(), binary()) -> jlib:xmlel().
make_fin_message(MamNs, IsComplete, IsStable, ResultSetEl, QueryID) ->
    #xmlel{
       name = <<"message">>,
       children = [make_fin_element_v03(MamNs, IsComplete, IsStable, ResultSetEl, QueryID)]}.

%% MAM v0.3
make_fin_element_v03(MamNs, IsComplete, IsStable, ResultSetEl, QueryID) ->
    #xmlel{
       name = <<"fin">>,
       attrs = [{<<"xmlns">>, MamNs}]
        ++ [{<<"complete">>, <<"true">>} || IsComplete]
        ++ [{<<"stable">>, <<"false">>} || not IsStable]
        ++ [{<<"queryid">>, QueryID} || is_binary(QueryID), QueryID =/= undefined],
       children = [ResultSetEl]}.

%% MAM v0.4.1 and above
-spec make_fin_element(binary(), boolean(), boolean(), jlib:xmlel()) -> jlib:xmlel().
make_fin_element(MamNs, IsComplete, IsStable, ResultSetEl) ->
    #xmlel{
       name = <<"fin">>,
       attrs = [{<<"xmlns">>, MamNs}]
        ++ [{<<"complete">>, <<"true">>} || IsComplete]
        ++ [{<<"stable">>, <<"false">>} || not IsStable],
       children = [ResultSetEl]}.


-spec parse_prefs(PrefsEl :: jlib:xmlel()) -> mod_mam:preference().
parse_prefs(El = #xmlel{ name = <<"prefs">> }) ->
    Default = exml_query:attr(El, <<"default">>),
    AlwaysJIDs = parse_jid_list(El, <<"always">>),
    NeverJIDs  = parse_jid_list(El, <<"never">>),
    {valid_behavior(Default), AlwaysJIDs, NeverJIDs}.


-spec valid_behavior(archive_behaviour_bin()) -> archive_behaviour().
valid_behavior(<<"always">>) -> always;
valid_behavior(<<"never">>)  -> never;
valid_behavior(<<"roster">>) -> roster.


-spec parse_jid_list(jlib:xmlel(), binary()) -> [ejabberd:literal_jid()].
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

-spec borders_decode(jlib:xmlel()) -> 'undefined' | mod_mam:borders().
borders_decode(QueryEl) ->
    AfterID  = tag_id(QueryEl, <<"after_id">>),
    BeforeID = tag_id(QueryEl, <<"before_id">>),
    FromID   = tag_id(QueryEl, <<"from_id">>),
    ToID     = tag_id(QueryEl, <<"to_id">>),
    borders(AfterID, BeforeID, FromID, ToID).

-spec form_borders_decode(jlib:xmlel()) -> 'undefined' | mod_mam:borders().
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


-spec tag_id(jlib:xmlel(), binary()) -> 'undefined' | integer().
tag_id(QueryEl, Name) ->
    BExtMessID = exml_query:attr(QueryEl, Name, <<>>),
    maybe_external_binary_to_mess_id(BExtMessID).

-spec form_field_mess_id(jlib:xmlel(), binary()) -> 'undefined' | integer().
form_field_mess_id(QueryEl, Name) ->
    BExtMessID = form_field_value_s(QueryEl, Name),
    maybe_external_binary_to_mess_id(BExtMessID).

-spec decode_optimizations(jlib:xmlel()) -> 'false' | 'opt_count' | 'true'.
decode_optimizations(QueryEl) ->
    case {exml_query:subelement(QueryEl, <<"simple">>),
          exml_query:subelement(QueryEl, <<"opt_count">>)} of
        {undefined, undefined} -> false;
        {undefined, _}     -> opt_count;
        _              -> true
    end.

-spec form_decode_optimizations(jlib:xmlel()) -> false | opt_count | true.
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

is_mam_namespace(?NS_MAM)    -> true;
is_mam_namespace(?NS_MAM_03) -> true;
is_mam_namespace(?NS_MAM_04) -> true;
is_mam_namespace(?NS_MAM_06) -> true;
is_mam_namespace(_)          -> false.


%% -----------------------------------------------------------------------
%% Forms

-spec form_field_value(jlib:xmlel(), binary()) -> undefined | binary().
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
-spec find_field(list(jlib:xmlel()), binary()) -> undefined | jlib:xmlel().
find_field([#xmlel{ name = <<"field">> } = Field | Fields], Name) ->
    case exml_query:attr(Field, <<"var">>) of
        Name -> Field;
        _ -> find_field(Fields, Name)
    end;
find_field([_|Fields], Name) -> %% skip whitespaces
    find_field(Fields, Name);
find_field([], _Name) ->
    undefined.

-spec field_to_value(jlib:xmlel()) -> binary().
field_to_value(FieldEl) ->
    exml_query:path(FieldEl, [{element, <<"value">>}, cdata], <<>>).

-spec message_form(Mod :: mod_mam | mod_mam_muc, Host :: ejabberd:lserver(), binary()) ->
    jlib:xmlel().
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
-spec normalize_search_text(binary() | string() | undefined) -> string() | undefined.
normalize_search_text(Text) ->
    normalize_search_text(Text, "%").

-spec normalize_search_text(binary() | string() | undefined, string()) -> string() | undefined.
normalize_search_text(undefined, _WordSeparator) ->
    undefined;
normalize_search_text(Text, WordSeparator) ->
    BodyString = unicode:characters_to_list(Text),
    LowerBody = string:to_lower(BodyString),
    ReOpts = [{return, list}, global, unicode, ucp],
    Re0 = re:replace(LowerBody, "[, .:;-?!]+", " ", ReOpts),
    Re1 = re:replace(Re0, "([^\\w ]+)|(^\\s+)|(\\s+$)", "", ReOpts),
    re:replace(Re1, "\s+", WordSeparator, ReOpts).

-spec packet_to_search_body(Module :: mod_mam | mod_mam_muc, Host :: ejabberd:server(),
                            Packet :: xmlel()) -> string().
packet_to_search_body(Module, Host, Packet) ->
    case has_full_text_search(Module, Host) of
        true ->
            BodyValue = exml_query:path(Packet, [{element, <<"body">>}, cdata],
                                        <<"">>),
            mod_mam_utils:normalize_search_text(BodyValue, " ");
        false -> ""
    end.

-spec has_full_text_search(Module :: mod_mam | mod_mam_muc, Host :: ejabberd:server()) -> boolean().
has_full_text_search(Module, Host) ->
    gen_mod:get_module_opt(Host, Module, full_text_search, true).

%% -----------------------------------------------------------------------
%% JID serialization

-spec jid_to_opt_binary(UserJID :: ejabberd:jid(), JID :: ejabberd:jid()
                        ) -> ejabberd:literal_jid().
jid_to_opt_binary(#jid{lserver=LServer, luser=LUser},
                  #jid{lserver=LServer, luser=LUser, lresource= <<>>}) ->
    <<>>;
jid_to_opt_binary(#jid{lserver=LServer, luser=LUser},
                  #jid{lserver=LServer, luser=LUser, lresource= LResource}) ->
    <<$/, LResource/binary>>;
jid_to_opt_binary(#jid{lserver=LServer},
                  #jid{lserver=LServer, luser=LUser, lresource= <<>>}) ->
    %% Both clients are on the same server.
    <<LUser/binary>>;
jid_to_opt_binary(#jid{lserver=LServer},
                  #jid{lserver=LServer, luser=LUser, lresource=LResource}) ->
    %% Both clients are on the same server.
    <<LUser/binary, $/, LResource/binary>>;
jid_to_opt_binary(_,
                  #jid{lserver=LServer, luser=LUser, lresource= <<>>}) ->
    <<LServer/binary, $:, LUser/binary>>;
jid_to_opt_binary(_,
                  #jid{lserver=LServer, luser=LUser, lresource=LResource}) ->
    <<LServer/binary, $@, LUser/binary, $/, LResource/binary>>.


-spec expand_minified_jid(UserJID :: ejabberd:jid(),
                          OptJID :: ejabberd:literal_jid()) -> ejabberd:literal_jid().
expand_minified_jid(#jid{lserver=LServer, luser=LUser}, <<>>) ->
    <<LUser/binary, $@, LServer/binary>>;
expand_minified_jid(#jid{lserver=LServer, luser=LUser}, <<$/, LResource/binary>>) ->
    <<LUser/binary, $@, LServer/binary, $/, LResource/binary>>;
expand_minified_jid(UserJID, Encoded) ->
    Part = binary:match(Encoded, [<<$@>>, <<$/>>, <<$:>>]),
    expand_minified_jid(Part, UserJID, Encoded).

-spec expand_minified_jid('nomatch' | {non_neg_integer(), 1},
            ejabberd:jid(), Encoded :: ejabberd:luser() | binary()) -> binary().
expand_minified_jid(nomatch,  #jid{lserver=ThisServer}, LUser) ->
    <<LUser/binary, $@, ThisServer/binary>>;
expand_minified_jid({Pos, 1}, #jid{lserver=ThisServer}, Encoded) ->
    case Encoded of
        <<LServer:Pos/binary, $:, LUser/binary>> ->
            <<LUser/binary, $@, LServer/binary>>;
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
-spec bare_jid(undefined | ejabberd:jid()) -> undefined | binary().
bare_jid(undefined) -> undefined;
bare_jid(JID) ->
    jid:to_binary(jid:to_bare(jid:to_lower(JID))).

-spec full_jid(ejabberd:jid()) -> binary().
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

-spec calculate_msg_id_borders(rsm_in() | undefined,
                               mod_mam:borders() | undefined,
                               mod_mam:unix_timestamp() | undefined,
                               mod_mam:unix_timestamp() | undefined) -> R when
      R :: {integer() | undefined, integer() | undefined}.
calculate_msg_id_borders(#rsm_in{id = undefined}, Borders, Start, End) ->
    calculate_msg_id_borders(undefined, Borders, Start, End);
calculate_msg_id_borders(#rsm_in{direction = aft, id = Id}, Borders, Start, End) ->
    {StartId, EndId} = mod_mam_utils:calculate_msg_id_borders(undefined, Borders, Start, End),
    NextId = Id + 1,
    {mod_mam_utils:maybe_max(StartId, NextId), EndId};
calculate_msg_id_borders(#rsm_in{direction = before, id = Id}, Borders, Start, End) ->
    {StartId, EndId} = mod_mam_utils:calculate_msg_id_borders(undefined, Borders, Start, End),
    PrevId = Id - 1,
    {StartId, mod_mam_utils:maybe_min(EndId, PrevId)};
calculate_msg_id_borders(_, Borders, Start, End) ->
    mod_mam_utils:calculate_msg_id_borders(Borders, Start, End).

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


-spec is_last_page(PageSize, TotalCount, Offset, MessageRows) -> boolean() when
    PageSize    :: non_neg_integer(),
    TotalCount  :: non_neg_integer()|undefined,
    Offset      :: non_neg_integer()|undefined,
    MessageRows :: list().
is_last_page(PageSize, _TotalCount, _Offset, MessageRows)
    when length(MessageRows) < PageSize ->
    true;
is_last_page(PageSize, TotalCount, Offset, MessageRows)
  when is_integer(TotalCount), is_integer(Offset),
       length(MessageRows) =:= PageSize ->
    %% Number of messages on skipped pages from the beginning plus the current page
    PagedCount = Offset + PageSize,
    TotalCount =:= PagedCount; %% false means full page but not the last one in the result set
is_last_page(_PageSize, _TotalCount, _Offset, _MessageRows) ->
    %% When is_integer(TotalCount), is_integer(Offset)
    %%     it's not possible case: the page is bigger then page size.
    %% Otherwise either TotalCount or Offset is undefined because of optimizations.
    false.


%% -----------------------------------------------------------------------
%% Ejabberd

-spec send_message(ejabberd:jid(), ejabberd:jid(), jlib:xmlel()
                  ) -> mongoose_acc:t().

-ifdef(MAM_COMPACT_FORWARDED).

send_message(_From, To, Mess) ->
    From = jid:from_binary(exml_query:attr(Mess, <<"from">>)),
    ejabberd_sm:route(From, To, Mess).

-else.

send_message(From, To, Mess) ->
    ejabberd_sm:route(From, To, Mess).

-endif.


-spec is_jid_in_user_roster(ejabberd:jid(), ejabberd:jid()) -> boolean().
is_jid_in_user_roster(#jid{lserver=LServer, luser=LUser},
                      #jid{} = RemJID) ->
    RemBareJID = jid:to_bare(RemJID),
    {Subscription, _Groups} =
        ejabberd_hooks:run_fold(
          roster_get_jid_info, LServer,
          {none, []}, [LUser, LServer, RemBareJID]),
    Subscription == from orelse Subscription == both.


-spec success_sql_query(atom() | ejabberd:server(), _) -> any().
success_sql_query(HostOrConn, Query) ->
    Result = mongoose_rdbms:sql_query(HostOrConn, Query),
    error_on_sql_error(HostOrConn, Query, Result).

-spec success_sql_execute(atom() | ejabberd:server(), atom(), [term()]) -> any().
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
