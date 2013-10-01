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
         external_binary_to_mess_id/1]).

%% XML
-export([is_archived_elem_for/2,
         replace_archived_elem/3,
         append_archived_elem/3,
         delete_archived_elem/2,
         get_one_of_path/2,
         get_one_of_path/3,
         is_complete_message/1,
         wrap_message/5,
         result_set/4,
         result_query/1,
         result_prefs/3,
         parse_prefs/1
]).

%% SQL
-export([success_sql_query/2]).

%% Other
-export([maybe_integer/2,
         is_function_exist/3]).

%% Ejabberd
-export([send_message/3,
         is_jid_in_user_roster/2]).


-include_lib("ejabberd/include/ejabberd.hrl").
-include_lib("ejabberd/include/jlib.hrl").
-include_lib("exml/include/exml.hrl").

-define(MAYBE_BIN(X), (is_binary(X) orelse (X) =:= undefined)).

%% Constants
rsm_ns_binary() -> <<"http://jabber.org/protocol/rsm">>.
mam_ns_binary() -> <<"urn:xmpp:mam:tmp">>.

%% ----------------------------------------------------------------------
%% Datetime types
-type iso8601_datetime_binary() :: binary().
%% Microseconds from 01.01.1970
-type unix_timestamp() :: non_neg_integer().

-type elem() :: #xmlel{}.
-type archive_behaviour() :: roster | always | never.
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

%% @doc Returns time in `now()' format.
-spec iso8601_datetime_binary_to_timestamp(iso8601_datetime_binary()) ->
    erlang:timestamp().
iso8601_datetime_binary_to_timestamp(DateTime) when is_binary(DateTime) ->
    jlib:datetime_string_to_timestamp(binary_to_list(DateTime)).

datetime_to_microseconds({{_,_,_}, {_,_,_}} = DateTime) ->
    S1 = calendar:datetime_to_gregorian_seconds(DateTime),
    S0 = calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}),
    Seconds = S1 - S0,
    Seconds * 1000000.

microseconds_to_datetime(MicroSeconds) when is_integer(MicroSeconds) ->
    calendar:now_to_datetime(mod_mam_utils:microseconds_to_now(MicroSeconds)).

%% -----------------------------------------------------------------------
%% UID

generate_message_id() ->
    {ok, NodeId} = ejabberd_node_id:node_id(),
    %% Use monotone function here.
    encode_compact_uuid(now_to_microseconds(now()), NodeId).

%% @doc Create a message ID (UID).
%%
%% It removes a leading 0 from 64-bit binary representation.
%% It puts node id as a last byte.
%% The maximum date, that can be encoded is `{{4253,5,31},{22,20,37}}'.
%% @end
encode_compact_uuid(Microseconds, NodeId)
    when is_integer(Microseconds), is_integer(NodeId) ->
    (Microseconds bsl 8) + NodeId.

%% @doc Extract date and node id from a message id.
decode_compact_uuid(Id) ->
    Microseconds = Id bsr 8,
    NodeId = Id band 255,
    {Microseconds, NodeId}.

%% @doc Encode a message ID to pass it to the user.
mess_id_to_external_binary(MessID) when is_integer(MessID) ->
    list_to_binary(integer_to_list(MessID, 32)).

%% @doc Decode a message ID received from the user.
external_binary_to_mess_id(BExtMessID) when is_binary(BExtMessID) ->
    list_to_integer(binary_to_list(BExtMessID), 32).

%% -----------------------------------------------------------------------
%% XML

%% @doc Return true, if the first element points on `By'.
is_archived_elem_for(#xmlel{name = <<"archived">>, attrs=As}, By) ->
    lists:member({<<"by">>, By}, As);
is_archived_elem_for(_, _) ->
    false.

replace_archived_elem(By, Id, Packet) ->
    append_archived_elem(By, Id,
    delete_archived_elem(By, Packet)).

append_archived_elem(By, Id, Packet) ->
    Archived = #xmlel{
        name = <<"archived">>,
        attrs=[{<<"by">>, By}, {<<"id">>, Id}]},
    xml:append_subtags(Packet, [Archived]).

delete_archived_elem(By, Packet=#xmlel{children=Cs}) ->
    Packet#xmlel{children=[C || C <- Cs, not is_archived_elem_for(C, By)]}.


get_one_of_path(Elem, List) ->
    get_one_of_path(Elem, List, <<>>).

get_one_of_path(Elem, [H|T], Def) ->
    case xml:get_path_s(Elem, H) of
        Def -> get_one_of_path(Elem, T, Def);
        Val  -> Val
    end;
get_one_of_path(_Elem, [], Def) ->
    Def.


%% @doc Checks, that the stanza is a message with body.
%%
%% Servers SHOULD NOT archive messages that do not have a `<body/>' child tag.
%% Servers SHOULD NOT delayed messages.
%% @end
-spec is_complete_message(Packet::#xmlel{}) -> boolean().
is_complete_message(Packet=#xmlel{name = <<"message">>}) ->
    case xml:get_tag_attr_s(<<"type">>, Packet) of
    Type when Type == <<"">>;
              Type == <<"normal">>;
              Type == <<"chat">>;
              Type == <<"groupchat">> ->
        case {xml:get_subtag(Packet, <<"body">>),
              xml:get_subtag(Packet, <<"delay">>)} of
            {false, _} -> false;
            {_, false} -> true;
            {_,     _} -> false
        end;
    %% Skip <<"error">> type
    _ -> false
    end;
is_complete_message(_) -> false.


%% @doc Forms `<forwarded/>' element, according to the XEP.
-spec wrap_message(Packet::elem(), QueryID::binary(),
                   MessageUID::term(), DateTime::calendar:datetime(), SrcJID::jid()) ->
        Wrapper::elem().
wrap_message(Packet, QueryID, MessageUID, DateTime, SrcJID) ->
    #xmlel{
        name = <<"message">>,
        attrs = [],
        children = [result(QueryID, MessageUID,
                           [forwarded(Packet, DateTime, SrcJID)])]}.

-spec forwarded(elem(), calendar:datetime(), jid()) -> elem().
forwarded(Packet, DateTime, SrcJID) ->
    #xmlel{
        name = <<"forwarded">>,
        attrs = [{<<"xmlns">>, <<"urn:xmpp:forward:0">>}],
        children = [delay(DateTime, SrcJID), Packet]}.

-spec delay(calendar:datetime(), jid()) -> elem().
delay(DateTime, SrcJID) ->
    jlib:timestamp_to_xml(DateTime, utc, SrcJID, <<>>).


%% @doc Generates tag `<result />'.
%%
%% This element will be added in each forwarded message.
%% @end
result(QueryID, MessageUID, Children) when is_list(Children) ->
    #xmlel{
        name = <<"result">>,
        attrs = [{<<"queryid">>, QueryID} || QueryID =/= undefined] ++
                [{<<"xmlns">>, mam_ns_binary()},
                 {<<"id">>, MessageUID}],
        children = Children}.


%% @doc Generates `<set />' tag.
%%
%% This element will be added into "iq/query".
%% @end
-spec result_set(FirstId, LastId, FirstIndexI, CountI) -> elem() when
    FirstId :: binary() | undefined,
    LastId  :: binary() | undefined,
    FirstIndexI :: non_neg_integer() | undefined,
    CountI      :: non_neg_integer().
result_set(FirstId, LastId, FirstIndexI, CountI)
    when ?MAYBE_BIN(FirstId), ?MAYBE_BIN(LastId) ->
    %% <result xmlns='urn:xmpp:mam:tmp' queryid='f27' id='28482-98726-73623' />
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

result_query(SetEl) ->
     #xmlel{
        name = <<"query">>,
        attrs = [{<<"xmlns">>, mam_ns_binary()}],
        children = [SetEl]}.

-spec result_prefs(DefaultMode, AlwaysJIDs, NeverJIDs) -> ResultPrefsEl when
    DefaultMode :: archive_behaviour(),
    AlwaysJIDs  :: [binary()],
    NeverJIDs   :: [binary()],
    ResultPrefsEl :: elem().
result_prefs(DefaultMode, AlwaysJIDs, NeverJIDs) ->
    AlwaysEl = #xmlel{name = <<"always">>,
                      children = encode_jids(AlwaysJIDs)},
    NeverEl  = #xmlel{name = <<"never">>,
                      children = encode_jids(NeverJIDs)},
    #xmlel{
       name = <<"prefs">>,
       attrs = [{<<"xmlns">>,mam_ns_binary()},
                {<<"default">>, atom_to_binary(DefaultMode, utf8)}],
       children = [AlwaysEl, NeverEl]
    }.

encode_jids(JIDs) ->
    [#xmlel{name = <<"jid">>, children = [#xmlcdata{content = JID}]}
     || JID <- JIDs].


-spec parse_prefs(PrefsEl) -> {DefaultMode, AlwaysJIDs, NeverJIDs} when
    PrefsEl :: elem(),
    DefaultMode :: archive_behaviour(),
    AlwaysJIDs  :: [binary()],
    NeverJIDs   :: [binary()].
parse_prefs(El=#xmlel{name = <<"prefs">>, attrs = Attrs}) ->
    {value, Default} = xml:get_attr(<<"default">>, Attrs),
    AlwaysJIDs = parse_jid_list(El, <<"always">>),
    NeverJIDs  = parse_jid_list(El, <<"never">>),
    {valid_behavior(Default), AlwaysJIDs, NeverJIDs}.

-spec valid_behavior(archive_behaviour_bin()) -> archive_behaviour().
valid_behavior(<<"always">>) -> always;
valid_behavior(<<"never">>)  -> never;
valid_behavior(<<"roster">>) -> roster.

parse_jid_list(El, Name) ->
    case xml:get_subtag(El, Name) of
        false -> [];
        #xmlel{children = JIDEls} ->
            [xml:get_tag_cdata(JIDEl) || JIDEl <- JIDEls]
    end.

%% -----------------------------------------------------------------------
%% Other

maybe_integer(<<>>, Def) -> Def;
maybe_integer(Bin, _Def) when is_binary(Bin) ->
    list_to_integer(binary_to_list(Bin)).

is_function_exist({M, _}, F, A) ->
    %% M is a tuple module
    is_function_exist(M, F, A+1);
is_function_exist(M, F, A) ->
    lists:member({F, A}, M:module_info(exports)).

%% -----------------------------------------------------------------------
%% Ejabberd

send_message(From, To, Mess) ->
    ejabberd_sm:route(From, To, Mess).

is_jid_in_user_roster(#jid{lserver=LServer, luser=LUser},
                      #jid{} = RemJID) ->
    RemBareJID = jlib:jid_remove_resource(RemJID),
    {Subscription, _Groups} =
    ejabberd_hooks:run_fold(
        roster_get_jid_info, LServer,
        {none, []}, [LUser, LServer, RemBareJID]),
    Subscription == from orelse Subscription == both.

success_sql_query(Host, Query) ->
    case ejabberd_odbc:sql_query(Host, Query) of
        {error, Reason} ->
            ?ERROR_MSG("SQL-error on ~p.~nQuery ~p~nReason ~p~n",
                       [Host, Query, Reason]),
            error(sql_error);
        Result ->
            Result
    end.

