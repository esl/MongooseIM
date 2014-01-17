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
         is_complete_message/3,
         wrap_message/5,
         result_set/4,
         result_query/1,
         result_prefs/3,
         parse_prefs/1,
         borders_decode/1,
         decode_optimizations/1]).

%% JID serialization
-export([jid_to_opt_binary/2,
         expand_minified_jid/2]).

%% SQL
-export([success_sql_query/2]).

%% Other
-export([maybe_integer/2,
         is_function_exist/3,
         apply_start_border/2,
         apply_end_border/2]).

%% Ejabberd
-export([send_message/3,
         is_jid_in_user_roster/2]).

%-define(MAM_ARCHIVE_PRESENCE, true).
%-define(MAM_INLINE_UTILS, true).

-ifdef(MAM_INLINE_UTILS).
-compile({inline, [
        rsm_ns_binary/0,
        mam_ns_binary/0,
        now_to_microseconds/1,
        iso8601_datetime_binary_to_timestamp/1,
        is_archived_elem_for/2,
        encode_compact_uuid/2,
        get_one_of_path/3,
        is_incoming_presence/3,
        delay/2,
        forwarded/3,
        result/3,
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
    list_to_binary(integer_to_list(MessID, 36)).

maybe_external_binary_to_mess_id(undefined) ->
    undefined;
maybe_external_binary_to_mess_id(<<>>) ->
    undefined;
maybe_external_binary_to_mess_id(BExtMessID) ->
    external_binary_to_mess_id(BExtMessID).

%% @doc Decode a message ID received from the user.
external_binary_to_mess_id(BExtMessID) when is_binary(BExtMessID) ->
    list_to_integer(binary_to_list(BExtMessID), 36).

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
-spec is_complete_message(Mod, Dir, Packet) -> boolean() when
    Mod :: module(),
    Dir :: incoming | outgoing,
    Packet :: #xmlel{}.
is_complete_message(_, _, Packet=#xmlel{name = <<"message">>}) ->
    case xml:get_tag_attr_s(<<"type">>, Packet) of
    Type when Type == <<"">>;
              Type == <<"normal">>;
              Type == <<"chat">>;
              Type == <<"groupchat">> ->
        case {xml:get_subtag(Packet, <<"body">>),
              %% Used in MAM
              xml:get_subtag(Packet, <<"result">>),
              %% Used in mod_offline
              xml:get_subtag(Packet, <<"delay">>)} of
            %% Forwarded by MAM message or just a message without body
            {false, _,     _    } -> false;
            {_,     false, false} -> true;
            %% Forwarded by MAM message or delivered by mod_offline
            %% See mam_SUITE:offline_message for a test case
            {_,     _,     _    } -> false
        end;
    %% Skip <<"error">> type
    _ -> false
    end;
is_complete_message(Mod, Dir, Packet) ->
    is_incoming_presence(Mod, Dir, Packet).

-ifdef(MAM_ARCHIVE_PRESENCE).

is_incoming_presence(_, incoming, Packet=#xmlel{name = <<"presence">>}) ->
    xml:get_subtag(Packet, <<"delay">>) == false;
is_incoming_presence(_, _, _) -> false.

-else.

is_incoming_presence(_, _, _) -> false.

-endif.


-ifdef(MAM_COMPACT_FORWARDED).

%% @doc Forms a simple forwarded message (not according XEP).
%%
%% ```
%% <message xmlns="jabber:client"
%%    from="room@muc.server/alice"
%%    to="bob@server/res1"
%%    type="groupchat">
%%  <body>1</body>
%%  <delay xmlns="urn:xmpp:delay" id="9RUQN9VBP7G1" query_id="q1" stamp="2014-01-13T14:16:57Z" />
%% </message>
%% '''
-spec wrap_message(Packet::elem(), QueryID::binary(),
                   MessageUID::term(), DateTime::calendar:datetime(), SrcJID::jid()) ->
        Wrapper::elem().
wrap_message(Packet, QueryID, MessageUID, DateTime, SrcJID) ->
    Delay = delay(DateTime, SrcJID, QueryID, MessageUID),
    Packet2 = xml:append_subtags(Packet, [Delay]),
    xml:replace_tag_attr(<<"from">>, jlib:jid_to_binary(SrcJID), Packet2).

-spec delay(calendar:datetime(), jid(), binary(), binary()) -> elem().
delay(DateTime, _SrcJID, QueryID, MessageUID) ->
    jlib:timestamp_to_mam_xml(DateTime, utc, QueryID, MessageUID).

-else.

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
    %% <result xmlns='urn:xmpp:mam:tmp' queryid='f27' id='28482-98726-73623' />
    #xmlel{
        name = <<"result">>,
        attrs = [{<<"queryid">>, QueryID} || QueryID =/= undefined, QueryID =/= <<>>] ++
                [{<<"xmlns">>, mam_ns_binary()},
                 {<<"id">>, MessageUID}],
        children = Children}.

-endif.


%% @doc Generates `<set />' tag.
%%
%% This element will be added into "iq/query".
%% @end
-spec result_set(FirstId, LastId, FirstIndexI, CountI) -> elem() when
    FirstId :: binary() | undefined,
    LastId  :: binary() | undefined,
    FirstIndexI :: non_neg_integer() | undefined,
    CountI      :: non_neg_integer() | undefined.
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

borders_decode(QueryEl) ->
    AfterID  = tag_id(QueryEl, <<"after_id">>),
    BeforeID = tag_id(QueryEl, <<"before_id">>),
    FromID   = tag_id(QueryEl, <<"from_id">>),
    ToID     = tag_id(QueryEl, <<"to_id">>),
    borders(AfterID, BeforeID, FromID, ToID).

borders(undefined, undefined, undefined, undefined) ->
    undefined;
borders(AfterID, BeforeID, FromID, ToID) ->
    #mam_borders{
        after_id  = AfterID,
        before_id = BeforeID,
        from_id   = FromID,
        to_id     = ToID
    }.

tag_id(QueryEl, Name) ->
    BExtMessID = xml:get_tag_attr_s(Name, QueryEl),
    maybe_external_binary_to_mess_id(BExtMessID).

decode_optimizations(QueryEl) ->
    case {xml:get_subtag(QueryEl, <<"simple">>),
          xml:get_subtag(QueryEl, <<"opt_count">>)} of
        {false, false} -> false;
        {false, _}     -> opt_count;
        _              -> true
    end.


%% -----------------------------------------------------------------------
%% JID serialization

-spec jid_to_opt_binary(UserJID, JID) -> OptJID when
    UserJID :: #jid{},
    JID :: #jid{},
    OptJID  :: binary().
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

-spec expand_minified_jid(UserJID, OptJID) -> BinJID when
    UserJID :: #jid{},
    OptJID  :: binary(),
    BinJID  :: binary().
expand_minified_jid(#jid{lserver=LServer, luser=LUser}, <<>>) ->
    <<LUser/binary, $@, LServer/binary>>;
expand_minified_jid(#jid{lserver=LServer, luser=LUser}, <<$/, LResource/binary>>) ->
    <<LUser/binary, $@, LServer/binary, $/, LResource/binary>>;
expand_minified_jid(UserJID, Encoded) ->
    Part = binary:match(Encoded, [<<$@>>, <<$/>>, <<$:>>]),
    expand_minified_jid_2(Part, UserJID, Encoded).

expand_minified_jid_2(nomatch,  #jid{lserver=ThisServer}, LUser) ->
    <<LUser/binary, $@, ThisServer/binary>>;
expand_minified_jid_2({Pos, 1}, #jid{lserver=ThisServer}, Encoded) ->
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
    UserJID = jlib:binary_to_jid(<<"alice@room">>),
    [?_assertEqual(JID,
        expand_minified_jid(UserJID,
            jid_to_opt_binary(UserJID, jlib:binary_to_jid(JID))))
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
    code:add_path(filename:join([EJ, "..", "stringprep", "ebin"])),
    ok = application:start(stringprep).

is_loaded_application(AppName) when is_atom(AppName) ->
    lists:keymember(AppName, 1, application:loaded_applications()).

-endif.

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


apply_start_border(undefined, StartID) ->
    StartID;
apply_start_border(#mam_borders{after_id=AfterID, from_id=FromID}, StartID) ->
    maybe_max(maybe_next_id(AfterID), maybe_max(FromID, StartID)).
                                        
apply_end_border(undefined, EndID) ->
    EndID;
apply_end_border(#mam_borders{before_id=BeforeID, to_id=ToID}, EndID) ->
    maybe_min(maybe_previous_id(BeforeID), maybe_min(ToID, EndID)).

maybe_min(undefined, Y) ->
    Y;
maybe_min(X, undefined) ->
    X;
maybe_min(X, Y) ->
    min(X, Y). 


maybe_max(undefined, Y) ->
    Y;
maybe_max(X, undefined) ->
    X;
maybe_max(X, Y) ->
    max(X, Y). 

maybe_next_id(undefined) ->
    undefined;
maybe_next_id(X) ->
    X + 1.

maybe_previous_id(undefined) ->
    undefined;
maybe_previous_id(X) ->
    X - 1.


%% -----------------------------------------------------------------------
%% Ejabberd

-ifdef(MAM_COMPACT_FORWARDED).

send_message(_From, To, Mess) ->
    {value, BFrom} = xml:get_tag_attr(<<"from">>, Mess),
    From = jlib:binary_to_jid(BFrom),
    ejabberd_sm:route(From, To, Mess).

-else.

send_message(From, To, Mess) ->
    ejabberd_sm:route(From, To, Mess).

-endif.

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

