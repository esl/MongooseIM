%%%----------------------------------------------------------------------------
%%% @copyright (C) 2020, Erlang Solutions Ltd.
%%% @doc
%%%   This module implements storage of the latest chat markers
%%%   sent by the users. This can be used to optimize mod_offline
%%%   functionality, or to implement custom fetching protocol and
%%%   avoid storage of chat markers in MAM.
%%%
%%%   Please be aware of the next implementation details:
%%%
%%%    1) Current implementation is based on user_send_packet hook.
%%%       It doesn't work for s2s connections, but usage of another
%%%       hook (e.g. filter_local_packet) makes implementation harder
%%%       and results in multiple processing of one and the same
%%%       chat marker notification (sent to different users by MUC).
%%%       However that is the only possible way to deal with group
%%%       chat messages sent from the room to the user over s2s.
%%%
%%%       ```
%%%                                            S2S
%%%                                             +
%%%                                             |
%%%                +--------------------+       |
%%%                |                    |       |   filter
%%%                |                    +--------------->
%%%       send     |                    |       |   filter
%%%       +------->+       ROOM         +--------------->
%%%                |                    |       |   filter
%%%                |                    +--------------->
%%%                |                    |       |
%%%                +--------------------+       |
%%%                                             |
%%%                                             +
%%%    '''
%%%
%%%    2) DB backend requires us to provide host information, and
%%%       the host is always the recipient's server in case one2one
%%%       messages, and a master domain of the MUC service in case
%%%       of groupchat.
%%%
%%%    3) It is the client application's responsibility to ensure that
%%%       chat markers move only forward. There is no verification of
%%%       chat markers in this module, it just stores the latest chat
%%%       marker information sent by the user.
%%%
%%%    4) MUC light doesn't have message serialization! So it doesn't
%%%       guaranty one and the same message order for different users.
%%%       This can result in a race condition situation when different
%%%       users track (and mark) different messages as the last in a
%%%       chat history. However, this is a rare situation, and it self
%%%       recovers on the next message in the room. Anyway storing chat
%%%       markers in MAM doesn't fix this problem.
%%%
%%% @end
%%%----------------------------------------------------------------------------
-module(mod_smart_markers).

-include("jlib.hrl").

-xep([{xep, 333}, {version, "0.3"}]).
-behaviour(gen_mod).

%% gen_mod API
-export([start/2, stop/1]).

%% gen_mod API
-export([get_chat_markers/4]).

%% Hook handlers
-export([user_send_packet/4]).

%%--------------------------------------------------------------------
%% Type declarations
%%--------------------------------------------------------------------
-type maybe_thread() :: undefined|binary().
-type chat_marker_type() :: received | displayed | acknowledged.
-type chat_type() :: one2one | groupchat.

-type chat_marker() :: #{from := jid:jid(),
                         to := jid:jid(),
                         thread := maybe_thread(), %%it is not optional!!!
                         type := chat_marker_type(),
                         timestamp := integer(), %microsecond
                         id := binary()}.

-export_type([chat_marker/0]).

%%--------------------------------------------------------------------
%% DB backend behaviour definition
%%--------------------------------------------------------------------
-callback init(Host :: jid:lserver(), Opts :: proplists:proplist()) -> ok.

%%% 'from', 'to', 'thread' and 'type' keys of the ChatMarker map serve
%%% as a composite database key. If key is not available in the database,
%%% then chat marker must be added. Otherwise this function must update
%%% chat marker record for that composite key.
-callback update_chat_marker(Host :: jid:lserver(), ChatMarker :: chat_marker()) -> ok.

%%% This function must return the latest chat markers sent to the
%%% user/room (with or w/o thread) later than provided timestamp.
-callback get_chat_markers(Host :: jid:lserver(), To :: jid:jid(),
                           Thread :: maybe_thread(), Timestamp :: integer()) ->
                              [chat_marker()].

%%--------------------------------------------------------------------
%% gen_mod API
%%--------------------------------------------------------------------
-spec start(Host :: jid:lserver(), Opts :: proplists:proplist()) -> any().
start(Host, Opts) ->
    gen_mod:start_backend_module(?MODULE, add_default_backend(Opts),
                                 [get_chat_markers, update_chat_marker]),
    mod_smart_markers_backend:init(Host, Opts),
    ejabberd_hooks:add(hooks(Host)).

-spec stop(Host :: jid:lserver()) -> ok.
stop(Host) ->
    ejabberd_hooks:delete(hooks(Host)).

%%--------------------------------------------------------------------
%% Hook handlers
%%--------------------------------------------------------------------
-spec hooks(Host :: jid:lserver()) -> [ejabberd_hooks:hook()].
hooks(Host) ->
    [{user_send_packet, Host, ?MODULE, user_send_packet, 90}].

-spec user_send_packet(Acc :: mongoose_acc:t(), From :: jid:jid(), To :: jid:jid(),
                       Packet :: exml:element()) -> mongoose_acc:t().
user_send_packet(Acc, From, To, Packet = #xmlel{name = <<"message">>}) ->
    case is_valid_message(From, To, Packet) of
        {true, Host} ->
            maybe_update_chat_markers(Host, Acc, From, To, Packet);
        _ -> Acc
    end;
user_send_packet(Acc, _From, _To, _Packet) ->
    Acc.

%%--------------------------------------------------------------------
%% Other API
%%--------------------------------------------------------------------
-spec get_chat_markers(ChatType :: chat_type(), To :: jid:jid(),
                       Thread :: maybe_thread(), TS :: os:timestamp()) -> [chat_marker()].
get_chat_markers(ChatType, #jid{lserver = LServer} = To, Thread, TS) ->
    %% internal API, no room access rights verification here!
    Host = case ChatType of
               one2one -> LServer;
               groupchat ->
                   {ok, H} = mongoose_subhosts:get_host(LServer),
                   H
           end,
    mod_smart_markers_backend:get_chat_markers(Host, To, Thread, TS).

%%--------------------------------------------------------------------
%% Local functions
%%--------------------------------------------------------------------
-spec maybe_update_chat_markers(Host :: jid:lserver(), Acc :: mongoose_acc:t(),
                                From :: jid:jid(), To :: jid:jid(),
                                Packet :: exml:element()) -> mongoose_acc:t().
maybe_update_chat_markers(Host, Acc, From, To, Packet) ->
    TS = mongoose_acc:timestamp(Acc),
    case extract_chat_markers(TS, From, To, Packet) of
        [] -> Acc;
        ChatMarkers ->
            [mod_smart_markers_backend:update_chat_marker(Host, CM) || CM <- ChatMarkers],
            mongoose_acc:set_permanent(?MODULE, timestamp, TS, Acc)
    end.

-spec extract_chat_markers(Timestamp::integer(), From :: jid:jid(), To :: jid:jid(),
                           Packet :: exml:element()) -> [chat_marker()].
extract_chat_markers(TS, From, To, Packet) ->
    case get_chat_markers(Packet) of
        [] -> [];
        ChatMarkers ->
            CM = #{from => From, to => To, thread => get_thread(Packet), timestamp => TS},
            [CM#{type => Type, id => Id} || {Type, Id} <- ChatMarkers]
    end.

-spec get_chat_markers(exml:element()) -> [{chat_marker_type(), Id :: binary()}].
get_chat_markers(#xmlel{children = Children}) ->
    lists:filtermap(fun is_chat_marker_element/1, Children).

-spec is_chat_marker_element(exml:element()) ->
    false | {true, {chat_marker_type(), Id :: binary}}.
is_chat_marker_element(#xmlel{name = <<"received">>} = El) ->
    check_chat_marker_attributes(received, El);
is_chat_marker_element(#xmlel{name = <<"displayed">>} = El) ->
    check_chat_marker_attributes(displayed, El);
is_chat_marker_element(#xmlel{name = <<"acknowledged">>} = El) ->
    check_chat_marker_attributes(acknowledged, El);
is_chat_marker_element(_) ->
    false.

-spec check_chat_marker_attributes(chat_marker_type(), exml:element()) ->
    false | {true, {chat_marker_type(), Id :: binary()}}.
check_chat_marker_attributes(Type, El) ->
    NS = exml_query:attr(El, <<"xmlns">>),
    Id = exml_query:attr(El, <<"id">>),
    ?NS_CHAT_MARKERS =:= NS andalso Id =/= undefined andalso {true, {Type, Id}}.

-spec get_thread(exml:element()) -> maybe_thread().
get_thread(El) ->
    case exml_query:path(El, [{element, <<"thread">>}, cdata]) of
        Thread when Thread =/= <<>> -> Thread;
        _ -> undefined
    end.

-spec is_valid_message(From :: jid:jid(), To :: jid:jid(),
                       Packet :: exml:element()) -> false | {true, Host :: jid:lserver()}.
is_valid_message(From, To, Packet) ->
    case exml_query:attr(Packet, <<"type">>, undefined) of
        <<"groupchat">> ->
            can_access_room(From, To) andalso get_host(groupchat, To#jid.lserver);
        _ ->
            get_host(one2one, To#jid.lserver)
    end.

-spec get_host(chat_type(), jid:lserver()) -> false | {true, jid:lserver()}.
get_host(groupchat, SubHost) ->
    case mongoose_subhosts:get_host(SubHost) of
        undefined -> false;
        {ok, Host} -> {true, Host}
    end;
get_host(one2one, Host) ->
    Hosts = ejabberd_config:get_global_option(hosts),
    case lists:member(Host, Hosts) of
        false -> false;
        _ -> {true, Host}
    end.

-spec can_access_room(User :: jid:jid(), Room :: jid:jid()) -> boolean().
can_access_room(User, Room) ->
    mongoose_hooks:can_access_room(Room#jid.lserver, false, Room, User).

add_default_backend(Opts) ->
    case lists:keyfind(backend, 2, Opts) of
        false ->
            [{backend, rdbms} | Opts];
        _ ->
            Opts
    end.