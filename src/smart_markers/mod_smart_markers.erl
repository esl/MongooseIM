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
%%%    1) Current implementation is based on user_send_message hook.
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
%%%       guarantee one and the same message order for different users.
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
-include("mod_muc_light.hrl").
-include("mongoose_config_spec.hrl").

-xep([{xep, 333}, {version, "0.4"}]).
-behaviour(gen_mod).

%% gen_mod API
-export([start/2, stop/1, supported_features/0, config_spec/0]).

%% Internal API
-export([get_chat_markers/3]).

%% Hook handlers
-export([process_iq/5, user_send_message/3, filter_local_packet/3,
         remove_user/3, remove_domain/3, forget_room/3, room_new_affiliations/3]).
-ignore_xref([process_iq/5]).

%%--------------------------------------------------------------------
%% Type declarations
%%--------------------------------------------------------------------
-type maybe_thread() :: undefined | binary().
-type chat_type() :: one2one | groupchat.
-type chat_marker() :: #{from := jid:jid(),
                         to := jid:jid(),
                         thread := maybe_thread(), % it is not optional!
                         type := mongoose_chat_markers:chat_marker_type(),
                         timestamp := integer(), % microsecond
                         id := binary()}.

-export_type([maybe_thread/0,
              chat_marker/0]).

%% gen_mod API
-spec start(mongooseim:host_type(), gen_mod:module_opts()) -> any().
start(HostType, #{iqdisc := IQDisc, keep_private := Private} = Opts) ->
    mod_smart_markers_backend:init(HostType, Opts),
    gen_iq_handler:add_iq_handler_for_domain(
      HostType, ?NS_ESL_SMART_MARKERS, ejabberd_sm,
      fun ?MODULE:process_iq/5, #{keep_private => Private}, IQDisc),
    gen_hook:add_handlers(hooks(HostType, Opts)).

-spec stop(mongooseim:host_type()) -> ok.
stop(HostType) ->
    Opts = gen_mod:get_module_opts(HostType, ?MODULE),
    case gen_mod:get_opt(backend, Opts) of
        rdbms_async -> mod_smart_markers_rdbms_async:stop(HostType);
        _ -> ok
    end,
    gen_iq_handler:remove_iq_handler_for_domain(HostType, ?NS_ESL_SMART_MARKERS, ejabberd_sm),
    gen_hook:delete_handlers(hooks(HostType, Opts)).

-spec supported_features() -> [atom()].
supported_features() ->
    [dynamic_domains].

-spec config_spec() -> mongoose_config_spec:config_section().
config_spec() ->
    #section{
       items = #{<<"keep_private">> => #option{type = boolean},
                 <<"backend">> => #option{type = atom, validate = {enum, [rdbms, rdbms_async]}},
                 <<"async_writer">> => async_config_spec(),
                 <<"iqdisc">> => mongoose_config_spec:iqdisc()},
       defaults = #{<<"keep_private">> => false,
                    <<"backend">> => rdbms,
                    <<"iqdisc">> => no_queue}
    }.

async_config_spec() ->
    #section{
       items = #{<<"pool_size">> => #option{type = integer, validate = non_negative}},
       defaults = #{<<"pool_size">> => 2 * erlang:system_info(schedulers_online)},
       include = always
      }.

%% IQ handlers
-spec process_iq(mongoose_acc:t(), jid:jid(), jid:jid(), jlib:iq(), map()) ->
    {mongoose_acc:t(), jlib:iq()}.
process_iq(Acc, _From, _To, #iq{type = set, sub_el = SubEl} = IQ, _Extra) ->
    {Acc, IQ#iq{type = error, sub_el = [SubEl, mongoose_xmpp_errors:not_allowed()]}};
process_iq(Acc, From, _To, #iq{type = get, sub_el = SubEl} = IQ, #{keep_private := Private}) ->
    Req = maps:from_list(SubEl#xmlel.attrs),
    MaybePeer = jid:from_binary(maps:get(<<"peer">>, Req, undefined)),
    MaybeAfter = parse_ts(maps:get(<<"after">>, Req, undefined)),
    MaybeThread = maps:get(<<"thread">>, Req, undefined),
    Res = fetch_markers(IQ, Acc, From, MaybePeer, MaybeThread, MaybeAfter, Private),
    {Acc, Res}.

-spec parse_ts(undefined | binary()) -> integer() | error.
parse_ts(undefined) ->
    0;
parse_ts(BinTS) ->
    try calendar:rfc3339_to_system_time(binary_to_list(BinTS))
    catch error:_Error -> error
    end.

-spec fetch_markers(jlib:iq(),
                    mongoose_acc:t(),
                    From :: jid:jid(),
                    MaybePeer :: error | jid:jid(),
                    maybe_thread(),
                    MaybeTS :: error | integer(),
                    MaybePrivate :: boolean()) -> jlib:iq().
fetch_markers(IQ, _, _, error, _, _, _) ->
    IQ#iq{type = error,
          sub_el = [mongoose_xmpp_errors:bad_request(<<"en">>, <<"invalid-peer">>)]};
fetch_markers(IQ, _, _, _, _, error, _) ->
    IQ#iq{type = error,
          sub_el = [mongoose_xmpp_errors:bad_request(<<"en">>, <<"invalid-timestamp">>)]};
fetch_markers(IQ, Acc, From, Peer, Thread, TS, Private) ->
    HostType = mongoose_acc:host_type(Acc),
    Markers = mod_smart_markers_backend:get_conv_chat_marker(HostType, From, Peer, Thread, TS, Private),
    SubEl = #xmlel{name = <<"query">>,
                   attrs = [{<<"xmlns">>, ?NS_ESL_SMART_MARKERS},
                            {<<"peer">>, jid:to_bare_binary(Peer)}],
                   children = build_result(Markers)},
    IQ#iq{type = result, sub_el = SubEl}.

build_result(Markers) ->
    [ #xmlel{name = <<"marker">>,
             attrs = [{<<"id">>, MsgId},
                      {<<"from">>, jid:to_binary(From)},
                      {<<"type">>, atom_to_binary(Type)},
                      {<<"timestamp">>, ts_to_bin(MsgTS)}
                      | maybe_thread(MsgThread) ]}
      || #{from := From, thread := MsgThread, type := Type, timestamp := MsgTS, id := MsgId} <- Markers ].

ts_to_bin(TS) ->
    list_to_binary(calendar:system_time_to_rfc3339(TS, [{offset, "Z"}, {unit, microsecond}])).

maybe_thread(undefined) ->
    [];
maybe_thread(Bin) ->
    [{<<"thread">>, Bin}].

%% HOOKS
-spec hooks(mongooseim:host_type(), gen_mod:module_opts()) -> gen_hook:hook_list().
hooks(HostType, #{keep_private := KeepPrivate}) ->
    [{user_send_message, HostType, fun ?MODULE:user_send_message/3, #{}, 90} |
    private_hooks(HostType, KeepPrivate) ++ removal_hooks(HostType)].

private_hooks(_HostType, false) ->
    [];
private_hooks(HostType, true) ->
    [{filter_local_packet, HostType, fun ?MODULE:filter_local_packet/3, #{}, 20}].

removal_hooks(HostType) ->
    [{remove_user, HostType, fun ?MODULE:remove_user/3, #{}, 60},
     {remove_domain, HostType, fun ?MODULE:remove_domain/3, #{}, 60},
     {forget_room, HostType, fun ?MODULE:forget_room/3, #{}, 85},
     {room_new_affiliations, HostType, fun ?MODULE:room_new_affiliations/3, #{}, 60}].

-spec user_send_message(mongoose_acc:t(), mongoose_c2s_hooks:params(), gen_hook:extra()) ->
    mongoose_c2s_hooks:result().
user_send_message(Acc, _, _) ->
    {From, To, Packet} = mongoose_acc:packet(Acc),
    case has_valid_markers(Acc, From, To, Packet) of
        {true, HostType, Markers} ->
            {ok, update_chat_markers(Acc, HostType, Markers)};
        _ ->
            {ok, Acc}
    end.

-spec filter_local_packet(Acc, Params, Extra) -> {ok, Acc} | {stop, drop} when
      Acc :: mongoose_hooks:filter_packet_acc(),
      Params :: map(),
      Extra :: gen_hook:extra().
filter_local_packet(Filter = {_From, _To, _Acc, Msg = #xmlel{name = <<"message">>}}, _, _) ->
    case mongoose_chat_markers:has_chat_markers(Msg) of
        false -> {ok, Filter};
        true -> {stop, drop}
    end;
filter_local_packet(Filter, _, _) ->
    {ok, Filter}.

-spec remove_user(Acc, Params, Extra) -> {ok, Acc} when
      Acc :: mongoose_acc:t(),
      Params :: #{jid := jid:jid()},
      Extra :: gen_hook:extra().
remove_user(Acc, #{jid := #jid{luser = User, lserver = Server}}, _) ->
    HostType = mongoose_acc:host_type(Acc),
    mod_smart_markers_backend:remove_user(HostType, jid:make_bare(User, Server)),
    {ok, Acc}.

-spec remove_domain(Acc, Params, Extra) -> {ok, Acc} when
      Acc :: mongoose_domain_api:remove_domain_acc(),
      Params :: #{domain := jid:lserver()},
      Extra :: #{host_type := mongooseim:host_type()}.
remove_domain(Acc, #{domain := Domain}, #{host_type := HostType}) ->
    mod_smart_markers_backend:remove_domain(HostType, Domain),
    {ok, Acc}.

-spec forget_room(Acc, Params, Extra) -> {ok, Acc} when
      Acc :: mongoose_acc:t(),
      Params :: #{muc_host := jid:lserver(), room := jid:luser()},
      Extra :: #{host_type := mongooseim:host_type()}.
forget_room(Acc, #{muc_host := RoomS, room := RoomU}, #{host_type := HostType}) ->
    mod_smart_markers_backend:remove_to(HostType, jid:make_noprep(RoomU, RoomS, <<>>)),
    {ok, Acc}.

%% The new affs can be found in the Acc:element, where we can scan for 'none' ones
-spec room_new_affiliations(Acc, Params, Extra) -> {ok, Acc} when
      Acc :: mongoose_acc:t(),
      Params :: #{room := jid:jid()},
      Extra :: gen_hook:extra().
room_new_affiliations(Acc, #{room := RoomJID}, _) ->
    HostType = mod_muc_light_utils:acc_to_host_type(Acc),
    Packet = mongoose_acc:element(Acc),
    maybe_remove_to_for_users(Packet, RoomJID, HostType),
    {ok, Acc}.

-spec maybe_remove_to_for_users(exml:element() | undefined, jid:jid(), mongooseim:host_type()) -> ok.
maybe_remove_to_for_users(undefined, _, _) -> ok;
maybe_remove_to_for_users(Packet, RoomJID, HostType) ->
    Users = exml_query:paths(Packet, [{element_with_ns, ?NS_MUC_LIGHT_AFFILIATIONS},
                                      {element_with_attr, <<"affiliation">>, <<"none">>},
                                      cdata]),
    FromJIDs = lists:map(fun(U) -> jid:to_bare(jid:from_binary(U)) end, Users),
    RemoveFun = fun(FromJID) -> mod_smart_markers_backend:remove_to_for_user(HostType, FromJID, RoomJID) end,
    lists:foreach(RemoveFun, FromJIDs).

%%--------------------------------------------------------------------
%% Other API
%%--------------------------------------------------------------------
-spec get_chat_markers(jid:jid(), maybe_thread(), integer()) ->
    [chat_marker()].
get_chat_markers(#jid{lserver = LServer} = To, Thread, TS) ->
    %% internal API, no room access rights verification here!
    {ok, HostType} = mongoose_domain_api:get_host_type(LServer),
    mod_smart_markers_backend:get_chat_markers(HostType, To, Thread, TS).

%%--------------------------------------------------------------------
%% Local functions
%%--------------------------------------------------------------------
-spec update_chat_markers(mongoose_acc:t(), mongooseim:host_type(), [chat_marker()]) ->
    mongoose_acc:t().
update_chat_markers(Acc, HostType, Markers) ->
    TS = mongoose_acc:timestamp(Acc),
    [mod_smart_markers_backend:update_chat_marker(HostType, CM) || CM <- Markers],
    mongoose_acc:set_permanent(?MODULE, timestamp, TS, Acc).

-spec has_valid_markers(mongoose_acc:t(), jid:jid(), jid:jid(), exml:element()) ->
    false | {true, mongooseim:host_type(), Markers :: [chat_marker()]}.
has_valid_markers(Acc, From, To, Packet) ->
    case extract_chat_markers(Acc, From, To, Packet) of
        [] -> false;
        Markers ->
            case is_valid_host(Acc, From, To) of
                false -> false;
                {true, HostType} -> {true, HostType, Markers}
            end
    end.

-spec is_valid_host(mongoose_acc:t(), jid:jid(), jid:jid()) ->
    false | {true, mongooseim:host_type()}.
is_valid_host(Acc, From, To) ->
    case mongoose_acc:stanza_type(Acc) of
        <<"groupchat">> -> get_host(Acc, From, To, groupchat);
        _ -> get_host(Acc, From, To, one2one)
    end.

-spec extract_chat_markers(mongoose_acc:t(), jid:jid(), jid:jid(), exml:element()) ->
	[chat_marker()].
extract_chat_markers(Acc, From, To, Packet) ->
    case mongoose_chat_markers:list_chat_markers(Packet) of
        [] -> [];
        ChatMarkers ->
            TS = mongoose_acc:timestamp(Acc),
            CM = #{from => From,
                   to => jid:to_bare(To),
                   thread => get_thread(Packet),
                   timestamp => TS,
                   type => undefined,
                   id => undefined},
            [CM#{type => Type, id => Id} || {Type, Id} <- ChatMarkers]
    end.

-spec get_thread(exml:element()) -> maybe_thread().
get_thread(El) ->
    case exml_query:path(El, [{element, <<"thread">>}, cdata]) of
        Thread when Thread =/= <<>> -> Thread;
        _ -> undefined
    end.

-spec get_host(mongoose_acc:t(), jid:jid(), jid:jid(), chat_type()) ->
    false | {true, mongooseim:host_type()}.
get_host(Acc, From, To, groupchat) ->
    HostType = mod_muc_light_utils:room_jid_to_host_type(To),
    can_access_room(HostType, Acc, From, To) andalso {true, HostType};
get_host(_Acc, _From, To, one2one) ->
    LServer = To#jid.lserver,
    case mongoose_domain_api:get_domain_host_type(LServer) of
        {ok, HostType} -> {true, HostType};
        {error, not_found} -> false
    end.

-spec can_access_room(HostType :: mongooseim:host_type(),
                      Acc :: mongoose_acc:t(),
                      User :: jid:jid(),
                      Room :: jid:jid()) -> boolean().
can_access_room(HostType, Acc, User, Room) ->
    mongoose_hooks:can_access_room(HostType, Acc, Room, User).
