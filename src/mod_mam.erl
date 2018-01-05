%%%-------------------------------------------------------------------
%%% @author Uvarov Michael <arcusfelis@gmail.com>
%%% @copyright (C) 2013, Uvarov Michael
%%% @doc XEP-0313: Message Archive Management
%%%
%%% The module uses several backend modules:
%%%
%%% <ul>
%%% <li>Preference manager ({@link mod_mam_muc_odbc_prefs});</li>
%%% <li>Writer ({@link mod_mam_muc_odbc_arch} or {@link mod_mam_muc_odbc_async_pool_writer});</li>
%%% <li>Archive manager ({@link mod_mam_muc_odbc_arch});</li>
%%% <li>User's ID generator ({@link mod_mam_muc_user}).</li>
%%% </ul>
%%%
%%% Preferencies can be also stored in Mnesia ({@link mod_mam_mnesia_prefs}).
%%% This module handles simple archives.
%%%
%%% This module should be started for each host.
%%% Message archivation is not shaped here (use standard support for this).
%%% MAM's IQs are shaped inside {@link shaper_srv}.
%%%
%%% Message identifiers (or UIDs in the spec) are generated based on:
%%%
%%% <ul>
%%% <li>date (using `timestamp()');</li>
%%% <li>node number (using {@link ejabberd_node_id}).</li>
%%% </ul>
%%% @end
%%%-------------------------------------------------------------------
-module(mod_mam).
-behavior(gen_mod).
-xep([{xep, 313}, {version, "0.2"}]).
-xep([{xep, 313}, {version, "0.3"}]).
-xep([{xep, 313}, {version, "0.4.1"}]).
-xep([{xep, 313}, {version, "0.5"}]).
-xep([{xep, 313}, {version, "0.6"}]).
%% ----------------------------------------------------------------------
%% Exports

%% Client API
-export([delete_archive/2,
         archive_size/2,
         archive_id/2]).

%% gen_mod handlers
-export([start/2, stop/1]).

%% ejabberd handlers
-export([process_mam_iq/4,
         user_send_packet/4,
         remove_user/3,
         filter_packet/1,
         determine_amp_strategy/5,
         sm_filter_offline_message/4]).

%%private
-export([archive_message/8]).
-export([lookup_messages/2]).
-export([archive_id_int/2]).

%% ----------------------------------------------------------------------
%% Imports

-import(mod_mam_utils,
        [microseconds_to_now/1]).

%% UID
-import(mod_mam_utils,
        [generate_message_id/0,
         decode_compact_uuid/1]).

%% XML
-import(mod_mam_utils,
        [maybe_add_arcid_elems/5,
         wrap_message/6,
         result_set/4,
         result_query/2,
         result_prefs/4,
         make_fin_message/5,
         make_fin_element/4,
         parse_prefs/1,
         borders_decode/1,
         is_mam_result_message/1]).

%% Forms
-import(mod_mam_utils,
        [message_form/3]).

%% Other
-import(mod_mam_utils,
        [mess_id_to_external_binary/1,
         is_last_page/4]).

%% ejabberd
-import(mod_mam_utils,
        [send_message/3,
         is_jid_in_user_roster/2]).


-include("ejabberd.hrl").
-include("jlib.hrl").
-include("amp.hrl").
-include_lib("exml/include/exml.hrl").

%% ----------------------------------------------------------------------
%% Datetime types
%% Microseconds from 01.01.1970
-type unix_timestamp() :: non_neg_integer().

%% ----------------------------------------------------------------------
%% Other types
-type archive_behaviour()   :: atom(). % roster | always | never.
-type message_id()          :: non_neg_integer().

-type archive_id()          :: non_neg_integer().

-type borders()             :: #mam_borders{}.
-type lookup_result() :: {TotalCount :: non_neg_integer() | undefined,
                          Offset :: non_neg_integer() | undefined,
                          MessageRows :: [{message_id(), jid(), jlib:xmlel()}]}.

%% Internal types
-type iterator_fun() :: fun(() -> {'ok', {_, _}}).
-type rewriter_fun() :: fun((JID :: ejabberd:literal_jid())
                            -> ejabberd:literal_jid()).
-type restore_option() :: {rewrite_jids, rewriter_fun() | [{binary(), binary()}]}
                        | new_message_ids.

-type preference() :: {DefaultMode :: archive_behaviour(),
                       AlwaysJIDs  :: [ejabberd:literal_jid()],
                       NeverJIDs   :: [ejabberd:literal_jid()]}.
-export_type([rewriter_fun/0,
              borders/0,
              preference/0,
              archive_behaviour/0,
              iterator_fun/0,
              unix_timestamp/0,
              archive_id/0,
              lookup_result/0,
              message_id/0,
              restore_option/0
             ]).

%% ----------------------------------------------------------------------
%% API

-spec delete_archive(ejabberd:server(), ejabberd:user()) -> 'ok'.
delete_archive(Server, User)
  when is_binary(Server), is_binary(User) ->
    ?DEBUG("Remove user ~p from ~p.", [User, Server]),
    ArcJID = jid:make(User, Server, <<>>),
    Host = server_host(ArcJID),
    ArcID = archive_id_int(Host, ArcJID),
    remove_archive(Host, ArcID, ArcJID),
    ok.


-spec archive_size(ejabberd:server(), ejabberd:user()) -> integer().
archive_size(Server, User)
  when is_binary(Server), is_binary(User) ->
    ArcJID = jid:make(User, Server, <<>>),
    Host = server_host(ArcJID),
    ArcID = archive_id_int(Host, ArcJID),
    archive_size(Host, ArcID, ArcJID).


-spec archive_id(ejabberd:server(), ejabberd:user()) -> integer().
archive_id(Server, User)
  when is_binary(Server), is_binary(User) ->
    ArcJID = jid:make(User, Server, <<>>),
    Host = server_host(ArcJID),
    archive_id_int(Host, ArcJID).

%% gen_mod callbacks
%% Starting and stopping functions for users' archives

-spec start(Host :: ejabberd:server(), Opts :: list()) -> any().
start(Host, Opts) ->
    ?DEBUG("mod_mam starting", []),
    case gen_mod:get_opt(archive_groupchats, Opts, undefined) of
        undefined ->
            ?WARNING_MSG("mod_mam is enabled without explicit archive_groupchats option value."
                         " It will default to `false` in one of future releases."
                         " Please check the mod_mam documentation for more details.", []);
        _ ->
            ok
    end,
    case gen_mod:get_opt(add_archived_element, Opts, undefined) of
        undefined -> ok;
        _ ->
            mongoose_deprecations:log(mam02, "<archived/> element is going to be removed in release 3.0.0"
                         " It is not recommended to use it."
                                             " Consider using a <stanza-id/> element instead")
    end,

    ejabberd_users:start(Host),
    %% `parallel' is the only one recommended here.
    IQDisc = gen_mod:get_opt(iqdisc, Opts, parallel), %% Type
    mod_disco:register_feature(Host, ?NS_MAM),
    mod_disco:register_feature(Host, ?NS_MAM_03),
    mod_disco:register_feature(Host, ?NS_MAM_04),
    mod_disco:register_feature(Host, ?NS_MAM_06),
    gen_iq_handler:add_iq_handler(ejabberd_sm, Host, ?NS_MAM,
                                  ?MODULE, process_mam_iq, IQDisc),
    gen_iq_handler:add_iq_handler(ejabberd_sm, Host, ?NS_MAM_03,
                                  ?MODULE, process_mam_iq, IQDisc),
    gen_iq_handler:add_iq_handler(ejabberd_sm, Host, ?NS_MAM_04,
                                  ?MODULE, process_mam_iq, IQDisc),
    gen_iq_handler:add_iq_handler(ejabberd_sm, Host, ?NS_MAM_06,
                                  ?MODULE, process_mam_iq, IQDisc),
    ejabberd_hooks:add(user_send_packet, Host, ?MODULE, user_send_packet, 90),
    ejabberd_hooks:add(rest_user_send_packet, Host, ?MODULE, user_send_packet, 90),
    ejabberd_hooks:add(filter_local_packet, Host, ?MODULE, filter_packet, 90),
    ejabberd_hooks:add(remove_user, Host, ?MODULE, remove_user, 50),
    ejabberd_hooks:add(anonymous_purge_hook, Host, ?MODULE, remove_user, 50),
    ejabberd_hooks:add(amp_determine_strategy, Host, ?MODULE, determine_amp_strategy, 20),
    ejabberd_hooks:add(sm_filter_offline_message, Host, ?MODULE, sm_filter_offline_message, 50),
    mongoose_metrics:ensure_metric(Host, [backends, ?MODULE, lookup], histogram),
    mongoose_metrics:ensure_metric(Host, [Host, modMamLookups, simple], spiral),
    mongoose_metrics:ensure_metric(Host, [backends, ?MODULE, archive], histogram),
    ok.


-spec stop(Host :: ejabberd:server()) -> any().
stop(Host) ->
    ?DEBUG("mod_mam stopping", []),
    ejabberd_hooks:delete(sm_filter_offline_message, Host, ?MODULE, sm_filter_offline_message, 50),
    ejabberd_hooks:delete(user_send_packet, Host, ?MODULE, user_send_packet, 90),
    ejabberd_hooks:delete(rest_user_send_packet, Host, ?MODULE, user_send_packet, 90),
    ejabberd_hooks:delete(filter_local_packet, Host, ?MODULE, filter_packet, 90),
    ejabberd_hooks:delete(remove_user, Host, ?MODULE, remove_user, 50),
    ejabberd_hooks:delete(anonymous_purge_hook, Host, ?MODULE, remove_user, 50),
    ejabberd_hooks:delete(amp_determine_strategy, Host, ?MODULE, determine_amp_strategy, 20),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host, ?NS_MAM),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host, ?NS_MAM_03),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host, ?NS_MAM_04),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host, ?NS_MAM_06),
    mod_disco:unregister_feature(Host, ?NS_MAM),
    mod_disco:unregister_feature(Host, ?NS_MAM_03),
    mod_disco:unregister_feature(Host, ?NS_MAM_04),
    mod_disco:unregister_feature(Host, ?NS_MAM_06),
    ok.

%% ----------------------------------------------------------------------
%% hooks and handlers

%% `To' is an account or server entity hosting the archive.
%% Servers that archive messages on behalf of local users SHOULD expose archives
%% to the user on their bare JID (i.e. `From.luser'),
%% while a MUC service might allow MAM queries to be sent to the room's bare JID
%% (i.e `To.luser').
-spec process_mam_iq(From :: ejabberd:jid(), To :: ejabberd:jid(), Acc :: mongoose_acc:t(),
                     IQ :: ejabberd:iq()) -> {mongoose_acc:t(), ejabberd:iq() | ignore}.
process_mam_iq(From=#jid{lserver=Host}, To, Acc, IQ) ->
    Action = mam_iq:action(IQ),
    Res = case is_action_allowed(Action, From, To) of
        true  ->
            case mod_mam_utils:wait_shaper(Host, Action, From) of
                ok ->
                    handle_error_iq(Host, To, Action,
                                    handle_mam_iq(Action, From, To, IQ));
                {error, max_delay_reached} ->
                    ?WARNING_MSG("issue=max_delay_reached, action=~p, host=~p, from=~p",
                                 [Action, Host, From]),
                    ejabberd_hooks:run(mam_drop_iq, Host,
                                       [Host, To, IQ, Action, max_delay_reached]),
                    return_max_delay_reached_error_iq(IQ)
            end;
        false ->
            ejabberd_hooks:run(mam_drop_iq, Host,
                               [Host, To, IQ, Action, action_not_allowed]),
            return_action_not_allowed_error_iq(IQ)
    end,
    {Acc, Res}.


%% @doc Handle an outgoing message.
%%
%% Note: for outgoing messages, the server MUST use the value of the 'to'
%%       attribute as the target JID.
-spec user_send_packet(Acc :: map(), From :: ejabberd:jid(),
                       To :: ejabberd:jid(),
                       Packet :: jlib:xmlel()) -> map().
user_send_packet(Acc, From, To, Packet) ->
    ?DEBUG("Send packet~n    from ~p ~n    to ~p~n    packet ~p.",
           [From, To, Packet]),
    handle_package(outgoing, false, From, To, From, Packet),
    Acc.


%% @doc Handle an incoming message.
%%
%% Note: For incoming messages, the server MUST use the value of the
%%       'from' attribute as the target JID.
%%
%% Return drop to drop the packet, or the original input to let it through.
%% From and To are jid records.
-type fpacket() :: {From :: ejabberd:jid(),
                    To :: ejabberd:jid(),
                    Acc :: mongoose_acc:t(),
                    Packet :: xmlel()}.
-spec filter_packet(Value :: fpacket() | drop) -> fpacket() | drop.
filter_packet(drop) ->
    drop;
filter_packet({From, To=#jid{luser=LUser, lserver=LServer}, Acc, Packet}) ->
    % let them to their amp-related mambo jumbo on stanza
    ?DEBUG("Receive packet~n    from ~p ~n    to ~p~n    packet ~p.",
           [From, To, Packet]),
    {AmpEvent, PacketAfterArchive} =
        case ejabberd_users:does_user_exist(LUser, LServer) of
            false ->
                {mam_failed, Packet};
            true ->
                PacketWithoutAmp = mod_amp:strip_amp_el_from_request(Packet),
                case process_incoming_packet(From, To, PacketWithoutAmp) of
                    undefined -> {mam_failed, Packet};
                    MessID -> {archived, maybe_add_arcid_elems(To, MessID, Packet,
                                                               add_archived_element(LServer),
                                                               add_stanzaid_element(LServer))}
                end
        end,
    Acc1 = mongoose_acc:put(element, PacketAfterArchive, Acc),
    Acc2 = mod_amp:check_packet(Acc1, From, AmpEvent),
    {From, To, Acc, mongoose_acc:get(element, Acc2)}.


process_incoming_packet(From, To, Packet) ->
    handle_package(incoming, true, To, From, From, Packet).

%% @doc A ejabberd's callback with diferent order of arguments.
-spec remove_user(mongoose_acc:t(), ejabberd:user(), ejabberd:server()) -> mongoose_acc:t().
remove_user(Acc, User, Server) ->
    delete_archive(Server, User),
    Acc.

sm_filter_offline_message(_Drop=false, _From, _To, Packet) ->
    %% If ...
    is_mam_result_message(Packet);
    %% ... than drop the message
sm_filter_offline_message(Other, _From, _To, _Packet) ->
    Other.

%% ----------------------------------------------------------------------
%% Internal functions

-spec server_host(ejabberd:jid()) -> ejabberd:lserver().
server_host(#jid{lserver=LServer}) ->
    LServer.


-spec is_action_allowed(Action :: mam_iq:action(), From :: ejabberd:jid(),
                        To :: ejabberd:jid()) -> boolean().
is_action_allowed(Action, From, To=#jid{lserver=Host}) ->
    case acl:match_rule(Host, Action, From, default) of
        allow   -> true;
        deny    -> false;
        default -> is_action_allowed_by_default(Action, From, To)
    end.

-spec is_action_allowed_by_default(Action :: mam_iq:action(), From :: ejabberd:jid(),
                                   To :: ejabberd:jid()) -> boolean().
is_action_allowed_by_default(_Action, From, To) ->
    compare_bare_jids(From, To).


-spec compare_bare_jids(ejabberd:simple_jid() | ejabberd:jid(),
                        ejabberd:simple_jid() | ejabberd:jid()) -> boolean().
compare_bare_jids(JID1, JID2) ->
    jid:to_bare(JID1) =:= jid:to_bare(JID2).

-spec handle_mam_iq('mam_get_prefs', From :: ejabberd:jid(), To :: ejabberd:jid(),
                    IQ :: ejabberd:iq()) -> ejabberd:iq() | {error, term(), ejabberd:iq()}.
handle_mam_iq(Action, From, To, IQ) ->
    case Action of
        mam_get_prefs ->
            handle_get_prefs(To, IQ);
        mam_set_prefs ->
            handle_set_prefs(To, IQ);
        mam_lookup_messages ->
            handle_lookup_messages(From, To, IQ);
        mam_set_message_form ->
            handle_set_message_form(From, To, IQ);
        mam_get_message_form ->
            handle_get_message_form(From, To, IQ);
        mam_purge_single_message ->
            handle_purge_single_message(To, IQ);
        mam_purge_multiple_messages ->
            handle_purge_multiple_messages(To, IQ)
    end.

-spec handle_set_prefs(ejabberd:jid(), ejabberd:iq()) ->
                              ejabberd:iq() | {error, term(), ejabberd:iq()}.
handle_set_prefs(ArcJID=#jid{},
                 IQ=#iq{sub_el = PrefsEl}) ->
    {DefaultMode, AlwaysJIDs, NeverJIDs} = parse_prefs(PrefsEl),
    ?DEBUG("Parsed data~n\tDefaultMode ~p~n\tAlwaysJIDs ~p~n\tNeverJIDS ~p~n",
           [DefaultMode, AlwaysJIDs, NeverJIDs]),
    Host = server_host(ArcJID),
    ArcID = archive_id_int(Host, ArcJID),
    Res = set_prefs(Host, ArcID, ArcJID, DefaultMode, AlwaysJIDs, NeverJIDs),
    handle_set_prefs_result(Res, DefaultMode, AlwaysJIDs, NeverJIDs, IQ).

handle_set_prefs_result(ok, DefaultMode, AlwaysJIDs, NeverJIDs, IQ) ->
    Namespace = IQ#iq.xmlns,
    ResultPrefsEl = result_prefs(DefaultMode, AlwaysJIDs, NeverJIDs, Namespace),
    IQ#iq{type = result, sub_el = [ResultPrefsEl]};
handle_set_prefs_result({error, Reason},
                        _DefaultMode, _AlwaysJIDs, _NeverJIDs, IQ) ->
    return_error_iq(IQ, Reason).


-spec handle_get_prefs(ejabberd:jid(), IQ :: ejabberd:iq()) ->
                              ejabberd:iq() | {error, term(), ejabberd:iq()}.
handle_get_prefs(ArcJID=#jid{}, IQ=#iq{}) ->
    Host = server_host(ArcJID),
    ArcID = archive_id_int(Host, ArcJID),
    Res = get_prefs(Host, ArcID, ArcJID, always),
    handle_get_prefs_result(Res, IQ).

handle_get_prefs_result({DefaultMode, AlwaysJIDs, NeverJIDs}, IQ) ->
    ?DEBUG("Extracted data~n\tDefaultMode ~p~n\tAlwaysJIDs ~p~n\tNeverJIDS ~p~n",
           [DefaultMode, AlwaysJIDs, NeverJIDs]),
    Namespace = IQ#iq.xmlns,
    ResultPrefsEl = result_prefs(DefaultMode, AlwaysJIDs, NeverJIDs, Namespace),
    IQ#iq{type = result, sub_el = [ResultPrefsEl]};
handle_get_prefs_result({error, Reason}, IQ) ->
    return_error_iq(IQ, Reason).


-spec handle_lookup_messages(From :: ejabberd:jid(), ArcJID :: ejabberd:jid(),
                             IQ :: ejabberd:iq()) ->
                                    ejabberd:iq() | {error, term(), ejabberd:iq()}.
handle_lookup_messages(#jid{} = From, #jid{} = ArcJID,
                       #iq{xmlns=MamNs, sub_el = QueryEl} = IQ) ->
    Host = server_host(ArcJID),
    ArcID = archive_id_int(Host, ArcJID),
    QueryID = exml_query:attr(QueryEl, <<"queryid">>, <<>>),
    ExtraParamsModule = param(Host, extra_lookup_params, undefined),
    Params0 = mam_iq:query_to_lookup_params(IQ, param(Host, max_result_limit, 50),
                                            param(Host, default_result_limit, 50),
                                            ExtraParamsModule),
    Params = mam_iq:lookup_params_with_archive_details(Params0, ArcID, ArcJID),
    case lookup_messages(Host, Params) of
        {error, 'policy-violation'} ->
            ?DEBUG("Policy violation by ~p.", [jid:to_binary(From)]),
            ErrorEl = jlib:stanza_errort(<<"">>, <<"modify">>, <<"policy-violation">>,
                                         <<"en">>, <<"Too many results">>),
            IQ#iq{type = error, sub_el = [ErrorEl]};
        {error, Reason} ->
            report_issue(Reason, mam_lookup_failed, ArcJID, IQ),
            return_error_iq(IQ, Reason);
        {ok, {TotalCount, Offset, MessageRows}} ->
            {FirstMessID, LastMessID} = forward_messages(From, ArcJID, MamNs,
                                                         QueryID, MessageRows, false),
            ResultSetEl = result_set(FirstMessID, LastMessID, Offset, TotalCount),
            ResultQueryEl = result_query(ResultSetEl, MamNs),
            %% On receiving the query, the server pushes to the client a series of
            %% messages from the archive that match the client's given criteria,
            %% and finally returns the <iq/> result.
            IQ#iq{type = result, sub_el = [ResultQueryEl]}
    end.


-spec handle_set_message_form(From :: ejabberd:jid(), ArcJID :: ejabberd:jid(),
                              IQ :: ejabberd:iq()) ->
    ejabberd:iq() | ignore | {error, term(), ejabberd:iq()}.
handle_set_message_form(#jid{} = From, #jid{} = ArcJID,
                        #iq{xmlns=MamNs, sub_el = QueryEl} = IQ) ->
    Host = server_host(ArcJID),
    ArcID = archive_id_int(Host, ArcJID),
    QueryID = exml_query:attr(QueryEl, <<"queryid">>, <<>>),
    ExtraParamsModule = param(Host, extra_lookup_params, undefined),
    Params0 = mam_iq:form_to_lookup_params(IQ, param(Host, max_result_limit, 50),
                                           param(Host, default_result_limit, 50),
                                           ExtraParamsModule),
    Params = mam_iq:lookup_params_with_archive_details(Params0, ArcID, ArcJID),
    PageSize = maps:get(page_size, Params),
    case lookup_messages(Host, Params) of
        {error, Reason} ->
            report_issue(Reason, mam_lookup_failed, ArcJID, IQ),
            return_error_iq(IQ, Reason);
        {ok, {TotalCount, Offset, MessageRows}} when IQ#iq.xmlns =:= ?NS_MAM_03 ->
            ResIQ = IQ#iq{type = result, sub_el = []},
            %% Server accepts the query
            ejabberd_router:route(ArcJID, From, jlib:iq_to_xml(ResIQ)),

            {FirstMessID, LastMessID} = forward_messages(From, ArcJID, MamNs,
                                                         QueryID, MessageRows, true),

            %% Make fin message
            IsLastPage = is_last_page(PageSize, TotalCount, Offset, MessageRows),
            IsStable = true,
            ResultSetEl = result_set(FirstMessID, LastMessID, Offset, TotalCount),
            FinMsg = make_fin_message(IQ#iq.xmlns, IsLastPage, IsStable, ResultSetEl, QueryID),
            ejabberd_sm:route(ArcJID, From, FinMsg),

            %% IQ was sent above
            ignore;
        {ok, {TotalCount, Offset, MessageRows}} ->
            %% Forward messages
            {FirstMessID, LastMessID} = forward_messages(From, ArcJID, MamNs,
                                                         QueryID, MessageRows, true),
            %% Make fin iq
            IsLastPage = is_last_page(PageSize, TotalCount, Offset, MessageRows),
            IsStable = true,
            ResultSetEl = result_set(FirstMessID, LastMessID, Offset, TotalCount),
            FinElem = make_fin_element(IQ#iq.xmlns, IsLastPage, IsStable, ResultSetEl),
            IQ#iq{type = result, sub_el = [FinElem]}
    end.

forward_messages(From, ArcJID, MamNs, QueryID, MessageRows, SetClientNs) ->
    %% Forward messages
    {FirstMessID, LastMessID} =
    case MessageRows of
        [] -> {undefined, undefined};
        [_|_] -> {message_row_to_ext_id(hd(MessageRows)),
                  message_row_to_ext_id(lists:last(MessageRows))}
    end,

    [send_message(ArcJID, From,
                  message_row_to_xml(MamNs, M, QueryID, SetClientNs))
     || M <- MessageRows],
    {FirstMessID, LastMessID}.

-spec handle_get_message_form(ejabberd:jid(), ejabberd:jid(), ejabberd:iq()) ->
                                     ejabberd:iq().
handle_get_message_form(_From=#jid{lserver = Host}, _ArcJID=#jid{}, IQ=#iq{}) ->
    return_message_form_iq(Host, IQ).


%% @doc Purging multiple messages
-spec handle_purge_multiple_messages(ejabberd:jid(), IQ :: ejabberd:iq()) ->
                                            ejabberd:iq() | {error, term(), ejabberd:iq()}.
handle_purge_multiple_messages(ArcJID=#jid{},
                               IQ=#iq{sub_el = PurgeEl}) ->
    Now = p1_time_compat:system_time(micro_seconds),
    Host = server_host(ArcJID),
    ArcID = archive_id_int(Host, ArcJID),
    %% Filtering by date.
    %% Start :: integer() | undefined
    Start = mam_iq:elem_to_start_microseconds(PurgeEl),
    End   = mam_iq:elem_to_end_microseconds(PurgeEl),
    %% Set borders.
    Borders = borders_decode(PurgeEl),
    %% Filtering by contact.
    With  = mam_iq:elem_to_with_jid(PurgeEl),
    Res = purge_multiple_messages(Host, ArcID, ArcJID, Borders,
                                  Start, End, Now, With),
    return_purge_multiple_message_iq(IQ, Res).


-spec handle_purge_single_message(ejabberd:jid(), IQ :: ejabberd:iq()) ->
                                         ejabberd:iq() | {error, term(), ejabberd:iq()}.
handle_purge_single_message(ArcJID=#jid{},
                            IQ=#iq{sub_el = PurgeEl}) ->
    Now = p1_time_compat:system_time(micro_seconds),
    Host = server_host(ArcJID),
    ArcID = archive_id_int(Host, ArcJID),
    BExtMessID = exml_query:attr(PurgeEl, <<"id">>, <<>>),
    MessID = mod_mam_utils:external_binary_to_mess_id(BExtMessID),
    PurgingResult = purge_single_message(Host, MessID, ArcID, ArcJID, Now),
    return_purge_single_message_iq(IQ, PurgingResult).

determine_amp_strategy(Strategy = #amp_strategy{deliver = [none]},
                       FromJID, ToJID, Packet, initial_check) ->
    #jid{luser = LUser, lserver = LServer} = ToJID,
    ShouldBeStored = is_archivable_message(LServer, incoming, Packet)
        andalso is_interesting(ToJID, FromJID)
        andalso ejabberd_auth:is_user_exists(LUser, LServer),
    case ShouldBeStored of
        true -> Strategy#amp_strategy{deliver = [stored, none]};
        false -> Strategy
    end;
determine_amp_strategy(Strategy, _, _, _, _) ->
    Strategy.

-spec handle_package(Dir :: incoming | outgoing, ReturnMessID :: boolean(),
                     LocJID :: ejabberd:jid(), RemJID :: ejabberd:jid(), SrcJID :: ejabberd:jid(),
                     Packet :: jlib:xmlel()) -> MaybeMessID :: binary() | undefined.
handle_package(Dir, ReturnMessID,
               LocJID = #jid{},
               RemJID = #jid{},
               SrcJID = #jid{}, Packet) ->
    Host = server_host(LocJID),
    case is_archivable_message(Host, Dir, Packet)
         andalso should_archive_if_groupchat(Host, exml_query:attr(Packet, <<"type">>)) of
        true ->
            ArcID = archive_id_int(Host, LocJID),
            case is_interesting(Host, LocJID, RemJID, ArcID) of
                true ->
                    MessID = generate_message_id(),
                    Result = archive_message(Host, MessID, ArcID,
                                             LocJID, RemJID, SrcJID, Dir, Packet),
                    return_external_message_id_if_ok(ReturnMessID, Result, MessID);
                false ->
                    undefined
            end;
        false ->
            undefined
    end.

should_archive_if_groupchat(Host, <<"groupchat">>) ->
    gen_mod:get_module_opt(Host, ?MODULE, archive_groupchats, true);
should_archive_if_groupchat(_, _) ->
    true.

-spec return_external_message_id_if_ok(ReturnMessID :: boolean(),
                                       ArchivingResult :: ok | any(),
                                       MessID :: integer()) -> binary() | undefined.
return_external_message_id_if_ok(true, ok, MessID) -> mess_id_to_external_binary(MessID);
return_external_message_id_if_ok(_, _, _MessID) -> undefined.

is_interesting(LocJID, RemJID) ->
    Host = server_host(LocJID),
    ArcID = archive_id_int(Host, LocJID),
    is_interesting(Host, LocJID, RemJID, ArcID).

is_interesting(Host, LocJID, RemJID, ArcID) ->
    case get_behaviour(Host, ArcID, LocJID, RemJID, always) of
        always -> true;
        never  -> false;
        roster -> is_jid_in_user_roster(LocJID, RemJID)
    end.

%% ----------------------------------------------------------------------
%% Backend wrappers

-spec archive_id_int(ejabberd:server(), ejabberd:jid()) ->
                            non_neg_integer() | undefined.
archive_id_int(Host, ArcJID=#jid{}) ->
    ejabberd_hooks:run_fold(mam_archive_id, Host, undefined, [Host, ArcJID]).


-spec archive_size(ejabberd:server(), archive_id(), ejabberd:jid()) -> integer().
archive_size(Host, ArcID, ArcJID=#jid{}) ->
    ejabberd_hooks:run_fold(mam_archive_size, Host, 0, [Host, ArcID, ArcJID]).


-spec get_behaviour(ejabberd:server(), archive_id(), LocJID :: ejabberd:jid(),
                    RemJID :: ejabberd:jid(), Default :: 'always') -> atom().
get_behaviour(Host, ArcID,
              LocJID=#jid{},
              RemJID=#jid{}, DefaultBehaviour) ->
    ejabberd_hooks:run_fold(mam_get_behaviour, Host, DefaultBehaviour,
                            [Host, ArcID, LocJID, RemJID]).


-spec set_prefs(ejabberd:server(), archive_id(), ArcJID :: ejabberd:jid(),
                DefaultMode :: atom(), AlwaysJIDs :: [ejabberd:literal_jid()],
                NeverJIDs :: [ejabberd:literal_jid()]) -> any().
set_prefs(Host, ArcID, ArcJID, DefaultMode, AlwaysJIDs, NeverJIDs) ->
    ejabberd_hooks:run_fold(mam_set_prefs, Host, {error, not_implemented},
                            [Host, ArcID, ArcJID, DefaultMode, AlwaysJIDs, NeverJIDs]).


%% @doc Load settings from the database.
-spec get_prefs(Host :: ejabberd:server(), ArcID :: archive_id(),
                ArcJID :: ejabberd:jid(), GlobalDefaultMode :: archive_behaviour()
               ) -> preference() | {error, Reason :: term()}.
get_prefs(Host, ArcID, ArcJID, GlobalDefaultMode) ->
    ejabberd_hooks:run_fold(mam_get_prefs, Host,
                            {GlobalDefaultMode, [], []},
                            [Host, ArcID, ArcJID]).


-spec remove_archive(ejabberd:server(), archive_id(), ejabberd:jid()) -> 'ok'.
remove_archive(Host, ArcID, ArcJID=#jid{}) ->
    ejabberd_hooks:run(mam_remove_archive, Host, [Host, ArcID, ArcJID]),
    ok.

-spec lookup_messages(Host :: ejabberd:server(),
                      Params :: map()) ->
    {ok, mod_mam:lookup_result()}
    | {error, 'policy-violation'}
    | {error, Reason :: term()}.
lookup_messages(Host, #{search_text := SearchText} = Params) ->
    case SearchText /= undefined andalso not mod_mam_utils:has_full_text_search(?MODULE, Host) of
        true -> %% Use of disabled full text search
            {error, 'not-supported'};
        false ->
            StartT = os:timestamp(),
            R = ejabberd_hooks:run_fold(mam_lookup_messages, Host, {ok, {0, 0, []}},
                                        [Host, Params]),
            Diff = timer:now_diff(os:timestamp(), StartT),
            mongoose_metrics:update(Host, [backends, ?MODULE, lookup], Diff),
            R
    end.


-spec archive_message(Host :: ejabberd:server(), MessID :: message_id(),
                      ArcID :: archive_id(), LocJID :: ejabberd:jid(), RemJID :: ejabberd:jid(),
                      SrcJID :: ejabberd:jid(), Dir :: incoming | outgoing, Packet :: term()
                     ) -> ok | {error, timeout}.
archive_message(Host, MessID, ArcID, LocJID, RemJID, SrcJID, Dir, Packet) ->
    StartT = os:timestamp(),
    R = ejabberd_hooks:run_fold(mam_archive_message, Host, ok,
                                [Host, MessID, ArcID, LocJID, RemJID, SrcJID, Dir, Packet]),
    Diff = timer:now_diff(os:timestamp(), StartT),
    mongoose_metrics:update(Host, [backends, ?MODULE, archive], Diff),
    R.

-spec purge_single_message(Host :: ejabberd:server(),
                           MessID :: message_id(), ArcID :: archive_id(),
                           ArcJID :: ejabberd:jid(),
                           Now :: unix_timestamp()) ->
                                  ok  | {error, 'not-found'}
                                      | {error, Reason :: term()}.
purge_single_message(Host, MessID, ArcID, ArcJID, Now) ->
    ejabberd_hooks:run_fold(mam_purge_single_message, Host, ok,
                            [Host, MessID, ArcID, ArcJID, Now]).

-spec purge_multiple_messages(Host :: ejabberd:server(), ArcID :: archive_id(),
                              ArcJID  :: ejabberd:jid(), Borders :: borders() | undefined,
                              Start :: unix_timestamp() | undefined,
                              End :: unix_timestamp() | undefined,
                              Now :: unix_timestamp(), WithJID :: ejabberd:jid() | undefined) ->
                                     ok | {error, Reason :: term()}.
purge_multiple_messages(Host, ArcID, ArcJID, Borders, Start, End, Now, WithJID) ->
    ejabberd_hooks:run_fold(mam_purge_multiple_messages, Host, ok,
                            [Host, ArcID, ArcJID, Borders, Start, End, Now, WithJID]).

%% ----------------------------------------------------------------------
%% Helpers

-type messid_jid_packet() :: {MessId :: integer(),
                              SrcJID :: ejabberd:jid(),
                              Packet :: jlib:xmlel()}.
-spec message_row_to_xml(binary(), messid_jid_packet(), QueryId :: binary(), boolean()) ->
    jlib:xmlel().
message_row_to_xml(MamNs, {MessID, SrcJID, Packet}, QueryID, SetClientNs) ->
    {Microseconds, _NodeMessID} = decode_compact_uuid(MessID),
    DateTime = calendar:now_to_universal_time(microseconds_to_now(Microseconds)),
    BExtMessID = mess_id_to_external_binary(MessID),
    Packet1 = mod_mam_utils:maybe_set_client_xmlns(SetClientNs, Packet),
    wrap_message(MamNs, Packet1, QueryID, BExtMessID, DateTime, SrcJID).

-spec message_row_to_ext_id(messid_jid_packet()) -> binary().
message_row_to_ext_id({MessID, _, _}) ->
    mess_id_to_external_binary(MessID).

handle_error_iq(Host, To, Action, {error, Reason, IQ}) ->
    ejabberd_hooks:run(mam_drop_iq, Host, [Host, To, IQ, Action, Reason]),
    IQ;
handle_error_iq(_Host, _To, _Action, IQ) ->
    IQ.

-spec return_action_not_allowed_error_iq(ejabberd:iq()) -> ejabberd:iq().
return_action_not_allowed_error_iq(IQ) ->
    ErrorEl = jlib:stanza_errort(<<"">>, <<"cancel">>, <<"not-allowed">>,
                                 <<"en">>, <<"The action is not allowed.">>),
    IQ#iq{type = error, sub_el = [ErrorEl]}.

return_purge_multiple_message_iq(IQ, ok) ->
    return_purge_success(IQ);
return_purge_multiple_message_iq(IQ, {error, Reason}) ->
    return_error_iq(IQ, Reason).

-spec return_purge_single_message_iq(ejabberd:iq(),
                                     ok  | {error, 'not-found'}
                                     | {error, Reason :: term()}) ->
                                            ejabberd:iq().
return_purge_single_message_iq(IQ, ok) ->
    return_purge_success(IQ);
return_purge_single_message_iq(IQ, {error, 'not-found'}) ->
    return_purge_not_found_error_iq(IQ);
return_purge_single_message_iq(IQ, {error, Reason}) ->
    return_error_iq(IQ, Reason).

-spec return_purge_success(ejabberd:iq()) -> ejabberd:iq().
return_purge_success(IQ) ->
    IQ#iq{type = result, sub_el = []}.

-spec return_purge_not_found_error_iq(ejabberd:iq()) -> ejabberd:iq().
return_purge_not_found_error_iq(IQ) ->
    %% Message not found.
    ErrorEl = jlib:stanza_errort(<<"">>, <<"cancel">>, <<"item-not-found">>,
                                 <<"en">>, <<"The provided UID did not match ",
                                             "any message stored in archive.">>),
    IQ#iq{type = error, sub_el = [ErrorEl]}.


-spec return_max_delay_reached_error_iq(ejabberd:iq()) -> ejabberd:iq().
return_max_delay_reached_error_iq(IQ) ->
    %% Message not found.
    ErrorEl = ?ERRT_RESOURCE_CONSTRAINT(
                 <<"en">>, <<"The action is cancelled because of flooding.">>),
    IQ#iq{type = error, sub_el = [ErrorEl]}.


-spec return_error_iq(ejabberd:iq(), Reason :: term()) -> {error, term(), ejabberd:iq()}.
return_error_iq(IQ, {Reason, {stacktrace, _Stacktrace}}) ->
    return_error_iq(IQ, Reason);
return_error_iq(IQ, timeout) ->
    {error, timeout, IQ#iq{type = error, sub_el = [?ERR_SERVICE_UNAVAILABLE]}};
return_error_iq(IQ, not_implemented) ->
    {error, not_implemented, IQ#iq{type = error, sub_el = [?ERR_FEATURE_NOT_IMPLEMENTED]}};
return_error_iq(IQ, Reason) ->
    {error, Reason, IQ#iq{type = error, sub_el = [?ERR_INTERNAL_SERVER_ERROR]}}.

return_message_form_iq(Host, IQ) ->
    IQ#iq{type = result, sub_el = [message_form(?MODULE, Host, IQ#iq.xmlns)]}.

report_issue({Reason, {stacktrace, Stacktrace}}, Issue, ArcJID, IQ) ->
    report_issue(Reason, Stacktrace, Issue, ArcJID, IQ);
report_issue(Reason, Issue, ArcJID, IQ) ->
    report_issue(Reason, [], Issue, ArcJID, IQ).

report_issue(timeout, _Stacktrace, _Issue, _ArcJID, _IQ) ->
    expected;
report_issue(not_implemented, _Stacktrace, _Issue, _ArcJID, _IQ) ->
    expected;
report_issue(Reason, Stacktrace, Issue, #jid{lserver=LServer, luser=LUser}, IQ) ->
    ?ERROR_MSG("issue=~p, server=~p, user=~p, reason=~p, iq=~p, stacktrace=~p",
               [Issue, LServer, LUser, Reason, IQ, Stacktrace]).


%% ----------------------------------------------------------------------
%% Dynamic params module

param(Host, Opt, Default) ->
    mod_mam_utils:param(?MODULE, Host, Opt, Default).

is_archivable_message(Host, Dir, Packet) ->
    mod_mam_utils:call_is_archivable_message(?MODULE, Host, Dir, Packet).

add_archived_element(Host) ->
    mod_mam_utils:add_archived_element(?MODULE, Host).

add_stanzaid_element(Host) ->
    mod_mam_utils:add_stanzaid_element(?MODULE, Host).
