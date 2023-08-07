-module(inbox_helper).

-include_lib("escalus/include/escalus.hrl").
-include_lib("escalus/include/escalus_xmlns.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("exml/include/exml.hrl").
-include_lib("inbox.hrl").
-include_lib("eunit/include/eunit.hrl").

% Generic inbox
-export([
         skip_or_run_inbox_tests/1,
         maybe_run_in_parallel/1,
         inbox_opts/0,
         inbox_opts/1,
         inbox_modules/1,
         muclight_modules/0,
         clear_inbox_all/0,
         foreach_check_inbox/4,
         check_inbox/2, check_inbox/3, check_inbox/4,
         get_inbox/2, get_inbox/3,
         reset_inbox/2,
         get_result_el/2,
         get_inbox_form_stanza/0,
         make_inbox_stanza/0,
         make_inbox_stanza/1,
         make_inbox_stanza/2,
         make_reset_inbox_stanza/1,
         get_error_message/1,
         inbox_ns/0,
         inbox_ns_conversation/0,
         reload_inbox_option/2, reload_inbox_option/3,
         restore_inbox_option/1,
         timestamp_from_item/1,
         assert_has_no_stanzas/1,
         assert_invalid_inbox_form_value_error/3,
         assert_invalid_reset_inbox/4,
         assert_message_content/3,
         assert_invalid_form/4,
         check_result/2,
         maybe_make_queryid/1,
         maybe_check_queryid/2
        ]).
% 1-1 helpers
-export([
         given_conversations_between/2,
         send_msg/2,
         send_msg/3,
         send_and_mark_msg/2,
         send_and_mark_msg/3
        ]).
% MUC + MUC Light helpers
-export([
         create_room/2,
         create_room/3,
         create_room_and_check_inbox/3,
         create_room_send_msg_check_inbox/5,
         enter_room/2, enter_room/4,
         leave_room/3,
         make_members/3,
         mark_last_muclight_message/2,
         mark_last_muclight_message/3,
         nick/1,
         verify_is_none_aff_change/2,
         wait_for_groupchat_msg/1
        ]).
% Misc
-export([
         parse_form_iq/1,
         muclight_domain/0,
         muclight_config_domain/0,
         muc_domain/0,
         domain/0,
         to_bare_lower/1,
         extract_user_specs/1
        ]).

-import(muc_helper, [foreach_recipient/2]).
-import(muc_light_helper, [lbin/1]).
-import(distributed_helper, [subhost_pattern/1]).
-import(config_parser_helper, [mod_config/2]).

-define(NS_ESL_INBOX, <<"erlang-solutions.com:xmpp:inbox:0">>).
-define(NS_ESL_INBOX_CONVERSATION, <<"erlang-solutions.com:xmpp:inbox:0#conversation">>).

-type inbox_query_params() :: #{
        order => asc | desc | undefined, % by timestamp
        start => binary() | undefined, % ISO timestamp
        'end' => binary() | undefined, % ISO timestamp
        box => all | inbox | archive | other,
        archive => boolean()
       }.

-type inbox_check_params() :: #{
        case_sensitive => boolean(), % should jids be checked case sensitively
        check_resource => boolean() % should resource be verified
       }.

-type inbox_result_params() :: #{
        count => integer(),
        unread_messages => integer(),
        active_conversations => integer()
       }.

-type jid_verify_fun() :: fun((InnerMsg :: exml:element(),
                               Expected :: binary(),
                               AttrName :: binary()) -> any() | no_return()).

-type inbox_result() :: #{respond_iq := exml:element(),
                          respond_messages := [exml:element()]}.

-type conv() :: #conv{}.

%% ---------------------------------------------------------
%% Generic inbox
%% ---------------------------------------------------------

-spec inbox_ns() -> binary().
inbox_ns() ->
    ?NS_ESL_INBOX.

-spec inbox_ns_conversation() -> binary().
inbox_ns_conversation() ->
    ?NS_ESL_INBOX_CONVERSATION.

inbox_opts() ->
    config_parser_helper:default_mod_config(mod_inbox).

inbox_opts(regular) ->
    DefOps = #{boxes := Boxes} = inbox_opts(),
    DefOps#{boxes := Boxes ++ [<<"other">>]};
inbox_opts(async_pools) ->
    DefOps = #{boxes := Boxes} = inbox_opts(),
    DefOps#{backend => rdbms_async,
            async_writer => #{pool_size => 1},
            boxes => Boxes ++ [<<"other">>]}.

skip_or_run_inbox_tests(TestCases) ->
    case (not ct_helper:is_ct_running())
            orelse mongoose_helper:is_rdbms_enabled(domain()) of
        true -> TestCases;
        false -> {skip, require_rdbms}
    end.

maybe_run_in_parallel(Gs) ->
    %% These could be parallel but it seems like mssql CI can't handle the load
    case distributed_helper:rpc(
           distributed_helper:mim(), mongoose_rdbms, db_engine, [domain_helper:host_type()]) of
        odbc -> Gs;
        _ -> insert_parallels(Gs)
    end.

insert_parallels(Gs) ->
    Fun = fun({muclight_config, Conf, Tests}) ->
                  {muclight_config, Conf, Tests};
             ({bin, Conf, Tests}) ->
                  {bin, Conf, Tests};
             ({regular, Conf, Tests}) ->
                  {regular, Conf, Tests};
             ({async_pools, Conf, Tests}) ->
                  {async_pools, Conf, Tests};
             ({Group, Conf, Tests}) ->
                  {Group, [parallel | Conf], Tests}
          end,
    lists:map(Fun, Gs).

inbox_modules(Backend) ->
    [
     {mod_inbox, inbox_opts(Backend)}
    ].

muclight_modules() ->
    [
     {mod_muc_light, mod_config(mod_muc_light, #{backend => rdbms})}
    ].

foreach_check_inbox(Users, Unread, SenderJid, Msg) ->
    [begin
         UserJid = to_bare_lower(U),
         check_inbox(U, [#conv{unread = Unread, from = SenderJid, to = UserJid, content = Msg}])
     end || U <- Users].

-spec check_inbox(Client :: escalus:client(), Convs :: [conv()]) -> inbox_result().
check_inbox(Client, Convs) ->
    check_inbox(Client, Convs, #{}, #{}).

-spec check_inbox(escalus:client(), [conv()], inbox_query_params()) -> inbox_result().
check_inbox(Client, Convs, QueryOpts) ->
    check_inbox(Client, Convs, QueryOpts, #{}).

-spec check_inbox(Client :: escalus:client(),
                  Convs :: [conv()],
                  QueryOpts :: inbox_query_params(),
                  CheckOpts :: inbox_check_params()) -> inbox_result().
check_inbox(Client, Convs, QueryOpts, CheckOpts) ->
    ExpectedSortedConvs = case maps:get(order, QueryOpts, undefined) of
                              asc -> lists:reverse(Convs);
                              _ -> Convs
                          end,
    F = fun(#{respond_messages := ResultStanzas} = InboxResult) ->
                do_check_inbox(Client, QueryOpts, CheckOpts, ResultStanzas, ExpectedSortedConvs),
                InboxResult
        end,
    get_inbox(Client, QueryOpts, #{count => length(ExpectedSortedConvs)}, F).

do_check_inbox(Client, QueryOpts, CheckOpts, ResultStanzas, ExpectedSortedConvs) ->
    try
        check_inbox_result(Client, QueryOpts, CheckOpts, ResultStanzas, ExpectedSortedConvs)
    catch
        _:Reason:StackTrace ->
            error(#{ reason => inbox_mismatch,
                     inbox_items => lists:map(fun exml:to_binary/1, ResultStanzas),
                     expected_items => lists:map(fun pretty_conv/1, ExpectedSortedConvs),
                     query_params => QueryOpts,
                     check_params => CheckOpts,
                     error => Reason,
                     stacktrace => StackTrace })
    end.

check_inbox_result(Client, QueryOpts, CheckOpts, ResultStanzas, MsgCheckList) ->
    Merged = lists:zip(ResultStanzas, MsgCheckList),
    JIDVerifyFun = check_jid_fun(maps:get(case_sensitive, CheckOpts, false),
                                 maps:get(check_resource, CheckOpts, true)),
    lists:foreach(fun({ResultConvStanza, ExpectedConv}) ->
                          process_inbox_message(Client, ResultConvStanza, ExpectedConv, JIDVerifyFun, QueryOpts)
                  end, Merged),
    ResultStanzas.

process_inbox_message(Client, #xmlel{children = [Children]} = Message, #conv{unread = Unread, from = From, to = To,
                                             content = Content, verify = Fun}, JIDVerifyFun, QueryOpts) ->
    FromJid = ensure_conv_binary_jid(From),
    ToJid = ensure_conv_binary_jid(To),
    escalus:assert(is_message, Message),
    Unread = get_unread_count(Message),
    [InnerMsg] = get_inner_msg(Message),
    JIDVerifyFun(InnerMsg, FromJid, <<"from">>),
    JIDVerifyFun(InnerMsg, ToJid, <<"to">>),
    maybe_check_queryid(Children, QueryOpts),
    InnerContent = exml_query:path(InnerMsg, [{element, <<"body">>}, cdata], <<>>),
    Content = InnerContent,
    case Fun of
        F when is_function(F, 2) ->
            Fun(Client, InnerMsg);
        F when is_function(F, 3) ->
            Fun(Client, InnerMsg, Message)
    end,
    ok.

maybe_check_queryid(Children, #{iq_id := IqId}) ->
    ?assertEqual(IqId, exml_query:attr(Children, <<"queryid">>));
maybe_check_queryid(Children, #{queryid := QueryId}) ->
    ?assertEqual(QueryId, exml_query:attr(Children, <<"queryid">>));
maybe_check_queryid(_Children, #{}) ->
    ok.

-spec get_inbox(escalus:client(), inbox_result_params()) -> inbox_result().
get_inbox(Client, ExpectedResult) ->
    get_inbox(Client, #{}, ExpectedResult).

-spec get_inbox(escalus:client(), inbox_query_params(), inbox_result_params()) -> inbox_result().
get_inbox(Client, GetParams, ExpectedResult) ->
    get_inbox(Client, GetParams, ExpectedResult, fun(R) -> R end).

-spec get_inbox(Client :: escalus:client(),
                GetParams :: inbox_query_params(),
                ExpectedResult :: inbox_result_params(),
                Check :: fun((inbox_result()) -> inbox_result())) -> inbox_result().
get_inbox(Client, GetParams, #{count := ExpectedCount} = ExpectedResult, Check) ->
    GetInbox = make_inbox_stanza(GetParams),
    Validator = fun(#{respond_messages := Val}) -> ExpectedCount =:= length(Val) end,
    {ok, Inbox} = mongoose_helper:wait_until(
                    fun() -> Check(do_get_inbox(Client, GetInbox)) end,
                    Validator, #{name => inbox_size}),
    #{respond_iq := ResIQ} = Inbox,
    ?assert(escalus_pred:is_iq_result(GetInbox, ResIQ)),
    check_result(ResIQ, ExpectedResult),
    Inbox.

do_get_inbox(Client, GetInbox) ->
    escalus:send(Client, GetInbox),
    [ResIQ | Messages] = lists:reverse(receive_inbox(Client, GetInbox)),
    #{respond_iq => ResIQ,
      respond_messages => lists:reverse(Messages)}.

receive_inbox(Client, GetInbox) ->
    S = escalus:wait_for_stanza(Client),
    case escalus_pred:is_iq_error(GetInbox, S) of
        true -> ct:fail("Unexpected error IQ: ~p", [S]);
        false -> ok
    end,
    case escalus_pred:is_iq_result(GetInbox, S) of
        true  -> [S];
        false -> [S | receive_inbox(Client, GetInbox)]
    end.

get_unread_count(Msg) ->
    [Val] = exml_query:paths(Msg, [{element, <<"result">>}, {attr, <<"unread">>}]),
    binary_to_integer(Val).

get_result_el(Packet, Element) ->
    Val = exml_query:path(Packet, [{element, <<"fin">>}, {element, Element}, cdata]),
    case Val of
        <<>> ->
            ct:fail(#{ error => Element,
                       stanza => Packet });
        undefined ->
            io:format(Element);
        _ ->
            binary_to_integer(Val)
    end.

check_result(Packet, ExpectedResult) ->
    maps:filter(fun(K, V) ->
                        V == get_result_el(Packet, key_to_binary(K))
                end,
                ExpectedResult).

maybe_make_queryid(iq_id) ->
    #{iq_id => base16:encode(crypto:strong_rand_bytes(16))};
maybe_make_queryid(queryid) ->
    #{queryid => base16:encode(crypto:strong_rand_bytes(16))};
maybe_make_queryid(undefined) ->
    #{}.

timestamp_from_item(Item) ->
    ISOTStamp = exml_query:path(Item, [{element, <<"result">>}, {element, <<"forwarded">>},
                                       {element, <<"delay">>}, {attr, <<"stamp">>}]),
    calendar:rfc3339_to_system_time(binary_to_list(ISOTStamp), [{unit, microsecond}]).

clear_inbox_all() ->
    clear_inboxes([alice, bob, kate, mike]).

clear_inboxes(UserList) ->
    HostType = domain_helper:host_type(mim),
    Usernames = [{escalus_users:get_username(escalus_users:get_users(UserList),U),
                  escalus_users:get_server(escalus_users:get_users(UserList),U)}
                 || U <- UserList],
    [escalus_ejabberd:rpc(mod_inbox_utils, clear_inbox, [HostType, User, Server]) || {User, Server} <- Usernames].

reload_inbox_option(Config, KeyValueList) ->
    HostType = domain_helper:host_type(mim),
    Args = proplists:get_value(inbox_opts, Config),
    Args1 = maps:merge(Args, maps:from_list(KeyValueList)),
    dynamic_modules:restart(HostType, mod_inbox, Args1),
    lists:keyreplace(inbox_opts, 1, Config, {inbox_opts, Args1}).

reload_inbox_option(Config, Key, Value) ->
    HostType = domain_helper:host_type(mim),
    Args = proplists:get_value(inbox_opts, Config),
    Args1 = Args#{Key => Value},
    dynamic_modules:restart(HostType, mod_inbox, Args1),
    lists:keyreplace(inbox_opts, 1, Config, {inbox_opts, Args1}).

restore_inbox_option(Config) ->
    HostType = domain_helper:host_type(mim),
    Args = proplists:get_value(inbox_opts, Config),
    dynamic_modules:restart(HostType, mod_inbox, Args).

get_inner_msg(Msg) ->
    exml_query:paths(Msg, [{element, <<"result">>}, {element, <<"forwarded">>},
                           {element, <<"message">>}]).

get_error_message(Stanza) ->
    exml_query:path(Stanza, [{element, <<"error">>}, {element, <<"text">>}, cdata]).

get_inbox_form_stanza() ->
    escalus_stanza:iq_get(?NS_ESL_INBOX, []).

-spec make_inbox_stanza() -> exml:element().
make_inbox_stanza() ->
    make_inbox_stanza(#{}).

-spec make_inbox_stanza(GetParams :: inbox_query_params()) -> exml:element().
make_inbox_stanza(GetParams) ->
    GetIQ = inbox_iq(GetParams),
    QueryTag = #xmlel{name = <<"inbox">>,
                      attrs = [{<<"xmlns">>, ?NS_ESL_INBOX} | maybe_query_params(GetParams)],
                      children = [make_inbox_form(GetParams) | rsm(GetParams)]},
    GetIQ#xmlel{children = [QueryTag]}.

-spec make_inbox_stanza(GetParams :: inbox_query_params(), Verify :: boolean()) -> exml:element().
make_inbox_stanza(GetParams, Verify) ->
    GetIQ = inbox_iq(GetParams),
    QueryTag = #xmlel{name = <<"inbox">>,
                      attrs = [{<<"xmlns">>, ?NS_ESL_INBOX} | maybe_query_params(GetParams)],
                      children = [make_inbox_form(GetParams, Verify) | rsm(GetParams)]},
    GetIQ#xmlel{children = [QueryTag]}.

inbox_iq(#{iq_id := IqId}) ->
    GetIQ = escalus_stanza:iq_set(?NS_ESL_INBOX, []),
    escalus_stanza:set_id(GetIQ, IqId);
inbox_iq(_) ->
    escalus_stanza:iq_set(?NS_ESL_INBOX, []).

maybe_query_params(#{queryid := undefined}) ->
    [];
maybe_query_params(#{queryid := QueryId}) ->
    [{<<"queryid">>, QueryId}];
maybe_query_params(_) ->
    [].

rsm(Params) ->
    Max = maps:get(limit, Params, undefined),
    Before = maps:get(before, Params, undefined),
    After = maps:get('after', Params, undefined),
    Index = maps:get(index, Params, undefined),
    Elems = [#xmlel{name = <<"max">>,
                    children = [#xmlcdata{content = to_bin(Max)}]}
             || _ <- [Max], undefined =/= Max ] ++
            [#xmlel{name = <<"index">>,
                    children = [#xmlcdata{content = to_bin(Index)}]}
             || _ <- [Index], undefined =/= Index ] ++
            [#xmlel{name = <<"before">>,
                    children = [#xmlcdata{content = to_bin(Before)}]}
             || _ <- [Before], undefined =/= Before ] ++
            [#xmlel{name = <<"after">>,
                    children = [#xmlcdata{content = to_bin(After)}]}
             || _ <- [After], undefined =/= After ],
    case Elems of
        [] -> [];
        _ -> [#xmlel{name = <<"set">>,
                     attrs = [{<<"xmlns">>, ?NS_RSM}],
                     children = Elems}]
    end.

to_bin(N) when is_integer(N) -> integer_to_binary(N);
to_bin(Bin) when is_binary(Bin) -> Bin.


-spec reset_inbox(
        escalus:client(),
        jid:jid() | escalus:client() | atom() | binary() | string())
        -> ok.
reset_inbox(From, To) ->
        ResetStanza = make_reset_inbox_stanza(To),
        escalus:send(From, ResetStanza),
        Result = escalus:wait_for_stanza(From),
        ?assert(escalus_pred:is_iq_result(ResetStanza, Result)),
        ?assertNotEqual(undefined, exml_query:path(Result, [{element_with_ns, <<"reset">>,
                                                             inbox_ns_conversation()}])).

-spec make_reset_inbox_stanza(undefined | escalus:client() | binary()) -> exml:element().
make_reset_inbox_stanza(InterlocutorJid) when is_binary(InterlocutorJid) ->
    escalus_stanza:iq(
      <<"set">>,
      [#xmlel{name = <<"reset">>,
              attrs = [
                       {<<"xmlns">>, inbox_ns_conversation()},
                       {<<"jid">>, InterlocutorJid}
                      ]}]);
make_reset_inbox_stanza(undefined) ->
    escalus_stanza:iq(
      <<"set">>,
      [#xmlel{name = <<"reset">>,
              attrs = [
                       {<<"xmlns">>, inbox_ns_conversation()}
                      ]}]);
make_reset_inbox_stanza(InterlocutorJid) ->
    make_reset_inbox_stanza(escalus_utils:get_short_jid(InterlocutorJid)).

-spec check_jid_fun(IsCaseSensitive :: boolean(), CheckResource :: boolean()) -> jid_verify_fun().
check_jid_fun(true, true) ->
    fun(InnerMsg, Expected, El) -> Expected = exml_query:attr(InnerMsg, El) end;
check_jid_fun(false, true) ->
    fun(InnerMsg, Expected0, El) ->
            Expected = lbin(Expected0),
            Expected = lbin(exml_query:attr(InnerMsg, El)) end;
check_jid_fun(true, false) ->
    fun(InnerMsg, Expected, El) ->
            NoResExpected = bin_to_bare(Expected),
            NoResExpected = bin_to_bare(exml_query:attr(InnerMsg, El))
    end;
check_jid_fun(false, false) ->
    fun(InnerMsg, Expected, El) ->
            NoResExpected0 = escalus_client:short_jid(Expected),
            NoResExpected = lbin(NoResExpected0),
            NoResExpected = lbin(exml_query:attr(InnerMsg, El)) end.

bin_to_bare(Jid) ->
    case binary:split(Jid, <<"/">>) of
        [Bare, _Res] -> Bare;
        [Bare] -> Bare
    end.

-spec make_inbox_form(GetParams :: inbox_query_params()) -> exml:element().
make_inbox_form(GetParams) ->
    make_inbox_form(GetParams, true).

-spec make_inbox_form(GetParams :: inbox_query_params(), Verify :: boolean()) -> exml:element().
make_inbox_form(GetParams, true) ->
    BoxL = case maps:get(box, GetParams, both) of
               both -> [];
               Box -> [escalus_stanza:field_el(<<"box">>, <<"list-single">>, atom_to_binary(Box))]
           end,
    Archive = case maps:get(archive, GetParams, none) of
                  none -> [];
                  true -> [escalus_stanza:field_el(<<"archive">>, <<"boolean">>, <<"true">>)];
                  false -> [escalus_stanza:field_el(<<"archive">>, <<"boolean">>, <<"false">>)]
              end,
    OrderL = case maps:get(order, GetParams, undefined) of
                 undefined -> [];
                 Order -> [escalus_stanza:field_el(<<"order">>, <<"list-single">>, order_to_bin(Order))]
             end,
    StartL = case maps:get(start, GetParams, undefined) of
                 undefined -> [];
                 Start -> [escalus_stanza:field_el(<<"start">>, <<"text-single">>, Start)]
             end,
    EndL = case maps:get('end', GetParams, undefined) of
               undefined -> [];
               End -> [escalus_stanza:field_el(<<"end">>, <<"text-single">>, End)]
           end,
    FormTypeL = [escalus_stanza:field_el(<<"FORM_TYPE">>, <<"hidden">>, ?NS_ESL_INBOX)],
    HiddenReadL = [escalus_stanza:field_el(<<"hidden_read">>, <<"text-single">>,
                                           bool_to_bin(maps:get(hidden_read, GetParams, false)))],
    Fields = FormTypeL ++ BoxL ++ Archive ++ OrderL ++ StartL ++ EndL ++ HiddenReadL,
    escalus_stanza:x_data_form(<<"submit">>, Fields);

make_inbox_form(GetParams, false) ->
    Map = maps:map(fun (K, V) ->
                           escalus_stanza:field_el(K, <<"text-single">>, V) end,
                   GetParams),
    Fields = maps:values(Map),
    escalus_stanza:x_data_form(<<"submit">>, Fields).
%% ---------------------------------------------------------
%% 1-1 helpers
%% ---------------------------------------------------------
order_to_bin(asc) -> <<"asc">>;
order_to_bin(desc) -> <<"desc">>.

bool_to_bin(true) -> <<"true">>;
bool_to_bin(false) -> <<"false">>.

-spec given_conversations_between(From :: escalus:client(), ToList :: [escalus:client()]) ->
    #{ escalus:client() => [#conv{}] }.
given_conversations_between(From, ToList) ->
    lists:foldl(fun(N, #{ From := FromConvs } = Convs) ->
                        Ord = integer_to_binary(N),
                        ToClient = lists:nth(N, ToList),
                        Body = <<"Msg ", Ord/binary>>,
                        Msg0 = escalus_stanza:chat_to(ToClient, Body),
                        Msg = escalus_stanza:setattr(Msg0, <<"xmlns">>, ?NS_JABBER_CLIENT),
                        escalus:send(From, Msg),
                        Incoming = escalus:wait_for_stanza(ToClient),
                        escalus:assert(is_chat_message, Incoming),
                        VerifyXMLNSFun
                            = fun(_, InnerMsg) ->
                                      ?NS_JABBER_CLIENT = exml_query:attr(InnerMsg, <<"xmlns">>)
                              end,
                        NewConv = #conv{ from = From, to = ToClient,
                                         content = Body, time_after = server_side_time(),
                                         verify = VerifyXMLNSFun },
                        Convs#{
                          From := [NewConv#conv{ unread = 0 } | FromConvs],
                          ToClient => [NewConv#conv{ unread = 1 }]
                         }
                end, #{ From => [], time_before => server_side_time() },
                lists:seq(1, length(ToList))).

%% ---------------------------------------------------------
%% MUC + MUC Light helpers
%% ---------------------------------------------------------

leave_room(User, Room, Occupants) ->
    UnavailavbleStanza = escalus_stanza:presence(<<"unavailable">>),
    Stanza = muc_helper:stanza_to_room(UnavailavbleStanza, Room, nick(User)),
    escalus:send(User, Stanza),
    lists:foreach(fun escalus:wait_for_stanza/1, Occupants).

wait_for_groupchat_msg(Users) ->
    [escalus:assert(is_groupchat_message, escalus:wait_for_stanza(User)) || User <- Users].

make_members(Room, Admin, Users) ->
    Items = lists:map(fun(User) -> {escalus_utils:get_short_jid(User),<<"member">>} end,
                      Users),
    escalus:send(Admin, stanza_set_affiliations(Room, Items)),
    % gets iq result and affs changes from all users
    escalus:wait_for_stanzas(Admin, 1 + length(Users)),
    % Everybody gets aff changes of everybody
    lists:foreach(fun(User) -> escalus:wait_for_stanzas(User, length(Users)) end, Users).

% All users enter the room
enter_room(Room, Users) ->
    lists:foreach(fun(User) ->
                          escalus:send(User, stanza_muc_enter_room(Room, nick(User))) end,
                  Users),
    lists:foreach(fun(User) ->
                          escalus:wait_for_stanzas(User, length(Users) + 1) % everybody gets presence from everybody + a subject message
                  end,
                  Users).

% `User` enters the room `Room` where `Users` are occupants
enter_room(Room, User, Users, DelayedMessagesCount) ->
    escalus:send(User, stanza_muc_enter_room(Room, nick(User))),
    lists:foreach(fun escalus:wait_for_stanza/1, Users),
    escalus:wait_for_stanzas(User, length(Users) + 1 + 1 + DelayedMessagesCount). % User gets subject message and presences from everybody including himself

stanza_set_affiliations(Room, List) ->
    Payload = lists:map(fun aff_to_iq_item/1, List),
    muc_helper:stanza_to_room(escalus_stanza:iq_set(?NS_MUC_ADMIN, Payload), Room).

aff_to_iq_item({JID, Affiliation}) ->
    #xmlel{name = <<"item">>,
        attrs = [{<<"jid">>, JID}, {<<"affiliation">>, Affiliation}]}.

nick(User) -> escalus_utils:get_username(User).

stanza_muc_enter_room(Room, Nick) ->
    stanza_to_room(
        escalus_stanza:presence(<<"available">>,
                                [#xmlel{name = <<"x">>,
                                        attrs=[{<<"xmlns">>, <<"http://jabber.org/protocol/muc">>}]}
                                ]),
        Room, Nick).

stanza_to_room(Stanza, Room, Nick) ->
    escalus_stanza:to(Stanza, muc_helper:room_address(Room, Nick)).

create_room(Owner, MemberList) ->
    RoomName = pubsub_tools:pubsub_node_name(),
    create_room(Owner, MemberList, RoomName).

create_room(Owner, MemberList, RoomName) ->
    InitOccupants = [{M, member} || M <- MemberList],
    MembersAndOwner = [Owner | MemberList],
    muc_light_helper:given_muc_light_room(RoomName, Owner, InitOccupants),
    [begin mark_last_muclight_system_message(U, 1),
           foreach_recipient(MembersAndOwner, fun(_Stanza) -> ok end) end || U <- MembersAndOwner],
    RoomName.

create_room_and_check_inbox(Owner, MemberList, RoomName) ->
    InitOccupants = [{M, member} || M <- MemberList],
    FinalOccupants = [{Owner, owner} | InitOccupants],
    InitConfig = [{<<"roomname">>, <<"Just room name">>}],
    OwnerJid = lbin(escalus_client:short_jid(Owner)),
    MembersJids = [lbin(escalus_client:short_jid(M)) || M <- MemberList],
    MemberAndJids = lists:zip(MemberList, MembersJids),
    MembersAndOwner = [Owner | MemberList],
    %% Owner creates room
    escalus:send(Owner, muc_light_helper:stanza_create_room(RoomName, InitConfig, InitOccupants)),
    muc_light_helper:verify_aff_bcast(FinalOccupants, FinalOccupants),
    escalus:assert(is_iq_result, escalus:wait_for_stanza(Owner)),
    %% check for the owner. Unread from owner is affiliation change to owner
    check_inbox(Owner,[#conv{unread = 1,
                             from = muc_light_helper:room_bin_jid(RoomName),
                             to = OwnerJid,
                             verify = fun verify_is_owner_aff_change/2}]),
    %% check for the members. Every member has affiliation change to member
    [check_inbox(Member, [#conv{unread = 1,
                                from = muc_light_helper:room_bin_jid(RoomName),
                                to = Jid,
                                verify = fun verify_is_member_aff_change/2}])
     || {Member, Jid} <- MemberAndJids],
    %% Each room participant send chat marker
    [begin mark_last_muclight_system_message(U, 1),
           foreach_recipient(MembersAndOwner, fun(_Stanza) -> ok end) end || U <- MembersAndOwner],
    %% counter is reset for owner
    check_inbox(Owner, [#conv{unread = 0,
                              from = muc_light_helper:room_bin_jid(RoomName),
                              to = OwnerJid,
                              verify = fun verify_is_owner_aff_change/2}]),
    %% counter is reset for members
    [check_inbox(Member, [#conv{unread = 0,
                                from = muc_light_helper:room_bin_jid(RoomName),
                                to = Jid,
                                verify = fun verify_is_member_aff_change/2}])
     || {Member, Jid} <- MemberAndJids].

%% assume there is only one conversation
mark_last_muclight_message(User, AllUsers) ->
    mark_last_muclight_message(User, AllUsers, <<"displayed">>).

mark_last_muclight_message(User, AllUsers, MarkerType) ->
    %% User ask for inbox in order to get id of last message
    GetInbox = make_inbox_stanza(),
    escalus:send(User, GetInbox),
    Stanza = escalus:wait_for_stanza(User),
    ResIQ = escalus:wait_for_stanza(User),
    1 = get_result_el(ResIQ, <<"count">>),
    [InnerMsg] = get_inner_msg(Stanza),
    MsgId = exml_query:attr(InnerMsg, <<"id">>),
    From = exml_query:attr(InnerMsg, <<"from">>),
    FromBare = escalus_utils:get_short_jid(From),
    ChatMarkerWOType = escalus_stanza:chat_marker(FromBare, MarkerType, MsgId),
    ChatMarker = escalus_stanza:setattr(ChatMarkerWOType, <<"type">>, <<"groupchat">>),
    %% User marks last message
    escalus:send(User, ChatMarker),
    %% participants receive marker
    foreach_recipient(AllUsers, fun(Marker) ->
                                        true = escalus_pred:is_chat_marker(MarkerType, MsgId, Marker)
                                end).

mark_last_muclight_system_message(User, ExpectedCount) ->
    mark_last_muclight_system_message(User, ExpectedCount, <<"displayed">>).

mark_last_muclight_system_message(User, ExpectedCount, MarkerType) ->
    #{respond_messages := Stanzas} = get_inbox(User, #{}, #{count => ExpectedCount}),
    LastMsg = lists:last(Stanzas),
    [InnerMsg] = get_inner_msg(LastMsg),
    MsgId = exml_query:attr(InnerMsg, <<"id">>),
    From = exml_query:attr(InnerMsg, <<"from">>),
    ChatMarkerWOType = escalus_stanza:chat_marker(From, MarkerType, MsgId),
    ChatMarker = escalus_stanza:setattr(ChatMarkerWOType, <<"type">>, <<"groupchat">>),
    escalus:send(User, ChatMarker).

create_room_send_msg_check_inbox(Owner, MemberList, RoomName, Msg, Id) ->
    RoomJid = muc_light_helper:room_bin_jid(RoomName),
    OwnerJid = lbin(escalus_client:short_jid(Owner)),
    create_room_and_check_inbox(Owner, MemberList, RoomName),
    Stanza = escalus_stanza:set_id(
               escalus_stanza:groupchat_to(RoomJid, Msg), Id),
    escalus:send(Owner, Stanza),
    foreach_recipient([Owner | MemberList],
                      fun(ReceivedStanza) ->
                              escalus:assert(is_groupchat_message, ReceivedStanza)
                      end),
    %% send chat marker per each
    OwnerRoomJid = <<RoomJid/binary,"/", OwnerJid/binary>>,
    %% Owner sent the message so he has unread set to 0
    check_inbox(Owner, [#conv{unread = 0, from = OwnerRoomJid, to = OwnerJid, content = Msg}]),
    foreach_check_inbox(MemberList, 1, OwnerRoomJid, Msg),
    RoomJid.

verify_is_owner_aff_change(Client, Msg) ->
    verify_muc_light_aff_msg(Msg, [{Client,  owner}]).

verify_is_member_aff_change(Client, Msg) ->
    verify_muc_light_aff_msg(Msg, [{Client, member}]).

verify_is_none_aff_change(Client, Msg) ->
    verify_muc_light_aff_msg(Msg, [{Client, none}]).

verify_muc_light_aff_msg(Msg, AffUsersChanges) ->
    BinAffUsersChanges = muc_light_helper:bin_aff_users(AffUsersChanges),
    ProperNS = muc_light_helper:ns_muc_light_affiliations(),
    SubEl = exml_query:path(Msg, [{element_with_ns, ProperNS}]),
    undefined = exml_query:subelement(Msg, <<"prev-version">>),
    Items = exml_query:subelements(SubEl, <<"user">>),
    muc_light_helper:verify_aff_users(Items, BinAffUsersChanges).

%% ---------------------------------------------------------
%% Misc
%% ---------------------------------------------------------
parse_form_iq(IQ) ->
    FieldsEls = exml_query:paths(IQ, [{element, <<"query">>},
                                      {element, <<"x">>},
                                      {element, <<"field">>}]),
    lists:foldl(fun parse_form_field/2, #{ field_count => length(FieldsEls) }, FieldsEls).

parse_form_field(FieldEl, Acc0) ->
    Var = exml_query:attr(FieldEl, <<"var">>),
    Type = exml_query:attr(FieldEl, <<"type">>),
    Value = case exml_query:path(FieldEl, [{element, <<"value">>}, cdata]) of
                undefined -> exml_query:attr(FieldEl, <<"value">>);
                Val -> Val
            end,
    Info0 = #{ type => Type, value => Value },
    Info1 =
    case Type of
        <<"list-single">> ->
            Info0#{ options => exml_query:paths(FieldEl, [{element, <<"option">>},
                                                          {element, <<"value">>},
                                                          cdata]) };
        _ ->
            Info0
    end,
    Acc0#{ Var => Info1 }.


-spec muclight_domain() -> binary().
muclight_domain() ->
    Domain = domain(),
    <<"muclight.", Domain/binary>>.

-spec muclight_config_domain() -> binary().
muclight_config_domain() ->
    Domain = <<"@HOST@">>,
    <<"muclight.", Domain/binary>>.

-spec muc_domain() -> binary().
muc_domain() ->
    Domain = domain(),
    <<"muc.", Domain/binary>>.

-spec domain() -> binary().
domain() ->
    ct:get_config({hosts, mim, domain}).

-spec to_bare_lower(User :: escalus:client()) -> binary().
to_bare_lower(User) ->
    lbin(escalus_client:short_jid(User)).

%% Returns mim1-side time in ISO format
-spec server_side_time() -> binary().
server_side_time() ->
    USec = escalus_ejabberd:rpc(erlang, system_time, [microsecond]),
    TS = calendar:system_time_to_rfc3339(USec, [{offset, "Z"}, {unit, microsecond}]),
    list_to_binary(TS).

%% ---------------------------------------------------------
%% Error reporting
%% ---------------------------------------------------------

pretty_conv(#conv{ unread = Unread, from = From, to = To, content = Content, verify = Fun }) ->
    #{
      unread => Unread,
      from => ensure_conv_binary_jid(From),
      to => ensure_conv_binary_jid(To),
      content => Content,
      verify => Fun
    }.

ensure_conv_binary_jid(BinJid) when is_binary(BinJid) ->
    BinJid;
ensure_conv_binary_jid(Client) ->
    lbin(escalus_client:full_jid(Client)).

key_to_binary(unread_messages) ->
    <<"unread-messages">>;
key_to_binary(active_conversations) ->
    <<"active-conversations">>;
key_to_binary(count) ->
    <<"count">>.

send_msg(From, To) ->
    send_msg(From, To, "Test").

send_msg(From, To, Body) ->
    MsgId = escalus_stanza:id(),
    Msg = escalus_stanza:set_id(escalus_stanza:chat_to(To, Body), MsgId),
    escalus:send(From, Msg),
    MsgSent = escalus:wait_for_stanza(To),
    escalus:assert(is_chat_message, MsgSent),
    MsgSent.


send_and_mark_msg(From, To) ->
    send_and_mark_msg(From, To, "Test").

send_and_mark_msg(From, To, Body) ->
    Msg = send_msg(From, To, Body),
    MsgId = exml_query:attr(Msg, <<"id">>),
    ChatMarker = escalus_stanza:chat_marker(From, <<"displayed">>, MsgId),
    escalus:send(To, ChatMarker),
    Msg.

assert_has_no_stanzas(UsersList) when is_list(UsersList) ->
    lists:foreach(fun(User) -> ?assertNot(escalus_client:has_stanzas(User)) end, UsersList);
assert_has_no_stanzas(User) ->
    ?assertNot(escalus_client:has_stanzas(User)).

assert_invalid_reset_inbox(From, To, Field, Value) ->
    ResetStanza = make_reset_inbox_stanza(To),
    assert_invalid_form(From, ResetStanza, Field, Value).

assert_invalid_inbox_form_value_error(User, Field, Value) ->
    Stanza = make_inbox_stanza( #{ Field => Value }, false),
    assert_invalid_form(User, Stanza, Field, Value).

assert_invalid_form(User, Stanza, Field, Value) ->
    escalus:send(User, Stanza),
    [ResIQ] = escalus:wait_for_stanzas(User, 1),
    escalus_pred:is_iq_error(ResIQ),
    ErrorMsg = get_error_message(ResIQ),
    assert_message_content(ErrorMsg, Field, Value).

assert_message_content(Msg, Field, Value) ->
    ?assertNotEqual(nomatch, binary:match(Msg, Field)),
    ?assertNotEqual(nomatch, binary:match(Msg, Value)).

%% TODO: properly extract the specs from Bob
extract_user_specs(User) ->
    {client,_,_,_,_,UserSpecs} = User,
    {User, UserSpecs}.
