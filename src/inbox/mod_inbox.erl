%%%-------------------------------------------------------------------
%%% @copyright (C) 2018, Erlang-Solutions
%%% @doc
%%%
%%% @end
%%% Created : 30. Jan 2018 13:22
%%%-------------------------------------------------------------------
-module(mod_inbox).

-behaviour(gen_mod).
-behaviour(mongoose_module_metrics).

-include("jlib.hrl").
-include("mongoose_rsm.hrl").
-include("mod_inbox.hrl").
-include("mongoose_config_spec.hrl").
-include("mongoose_logger.hrl").
-include("mongoose_ns.hrl").

%% gen_mod
-export([start/2, stop/1, hooks/1, config_spec/0, supported_features/0]).

-export([process_iq/5]).

%% hook handlers
-export([user_send_message/3,
         filter_local_packet/3,
         inbox_unread_count/3,
         remove_user/3,
         remove_domain/3,
         disco_local_features/3,
         get_personal_data/3
        ]).

-export([process_inbox_boxes/1]).
-export([config_metrics/1]).

-type message_type() :: one2one | groupchat.
-type entry_key() :: {LUser :: jid:luser(),
                      LServer :: jid:lserver(),
                      ToBareJid :: jid:literal_jid()}.

-type get_inbox_params() :: #{
        start => integer(),
        'end' => integer(),
        order => asc | desc,
        hidden_read => true | false,
        box => binary(),
        limit => undefined | pos_integer(),
        rsm => jlib:rsm_in(),
        filter_on_jid => binary()
       }.

-type count_res() :: ok | {ok, non_neg_integer()} | {error, term()}.
-type write_res() :: ok | {error, any()}.

-export_type([entry_key/0, entry_properties/0, get_inbox_params/0]).
-export_type([count_res/0, write_res/0, inbox_res/0]).

%%--------------------------------------------------------------------
%% gdpr callbacks
%%--------------------------------------------------------------------
-spec get_personal_data(Acc, Params, Extra) -> {ok, Acc} when
      Acc :: gdpr:personal_data(),
      Params :: #{jid := jid:jid()},
      Extra :: #{host_type := mongooseim:host_type()}.
get_personal_data(Acc, #{jid := #jid{luser = LUser, lserver = LServer}}, #{host_type := HostType}) ->
    Schema = ["jid", "content", "unread_count", "timestamp"],
    InboxParams = #{
        start => 0,
        'end' => erlang:system_time(microsecond),
        order => asc,
        hidden_read => false
       },
    Entries = mod_inbox_backend:get_inbox(HostType, LUser, LServer, InboxParams),
    ProcessedEntries = lists:map(fun process_entry/1, Entries),
    NewAcc = [{inbox, Schema, ProcessedEntries} | Acc],
    {ok, NewAcc}.

process_entry(#{remote_jid := RemJID,
                msg := Content,
                unread_count := UnreadCount,
                timestamp := Timestamp}) ->
    TS = calendar:system_time_to_rfc3339(Timestamp, [{offset, "Z"}, {unit, microsecond}]),
    {RemJID, Content, UnreadCount, TS}.

%%--------------------------------------------------------------------
%% gen_mod callbacks
%%--------------------------------------------------------------------

-spec start(mongooseim:host_type(), gen_mod:module_opts()) -> ok.
start(HostType, #{iqdisc := IQDisc, groupchat := MucTypes} = Opts) ->
    mod_inbox_backend:init(HostType, Opts),
    lists:member(muc, MucTypes) andalso mod_inbox_muc:start(HostType),
    gen_iq_handler:add_iq_handler_for_domain(HostType, ?NS_ESL_INBOX, ejabberd_sm,
                                             fun ?MODULE:process_iq/5, #{}, IQDisc),
    gen_iq_handler:add_iq_handler_for_domain(HostType, ?NS_ESL_INBOX_CONVERSATION, ejabberd_sm,
                                             fun mod_inbox_entries:process_iq_conversation/5, #{}, IQDisc),
    start_cleaner(HostType, Opts),
    ok.

-spec stop(HostType :: mongooseim:host_type()) -> ok.
stop(HostType) ->
    gen_iq_handler:remove_iq_handler_for_domain(HostType, ?NS_ESL_INBOX, ejabberd_sm),
    gen_iq_handler:remove_iq_handler_for_domain(HostType, ?NS_ESL_INBOX_CONVERSATION, ejabberd_sm),
    stop_cleaner(HostType),
    mod_inbox_muc:stop(HostType),
    case mongoose_config:get_opt([{modules, HostType}, ?MODULE, backend]) of
        rdbms_async -> mod_inbox_rdbms_async:stop(HostType);
        _ -> ok
    end.

-spec supported_features() -> [atom()].
supported_features() ->
    [dynamic_domains].

-spec config_spec() -> mongoose_config_spec:config_section().
config_spec() ->
    Markers = mongoose_chat_markers:chat_marker_names(),
    #section{
        items = #{<<"backend">> => #option{type = atom, validate = {enum, [rdbms, rdbms_async]}},
                  <<"async_writer">> => async_config_spec(),
                  <<"reset_markers">> => #list{items = #option{type = binary,
                                                               validate = {enum, Markers}}},
                  <<"groupchat">> => #list{items = #option{type = atom,
                                                           validate = {enum, [muc, muclight]}}},
                  <<"boxes">> => #list{items = #option{type = binary, validate = non_empty},
                                       validate = unique},
                  <<"bin_ttl">> => #option{type = integer, validate = non_negative},
                  <<"bin_clean_after">> => #option{type = integer, validate = non_negative,
                                                   process = fun timer:hours/1},
                  <<"delete_domain_limit">> => #option{type = int_or_infinity,
                                                       validate = positive},
                  <<"aff_changes">> => #option{type = boolean},
                  <<"remove_on_kicked">> => #option{type = boolean},
                  <<"iqdisc">> => mongoose_config_spec:iqdisc(),
                  <<"max_result_limit">> => #option{type = int_or_infinity, validate = positive}
        },
        defaults = #{<<"backend">> => rdbms,
                     <<"groupchat">> => [muclight],
                     <<"boxes">> => [],
                     <<"bin_ttl">> => 30, % 30 days
                     <<"bin_clean_after">> => timer:hours(1),
                     <<"delete_domain_limit">> => infinity,
                     <<"aff_changes">> => true,
                     <<"remove_on_kicked">> => true,
                     <<"reset_markers">> => [<<"displayed">>],
                     <<"iqdisc">> => no_queue,
                     <<"max_result_limit">> => infinity
                    },
        process = fun ?MODULE:process_inbox_boxes/1
    }.

async_config_spec() ->
    #section{
       items = #{<<"pool_size">> => #option{type = integer, validate = non_negative}},
       defaults = #{<<"pool_size">> => 2 * erlang:system_info(schedulers_online)},
       include = always
      }.

process_inbox_boxes(Config = #{boxes := Boxes}) ->
    false = lists:any(fun(<<"all">>) -> true;
                         (<<"inbox">>) -> true;
                         (<<"archive">>) -> true;
                         (<<"bin">>) -> true;
                         (_) -> false
                      end, Boxes),
    AllBoxes = [<<"inbox">>, <<"archive">>, <<"bin">> | Boxes ],
    Config#{boxes := AllBoxes}.

%% Cleaner gen_server callbacks
start_cleaner(HostType, #{bin_ttl := TTL, bin_clean_after := Interval}) ->
    WOpts = #{host_type => HostType, action => fun mod_inbox_api:flush_global_bin/2,
              opts => TTL, interval => Interval},
    mongoose_collector:start_common(?MODULE, HostType, WOpts).

stop_cleaner(HostType) ->
    Name = gen_mod:get_module_proc(HostType, ?MODULE),
    ejabberd_sup:stop_child(Name).

%%%%%%%%%%%%%%%%%%%
%% Process IQ
-spec process_iq(Acc :: mongoose_acc:t(),
                 From :: jid:jid(),
                 To :: jid:jid(),
                 IQ :: jlib:iq(),
                 Extra :: gen_hook:extra()) -> {stop, mongoose_acc:t()} | {mongoose_acc:t(), jlib:iq()}.
process_iq(Acc, _From, _To, #iq{type = get, sub_el = SubEl} = IQ, #{host_type := HostType}) ->
    Form = build_inbox_form(HostType),
    SubElWithForm = SubEl#xmlel{ children = [Form] },
    {Acc, IQ#iq{type = result, sub_el = SubElWithForm}};
process_iq(Acc, #jid{luser = LUser, lserver = LServer},
           _To, #iq{type = set, sub_el = #xmlel{name = <<"empty-bin">>}} = IQ,
           #{host_type := HostType}) ->
    TS = mongoose_acc:timestamp(Acc),
    NumRemRows = mod_inbox_backend:empty_user_bin(HostType, LServer, LUser, TS),
    {Acc, IQ#iq{type = result, sub_el = [build_empty_bin(NumRemRows)]}};
process_iq(Acc, From, _To, #iq{type = set, sub_el = QueryEl} = IQ, _Extra) ->
    HostType = mongoose_acc:host_type(Acc),
    LUser = From#jid.luser,
    LServer = From#jid.lserver,
    case query_to_params(HostType, QueryEl) of
        {error, Error, Msg} ->
            {Acc, IQ#iq{type = error, sub_el = [mongoose_xmpp_errors:Error(<<"en">>, Msg)]}};
        Params ->
            List0 = mod_inbox_backend:get_inbox(HostType, LUser, LServer, Params),
            List1 = with_rsm(List0, Params),
            List2 = mongoose_hooks:extend_inbox_result(Acc, List1, IQ),
            forward_messages(Acc, List2, IQ, From),
            Res = IQ#iq{type = result, sub_el = [build_result_iq(List2)]},
            {Acc, Res}
    end.

-spec with_rsm([inbox_res()], get_inbox_params()) -> [inbox_res()].
with_rsm(List, #{order := asc, start := TS, filter_on_jid := BinJid, rsm := #rsm_in{}}) ->
    lists:reverse(drop_filter_on_jid(List, BinJid, TS, List));
with_rsm(List, #{order := asc, rsm := #rsm_in{}}) ->
    lists:reverse(List);
with_rsm(List, #{order := desc, 'end' := TS, filter_on_jid := BinJid}) ->
    drop_filter_on_jid(List, BinJid, TS, List);
with_rsm(List, _) ->
    List.

%% As IDs must be unique but timestamps are not, and SQL queries and orders by timestamp alone,
%% we query max+1 and then match to remove the entry that matches the ID given before.
-spec drop_filter_on_jid([inbox_res()], binary(), integer(), [inbox_res()]) -> [inbox_res()].
drop_filter_on_jid(_List, BinJid, TS, [#{remote_jid := BinJid, timestamp := TS} | Rest]) ->
    Rest;
drop_filter_on_jid(List, BinJid, TS, [_ | Rest]) ->
    drop_filter_on_jid(List, BinJid, TS, Rest);
drop_filter_on_jid(List, _, _, []) ->
    List.

-spec forward_messages(Acc :: mongoose_acc:t(),
                       List :: [inbox_res()],
                       QueryEl :: jlib:iq(),
                       To :: jid:jid()) -> [mongoose_acc:t()].
forward_messages(Acc, List, QueryEl, To) when is_list(List) ->
    Msgs = [ build_inbox_message(Acc, El, QueryEl) || El <- List],
    [ send_message(Acc, To, Msg) || Msg <- Msgs].

-spec send_message(mongoose_acc:t(), jid:jid(), exml:element()) -> mongoose_acc:t().
send_message(Acc, To = #jid{lserver = LServer}, Msg) ->
    BareTo = jid:to_bare(To),
    HostType = mongoose_acc:host_type(Acc),
    NewAcc0 = mongoose_acc:new(#{location => ?LOCATION,
                                 host_type => HostType,
                                 lserver => LServer,
                                 element => Msg,
                                 from_jid => BareTo,
                                 to_jid => To}),
    PermanentFields = mongoose_acc:get_permanent_fields(Acc),
    NewAcc = mongoose_acc:set_permanent(PermanentFields, NewAcc0),
    ejabberd_sm:route(BareTo, To, NewAcc).

%%%%%%%%%%%%%%%%%%%
%% Handlers
-spec user_send_message(mongoose_acc:t(), mongoose_c2s_hooks:params(), gen_hook:extra()) ->
    mongoose_c2s_hooks:result().
user_send_message(Acc, _, _) ->
    {From, To, Msg} = mongoose_acc:packet(Acc),
    Acc1 = maybe_process_message(Acc, From, To, Msg, outgoing),
    {ok, Acc1}.

-spec inbox_unread_count(Acc, Params, Extra) -> {ok, Acc} when
      Acc :: mongoose_acc:t(),
      Params :: #{user := jid:jid()},
      Extra :: gen_hook:extra().
inbox_unread_count(Acc, #{user := User}, _) ->
    Res = mongoose_acc:get(inbox, unread_count, undefined, Acc),
    NewAcc = get_inbox_unread(Res, Acc, User),
    {ok, NewAcc}.

-spec filter_local_packet(FPacketAcc, Params, Extra) -> {ok, FPacketAcc} when
      FPacketAcc :: mongoose_hooks:filter_packet_acc(),
      Params :: map(),
      Extra :: gen_hook:extra().
filter_local_packet({From, To, Acc, Msg = #xmlel{name = <<"message">>}}, _, _) ->
    Acc0 = maybe_process_message(Acc, From, To, Msg, incoming),
    {ok, {From, To, Acc0, Msg}};
filter_local_packet(FPacketAcc, _, _) ->
    {ok, FPacketAcc}.

-spec remove_user(Acc, Params, Extra) -> {ok, Acc} when
      Acc :: mongoose_acc:t(),
      Params :: #{jid := jid:jid()},
      Extra :: gen_hook:extra().
remove_user(Acc, #{jid := #jid{luser = User, lserver = Server}}, _) ->
    HostType = mongoose_acc:host_type(Acc),
    mod_inbox_utils:clear_inbox(HostType, User, Server),
    {ok, Acc}.

-spec remove_domain(Acc, Params, Extra) -> {ok | stop, Acc} when
      Acc :: mongoose_domain_api:remove_domain_acc(),
      Params :: #{domain := jid:lserver()},
      Extra :: gen_hook:extra().
remove_domain(Acc, #{domain := Domain}, #{host_type := HostType}) ->
    F = fun() ->
            mod_inbox_backend:remove_domain(HostType, Domain),
            Acc
        end,
    mongoose_domain_api:remove_domain_wrapper(Acc, F, ?MODULE).

-spec disco_local_features(Acc, Params, Extra) -> {ok, Acc} when
      Acc :: mongoose_disco:feature_acc(),
      Params :: map(),
      Extra :: gen_hook:extra().
disco_local_features(Acc = #{node := <<>>}, _, _) ->
    {ok, mongoose_disco:add_features([?NS_ESL_INBOX], Acc)};
disco_local_features(Acc, _, _) ->
    {ok, Acc}.

-spec maybe_process_message(Acc :: mongoose_acc:t(),
                            From :: jid:jid(),
                            To :: jid:jid(),
                            Msg :: exml:element(),
                            Dir :: mod_mam_utils:direction()) -> mongoose_acc:t().
maybe_process_message(Acc, From, To, Msg, Dir) ->
    Type = mongoose_lib:get_message_type(Acc),
    case should_be_stored_in_inbox(Acc, From, To, Msg, Dir, Type) of
        true ->
            do_maybe_process_message(Acc, From, To, Msg, Dir, Type);
        false ->
            Acc
    end.

do_maybe_process_message(Acc, From, To, Msg, Dir, Type) ->
    %% In case of PgSQL we can update inbox and obtain unread_count in one query,
    %% so we put it in accumulator here.
    %% In case of MySQL/MsSQL it costs an extra query, so we fetch it only if necessary
    %% (when push notification is created)
    HostType = mongoose_acc:host_type(Acc),
    case maybe_process_acceptable_message(HostType, From, To, Msg, Acc, Dir, Type) of
        ok -> Acc;
        {ok, UnreadCount} ->
            mongoose_acc:set(inbox, unread_count, UnreadCount, Acc);
        {error, Error} ->
            HostType = mongoose_acc:host_type(Acc),
            ?LOG_WARNING(#{what => inbox_process_message_failed,
                           from_jid => jid:to_binary(From), to_jid => jid:to_binary(To),
                           host_type => HostType, dir => incoming, reason => Error}),
            Acc
    end.

-spec maybe_process_acceptable_message(
        mongooseim:host_type(), jid:jid(), jid:jid(), exml:element(),
        mongoose_acc:t(), mod_mam_utils:direction(), message_type()) ->
    count_res().
maybe_process_acceptable_message(HostType, From, To, Msg, Acc, Dir, one2one) ->
    process_message(HostType, From, To, Msg, Acc, Dir, one2one);
maybe_process_acceptable_message(HostType, From, To, Msg, Acc, Dir, groupchat) ->
    case muclight_enabled(HostType) of
        true -> process_message(HostType, From, To, Msg, Acc, Dir, groupchat);
        false -> ok
    end.

-spec process_message(HostType :: mongooseim:host_type(),
                      From :: jid:jid(),
                      To :: jid:jid(),
                      Message :: exml:element(),
                      Acc :: mongoose_acc:t(),
                      Dir :: mod_mam_utils:direction(),
                      Type :: message_type()) -> count_res().
process_message(HostType, From, To, Message, Acc, outgoing, one2one) ->
    mod_inbox_one2one:handle_outgoing_message(HostType, From, To, Message, Acc);
process_message(HostType, From, To, Message, Acc, incoming, one2one) ->
    mod_inbox_one2one:handle_incoming_message(HostType, From, To, Message, Acc);
process_message(HostType, From, To, Message, Acc, outgoing, groupchat) ->
    mod_inbox_muclight:handle_outgoing_message(HostType, From, To, Message, Acc);
process_message(HostType, From, To, Message, Acc, incoming, groupchat) ->
    mod_inbox_muclight:handle_incoming_message(HostType, From, To, Message, Acc);
process_message(HostType, From, To, Message, _TS, Dir, Type) ->
    ?LOG_WARNING(#{what => inbox_unknown_message,
                   text => <<"Unknown message was not written into inbox">>,
                   exml_packet => Message,
                   from_jid => jid:to_binary(From), to_jid => jid:to_binary(To),
                   host_type => HostType, dir => Dir, inbox_message_type => Type}),
    ok.


%%%%%%%%%%%%%%%%%%%
%% Stanza builders
build_empty_bin(Num) ->
    #xmlel{name = <<"empty-bin">>,
           attrs = [{<<"xmlns">>, ?NS_ESL_INBOX}],
           children = [#xmlel{name = <<"num">>,
                              children = [#xmlcdata{content = integer_to_binary(Num)}]}]}.

-spec build_inbox_message(mongoose_acc:t(), inbox_res(), jlib:iq()) -> exml:element().
build_inbox_message(Acc, InboxRes, IQ) ->
    #xmlel{name = <<"message">>, attrs = [{<<"id">>, mongoose_bin:gen_from_timestamp()}],
           children = [build_result_el(Acc, InboxRes, IQ)]}.

-spec build_result_el(mongoose_acc:t(), inbox_res(), jlib:iq()) -> exml:element().
build_result_el(Acc, InboxRes = #{unread_count := Count}, #iq{id = IqId, sub_el = QueryEl}) ->
    AccTS = mongoose_acc:timestamp(Acc),
    Children = mod_inbox_utils:build_inbox_result_elements(InboxRes, AccTS),
    #xmlel{name = <<"result">>,
           attrs = [{<<"xmlns">>, ?NS_ESL_INBOX},
                    {<<"unread">>, integer_to_binary(Count)},
                    {<<"queryid">>, exml_query:attr(QueryEl, <<"queryid">>, IqId)}],
           children = Children}.

-spec build_result_iq([inbox_res()]) -> exml:element().
build_result_iq(List) ->
    AllUnread = [ N || #{unread_count := N} <- List, N =/= 0],
    Result = #{<<"count">> => length(List),
               <<"unread-messages">> => lists:sum(AllUnread),
               <<"active-conversations">> => length(AllUnread)},
    ResultBinary = maps:map(fun(K, V) ->
                                    #xmlel{name = K, children = [#xmlcdata{content = integer_to_binary(V)}]}
                            end, Result),
    ResultSetEl = result_set(List),
    #xmlel{name = <<"fin">>, attrs = [{<<"xmlns">>, ?NS_ESL_INBOX}],
           children = [ResultSetEl | maps:values(ResultBinary)]}.

-spec result_set([inbox_res()]) -> exml:element().
result_set([]) ->
    #xmlel{name = <<"set">>, attrs = [{<<"xmlns">>, ?NS_RSM}]};
result_set([#{remote_jid := FirstBinJid, timestamp := FirstTS} | _] = List) ->
    #{remote_jid := LastBinJid, timestamp := LastTS} = lists:last(List),
    BFirst = mod_inbox_utils:encode_rsm_id(FirstTS, FirstBinJid),
    BLast = mod_inbox_utils:encode_rsm_id(LastTS, LastBinJid),
    mod_mam_utils:result_set(BFirst, BLast, undefined, undefined).

%%%%%%%%%%%%%%%%%%%
%% iq-get
-spec build_inbox_form(mongooseim:host_type()) -> exml:element().
build_inbox_form(HostType) ->
    AllBoxes = mod_inbox_utils:all_valid_boxes_for_query(HostType),
    OrderOptions = [{<<"Ascending by timestamp">>, <<"asc">>},
                    {<<"Descending by timestamp">>, <<"desc">>}],
    Fields = [#{var => <<"start">>, type => <<"text-single">>},
              #{var => <<"end">>, type => <<"text-single">>},
              #{var => <<"hidden_read">>, type => <<"text-single">>, values => [<<"false">>]},
              #{var => <<"order">>, type => <<"list-single">>, values => [<<"desc">>],
                options => OrderOptions},
              #{var => <<"box">>, type => <<"list-single">>, values => [<<"all">>],
                options => AllBoxes},
              #{var => <<"archive">>, type => <<"boolean">>, values => [<<"false">>]}],
    mongoose_data_forms:form(#{ns => ?NS_ESL_INBOX, fields => Fields}).

%%%%%%%%%%%%%%%%%%%
%% iq-set
-spec query_to_params(mongooseim:host_type(), QueryEl :: exml:element()) ->
    get_inbox_params() | {error, atom(), binary()}.
query_to_params(HostType, QueryEl) ->
    Form = form_to_params(HostType, mongoose_data_forms:find_form(QueryEl)),
    Rsm = create_rsm(HostType, QueryEl),
    build_params(Form, Rsm).

-spec create_rsm(mongooseim:host_type(), exml:element()) -> none | jlib:rsm_in().
create_rsm(HostType, QueryEl) ->
    case {jlib:rsm_decode(QueryEl), get_max_result_limit(HostType)} of
        {Rsm, infinity} ->
            Rsm;
        {none, MaxResultLimit} ->
            #rsm_in{max = MaxResultLimit};
        {Rsm = #rsm_in{max = Max}, MaxResultLimit} when is_integer(Max) ->
            Rsm#rsm_in{max = min(Max, MaxResultLimit)};
        {Rsm, MaxResultLimit} ->
            Rsm#rsm_in{max = MaxResultLimit}
    end.

-spec build_params(get_inbox_params() | {error, atom(), binary()}, none | jlib:rsm_in()) ->
    get_inbox_params() | {error, atom(), binary()}.
build_params({error, Error, Msg}, _) ->
    {error, Error, Msg};
build_params(_, #rsm_in{max = Max, index = Index}) when Max =:= error; Index =:= error ->
    {error, bad_request, <<"bad-request">>};
build_params(_, #rsm_in{index = Index}) when Index =/= undefined ->
    {error, feature_not_implemented, <<"Inbox does not expose a total count and indexes">>};
build_params(Params, none) ->
    Params;
build_params(Params, #rsm_in{max = Max, id = undefined}) when Max =/= undefined ->
    Params#{limit => Max};
build_params(Params, Rsm) ->
    build_params_with_rsm(Params#{rsm => Rsm}, Rsm).

build_params_with_rsm(Params, #rsm_in{max = Max, id = <<>>, direction = before}) ->
    Params#{limit => Max, order => asc, start => 0};
build_params_with_rsm(Params, #rsm_in{max = Max, id = <<>>, direction = aft}) ->
    maps:remove('end', Params#{limit => Max});
build_params_with_rsm(Params, #rsm_in{max = Max, id = Id, direction = Dir}) when is_binary(Id) ->
    case {mod_inbox_utils:decode_rsm_id(Id), Dir} of
        {error, _} ->
            {error, bad_request, <<"bad-request">>};
        {{Stamp, Jid}, aft} ->
            Params#{limit => expand_limit(Max), filter_on_jid => Jid, 'end' => Stamp};
        {{Stamp, Jid}, undefined} ->
            Params#{limit => expand_limit(Max), filter_on_jid => Jid, 'end' => Stamp};
        {{Stamp, Jid}, before} ->
            Params#{limit => expand_limit(Max), order => asc, filter_on_jid => Jid, start => Stamp}
    end;
build_params_with_rsm(Params, _Rsm) ->
    Params.

-spec expand_limit(undefined) -> undefined;
                  (integer()) -> integer().
expand_limit(undefined) ->
    undefined;
expand_limit(Max) ->
    Max + 1.

-spec form_to_params(mongooseim:host_type(), FormEl :: exml:element() | undefined) ->
    get_inbox_params() | {error, bad_request, Msg :: binary()}.
form_to_params(_, undefined) ->
    #{ order => desc };
form_to_params(HostType, FormEl) ->
    #{kvs := ParsedFields} = mongoose_data_forms:parse_form_fields(FormEl),
    ?LOG_DEBUG(#{what => inbox_parsed_form_fields, parsed_fields => ParsedFields}),
    fields_to_params(HostType, maps:to_list(ParsedFields), #{ order => desc }).

-spec fields_to_params(mongooseim:host_type(),
                       [{Var :: binary(), Values :: [binary()]}], Acc :: get_inbox_params()) ->
    get_inbox_params() | {error, bad_request, Msg :: binary()}.
fields_to_params(_, [], Acc) ->
    Acc;
fields_to_params(HostType, [{<<"start">>, [StartISO]} | RFields], Acc) ->
    try calendar:rfc3339_to_system_time(binary_to_list(StartISO), [{unit, microsecond}]) of
        StartStamp ->
            fields_to_params(HostType, RFields, Acc#{ start => StartStamp })
    catch error:Error ->
            ?LOG_WARNING(#{what => inbox_invalid_form_field,
                           reason => Error, field => start, value => StartISO}),
            {error, bad_request, invalid_field_value(<<"start">>, StartISO)}
    end;
fields_to_params(HostType, [{<<"end">>, [EndISO]} | RFields], Acc) ->
    try calendar:rfc3339_to_system_time(binary_to_list(EndISO), [{unit, microsecond}]) of
        EndStamp ->
            fields_to_params(HostType, RFields, Acc#{ 'end' => EndStamp })
    catch error:Error ->
            ?LOG_WARNING(#{what => inbox_invalid_form_field,
                           reason => Error, field => 'end', value => EndISO}),
            {error, bad_request, invalid_field_value(<<"end">>, EndISO)}
    end;
fields_to_params(HostType, [{<<"order">>, [OrderBin]} | RFields], Acc) ->
    case binary_to_order(OrderBin) of
        error ->
            ?LOG_WARNING(#{what => inbox_invalid_form_field,
                           field => order, value => OrderBin}),
            {error, bad_request, invalid_field_value(<<"order">>, OrderBin)};
        Order ->
            fields_to_params(HostType, RFields, Acc#{ order => Order })
    end;

fields_to_params(HostType, [{<<"hidden_read">>, [HiddenRead]} | RFields], Acc) ->
    case mod_inbox_utils:binary_to_bool(HiddenRead) of
        error ->
            ?LOG_WARNING(#{what => inbox_invalid_form_field,
                           field => hidden_read, value => HiddenRead}),
            {error, bad_request, invalid_field_value(<<"hidden_read">>, HiddenRead)};
        Hidden ->
            fields_to_params(HostType, RFields, Acc#{ hidden_read => Hidden })
    end;

fields_to_params(HostType, [{<<"archive">>, [Value]} | RFields], Acc) ->
    case mod_inbox_utils:binary_to_bool(Value) of
        error ->
            ?LOG_WARNING(#{what => inbox_invalid_form_field,
                           field => archive, value => Value}),
            {error, bad_request, invalid_field_value(<<"archive">>, Value)};
        true ->
            fields_to_params(HostType, RFields, Acc#{ box => maps:get(box, Acc, <<"archive">>) });
        false ->
            fields_to_params(HostType, RFields, Acc#{ box => maps:get(box, Acc, <<"inbox">>) })
    end;

fields_to_params(HostType, [{<<"box">>, [Value]} | RFields], Acc) ->
    case validate_box(HostType, Value) of
        false ->
            ?LOG_WARNING(#{what => inbox_invalid_form_field,
                           field => box, value => Value}),
            {error, bad_request, invalid_field_value(<<"box">>, Value)};
        true ->
            fields_to_params(HostType, RFields, Acc#{ box => Value })
    end;

fields_to_params(_, [{Invalid, [InvalidFieldVal]} | _], _) ->
    ?LOG_WARNING(#{what => inbox_invalid_form_field, reason => unknown_field,
                   field => Invalid, value => InvalidFieldVal}),
    {error, bad_request, <<"Unknown inbox form field=", Invalid/binary, ", value=", InvalidFieldVal/binary>>}.

-spec binary_to_order(binary()) -> asc | desc | error.
binary_to_order(<<"desc">>) -> desc;
binary_to_order(<<"asc">>) -> asc;
binary_to_order(_) -> error.

validate_box(HostType, Box) ->
    AllBoxes = mod_inbox_utils:all_valid_boxes_for_query(HostType),
    lists:member(Box, AllBoxes).

invalid_field_value(Field, Value) ->
    <<"Invalid inbox form field value, field=", Field/binary, ", value=", Value/binary>>.

%%%%%%%%%%%%%%%%%%%
%% Helpers
get_inbox_unread(Value, Acc, _) when is_integer(Value) ->
    Acc;
get_inbox_unread(undefined, Acc, To) ->
    %% TODO this value should be bound to a stanza reference inside Acc
    InterlocutorJID = mongoose_acc:from_jid(Acc),
    InboxEntryKey = mod_inbox_utils:build_inbox_entry_key(To, InterlocutorJID),
    HostType = mongoose_acc:host_type(Acc),
    {ok, Count} = mod_inbox_backend:get_inbox_unread(HostType, InboxEntryKey),
    mongoose_acc:set(inbox, unread_count, Count, Acc).

hooks(HostType) ->
    [
        {disco_local_features, HostType, fun ?MODULE:disco_local_features/3, #{}, 99},
        {remove_user, HostType, fun ?MODULE:remove_user/3, #{}, 50},
        {remove_domain, HostType, fun ?MODULE:remove_domain/3, #{}, 50},
        {user_send_message, HostType, fun ?MODULE:user_send_message/3, #{}, 70},
        {filter_local_packet, HostType, fun ?MODULE:filter_local_packet/3, #{}, 90},
        {inbox_unread_count, HostType, fun ?MODULE:inbox_unread_count/3, #{}, 80},
        {get_personal_data, HostType, fun ?MODULE:get_personal_data/3, #{}, 50}
    ].

get_groupchat_types(HostType) ->
    gen_mod:get_module_opt(HostType, ?MODULE, groupchat).

get_max_result_limit(HostType) ->
    gen_mod:get_module_opt(HostType, ?MODULE, max_result_limit, infinity).

-spec config_metrics(mongooseim:host_type()) -> [{gen_mod:opt_key(), gen_mod:opt_value()}].
config_metrics(HostType) ->
    mongoose_module_metrics:opts_for_module(HostType, ?MODULE, [backend]).

-spec muclight_enabled(HostType :: mongooseim:host_type()) -> boolean().
muclight_enabled(HostType) ->
    Groupchats = get_groupchat_types(HostType),
    lists:member(muclight, Groupchats).

%%%%%%%%%%%%%%%%%%%
%% Message Predicates
-spec should_be_stored_in_inbox(
        mongoose_acc:t(), jid:jid(), jid:jid(), exml:element(), mod_mam_utils:direction(), message_type()) ->
    boolean().
should_be_stored_in_inbox(Acc, From, To, Msg, Dir, Type) ->
    mod_mam_utils:is_archivable_message(?MODULE, Dir, Msg, true)
    andalso mod_inbox_entries:should_be_stored_in_inbox(Msg)
    andalso inbox_owner_exists(Acc, From, To, Dir, Type).

-spec inbox_owner_exists(mongoose_acc:t(),
                         From :: jid:jid(),
                         To ::jid:jid(),
                         mod_mam_utils:direction(),
                         message_type()) -> boolean().
inbox_owner_exists(Acc, _, To, incoming, MessageType) -> % filter_local_packet
    HostType = mongoose_acc:host_type(Acc),
    mongoose_lib:does_local_user_exist(HostType, To, MessageType);
inbox_owner_exists(Acc, From, _, outgoing, _) -> % user_send_message
    HostType = mongoose_acc:host_type(Acc),
    ejabberd_auth:does_user_exist(HostType, From, stored).
