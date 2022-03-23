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
-include("mod_inbox.hrl").
-include("mongoose_config_spec.hrl").
-include("mongoose_logger.hrl").
-include("mongoose_ns.hrl").

-export([get_personal_data/3]).

%% gen_mod
-export([start/2, stop/1, config_spec/0, supported_features/0]).

-export([process_iq/5,
         user_send_packet/4,
         filter_local_packet/1,
         inbox_unread_count/2,
         remove_user/3,
         remove_domain/3,
         disco_local_features/1
        ]).

-ignore_xref([
    behaviour_info/1, disco_local_features/1, filter_local_packet/1, get_personal_data/3,
    inbox_unread_count/2, remove_domain/3, remove_user/3, user_send_packet/4
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
        archive => boolean() | integer()
       }.

-type count_res() :: ok | {ok, non_neg_integer()} | {error, term()}.
-type write_res() :: ok | {error, any()}.

-export_type([entry_key/0, get_inbox_params/0]).
-export_type([count_res/0, write_res/0, inbox_res/0]).

%%--------------------------------------------------------------------
%% gdpr callbacks
%%--------------------------------------------------------------------
-spec get_personal_data(gdpr:personal_data(), mongooseim:host_type(), jid:jid()) ->
    gdpr:personal_data().
get_personal_data(Acc, HostType, #jid{luser = LUser, lserver = LServer}) ->
    Schema = ["jid", "content", "unread_count", "timestamp"],
    InboxParams = #{
        start => 0,
        'end' => erlang:system_time(microsecond),
        order => asc,
        hidden_read => false
       },
    Entries = mod_inbox_backend:get_inbox(HostType, LUser, LServer, InboxParams),
    ProcessedEntries = lists:map(fun process_entry/1, Entries),
    [{inbox, Schema, ProcessedEntries} | Acc].

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
    ejabberd_hooks:add(hooks(HostType)),
    gen_iq_handler:add_iq_handler_for_domain(HostType, ?NS_ESL_INBOX, ejabberd_sm,
                                             fun ?MODULE:process_iq/5, #{}, IQDisc),
    gen_iq_handler:add_iq_handler_for_domain(HostType, ?NS_ESL_INBOX_CONVERSATION, ejabberd_sm,
                                             fun mod_inbox_entries:process_iq_conversation/5, #{}, IQDisc),
    ok.

-spec stop(HostType :: mongooseim:host_type()) -> ok.
stop(HostType) ->
    mod_inbox_muc:stop(HostType),
    case mongoose_config:get_opt([{modules, HostType}, ?MODULE, backend]) of
        rdbms_async -> mod_inbox_rdbms_async:stop(HostType);
        _ -> ok
    end,
    ejabberd_hooks:delete(hooks(HostType)),
    gen_iq_handler:remove_iq_handler_for_domain(HostType, ?NS_ESL_INBOX, ejabberd_sm),
    gen_iq_handler:remove_iq_handler_for_domain(HostType, ?NS_ESL_INBOX_CONVERSATION, ejabberd_sm).

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
                  <<"boxes">> => #list{items = #option{type = binary, validate = non_empty}},
                  <<"aff_changes">> => #option{type = boolean},
                  <<"remove_on_kicked">> => #option{type = boolean},
                  <<"iqdisc">> => mongoose_config_spec:iqdisc()
        },
        defaults = #{<<"backend">> => rdbms,
                     <<"groupchat">> => [muclight],
                     <<"boxes">> => [],
                     <<"aff_changes">> => true,
                     <<"remove_on_kicked">> => true,
                     <<"reset_markers">> => [<<"displayed">>],
                     <<"iqdisc">> => no_queue
                    },
        process = fun ?MODULE:process_inbox_boxes/1,
        format_items = map
    }.

async_config_spec() ->
    #section{
       items = #{<<"pool_size">> => #option{type = integer, validate = non_negative}},
       defaults = #{<<"pool_size">> => 2 * erlang:system_info(schedulers_online)},
       format_items = map,
       include = always
      }.

process_inbox_boxes(Config = #{boxes := Boxes}) ->
    AllBoxes = [<<"inbox">>, <<"archive">> | Boxes ],
    Config#{boxes := AllBoxes}.

%%%%%%%%%%%%%%%%%%%
%% Process IQ
-spec process_iq(Acc :: mongoose_acc:t(),
                 From :: jid:jid(),
                 To :: jid:jid(),
                 IQ :: jlib:iq(),
                 Extra :: map()) -> {stop, mongoose_acc:t()} | {mongoose_acc:t(), jlib:iq()}.
process_iq(Acc, _From, _To, #iq{type = get, sub_el = SubEl} = IQ, _Extra) ->
    Form = build_inbox_form(),
    SubElWithForm = SubEl#xmlel{ children = [Form] },
    {Acc, IQ#iq{type = result, sub_el = SubElWithForm}};
process_iq(Acc, From, _To, #iq{type = set, sub_el = QueryEl} = IQ, _Extra) ->
    HostType = mongoose_acc:host_type(Acc),
    LUser = From#jid.luser,
    LServer = From#jid.lserver,
    case query_to_params(QueryEl) of
        {error, bad_request, Msg} ->
            {Acc, IQ#iq{type = error, sub_el = [mongoose_xmpp_errors:bad_request(<<"en">>, Msg)]}};
        Params ->
            List = mod_inbox_backend:get_inbox(HostType, LUser, LServer, Params),
            forward_messages(Acc, List, IQ, From),
            Res = IQ#iq{type = result, sub_el = [build_result_iq(List)]},
            {Acc, Res}
    end.

-spec forward_messages(Acc :: mongoose_acc:t(),
                       List :: [inbox_res()],
                       QueryEl :: jlib:iq(),
                       To :: jid:jid()) -> list(mongoose_acc:t()).
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
-spec user_send_packet(Acc :: mongoose_acc:t(), From :: jid:jid(),
                       To :: jid:jid(), Packet :: exml:element()) ->
    mongoose_acc:t().
user_send_packet(Acc, From, To, #xmlel{name = <<"message">>} = Msg) ->
    maybe_process_message(Acc, From, To, Msg, outgoing);
user_send_packet(Acc, _From, _To, _Packet) ->
    Acc.

-spec inbox_unread_count(Acc :: mongoose_acc:t(), To :: jid:jid()) -> mongoose_acc:t().
inbox_unread_count(Acc, To) ->
    Res = mongoose_acc:get(inbox, unread_count, undefined, Acc),
    get_inbox_unread(Res, Acc, To).

-spec filter_local_packet(mongoose_hooks:filter_packet_acc() | drop) ->
    mongoose_hooks:filter_packet_acc() | drop.
filter_local_packet(drop) ->
    drop;
filter_local_packet({From, To, Acc, Msg = #xmlel{name = <<"message">>}}) ->
    Acc0 = maybe_process_message(Acc, From, To, Msg, incoming),
    {From, To, Acc0, Msg};
filter_local_packet({From, To, Acc, Packet}) ->
    {From, To, Acc, Packet}.

remove_user(Acc, User, Server) ->
    HostType = mongoose_acc:host_type(Acc),
    mod_inbox_utils:clear_inbox(HostType, User, Server),
    Acc.

-spec remove_domain(mongoose_hooks:simple_acc(),
                    mongooseim:host_type(), jid:lserver()) ->
    mongoose_hooks:simple_acc().
remove_domain(Acc, HostType, Domain) ->
    mod_inbox_backend:remove_domain(HostType, Domain),
    Acc.

-spec disco_local_features(mongoose_disco:feature_acc()) -> mongoose_disco:feature_acc().
disco_local_features(Acc = #{node := <<>>}) ->
    mongoose_disco:add_features([?NS_ESL_INBOX], Acc);
disco_local_features(Acc) ->
    Acc.

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
-spec build_inbox_message(mongoose_acc:t(), inbox_res(), jlib:iq()) -> exml:element().
build_inbox_message(Acc, InboxRes, IQ) ->
    #xmlel{name = <<"message">>, attrs = [{<<"id">>, mongoose_bin:gen_from_timestamp()}],
           children = [build_result_el(Acc, InboxRes, IQ)]}.

-spec build_result_el(mongoose_acc:t(), inbox_res(), jlib:iq()) -> exml:element().
build_result_el(Acc, #{msg := Msg,
                       unread_count := Count,
                       timestamp := Timestamp,
                       archive := Archive,
                       muted_until := MutedUntil} = InboxRes, #iq{id = IqId, sub_el = QueryEl} = IQ) ->
    QueryId = exml_query:attr(QueryEl, <<"queryid">>, IqId),
    AccTS = mongoose_acc:timestamp(Acc),
    Extensions = mongoose_hooks:extend_inbox_message(Acc, InboxRes, IQ),
    Forwarded = mod_inbox_utils:build_forward_el(Msg, Timestamp),
    Properties = mod_inbox_entries:extensions_result(Archive, MutedUntil, AccTS),
    QueryAttr = [{<<"queryid">>, QueryId} || QueryId =/= undefined, QueryId =/= <<>>],
    #xmlel{name = <<"result">>,
           attrs = [{<<"xmlns">>, ?NS_ESL_INBOX},
                    {<<"unread">>, integer_to_binary(Count)} | QueryAttr],
           children = [Forwarded | Properties] ++ Extensions}.

-spec build_result_iq([inbox_res()]) -> exml:element().
build_result_iq(List) ->
    AllUnread = [ N || #{unread_count := N} <- List, N =/= 0],
    Result = #{<<"count">> => length(List),
               <<"unread-messages">> => lists:sum(AllUnread),
               <<"active-conversations">> => length(AllUnread)},
    ResultBinary = maps:map(fun(K, V) ->
                                    #xmlel{name = K, children = [#xmlcdata{content = integer_to_binary(V)}]}
                            end, Result),
    #xmlel{name = <<"fin">>, attrs = [{<<"xmlns">>, ?NS_ESL_INBOX}],
           children = maps:values(ResultBinary)}.

%%%%%%%%%%%%%%%%%%%
%% iq-get
-spec build_inbox_form() -> exml:element().
build_inbox_form() ->
    OrderOptions = [
                    {<<"Ascending by timestamp">>, <<"asc">>},
                    {<<"Descending by timestamp">>, <<"desc">>}
                   ],
    FormFields = [
                  jlib:form_field({<<"FORM_TYPE">>, <<"hidden">>, ?NS_ESL_INBOX}),
                  text_single_form_field(<<"start">>),
                  text_single_form_field(<<"end">>),
                  list_single_form_field(<<"order">>, <<"desc">>, OrderOptions),
                  text_single_form_field(<<"hidden_read">>, <<"false">>),
                  jlib:form_field({<<"archive">>, <<"boolean">>, <<"false">>})
                 ],
    #xmlel{name = <<"x">>,
           attrs = [{<<"xmlns">>, ?NS_XDATA}, {<<"type">>, <<"form">>}],
           children = FormFields}.

-spec text_single_form_field(Var :: binary()) -> exml:element().
text_single_form_field(Var) ->
    #xmlel{name = <<"field">>, attrs = [{<<"var">>, Var}, {<<"type">>, <<"text-single">>}]}.

-spec text_single_form_field(Var :: binary(), DefaultValue :: binary()) -> exml:element().
text_single_form_field(Var, DefaultValue) ->
    #xmlel{name = <<"field">>,
           attrs = [{<<"var">>, Var}, {<<"type">>, <<"text-single">>}, {<<"value">>, DefaultValue}]}.

-spec list_single_form_field(Var :: binary(),
                             Default :: binary(),
                             Options :: [{Label :: binary(), Value :: binary()}]) ->
    exml:element().
list_single_form_field(Var, Default, Options) ->
    Value = form_field_value(Default),
    #xmlel{
       name = <<"field">>,
       attrs = [{<<"var">>, Var}, {<<"type">>, <<"list-single">>}],
       children = [Value | [ form_field_option(Label, OptValue) || {Label, OptValue} <- Options ]]
      }.

-spec form_field_option(Label :: binary(), Value :: binary()) -> exml:element().
form_field_option(Label, Value) ->
    #xmlel{
       name = <<"option">>,
       attrs = [{<<"label">>, Label}],
       children = [form_field_value(Value)]
      }.

-spec form_field_value(Value :: binary()) -> exml:element().
form_field_value(Value) ->
    #xmlel{name = <<"value">>, children = [#xmlcdata{content = Value}]}.

%%%%%%%%%%%%%%%%%%%
%% iq-set
-spec query_to_params(QueryEl :: exml:element()) ->
    get_inbox_params() | {error, bad_request, Msg :: binary()}.
query_to_params(QueryEl) ->
    case form_to_params(exml_query:subelement_with_ns(QueryEl, ?NS_XDATA)) of
        {error, bad_request, Msg} ->
            {error, bad_request, Msg};
        Params ->
            case maybe_rsm(exml_query:subelement_with_ns(QueryEl, ?NS_RSM)) of
                {error, Msg} -> {error, bad_request, Msg};
                undefined -> Params;
                Rsm -> Params#{limit => Rsm}
            end
    end.

-spec maybe_rsm(exml:element() | undefined) ->
    undefined | non_neg_integer() | {error, binary()}.
maybe_rsm(#xmlel{name = <<"set">>,
                 children = [#xmlel{name = <<"max">>,
                                    children = [#xmlcdata{content = Bin}]}]}) ->
    case mod_inbox_utils:maybe_binary_to_positive_integer(Bin) of
        {error, _} -> {error, wrong_rsm_message()};
        0 -> undefined;
        N -> N
    end;
maybe_rsm(undefined) ->
    undefined;
maybe_rsm(_) ->
    {error, wrong_rsm_message()}.

wrong_rsm_message() ->
    <<"bad-request">>.

-spec form_to_params(FormEl :: exml:element() | undefined) ->
    get_inbox_params() | {error, bad_request, Msg :: binary()}.
form_to_params(undefined) ->
    #{ order => desc };
form_to_params(FormEl) ->
    ParsedFields = jlib:parse_xdata_fields(exml_query:subelements(FormEl, <<"field">>)),
    ?LOG_DEBUG(#{what => inbox_parsed_form_fields, parsed_fields => ParsedFields}),
    fields_to_params(ParsedFields, #{ order => desc }).

-spec fields_to_params([{Var :: binary(), Values :: [binary()]}], Acc :: get_inbox_params()) ->
    get_inbox_params() | {error, bad_request, Msg :: binary()}.
fields_to_params([], Acc) ->
    Acc;
fields_to_params([{<<"start">>, [StartISO]} | RFields], Acc) ->
    try calendar:rfc3339_to_system_time(binary_to_list(StartISO), [{unit, microsecond}]) of
        StartStamp ->
            fields_to_params(RFields, Acc#{ start => StartStamp })
    catch error:Error ->
            ?LOG_WARNING(#{what => inbox_invalid_form_field,
                           reason => Error, field => start, value => StartISO}),
            {error, bad_request, invalid_field_value(<<"start">>, StartISO)}
    end;
fields_to_params([{<<"end">>, [EndISO]} | RFields], Acc) ->
    try calendar:rfc3339_to_system_time(binary_to_list(EndISO), [{unit, microsecond}]) of
        EndStamp ->
            fields_to_params(RFields, Acc#{ 'end' => EndStamp })
    catch error:Error ->
            ?LOG_WARNING(#{what => inbox_invalid_form_field,
                           reason => Error, field => 'end', value => EndISO}),
            {error, bad_request, invalid_field_value(<<"end">>, EndISO)}
    end;
fields_to_params([{<<"order">>, [OrderBin]} | RFields], Acc) ->
    case binary_to_order(OrderBin) of
        error ->
            ?LOG_WARNING(#{what => inbox_invalid_form_field,
                           field => order, value => OrderBin}),
            {error, bad_request, invalid_field_value(<<"order">>, OrderBin)};
        Order ->
            fields_to_params(RFields, Acc#{ order => Order })
    end;

fields_to_params([{<<"hidden_read">>, [HiddenRead]} | RFields], Acc) ->
    case mod_inbox_utils:binary_to_bool(HiddenRead) of
        error ->
            ?LOG_WARNING(#{what => inbox_invalid_form_field,
                           field => hidden_read, value => HiddenRead}),
            {error, bad_request, invalid_field_value(<<"hidden_read">>, HiddenRead)};
        Hidden ->
            fields_to_params(RFields, Acc#{ hidden_read => Hidden })
    end;

fields_to_params([{<<"archive">>, [Value]} | RFields], Acc) ->
    case mod_inbox_utils:binary_to_bool(Value) of
        error ->
            ?LOG_WARNING(#{what => inbox_invalid_form_field,
                           field => archive, value => Value}),
            {error, bad_request, invalid_field_value(<<"archive">>, Value)};
        true ->
            fields_to_params(RFields, Acc#{ archive => 1 });
        false ->
            fields_to_params(RFields, Acc#{ archive => 0 })
    end;

fields_to_params([{<<"FORM_TYPE">>, _} | RFields], Acc) ->
    fields_to_params(RFields, Acc);
fields_to_params([{Invalid, [InvalidFieldVal]} | _], _) ->
    ?LOG_INFO(#{what => inbox_invalid_form_field, reason => unknown_field,
                field => Invalid, value => InvalidFieldVal}),
    {error, bad_request, <<"Unknown inbox form field=", Invalid/binary, ", value=", InvalidFieldVal/binary>>}.

-spec binary_to_order(binary()) -> asc | desc | error.
binary_to_order(<<"desc">>) -> desc;
binary_to_order(<<"asc">>) -> asc;
binary_to_order(_) -> error.

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
     {remove_user, HostType, ?MODULE, remove_user, 50},
     {remove_domain, HostType, ?MODULE, remove_domain, 50},
     {user_send_packet, HostType, ?MODULE, user_send_packet, 70},
     {filter_local_packet, HostType, ?MODULE, filter_local_packet, 90},
     {inbox_unread_count, HostType, ?MODULE, inbox_unread_count, 80},
     {get_personal_data, HostType, ?MODULE, get_personal_data, 50},
     {disco_local_features, HostType, ?MODULE, disco_local_features, 99}
    ].

get_groupchat_types(HostType) ->
    gen_mod:get_module_opt(HostType, ?MODULE, groupchat).

config_metrics(HostType) ->
    OptsToReport = [{backend, rdbms}], %list of tuples {option, defualt_value}
    mongoose_module_metrics:opts_for_module(HostType, ?MODULE, OptsToReport).

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
inbox_owner_exists(Acc, From, _, outgoing, _) -> % user_send_packet
    HostType = mongoose_acc:host_type(Acc),
    ejabberd_auth:does_user_exist(HostType, From, stored).
