%%%-------------------------------------------------------------------
%%% @author ludwikbukowski
%%% @copyright (C) 2018, Erlang-Solutions
%%% @doc
%%%
%%% @end
%%% Created : 30. Jan 2018 13:22
%%%-------------------------------------------------------------------
-module(mod_inbox).
-author("ludwikbukowski").

-behaviour(gen_mod).
-behaviour(mongoose_module_metrics).

-include("jlib.hrl").
-include("mod_inbox.hrl").
-include("mongoose.hrl").
-include("mongoose_config_spec.hrl").
-include("mongoose_logger.hrl").
-include("mongoose_ns.hrl").

-export([get_personal_data/2]).

-export([start/2, stop/1, deps/2, config_spec/0]).
-export([process_iq/4,
         user_send_packet/4,
         filter_packet/1,
         inbox_unread_count/2,
         remove_user/3,
         remove_domain/3
        ]).

-export([config_metrics/1]).

-callback init(Host, Opts) -> ok when
               Host :: jid:lserver(),
               Opts :: list().

-callback get_inbox(LUsername, LServer, Params) -> get_inbox_res() when
                    LUsername :: jid:luser(),
                    LServer :: jid:lserver(),
                    Params :: get_inbox_params().

-callback set_inbox(LUsername, LServer, ToBareJid,
                    Content, Count, MsgId, Timestamp) -> inbox_write_res() when
                    LUsername :: jid:luser(),
                    LServer :: jid:lserver(),
                    ToBareJid :: binary(),
                    Content :: binary(),
                    Count :: integer(),
                    MsgId :: binary(),
                    Timestamp :: integer().

-callback remove_inbox_row(LUsername, LServer, ToBareJid) -> inbox_write_res() when
                           LUsername :: jid:luser(),
                           LServer :: jid:lserver(),
                           ToBareJid :: binary().

-callback set_inbox_incr_unread(LUsername, LServer, ToBareJid,
                                Content, MsgId, Timestamp) -> {ok, integer()} | ok when
                                LUsername :: jid:luser(),
                                LServer :: jid:lserver(),
                                ToBareJid :: binary(),
                                Content :: binary(),
                                MsgId :: binary(),
                                Timestamp :: integer().

-callback reset_unread(LUsername, LServer, BareJid, MsgId) -> inbox_write_res() when
                       LUsername :: jid:luser(),
                       LServer :: jid:lserver(),
                       BareJid :: binary(),
                       MsgId :: binary().

-callback clear_inbox(LServer) -> inbox_write_res() when
                      LServer :: jid:lserver().

-callback clear_inbox(LUsername, LServer) -> inbox_write_res() when
                      LUsername :: jid:luser(),
                      LServer :: jid:lserver().

-callback get_inbox_unread(LUsername, LServer, InterlocutorJID) -> {ok, integer()} when
                      LUsername :: jid:luser(),
                      LServer :: jid:lserver(),
                      InterlocutorJID :: jid:literal_jid().

-callback get_entry_properties(LUsername, LServer, EntryJID) -> Ret when
                      LUsername :: jid:luser(),
                      LServer :: jid:lserver(),
                      EntryJID :: jid:literal_jid(),
                      Ret :: entry_properties().

-callback set_entry_properties(LUsername, LServer, EntryJID, Params) -> Ret when
                      LUsername :: jid:luser(),
                      LServer :: jid:lserver(),
                      EntryJID :: jid:literal_jid(),
                      Params :: entry_properties(),
                      Ret :: entry_properties() | {error, binary()}.

-callback remove_domain(HostType :: mongooseim:host_type(),
                        LServer :: jid:lserver()) -> ok.

-type get_inbox_params() :: #{
        start => integer(),
        'end' => integer(),
        order => asc | desc,
        hidden_read => true | false,
        archive => boolean()
       }.

-export_type([get_inbox_params/0]).

%%--------------------------------------------------------------------
%% gdpr callbacks
%%--------------------------------------------------------------------
-spec get_personal_data(gdpr:personal_data(), jid:jid()) -> gdpr:personal_data().
get_personal_data(Acc, #jid{ luser = LUser, lserver = LServer }) ->
    Schema = ["jid", "content", "unread_count", "timestamp"],
    InboxParams = #{
        start => 0,
        'end' => erlang:system_time(microsecond),
        order => asc,
        hidden_read => false
       },
    Entries = mod_inbox_backend:get_inbox(LUser, LServer, InboxParams),
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
-spec deps(jid:lserver(), list()) -> gen_mod:deps_list().
deps(_Host, Opts) ->
    groupchat_deps(Opts).

-spec start(Host :: jid:server(), Opts :: list()) -> ok.
start(Host, Opts) ->
    FullOpts = case lists:keyfind(backend, 1, Opts) of
                   false -> [{backend, rdbms} | Opts];
                   _ -> Opts
               end,
    gen_mod:start_backend_module(?MODULE, FullOpts, callback_funs()),
    mod_inbox_backend:init(Host, FullOpts),
    mod_disco:register_feature(Host, ?NS_ESL_INBOX),
    IQDisc = gen_mod:get_opt(iqdisc, FullOpts, no_queue),
    MucTypes = get_groupchat_types(Host),
    lists:member(muc, MucTypes) andalso mod_inbox_muc:start(Host),
    ejabberd_hooks:add(hooks(Host)),
    store_bin_reset_markers(Host, FullOpts),
    gen_iq_handler:add_iq_handler(ejabberd_sm, Host, ?NS_ESL_INBOX,
                                  ?MODULE, process_iq, IQDisc),
    gen_iq_handler:add_iq_handler(ejabberd_sm, Host, ?NS_ESL_INBOX_CONVERSATION,
                                  mod_inbox_entries, process_iq_conversation, IQDisc).


-spec stop(Host :: jid:server()) -> ok.
stop(Host) ->
    mod_disco:unregister_feature(Host, ?NS_ESL_INBOX),
    mod_inbox_muc:stop(Host),
    ejabberd_hooks:delete(hooks(Host)),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host, ?NS_ESL_INBOX),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host, ?NS_ESL_INBOX_CONVERSATION).

-spec config_spec() -> mongoose_config_spec:config_section().
config_spec() ->
    #section{
        items = #{<<"reset_markers">> => #list{items = #option{type = atom,
                                                               validate = {enum, [displayed,
                                                                                  received,
                                                                                  acknowledged]}}},
                  <<"groupchat">> => #list{items = #option{type = atom,
                                                           validate = {enum, [muc, muclight]}}},
                  <<"aff_changes">> => #option{type = boolean},
                  <<"remove_on_kicked">> => #option{type = boolean},
                  <<"iqdisc">> => mongoose_config_spec:iqdisc()
        }
    }.

%%%%%%%%%%%%%%%%%%%
%% Process IQ
-spec process_iq(From :: jid:jid(),
                 To :: jid:jid(),
                 Acc :: mongoose_acc:t(),
                 IQ :: jlib:iq()) -> {stop, mongoose_acc:t()} | {mongoose_acc:t(), jlib:iq()}.
process_iq(_From, _To, Acc, #iq{type = get, sub_el = SubEl} = IQ) ->
    Form = build_inbox_form(),
    SubElWithForm = SubEl#xmlel{ children = [Form] },
    {Acc, IQ#iq{type = result, sub_el = SubElWithForm}};
process_iq(From, _To, Acc, #iq{type = set, id = QueryId, sub_el = QueryEl} = IQ) ->
    Username = From#jid.luser,
    Host = From#jid.lserver,
    case query_to_params(QueryEl) of
        {error, bad_request, Msg} ->
            {Acc, IQ#iq{type = error, sub_el = [mongoose_xmpp_errors:bad_request(<<"en">>, Msg)]}};
        Params ->
            List = mod_inbox_backend:get_inbox(Username, Host, Params),
            forward_messages(Acc, List, QueryId, From),
            Res = IQ#iq{type = result, sub_el = [build_result_iq(List)]},
            {Acc, Res}
    end.

-spec forward_messages(Acc :: mongoose_acc:t(),
                       List :: list(inbox_res()),
                       QueryId :: id(),
                       To :: jid:jid()) -> list(mongoose_acc:t()).
forward_messages(Acc, List, QueryId, To) when is_list(List) ->
    AccTS = mongoose_acc:timestamp(Acc),
    Msgs = [build_inbox_message(El, QueryId, AccTS) || El <- List],
    [send_message(Acc, To, Msg) || Msg <- Msgs].

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
-spec user_send_packet(Acc :: map(), From :: jid:jid(),
                       To :: jid:jid(),
                       Packet :: exml:element()) -> map().
user_send_packet(Acc, From, To, #xmlel{name = <<"message">>} = Msg) ->
    Host = From#jid.lserver,
    maybe_process_message(Acc, Host, From, To, Msg, outgoing),
    Acc;
user_send_packet(Acc, _From, _To, _Packet) ->
    Acc.

-spec inbox_unread_count(Acc :: mongooseim_acc:t(), To :: jid:jid()) -> mongooseim_acc:t().
inbox_unread_count(Acc, To) ->
    Res = mongoose_acc:get(inbox, unread_count, undefined, Acc),
    get_inbox_unread(Res, Acc, To).

-type fpacket() :: {From :: jid:jid(),
                    To :: jid:jid(),
                    Acc :: mongoose_acc:t(),
                    Packet :: exml:element()}.
-spec filter_packet(Value :: fpacket() | drop) -> fpacket() | drop.
filter_packet(drop) ->
    drop;
filter_packet({From, To, Acc, Msg = #xmlel{name = <<"message">>}}) ->
    Host = To#jid.lserver,
    %% In case of PgSQL we can we can update inbox and obtain unread_count in one query,
    %% so we put it in accumulator here.
    %% In case of MySQL/MsSQL it costs an extra query, so we fetch it only if necessary
    %% (when push notification is created)
    Acc0 = case maybe_process_message(Acc, Host, From, To, Msg, incoming) of
               {ok, UnreadCount} ->
                   mongoose_acc:set(inbox, unread_count, UnreadCount, Acc);
               _ ->
                   Acc
           end,
    {From, To, Acc0, Msg};

filter_packet({From, To, Acc, Packet}) ->
    {From, To, Acc, Packet}.

remove_user(Acc, User, Server) ->
    mod_inbox_utils:clear_inbox(User, Server),
    Acc.

-spec remove_domain(mongoose_hooks:simple_acc(),
                    mongooseim:host_type(), jid:lserver()) ->
    mongoose_hooks:simple_acc().
remove_domain(Acc, HostType, Domain) ->
    mod_inbox_backend:remove_domain(HostType, Domain),
    Acc.

-spec maybe_process_message(Acc :: mongoose_acc:t(),
                            Host :: host(),
                            From :: jid:jid(),
                            To :: jid:jid(),
                            Msg :: exml:element(),
                            Dir :: outgoing | incoming) -> ok | {ok, integer()}.
maybe_process_message(Acc, Host, From, To, Msg, Dir) ->
    case should_be_stored_in_inbox(Msg) andalso inbox_owner_exists(Acc, From, To, Dir) of
        true ->
            Type = get_message_type(Msg),
            maybe_process_acceptable_message(Host, From, To, Msg, Acc, Dir, Type);
        false ->
            ok
    end.

-spec inbox_owner_exists(Acc :: mongoose_acc:t(),
                         From :: jid:jid(),
                         To :: jid:jid(),
                         Dir :: outgoing | incoming) -> boolean().
inbox_owner_exists(Acc, From, _To, outgoing) ->
    HostType = mongoose_acc:host_type(Acc),
    mongoose_users:does_user_exist(HostType, From);
inbox_owner_exists(Acc, _From, To, incoming) ->
    HostType = mongoose_acc:host_type(Acc),
    mongoose_users:does_user_exist(HostType, To).

maybe_process_acceptable_message(Host, From, To, Msg, Acc, Dir, one2one) ->
            process_message(Host, From, To, Msg, Acc, Dir, one2one);
maybe_process_acceptable_message(Host, From, To, Msg, Acc, Dir, groupchat) ->
            muclight_enabled(Host) andalso
            process_message(Host, From, To, Msg, Acc, Dir, groupchat).

-spec process_message(Host :: host(),
                      From :: jid:jid(),
                      To :: jid:jid(),
                      Message :: exml:element(),
                      Acc :: mongoose_acc:t(),
                      Dir :: outgoing | incoming,
                      Type :: one2one | groupchat) -> ok | {ok, integer()}.
process_message(Host, From, To, Message, Acc, outgoing, one2one) ->
    mod_inbox_one2one:handle_outgoing_message(Host, From, To, Message, Acc);
process_message(Host, From, To, Message, Acc, incoming, one2one) ->
    mod_inbox_one2one:handle_incoming_message(Host, From, To, Message, Acc);
process_message(Host, From, To, Message, Acc, outgoing, groupchat) ->
    mod_inbox_muclight:handle_outgoing_message(Host, From, To, Message, Acc);
process_message(Host, From, To, Message, Acc, incoming, groupchat) ->
    mod_inbox_muclight:handle_incoming_message(Host, From, To, Message, Acc);
process_message(Host, From, To, Message, _TS, Dir, Type) ->
    ?LOG_WARNING(#{what => inbox_unknown_message,
                   text => <<"Unknown message was not written into inbox">>,
                   exml_packet => Message,
                   from_jid => jid:to_binary(From), to_jid => jid:to_binary(To),
                   server => Host, dir => Dir, inbox_message_type => Type}),
    ok.


%%%%%%%%%%%%%%%%%%%
%% Stanza builders
-spec build_inbox_message(inbox_res(), id(), integer()) -> exml:element().
build_inbox_message(#{msg := Content,
                      unread_count := Count,
                      timestamp := Timestamp,
                      archive := Archive,
                      muted_until := MutedUntil}, QueryId, AccTS) ->
    #xmlel{name = <<"message">>, attrs = [{<<"id">>, mod_inbox_utils:wrapper_id()}],
        children = [build_result_el(Content, QueryId, Count, Timestamp, Archive, MutedUntil, AccTS)]}.

-spec build_result_el(content(), id(), integer(), integer(), boolean(), integer(), integer()) -> exml:element().
build_result_el(Msg, QueryId, Count, Timestamp, Archive, MutedUntil, AccTS) ->
    Forwarded = build_forward_el(Msg, Timestamp),
    Properties = mod_inbox_entries:extensions_result(Archive, MutedUntil, AccTS),
    QueryAttr = [{<<"queryid">>, QueryId} || QueryId =/= undefined, QueryId =/= <<>>],
    #xmlel{name = <<"result">>,
           attrs = [{<<"xmlns">>, ?NS_ESL_INBOX},
                    {<<"unread">>, integer_to_binary(Count)} | QueryAttr],
           children = [Forwarded | Properties]}.

-spec build_result_iq(get_inbox_res()) -> exml:element().
build_result_iq(List) ->
    AllUnread = [ N || #{unread_count := N} <- List, N =/= 0],
    Result = #{<<"count">> => length(List),
               <<"unread-messages">> => lists:sum(AllUnread),
               <<"active-conversations">> => length(AllUnread)},
    ResultBinary = maps:map(fun(K, V) ->
                        build_result_el(K, integer_to_binary(V))  end, Result),
    #xmlel{name = <<"fin">>, attrs = [{<<"xmlns">>, ?NS_ESL_INBOX}],
           children = maps:values(ResultBinary)}.

-spec build_result_el(name_bin(), count_bin()) -> exml:element().
build_result_el(Name, CountBin) ->
    #xmlel{name = Name, children = [#xmlcdata{content = CountBin}]}.

-spec build_forward_el(content(), integer()) -> exml:element().
build_forward_el(Content, Timestamp) ->
    {ok, Parsed} = exml:parse(Content),
    Delay = build_delay_el(Timestamp),
    #xmlel{name = <<"forwarded">>, attrs = [{<<"xmlns">>, ?NS_FORWARD}],
           children = [Delay, Parsed]}.

-spec build_delay_el(Timestamp :: integer()) -> exml:element().
build_delay_el(Timestamp) ->
    TS = calendar:system_time_to_rfc3339(Timestamp, [{offset, "Z"}, {unit, microsecond}]),
    jlib:timestamp_to_xml(TS, undefined, undefined).

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
        Archive ->
            fields_to_params(RFields, Acc#{ archive => Archive })
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
    {User, Host} = jid:to_lus(To),
    InterlocutorJID = mongoose_acc:from_jid(Acc),
    RemBareJIDBin = jid:to_binary(jid:to_lus(InterlocutorJID)),
    {ok, Count} = mod_inbox_backend:get_inbox_unread(User, Host, RemBareJIDBin),
    mongoose_acc:set(inbox, unread_count, Count, Acc).

hooks(Host) ->
    [
     {remove_user, Host, ?MODULE, remove_user, 50},
     {remove_domain, Host, ?MODULE, remove_domain, 50},
     {user_send_packet, Host, ?MODULE, user_send_packet, 70},
     {filter_local_packet, Host, ?MODULE, filter_packet, 90},
     {inbox_unread_count, Host, ?MODULE, inbox_unread_count, 80},
     {get_personal_data, Host, ?MODULE, get_personal_data, 50}
    ].

get_groupchat_types(Host) ->
    gen_mod:get_module_opt(Host, ?MODULE, groupchat, [muclight]).

config_metrics(Host) ->
    OptsToReport = [{backend, rdbms}], %list of tuples {option, defualt_value}
    mongoose_module_metrics:opts_for_module(Host, ?MODULE, OptsToReport).

-spec store_bin_reset_markers(Host :: host(), Opts :: list()) -> boolean().
store_bin_reset_markers(Host, Opts) ->
    ResetMarkers = gen_mod:get_opt(reset_markers, Opts, [displayed]),
    ResetMarkersBin = [mod_inbox_utils:reset_marker_to_bin(Marker) || Marker <- ResetMarkers ],
    gen_mod:set_module_opt(Host, ?MODULE, reset_markers, ResetMarkersBin).

groupchat_deps(Opts) ->
    case lists:keyfind(groupchat, 1, Opts) of
        {groupchat, List} ->
            muclight_dep(List) ++ muc_dep(List);
        false ->
            []
    end.

muclight_dep(List) ->
    case lists:member(muclight, List) of
        true -> [{mod_muc_light, hard}];
        false -> []
    end.

muc_dep(List) ->
    case lists:member(muc, List) of
        true -> [{mod_muc, hard}];
        false -> []
    end.

callback_funs() ->
    [get_inbox, set_inbox, set_inbox_incr_unread,
     reset_unread, remove_inbox_row, clear_inbox, get_inbox_unread,
     get_entry_properties, set_entry_properties].

-spec muclight_enabled(Host :: binary()) -> boolean().
muclight_enabled(Host) ->
    Groupchats = get_groupchat_types(Host),
    lists:member(muclight, Groupchats).

-spec get_message_type(Msg :: exml:element()) -> groupchat | one2one.
get_message_type(Msg) ->
    case exml_query:attr(Msg, <<"type">>, undefined) of
        <<"groupchat">> ->
            groupchat;
        _ ->
            one2one
    end.

%%%%%%%%%%%%%%%%%%%
%% Message Predicates
-spec should_be_stored_in_inbox(Msg :: exml:element()) -> boolean().
should_be_stored_in_inbox(Msg) ->
    not is_forwarded_message(Msg) andalso
        not is_error_message(Msg) andalso
        not is_offline_message(Msg) andalso
        mod_inbox_entries:should_be_stored_in_inbox(Msg).

-spec is_forwarded_message(Msg :: exml:element()) -> boolean().
is_forwarded_message(Msg) ->
    case exml_query:subelement_with_ns(Msg, ?NS_FORWARD, undefined) of
        undefined ->
            false;
        _ ->
            true
    end.

-spec is_error_message(Msg :: exml:element()) -> boolean().
is_error_message(Msg) ->
    case exml_query:attr(Msg, <<"type">>, undefined) of
        <<"error">> ->
            true;
        _ ->
            false
    end.

-spec is_offline_message(Msg :: exml:element()) -> boolean().
is_offline_message(Msg) ->
    case exml_query:subelement_with_ns(Msg, ?NS_DELAY, undefined) of
        undefined ->
            false;
        _ ->
            true
    end.
