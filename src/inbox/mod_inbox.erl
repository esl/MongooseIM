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
-include("mod_inbox.hrl").
-include("jlib.hrl").
-include("jid.hrl").
-include("mongoose_ns.hrl").
-include("mongoose.hrl").
-include("mongoose_logger.hrl").

-behaviour(gdpr).

-export([get_personal_data/2]).

-export([start/2, stop/1, deps/2]).
-export([process_iq/4, user_send_packet/4, filter_packet/1, inbox_unread_count/2]).
-export([clear_inbox/2]).

-callback init(Host, Opts) -> ok when
               Host :: jid:lserver(),
               Opts :: list().

-callback get_inbox(LUsername, LServer, Params) -> get_inbox_res() when
                    LUsername :: jid:luser(),
                    LServer :: jid:lserver(),
                    Params :: get_inbox_params().

-callback set_inbox(Username, Server, ToBareJid,
                    Content, Count, MsgId, Timestamp) -> inbox_write_res() when
                    Username :: jid:luser(),
                    Server :: jid:lserver(),
                    ToBareJid :: binary(),
                    Content :: binary(),
                    Count :: integer(),
                    MsgId :: binary(),
                    Timestamp :: erlang:timestamp().

-callback remove_inbox(Username, Server, ToBareJid) -> inbox_write_res() when
                       Username :: jid:luser(),
                       Server :: jid:lserver(),
                       ToBareJid :: binary().

-callback set_inbox_incr_unread(Username, Server, ToBareJid,
                                Content, MsgId, Timestamp) -> {ok, integer()} | ok when
                                Username :: jid:luser(),
                                Server :: jid:lserver(),
                                ToBareJid :: binary(),
                                Content :: binary(),
                                MsgId :: binary(),
                                Timestamp :: erlang:timestamp().

-callback reset_unread(Username, Server, BareJid, MsgId) -> inbox_write_res() when
                       Username :: jid:luser(),
                       Server :: jid:lserver(),
                       BareJid :: binary(),
                       MsgId :: binary().

-callback clear_inbox(Server) -> inbox_write_res() when
                      Server :: jid:lserver().

-callback clear_inbox(Username, Server) -> inbox_write_res() when
                      Username :: jid:luser(),
                      Server :: jid:lserver().

-callback get_inbox_unread(Username, Server) -> {ok, integer()} when
                      Username :: jid:luser(),
                      Server :: jid:lserver().

-type get_inbox_params() :: #{
        start => erlang:timestamp(),
        'end' => erlang:timestamp(),
        order => asc | desc,
        hidden_read => true | false
       }.

-export_type([get_inbox_params/0]).

%%--------------------------------------------------------------------
%% gdpr callbacks
%%--------------------------------------------------------------------

-spec get_personal_data(jid:username(), jid:server()) ->
    [{gdpr:data_group(), gdpr:schema(), gdpr:entries()}].
get_personal_data(Username, Server) ->
    LUser = jid:nodeprep(Username),
    LServer = jid:nameprep(Server),
    Schema = ["jid", "content", "unread_count", "timestamp"],
    InboxParams = #{
        start => {0,0,0},
        'end' => erlang:timestamp(),
        order => asc,
        hidden_read => false
       },
    Entries = mod_inbox_backend:get_inbox(LUser, LServer, InboxParams),
    ProcessedEntries = [{ RemJID, Content, UnreadCount, jlib:now_to_utc_string(Timestamp) } ||
                        { RemJID, Content, UnreadCount, Timestamp } <- Entries],
    [{inbox, Schema, ProcessedEntries}].

%%--------------------------------------------------------------------
%% inbox callbacks
%%--------------------------------------------------------------------

-spec deps(jid:lserver(), list()) -> list().
deps(_Host, Opts) ->
    groupchat_deps(Opts).

-spec start(Host :: jid:server(), Opts :: list()) -> ok.
start(Host, Opts) ->
    FullOpts = case lists:keyfind(backend, 2, Opts) of
                   false ->
                       [{backend, rdbms} | Opts];
                   _ ->
                       Opts
               end,
    gen_mod:start_backend_module(?MODULE, FullOpts, callback_funs()),
    mod_inbox_backend:init(Host, FullOpts),
    mod_disco:register_feature(Host, ?NS_ESL_INBOX),
    IQDisc = gen_mod:get_opt(iqdisc, FullOpts, no_queue),
    MucTypes = get_groupchat_types(Host),
    lists:member(muc, MucTypes) andalso mod_inbox_muc:start(Host),
    ejabberd_hooks:add(hooks(Host)),
    store_bin_reset_markers(Host, FullOpts),
    gen_iq_handler:add_iq_handler(ejabberd_sm, Host, ?NS_ESL_INBOX, ?MODULE, process_iq, IQDisc).


-spec stop(Host :: jid:server()) -> ok.
stop(Host) ->
    mod_disco:unregister_feature(Host, ?NS_ESL_INBOX),
    mod_inbox_muc:stop(Host),
    ejabberd_hooks:delete(hooks(Host)),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host, ?NS_ESL_INBOX).


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
process_iq(From, To, Acc, #iq{type = set, id = QueryId, sub_el = QueryEl} = IQ) ->
    Username = From#jid.luser,
    Host = From#jid.lserver,
    case form_to_params(exml_query:subelement_with_ns(QueryEl, ?NS_XDATA)) of
        {error, bad_request, Msg} ->
            {Acc, IQ#iq{ type = error, sub_el = [ mongoose_xmpp_errors:bad_request(<<"en">>, Msg) ] }};
        Params ->
            List = mod_inbox_backend:get_inbox(Username, Host, Params),
            forward_messages(List, QueryId, To),
            Res = IQ#iq{type = result, sub_el = [build_result_iq(List)]},
            {Acc, Res}
    end.

-spec forward_messages(List :: list(inbox_res()),
                       QueryId :: id(),
                       To :: jid:jid()) -> list(mongoose_acc:t()).
forward_messages(List, QueryId, To) when is_list(List) ->
    Msgs = [build_inbox_message(El, QueryId) || El <- List],
    [send_message(To, Msg) || Msg <- Msgs].

-spec send_message(To :: jid:jid(), Msg :: exml:element()) -> mongoose_acc:t().
send_message(To, Msg) ->
    BareTo = jid:to_bare(To),
    ejabberd_sm:route(BareTo, To, Msg).

%%%%%%%%%%%%%%%%%%%
%% Handlers
-spec user_send_packet(Acc :: map(), From :: jid:jid(),
                       To :: jid:jid(),
                       Packet :: exml:element()) -> map().
user_send_packet(Acc, From, To, #xmlel{name = <<"message">>} = Msg) ->
    Host = From#jid.server,
    maybe_process_message(Host, From, To, Msg, outgoing),
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
    Host = To#jid.server,
    %% In case of PgSQL we can we can update inbox and obtain unread_count in one query,
    %% so we put it in accumulator here.
    %% In case of MySQL/MsSQL it costs an extra query, so we fetch it only if necessary
    %% (when push notification is created)
    Acc0 = case maybe_process_message(Host, From, To, Msg, incoming) of
               {ok, UnreadCount} ->
                   mongoose_acc:set(inbox, unread_count, UnreadCount, Acc);
               _ ->
                   Acc
           end,
    {From, To, Acc0, Msg};

filter_packet({From, To, Acc, Packet}) ->
    {From, To, Acc, Packet}.

-spec maybe_process_message(Host :: host(),
                            From :: jid:jid(),
                            To :: jid:jid(),
                            Msg :: exml:element(),
                            Dir :: outgoing | incoming) -> ok | {ok, integer()}.
maybe_process_message(Host, From, To, Msg, Dir) ->
    AcceptableMessage = should_be_stored_in_inbox(Msg),
    case AcceptableMessage of
        true ->
            Type = get_message_type(Msg),
            maybe_process_acceptable_message(Host, From, To, Msg, Dir, Type);
        false ->
            ok
    end.

maybe_process_acceptable_message(Host, From, To, Msg, Dir, one2one) ->
            process_message(Host, From, To, Msg, Dir, one2one);
maybe_process_acceptable_message(Host, From, To, Msg, Dir, groupchat) ->
            muclight_enabled(Host) andalso
            process_message(Host, From, To, Msg, Dir, groupchat).

-spec process_message(Host :: host(),
                      From :: jid:jid(),
                      To :: jid:jid(),
                      Message :: exml:element(),
                      Dir :: outgoing | incoming,
                      Type :: one2one | groupchat) -> ok | {ok, integer()}.
process_message(Host, From, To, Message, outgoing, one2one) ->
    mod_inbox_one2one:handle_outgoing_message(Host, From, To, Message);
process_message(Host, From, To, Message, incoming, one2one) ->
    mod_inbox_one2one:handle_incoming_message(Host, From, To, Message);
process_message(Host, From, To, Message, outgoing, groupchat) ->
    mod_inbox_muclight:handle_outgoing_message(Host, From, To, Message);
process_message(Host, From, To, Message, incoming, groupchat) ->
    mod_inbox_muclight:handle_incoming_message(Host, From, To, Message);
process_message(_, _, _, Message, _, _) ->
    ?WARNING_MSG("event=unknown_message_not_written_in_inbox,packet='~p'", [Message]),
    ok.


%%%%%%%%%%%%%%%%%%%
%% Stanza builders

-spec build_inbox_message(inbox_res(), id()) -> exml:element().
build_inbox_message({_Username, Content, Count, Timestamp}, QueryId) ->
    #xmlel{name = <<"message">>, attrs = [{<<"id">>, mod_inbox_utils:wrapper_id()}],
        children = [build_result_el(Content, QueryId, Count, Timestamp)]}.

-spec build_result_el(content(), id(), count_bin(), erlang:timestamp()) -> exml:element().
build_result_el(Msg, QueryId, BinUnread, Timestamp) ->
    Forwarded = build_forward_el(Msg, Timestamp),
    QueryAttr = [{<<"queryid">>, QueryId} || QueryId =/= undefined, QueryId =/= <<>>],
    #xmlel{name = <<"result">>, attrs = [{<<"xmlns">>, ?NS_ESL_INBOX}, {<<"unread">>, BinUnread}] ++
    QueryAttr, children = [Forwarded]}.

-spec build_result_iq(get_inbox_res()) -> exml:element().
build_result_iq(List) ->
    AllUnread = lists:filter(fun(E) -> E =/= 0 end,
                             [extract_unread_count(E) || E <- List]),

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

-spec build_forward_el(content(), erlang:timestamp()) -> exml:element().
build_forward_el(Content, Timestamp) ->
    {ok, Parsed} = exml:parse(Content),
    Delay = build_delay_el(Timestamp),
    #xmlel{name = <<"forwarded">>, attrs = [{<<"xmlns">>, ?NS_FORWARD}],
           children = [Delay, Parsed]}.

-spec build_delay_el(Timestamp :: erlang:timestamp()) -> exml:element().
build_delay_el({_, _, Micro} = Timestamp) ->
    {Day, {H, M, S}} = calendar:now_to_datetime(Timestamp),
    DateTimeMicro = {Day, {H, M, S, Micro}},
    jlib:timestamp_to_xml(DateTimeMicro, utc, undefined, undefined).

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
                  text_single_form_field(<<"hidden_read">>, <<"false">>)
                 ],
    #xmlel{ name = <<"x">>, attrs = [{<<"xmlns">>, ?NS_XDATA}, {<<"type">>, <<"form">>}],
            children = FormFields }.

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
    #xmlel{ name = <<"value">>, children = [#xmlcdata{ content = Value }] }.

%%%%%%%%%%%%%%%%%%%
%% Helpers
get_inbox_unread(Value, Acc, _) when is_integer(Value) ->
    Acc;
get_inbox_unread(undefined, Acc, To) ->
%% TODO this value should be bound to a stanza reference inside Acc
    {User, Host} = jid:to_lus(To),
    {ok, Count} = mod_inbox_utils:get_inbox_unread(User, Host),
    mongoose_acc:set(inbox, unread_count, Count, Acc).

hooks(Host) ->
    [
     {user_send_packet, Host, ?MODULE, user_send_packet, 70},
     {filter_local_packet, Host, ?MODULE, filter_packet, 90},
     {inbox_unread_count, Host, ?MODULE, inbox_unread_count, 80}
    ].

get_groupchat_types(Host) ->
    gen_mod:get_module_opt(Host, ?MODULE, groupchat, [muclight]).


-spec muclight_enabled(Host :: binary()) -> boolean().
muclight_enabled(Host) ->
    Groupchats = get_groupchat_types(Host),
    lists:member(muclight, Groupchats).

-spec form_to_params(FormEl :: exml:element() | undefined) ->
    get_inbox_params() | {error, bad_request, Msg :: binary()}.
form_to_params(undefined) ->
    #{ order => desc };
form_to_params(FormEl) ->
    ParsedFields = jlib:parse_xdata_fields(exml_query:subelements(FormEl, <<"field">>)),
    ?DEBUG("event=parsed_form_fields,parsed=~p", [ParsedFields]),
    fields_to_params(ParsedFields, #{ order => desc }).

-spec fields_to_params([{Var :: binary(), Values :: [binary()]}], Acc :: get_inbox_params()) ->
    get_inbox_params() | {error, bad_request, Msg :: binary()}.
fields_to_params([], Acc) ->
    Acc;
fields_to_params([{<<"start">>, [StartISO]} | RFields], Acc) ->
    case jlib:datetime_binary_to_timestamp(StartISO) of
        undefined ->
            ?DEBUG("event=invalid_inbox_form_field,field=start,value=~s", [StartISO]),
            {error, bad_request, invalid_field_value(<<"start">>, StartISO)};
        StartStamp ->
            fields_to_params(RFields, Acc#{ start => StartStamp })
    end;
fields_to_params([{<<"end">>, [EndISO]} | RFields], Acc) ->
    case jlib:datetime_binary_to_timestamp(EndISO) of
        undefined ->
            ?DEBUG("event=invalid_inbox_form_field,field=end,value=~s", [EndISO]),
            {error, bad_request, invalid_field_value(<<"end">>, EndISO)};
        EndStamp ->
            fields_to_params(RFields, Acc#{ 'end' => EndStamp })
    end;
fields_to_params([{<<"order">>, [OrderBin]} | RFields], Acc) ->
    case binary_to_order(OrderBin) of
        error ->
            ?DEBUG("event=invalid_inbox_form_field,field=order,value=~s", [OrderBin]),
            {error, bad_request, invalid_field_value(<<"order">>, OrderBin)};
        Order ->
            fields_to_params(RFields, Acc#{ order => Order })
    end;

fields_to_params([{<<"hidden_read">>, [HiddenRead]} | RFields], Acc) ->
    case binary_to_bool(HiddenRead) of
        error ->
            ?DEBUG("event=invalid_inbox_form_field,field=hidden_read,value=~s", [HiddenRead]),
            {error, bad_request, invalid_field_value(<<"hidden_read">>, HiddenRead)};
        Hidden ->
            fields_to_params(RFields, Acc#{ hidden_read => Hidden })
    end;

fields_to_params([{<<"FORM_TYPE">>, _} | RFields], Acc) ->
    fields_to_params(RFields, Acc);
fields_to_params([{Invalid, [InvalidFieldVal]} | _], _) ->
    ?DEBUG("event=invalid_inbox_form_field,parsed=~p", [Invalid]),
    {error, bad_request, <<"Unknown inbox form field=", Invalid/binary, ", value=", InvalidFieldVal/binary>>}.

-spec binary_to_order(binary()) -> asc | desc | error.
binary_to_order(<<"desc">>) -> desc;
binary_to_order(<<"asc">>) -> asc;
binary_to_order(_) -> error.

-spec binary_to_bool(binary()) -> true | false | error.
binary_to_bool(<<"true">>) -> true;
binary_to_bool(<<"false">>) -> false;
binary_to_bool(_) -> error.

-spec store_bin_reset_markers(Host :: host(), Opts :: list()) -> boolean().
store_bin_reset_markers(Host, Opts) ->
    ResetMarkers = gen_mod:get_opt(reset_markers, Opts, [displayed]),
    ResetMarkersBin = [mod_inbox_utils:reset_marker_to_bin(Marker) || Marker <- ResetMarkers ],
    gen_mod:set_module_opt(Host, ?MODULE, reset_markers, ResetMarkersBin).

-spec get_message_type(Msg :: exml:element()) -> groupchat | one2one.
get_message_type(Msg) ->
    case exml_query:attr(Msg, <<"type">>, undefined) of
        <<"groupchat">> ->
            groupchat;
        _ ->
            one2one
    end.

-spec clear_inbox(Username :: jid:luser(), Server :: host()) -> inbox_write_res().
clear_inbox(Username, Server) ->
    mod_inbox_utils:clear_inbox(Username, Server).

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
     reset_unread, remove_inbox, clear_inbox, get_inbox_unread].

invalid_field_value(Field, Value) ->
    <<"Invalid inbox form field value, field=", Field/binary, ", value=", Value/binary>>.

%%%%%%%%%%%%%%%%%%%
%% Message Predicates

-spec should_be_stored_in_inbox(Msg :: exml:element()) -> boolean().
should_be_stored_in_inbox(Msg) ->
    not is_forwarded_message(Msg) andalso
        not is_error_message(Msg) andalso
        not is_offline_message(Msg).

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

extract_unread_count({_, _, Count, _}) ->
    binary_to_integer(Count).
