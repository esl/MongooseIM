-module(mongoose_stanza_api).

%% API
-export([send_chat_message/4, send_headline_message/5, send_stanza/2, lookup_recent_messages/5]).

%% Event API
-export([open_session/2, close_session/1]).

-include("jlib.hrl").
-include("mongoose_rsm.hrl").
-include("mongoose_logger.hrl").

-type stream() :: #{jid := jid:jid(),
                    sid := ejabberd_sm:sid(),
                    host_type := mongooseim:host_type()}.

%% API

-spec send_chat_message(User :: jid:jid() | undefined, From :: jid:jid() | undefined,
                        To :: jid:jid(), Body :: binary()) ->
          {unknown_user | invalid_sender, iodata()} | {ok, map()}.
send_chat_message(User, From, To, Body) ->
    M = #{user => User, from => From, to => To, body => Body},
    fold(M, [fun check_sender/1, fun get_host_type/1, fun check_user/1,
             fun prepare_chat_message/1, fun send/1]).

-spec send_headline_message(User :: jid:jid() | undefined, From :: jid:jid() | undefined,
                            To :: jid:jid(), Body :: binary() | undefined,
                            Subject :: binary() | undefined) ->
          {unknown_user | invalid_sender, iodata()} | {ok, map()}.
send_headline_message(User, From, To, Body, Subject) ->
    M = #{user => User, from => From, to => To, body => Body, subject => Subject},
    fold(M, [fun check_sender/1, fun get_host_type/1, fun check_user/1,
             fun prepare_headline_message/1, fun send/1]).

-spec send_stanza(User :: jid:jid() | undefined, exml:element()) ->
          {unknown_user | invalid_sender | no_sender |
           invalid_recipient | no_recipient, iodata()} | {ok, map()}.
send_stanza(User, Stanza) ->
    M = #{user => User, stanza => Stanza},
    fold(M, [fun extract_from_jid/1, fun extract_to_jid/1, fun check_sender/1,
             fun get_host_type/1, fun check_user/1, fun prepare_stanza/1, fun send/1]).

-spec lookup_recent_messages(User :: jid:jid(), With :: jid:jid() | undefined,
                             Before :: mod_mam:unix_timestamp() | undefined, % microseconds
                             Limit :: non_neg_integer() | undefined, CheckUser :: boolean()) ->
          {unknown_user, iodata()} | {ok, {[mod_mam:message_row()], non_neg_integer()}}.
lookup_recent_messages(User, With, Before, Limit, CheckUser) ->
    M = #{user => User, with => With, before => Before, limit => Limit, check_user => CheckUser},
    fold(M, [fun get_host_type/1, fun check_user/1, fun lookup_messages/1]).

%% Event API

-spec open_session(jid:jid(), boolean()) -> {unknown_user, iodata()} | {ok, stream()}.
open_session(User, CheckUser) ->
    M = #{user => User, check_user => CheckUser},
    fold(M, [fun get_host_type/1, fun check_user/1, fun do_open_session/1]).

-spec close_session(stream()) -> {ok, closed}.
close_session(#{jid := Jid = #jid{lserver = S}, sid := Sid, host_type := HostType}) ->
    Acc = mongoose_acc:new(
            #{location => ?LOCATION,
              lserver => S,
              host_type => HostType,
              element => undefined}),
    ejabberd_sm:close_session(Acc, Sid, Jid, normal, #{}),
    {ok, closed}.

%% Steps

%% @doc Determine the user's bare JID and the 'from' JID, and check if they match
check_sender(M = #{user := User = #jid{}, from := From = #jid{}}) ->
    case jid:are_bare_equal(User, From) of
        true ->
            M#{check_user => false};
        false ->
            {invalid_sender, <<"Sender's JID is different from the user's JID">>}
    end;
check_sender(M = #{from := From = #jid{}}) ->
    M#{user => jid:to_bare(From), check_user => true};
check_sender(M = #{user := User = #jid{}}) ->
    M#{from => User, check_user => false};
check_sender(#{}) ->
    {no_sender, <<"Missing sender JID">>}.

extract_from_jid(M = #{stanza := Stanza}) ->
    case exml_query:attr(Stanza, <<"from">>) of
        undefined ->
            M;
        JidBin ->
            case jid:from_binary(JidBin) of
                error -> {invalid_sender, <<"Invalid sender JID">>};
                Jid -> M#{from => Jid}
            end
    end.

extract_to_jid(M = #{stanza := Stanza}) ->
    case exml_query:attr(Stanza, <<"to">>) of
        undefined ->
            {no_recipient, <<"Missing recipient JID">>};
        JidBin ->
            case jid:from_binary(JidBin) of
                error -> {invalid_recipient, <<"Invalid recipient JID">>};
                Jid -> M#{to => Jid}
            end
    end.

prepare_chat_message(M = #{from := From, to := To, body := Body}) ->
    FromBin = jid:to_binary(From),
    ToBin = jid:to_binary(To),
    M#{stanza => build_chat_message(FromBin, ToBin, Body)}.

prepare_headline_message(M = #{from := From, to := To, body := Body, subject := Subject}) ->
    FromBin = jid:to_binary(From),
    ToBin = jid:to_binary(To),
    M#{stanza => build_headline_message(FromBin, ToBin, Body, Subject)}.

prepare_stanza(M = #{stanza := Stanza}) ->
    M#{stanza := ensure_id(Stanza)}.

get_host_type(M = #{user := #jid{lserver = LServer}}) ->
    case mongoose_domain_api:get_domain_host_type(LServer) of
        {ok, HostType} ->
            M#{host_type => HostType};
        {error, not_found} ->
            {unknown_user, <<"User's domain does not exist">>}
    end.

check_user(M = #{check_user := false}) ->
    M;
check_user(M = #{check_user := true, user := UserJid, host_type := HostType}) ->
    case ejabberd_auth:does_user_exist(HostType, UserJid, stored) of
        true ->
            M;
        false ->
            {unknown_user, <<"User does not exist">>}
    end.

send(#{host_type := HostType, from := From, to := To, stanza := Stanza}) ->
    Acc = mongoose_acc:new(#{from_jid => From,
                             to_jid => To,
                             location => ?LOCATION,
                             host_type => HostType,
                             lserver => From#jid.lserver,
                             element => Stanza}),
    C2SData = mongoose_c2s:create_data(#{host_type => HostType, jid => From}),
    Params = mongoose_c2s:hook_arg(C2SData, session_established, internal, Stanza, undefined),
    case mongoose_c2s_hooks:user_send_packet(HostType, Acc, Params) of
        {ok, Acc1} ->
            {_, Acc2} = handle_message(HostType, Acc1, Params),
            ejabberd_router:route(From, To, Acc2);
        {stop, Acc1} ->
            Acc1
    end,
    {ok, #{<<"id">> => get_id(Stanza)}}.

lookup_messages(#{user := UserJid, with := WithJid, before := Before, limit := Limit,
                  host_type := HostType}) ->
    #jid{luser = LUser, lserver = LServer} = UserJid,
    MaxResultLimit = mod_mam_params:max_result_limit(mod_mam_pm, HostType),
    Limit2 = min(Limit, MaxResultLimit),
    Params = #{archive_id => mod_mam_pm:archive_id(LServer, LUser),
               owner_jid => UserJid,
               borders => undefined,
               rsm => #rsm_in{direction = before, id = undefined}, % last msgs
               start_ts => undefined,
               end_ts => Before,
               now => os:system_time(microsecond),
               with_jid => WithJid,
               search_text => undefined,
               page_size => Limit2,
               limit_passed => false,
               max_result_limit => MaxResultLimit,
               is_simple => true},
    {ok, {_, _, Rows}} = mod_mam_pm:lookup_messages(HostType, Params),
    {ok, {Rows, Limit2}}.

do_open_session(#{host_type := HostType, user := JID}) ->
    SID = ejabberd_sm:make_new_sid(),
    UUID = uuid:uuid_to_string(uuid:get_v4(), binary_standard),
    Resource = <<"sse-", UUID/binary>>,
    NewJid = jid:replace_resource(JID, Resource),
    ejabberd_sm:open_session(HostType, SID, NewJid, 1, #{}),
    {ok, #{sid => SID, jid => NewJid, host_type => HostType}}.

%% Helpers

-spec build_chat_message(jid:literal_jid(), jid:literal_jid(), binary()) -> exml:element().
build_chat_message(From, To, Body) ->
    #xmlel{name = <<"message">>,
           attrs = add_id(#{<<"type">> => <<"chat">>, <<"from">> => From, <<"to">> => To}),
           children = [#xmlel{name = <<"body">>,
                              children = [#xmlcdata{content = Body}]}]
          }.

-spec build_headline_message(jid:literal_jid(), jid:literal_jid(),
                             binary() | undefined, binary() | undefined) -> exml:element().
build_headline_message(From, To, Body, Subject) ->
    Children = maybe_cdata_elem(<<"subject">>, Subject) ++
               maybe_cdata_elem(<<"body">>, Body),
    Attrs = add_id(#{<<"type">> => <<"headline">>, <<"from">> => From, <<"to">> => To}),
    #xmlel{name = <<"message">>, attrs = Attrs, children = Children}.

-spec ensure_id(exml:element()) -> exml:element().
ensure_id(Stanza) ->
    case get_id(Stanza) of
        undefined -> Stanza#xmlel{attrs = add_id(Stanza#xmlel.attrs)};
        _ -> Stanza
    end.

maybe_cdata_elem(_, undefined) -> [];
maybe_cdata_elem(Name, Text) when is_binary(Text) ->
    [cdata_elem(Name, Text)].

cdata_elem(Name, Text) when is_binary(Name), is_binary(Text) ->
    #xmlel{name = Name, children = [#xmlcdata{content = Text}]}.

add_id(Attrs) ->
    Attrs#{<<"id">> => mongoose_bin:gen_from_crypto()}.

-spec get_id(exml:element()) -> binary() | undefined.
get_id(Stanza) ->
    exml_query:attr(Stanza, <<"id">>, undefined).

fold({_, _} = Result, _) ->
    Result;
fold(M, [Step | Rest]) when is_map(M) ->
    fold(Step(M), Rest).

handle_message(HostType, Acc, Params) ->
    case mongoose_acc:stanza_name(Acc) of
        <<"message">> ->
            mongoose_c2s_hooks:user_send_message(HostType, Acc, Params);
        _ ->
            {ok, Acc}
    end.
