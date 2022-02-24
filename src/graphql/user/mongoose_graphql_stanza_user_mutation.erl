-module(mongoose_graphql_stanza_user_mutation).

-export([execute/4]).

-ignore_xref([execute/4]).

-include("../mongoose_graphql_types.hrl").
-include("mongoose_logger.hrl").
-include("jlib.hrl").

-type result() :: {ok, map()} | {error, term()}.

-spec execute(graphql:endpoint_context(), graphql:ast(), binary(), map()) ->
    result().
execute(Ctx, _Obj, <<"sendMessage">>, Opts) ->
    send_message(Ctx, Opts);
execute(Ctx, _Obj, <<"sendMessageHeadLine">>, Opts) ->
    send_message_headline(Ctx, Opts);
execute(Ctx, _Obj, <<"sendStanza">>, Opts) ->
    send_stanza(Ctx, Opts).

send_message(Ctx, Opts) ->
    with_from(Ctx, Opts, fun send_message2/1).

send_message_headline(Ctx, Opts) ->
    with_from(Ctx, Opts, fun send_message_headline2/1).

send_message2(#{<<"from">> := From, <<"to">> := To, <<"body">> := Body}) ->
    Packet = mongoose_stanza_helper:build_message(jid:to_binary(From), jid:to_binary(To), Body),
    do_routing(From, To, Packet).

send_message_headline2(Opts = #{<<"from">> := From, <<"to">> := To}) ->
    Packet = build_message_with_headline(jid:to_binary(From), jid:to_binary(To), Opts),
    do_routing(From, To, Packet).

send_stanza(#{user := User}, #{<<"stanza">> := Packet}) ->
    From = jid:from_binary(exml_query:attr(Packet, <<"from">>)),
    To = jid:from_binary(exml_query:attr(Packet, <<"to">>)),
    case compare_bare_jids(User, From) of
        true ->
            do_routing(From, To, Packet);
        false ->
            {error, bad_from_jid}
    end.

do_routing(From = #jid{lserver = LServer}, To, Packet) ->
    case mongoose_graphql_helper:check_user(From) of
        {ok, HostType} ->
            do_routing2(HostType, LServer, From, To, Packet);
        Error ->
            Error
    end.

do_routing2(HostType, LServer, From, To, Packet) ->
    %% Based on mod_commands:do_send_packet/3
    Acc = mongoose_acc:new(#{location => ?LOCATION,
                              host_type => HostType,
                              lserver => LServer,
                              element => Packet}),
    Acc1 = mongoose_hooks:user_send_packet(Acc, From, To, Packet),
    Acc2 = ejabberd_router:route(From, To, Acc1),
    MessID = mod_mam_utils:get_mam_id_ext(Acc2),
    {ok, #{ <<"id">> => MessID }}.

build_message_with_headline(FromBin, ToBin,
                            #{<<"body">> := Body, <<"subject">> := Subject}) ->
    Children = maybe_cdata_elem(<<"subject">>, Subject) ++
               maybe_cdata_elem(<<"body">>, Body),
    Attrs = [{<<"type">>, <<"headline">>},
             {<<"id">>, mongoose_bin:gen_from_crypto()},
             {<<"from">>, FromBin},
             {<<"to">>, ToBin}],
    #xmlel{name = <<"message">>, attrs = Attrs, children = Children}.

maybe_cdata_elem(_, null) -> [];
maybe_cdata_elem(_, <<>>) -> [];
maybe_cdata_elem(Name, Text) when is_binary(Text) ->
    [cdata_elem(Name, Text)].

cdata_elem(Name, Text) when is_binary(Name), is_binary(Text) ->
    #xmlel{name = Name, children = [#xmlcdata{content = Text}]}.

compare_bare_jids(#jid{luser = U, lserver = S}, #jid{luser = U, lserver = S}) -> true;
compare_bare_jids(_, _) -> false.

with_from(_Ctx = #{user := User}, Opts, Next) ->
    case maps:get(<<"from">>, Opts, null) of
        null ->
            Next(Opts#{<<"from">> => User});
        From ->
            case compare_bare_jids(User, From) of
                true ->
                    %% We still can allow a custom resource
                    Next(Opts#{<<"from">> => From});
                false ->
                    ?LOG_ERROR(#{what => bad_from_jid,
                                 user_jid => User, from_jid => From}),
                    {error, bad_from_jid}
            end
    end.
