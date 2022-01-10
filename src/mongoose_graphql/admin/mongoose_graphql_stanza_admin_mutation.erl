-module(mongoose_graphql_stanza_admin_mutation).

-export([execute/4]).

-ignore_xref([execute/4]).

-include("../mongoose_graphql_types.hrl").
-include("mongoose_logger.hrl").
-include("jlib.hrl").

execute(_Ctx, _Obj, <<"sendMessage">>, Opts) ->
    send_message(Opts);
execute(_Ctx, _Obj, <<"sendMessageHeadLine">>, Opts) ->
    send_message_headline(Opts);
execute(_Ctx, _Obj, <<"sendStanza">>, Opts) ->
    send_stanza(Opts).

send_message(Opts = #{<<"from">> := From, <<"to">> := To, <<"body">> := Body}) ->
    Packet = mongoose_stanza_helper:build_message(jid:to_binary(From), jid:to_binary(To), Body),
    do_routing(From, To, Packet).

send_message_headline(Opts = #{<<"from">> := From, <<"to">> := To}) ->
    Packet = build_message_with_headline(jid:to_binary(From), jid:to_binary(To), Opts),
    do_routing(From, To, Packet).

send_stanza(Opts = #{<<"stanza">> := Packet}) ->
    From = jid:from_binary(exml_query:attr(Packet, <<"from">>)),
    To = jid:from_binary(exml_query:attr(Packet, <<"to">>)),
    do_routing(From, To, Packet).

do_routing(From = #jid{lserver = LServer}, To, Packet) ->
    case mongoose_domain_api:get_domain_host_type(LServer) of
        {ok, HostType} ->
            do_routing(HostType, LServer, From, To, Packet);
        _ ->
            {error, #{what => unknown_domain, domain => LServer}}
    end.

do_routing(HostType, LServer, From, To, Packet) ->
   case ejabberd_auth:does_user_exist(HostType, From, stored) of
       true ->
           do_routing2(HostType, LServer, From, To, Packet);
       false ->
            {error, #{what => non_existing_user, jid => jid:to_binary(From)}}
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
