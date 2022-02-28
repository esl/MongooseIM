-module(mongoose_stanza_helper).
-export([build_message/3]).
-export([build_message_with_headline/3]).
-export([parse_from_to/2]).
-export([get_last_messages/4]).
-export([route/4]).

-include("jlib.hrl").
-include("mongoose_logger.hrl").

-spec build_message(From :: binary(), To :: binary(), Body :: binary()) -> exml:element().
build_message(From, To, Body) ->
    #xmlel{name = <<"message">>,
           attrs = [{<<"type">>, <<"chat">>},
                    {<<"id">>, mongoose_bin:gen_from_crypto()},
                    {<<"from">>, From},
                    {<<"to">>, To}],
           children = [#xmlel{name = <<"body">>,
                              children = [#xmlcdata{content = Body}]}]
          }.

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

-spec parse_from_to(jid:jid() | binary() | undefined, jid:jid() | binary() | undefined) ->
    {ok, jid:jid(), jid:jid()} | {error, missing} | {error, type_error, string()}.
parse_from_to(F, T) when F == undefined; T == undefined ->
    {error, missing};
parse_from_to(F, T) ->
    case parse_jid_list([F, T]) of
        {ok, [Fjid, Tjid]} -> {ok, Fjid, Tjid};
        E -> E
    end.

-spec parse_jid_list(BinJids :: [binary()]) -> {ok, [jid:jid()]} | {error, type_error, string()}.
parse_jid_list([_ | _] = BinJids) ->
    Jids = lists:map(fun parse_jid/1, BinJids),
    case [Msg || {error, Msg} <- Jids] of
        [] -> {ok, Jids};
        Errors -> {error, type_error, lists:join("; ", Errors)}
    end.

-spec parse_jid(binary() | jid:jid()) -> jid:jid() | {error, string()}.
parse_jid(#jid{} = Jid) ->
    Jid;
parse_jid(Jid) when is_binary(Jid) ->
    case jid:from_binary(Jid) of
        error -> {error, io_lib:format("Invalid jid: ~p", [Jid])};
        B -> B
    end.

get_last_messages(Caller, Limit, With, Before) ->
    With2 = null_as_undefined(With),
    Before2 = null_as_undefined(Before), %% Before is in microseconds
    Limit2 = min(500, Limit),
    Rows = mongoose_stanza_api:lookup_recent_messages(Caller, With2, Before2, Limit2),
    Maps = lists:map(fun row_to_map/1, Rows),
    {ok, #{<<"stanzas">> => Maps, <<"limit">> => Limit2}}.

null_as_undefined(null) -> undefined;
null_as_undefined(Value) -> Value.

-spec row_to_map(mod_mam:message_row()) -> {ok, map()}.
row_to_map(#{id := Id, jid := From, packet := Msg}) ->
    {Microseconds, _} = mod_mam_utils:decode_compact_uuid(Id),
    StanzaID = mod_mam_utils:mess_id_to_external_binary(Id),
    Map = #{<<"sender">> => From, <<"timestamp">> => Microseconds,
            <<"stanza_id">> => StanzaID, <<"stanza">> => Msg},
    {ok, Map}.

route(From = #jid{lserver = LServer}, To, Packet, SkipAuth) ->
    case mongoose_graphql_helper:check_user(From, SkipAuth) of
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
