-module(mongoose_stanza_helper).
-export([build_message/3]).
-export([parse_from_to/2]).

-include("jlib.hrl").

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
