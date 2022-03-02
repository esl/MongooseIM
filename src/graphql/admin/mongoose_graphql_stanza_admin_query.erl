-module(mongoose_graphql_stanza_admin_query).

-export([execute/4]).

-ignore_xref([execute/4]).

-include("../mongoose_graphql_types.hrl").
-include("mongoose_logger.hrl").

-type result() :: {ok, map()} | {error, term()}.

-spec execute(graphql:endpoint_context(), graphql:ast(), binary(), map()) ->
        result().
execute(_Ctx, _Obj, <<"getLastMessages">>, Opts) ->
    get_last_messages(Opts).

get_last_messages(#{<<"caller">> := Caller, <<"limit">> := Limit,
                    <<"with">> := With, <<"before">> := Before})
        when is_integer(Limit) ->
    case mongoose_graphql_helper:check_user(Caller) of
        {ok, _HostType} ->
            get_last_messages2(Caller, Limit, With, Before);
        Error ->
            Error
    end.

get_last_messages2(Caller, Limit, With, Before) ->
    With2 = null_as_undefined(With),
    Before2 = null_as_undefined(Before), %% Before is in microseconds
    Limit2 = min(500, Limit),
    Rows = mongoose_stanza_api:lookup_recent_messages(Caller, With2, Before2, Limit2),
    Maps = lists:map(fun mongoose_graphql_stanza_helper:row_to_map/1, Rows),
    {ok, #{<<"stanzas">> => Maps, <<"limit">> => Limit2}}.

null_as_undefined(null) -> undefined;
null_as_undefined(Value) -> Value.
