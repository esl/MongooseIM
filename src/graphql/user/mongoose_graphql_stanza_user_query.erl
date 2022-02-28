-module(mongoose_graphql_stanza_user_query).

-export([execute/4]).

-ignore_xref([execute/4]).

-include("../mongoose_graphql_types.hrl").
-include("mongoose_logger.hrl").

-type result() :: {ok, map()} | {error, term()}.

-spec execute(graphql:endpoint_context(), graphql:ast(), binary(), map()) ->
        result().
execute(#{user := User}, _Obj, <<"getLastMessages">>, Opts) ->
    get_last_messages(Opts, User).

get_last_messages(#{<<"limit">> := Limit,
                    <<"with">> := With, <<"before">> := Before}, Caller)
        when is_integer(Limit) ->
    mongoose_stanza_helper:get_last_messages(Caller, Limit, With, Before).
