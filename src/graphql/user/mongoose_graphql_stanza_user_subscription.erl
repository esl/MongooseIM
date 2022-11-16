-module(mongoose_graphql_stanza_user_subscription).
-behaviour(mongoose_graphql).

-import(mongoose_graphql_helper, [format_result/2]).

-export([execute/4]).

-ignore_xref([execute/4]).

-include("../mongoose_graphql_types.hrl").

execute(Ctx, _Obj, <<"subscribeForMessages">>, #{}) ->
    subscribe_for_messages(Ctx).

subscribe_for_messages(#{event := terminate, stream := Session}) ->
    mongoose_stanza_api:close_session(Session),
    {ok, null, [{stream, closed}]};
subscribe_for_messages(#{event := Event}) ->
    mongoose_graphql_stanza_helper:handle_event(Event);
subscribe_for_messages(#{user := Jid}) ->
    {ok, Stream} = mongoose_stanza_api:open_session(Jid, false),
    {ok, null, [{stream, Stream}]}.
