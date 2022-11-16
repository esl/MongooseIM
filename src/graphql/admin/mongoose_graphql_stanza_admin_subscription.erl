-module(mongoose_graphql_stanza_admin_subscription).
-behaviour(mongoose_graphql).

-import(mongoose_graphql_helper, [format_result/2]).

-export([execute/4]).

-ignore_xref([execute/4]).

-include("../mongoose_graphql_types.hrl").

execute(Ctx, _Obj, <<"subscribeForMessages">>, Args) ->
    subscribe_for_messages(Ctx, Args).

subscribe_for_messages(#{event := terminate, stream := Session}, _) ->
    mongoose_stanza_api:close_session(Session),
    {ok, null, [{stream, closed}]};
subscribe_for_messages(#{event := Event}, _) ->
    mongoose_graphql_stanza_helper:handle_event(Event);
subscribe_for_messages(_Ctx, #{<<"caller">> := Jid}) ->
    case mongoose_stanza_api:open_session(Jid, true) of
        {ok, Stream} ->
            {ok, null, [{stream, Stream}]};
        Error ->
            format_result(Error, #{caller => Jid})
    end.
