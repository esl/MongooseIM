-module(mongoose_graphql_inbox_user_mutation).

-behaviour(mongoose_graphql).

-export([execute/4]).

-import(mongoose_graphql_helper, [format_result/2]).

-ignore_xref([execute/4]).

execute(#{user := UserJID}, inbox, <<"flushBin">>, #{<<"days">> := Days}) ->
    Res = mod_inbox_api:flush_user_bin(UserJID, Days),
    format_result(Res, #{user => jid:to_binary(UserJID)}).
