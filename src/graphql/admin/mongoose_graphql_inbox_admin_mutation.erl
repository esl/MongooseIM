-module(mongoose_graphql_inbox_admin_mutation).

-behaviour(mongoose_graphql).

-export([execute/4]).

-import(mongoose_graphql_helper, [format_result/2, null_to_default/2]).

-ignore_xref([execute/4]).

execute(_Ctx, inbox, <<"flushUserBin">>, #{<<"user">> := UserJID, <<"days">> := Days}) ->
    Res = mod_inbox_api:flush_user_bin(UserJID, null_to_default(Days, 0)),
    format_result(Res, #{user => jid:to_binary(UserJID)});
execute(_Ctx, inbox, <<"flushDomainBin">>, #{<<"domain">> := Domain, <<"days">> := Days}) ->
    Res = mod_inbox_api:flush_domain_bin(Domain, null_to_default(Days, 0)),
    format_result(Res, #{domain => Domain});
execute(_Ctx, inbox, <<"flushGlobalBin">>, #{<<"hostType">> := HostType, <<"days">> := Days}) ->
    Res = mod_inbox_api:flush_global_bin(HostType, null_to_default(Days, 0)),
    format_result(Res, #{host_type => HostType}).
