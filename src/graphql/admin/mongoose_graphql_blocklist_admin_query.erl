-module(mongoose_graphql_blocklist_admin_query).

-behaviour(mongoose_graphql).

%% mongoose_graphql callbacks
-export([execute/4]).

-include("jlib.hrl").

-import(mongoose_graphql_helper, [make_error/3, null_to_undefined/1, undefined_to_null/1]).

%% ================================================================
%% mongoose_graphql callback
%% ================================================================

execute(_Ctx, blocklist, <<"listBlockedUsers">>, Args) ->
    list_blocked_users(Args);
execute(_Ctx, blocklist, <<"userCount">>, Args) ->
    user_count(Args).

%% ================================================================
%% Action implementation
%% ================================================================

list_blocked_users(#{<<"domain">> := Domain} = Args) ->
    Limit = null_to_undefined(maps:get(<<"limit">>, Args, undefined)),
    Index = null_to_undefined(maps:get(<<"index">>, Args, undefined)),
    Opts = maps:filter(fun(_K, V) -> V =/= undefined end, #{limit => Limit, offset => Index}),
    case mod_blocklist_api:list_blocked_users(jid:nameprep(Domain), Opts) of
        {domain_not_found, Msg} ->
            make_error(domain_not_found, Msg, #{domain => Domain});
        {ok, Users} ->
            {ok, [{ok, #{<<"jid">> => Jid, <<"reason">> => undefined_to_null(Reason)}} || {Jid, Reason} <- Users]}
    end.

user_count(#{<<"domain">> := Domain}) ->
    case mod_blocklist_api:count_blocked_users(jid:nameprep(Domain)) of
        {domain_not_found, Msg} ->
            make_error(domain_not_found, Msg, #{domain => Domain});
        {ok, UserCount} ->
            {ok, UserCount}
    end.
