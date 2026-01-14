-module(mongoose_graphql_broadcast_admin_mutation).
-behaviour(mongoose_graphql).

-export([execute/4]).

-ignore_xref([execute/4]).

-include("../mongoose_graphql_types.hrl").

-import(mongoose_graphql_helper, [make_error/2, null_to_undefined/1]).

execute(_Ctx, _Obj, <<"startBroadcast">>, Args) ->
    start_broadcast(Args);
execute(_Ctx, _Obj, <<"abortBroadcast">>, Args) ->
    abort_broadcast(Args);
execute(_Ctx, _Obj, <<"deleteBroadcasts">>, Args) ->
    delete_broadcasts(Args).

start_broadcast(#{<<"domain">> := Domain,
                  <<"name">> := Name,
                  <<"senderJid">> := SenderJid,
                  <<"subject">> := Subject,
                  <<"body">> := Body,
                  <<"ratePerSecond">> := Rate,
                  <<"recipients">> := Recipients}) ->
    Recipients2 = recipients_to_api(Recipients),
    Subject2 = null_to_undefined(Subject),
    case mod_broadcast_api:start_broadcast(#{domain => Domain,
                                            name => Name,
                                            sender_jid => SenderJid,
                                            subject => Subject2,
                                            body => Body,
                                            rate_per_second => Rate,
                                            recipients => Recipients2}) of
        {ok, Job} ->
            {ok, mongoose_graphql_broadcast_admin_query:job_to_gql(Job)};
        Error ->
            make_error(Error, #{domain => Domain})
    end.

abort_broadcast(#{<<"id">> := Id}) ->
    case mod_broadcast_api:abort_broadcast(Id) of
        {ok, Job} -> {ok, mongoose_graphql_broadcast_admin_query:job_to_gql(Job)};
        Error -> make_error(Error, #{id => Id})
    end.

delete_broadcasts(#{<<"ids">> := Ids}) ->
    Ids2 = null_to_undefined(Ids),
    case mod_broadcast_api:delete_broadcasts(#{ids => Ids2}) of
        {ok, #{deleted_count := C}} -> {ok, #{<<"deletedCount">> => C}};
        Error -> make_error(Error, #{})
    end.

recipients_to_api(#{<<"type">> := <<"ALL_USERS">>}) ->
    #{type => all_users};
recipients_to_api(#{<<"type">> := 'ALL_USERS'}) ->
    #{type => all_users};
recipients_to_api(#{<<"type">> := <<"USERNAMES">>, <<"usernames">> := Usernames}) ->
    #{type => usernames, usernames => Usernames};
recipients_to_api(#{<<"type">> := 'USERNAMES', <<"usernames">> := Usernames}) ->
    #{type => usernames, usernames => Usernames};
recipients_to_api(#{<<"type">> := <<"USERNAMES">>}) ->
    #{type => usernames, usernames => []};
recipients_to_api(#{<<"type">> := 'USERNAMES'}) ->
    #{type => usernames, usernames => []};
recipients_to_api(_) ->
    #{type => all_users}.
