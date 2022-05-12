-module(mod_auth_token_rdbms).
-behaviour(mod_auth_token_backend).

-export([start/1,
         get_valid_sequence_number/2,
         revoke/2,
         clean_tokens/2]).

-include("mongoose.hrl").

-spec start(mongooseim:host_type()) -> ok.
start(HostType) ->
    prepare_queries(HostType).

%% Assumption: all sequence numbers less than the current valid one
%%             are not valid.
-spec get_valid_sequence_number(mongooseim:host_type(), jid:jid()) -> integer().
get_valid_sequence_number(HostType, JID) ->
    BBareJID = jid:to_bare_binary(JID),
    {atomic, Selected} =
        mongoose_rdbms:sql_transaction(
          HostType, fun() -> get_sequence_number_t(HostType, BBareJID) end),
    mongoose_rdbms:selected_to_integer(Selected).

-spec revoke(mongooseim:host_type(), jid:jid()) -> ok | not_found.
revoke(HostType, JID) ->
    BBareJID = jid:to_bare_binary(JID),
    QueryResult = execute_revoke_token(HostType, BBareJID),
    ?LOG_DEBUG(#{what => auth_token_revoke, owner => BBareJID, sql_result => QueryResult}),
    case QueryResult of
        {updated, 1} -> ok;
        {updated, 0} -> not_found
    end.

-spec clean_tokens(mongooseim:host_type(), jid:jid()) -> ok.
clean_tokens(HostType, Owner) ->
    BBareJID = jid:to_bare_binary(Owner),
    execute_delete_token(HostType, BBareJID),
    ok.

-spec prepare_queries(mongooseim:host_type()) -> ok.
prepare_queries(HostType) ->
    mongoose_rdbms:prepare(auth_token_select, auth_token, [owner],
                           <<"SELECT seq_no FROM auth_token WHERE owner = ?">>),
    mongoose_rdbms:prepare(auth_token_revoke, auth_token, [owner],
                           <<"UPDATE auth_token SET seq_no=seq_no+1 WHERE owner = ?">>),
    mongoose_rdbms:prepare(auth_token_delete, auth_token, [owner],
                           <<"DELETE from auth_token WHERE owner = ?">>),
    rdbms_queries:prepare_upsert(HostType, auth_token_upsert, auth_token,
                                 [<<"owner">>, <<"seq_no">>], [], [<<"owner">>]),
    ok.

-spec execute_revoke_token(mongooseim:host_type(), jid:literal_jid()) -> mongoose_rdbms:query_result().
execute_revoke_token(HostType, Owner) ->
    mongoose_rdbms:execute_successfully(HostType, auth_token_revoke, [Owner]).

-spec execute_delete_token(mongooseim:host_type(), jid:literal_jid()) -> mongoose_rdbms:query_result().
execute_delete_token(HostType, Owner) ->
    mongoose_rdbms:execute_successfully(HostType, auth_token_delete, [Owner]).

-spec get_sequence_number_t(mongooseim:host_type(), jid:literal_jid()) -> mongoose_rdbms:query_result().
get_sequence_number_t(HostType, Owner) ->
    {updated, _} =
        rdbms_queries:execute_upsert(HostType, auth_token_upsert, [Owner, 1], [], [Owner]),
    mongoose_rdbms:execute_successfully(HostType, auth_token_select, [Owner]).
