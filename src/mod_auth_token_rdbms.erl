-module(mod_auth_token_rdbms).
-behaviour(mod_auth_token).

-export([start/1,
         get_valid_sequence_number/1,
         revoke/1,
         clean_tokens/1]).

-include("jlib.hrl").
-include("mongoose.hrl").

-spec start(jid:lserver()) -> ok.
start(LServer) ->
    prepare_queries(LServer).

%% Assumption: all sequence numbers less than the current valid one
%%             are not valid.
-spec get_valid_sequence_number(JID) -> integer() when
      JID :: jid:jid().
get_valid_sequence_number(#jid{lserver = LServer} = JID) ->
    BBareJID = jid:to_binary(jid:to_bare(JID)),
    {atomic, Selected} =
        mongoose_rdbms:sql_transaction(
          LServer, fun() -> get_sequence_number_t(LServer, BBareJID) end),
    mongoose_rdbms:selected_to_integer(Selected).

-spec revoke(JID) -> ok | not_found when
      JID :: jid:jid().
revoke(#jid{lserver = LServer} = JID) ->
    BBareJID = jid:to_binary(jid:to_bare(JID)),
    QueryResult = execute_revoke_token(LServer, BBareJID),
    ?LOG_DEBUG(#{what => auth_token_revoke, owner => BBareJID, sql_result => QueryResult}),
    case QueryResult of
        {updated, 1} -> ok;
        {updated, 0} -> not_found
    end.

-spec clean_tokens(Owner) -> ok when
      Owner :: jid:jid().
clean_tokens(#jid{lserver = LServer} = Owner) ->
    BBareJID = jid:to_binary(jid:to_bare(Owner)),
    execute_delete_token(LServer, BBareJID),
    ok.

-spec prepare_queries(jid:lserver()) -> ok.
prepare_queries(LServer) ->
    mongoose_rdbms:prepare(auth_token_select, auth_token, [owner],
                           <<"SELECT seq_no FROM auth_token WHERE owner = ?">>),
    mongoose_rdbms:prepare(auth_token_revoke, auth_token, [owner],
                           <<"UPDATE auth_token SET seq_no=seq_no+1 WHERE owner = ?">>),
    mongoose_rdbms:prepare(auth_token_delete, auth_token, [owner],
                           <<"DELETE from auth_token WHERE owner = ?">>),
    rdbms_queries:prepare_upsert(LServer, auth_token_upsert, auth_token,
                                 [<<"owner">>, <<"seq_no">>], [], [<<"owner">>]),
    ok.

-spec execute_revoke_token(jid:lserver(), jid:literal_jid()) -> mongoose_rdbms:query_result().
execute_revoke_token(LServer, Owner) ->
    mongoose_rdbms:execute_successfully(LServer, auth_token_revoke, [Owner]).

-spec execute_delete_token(jid:lserver(), jid:literal_jid()) -> mongoose_rdbms:query_result().
execute_delete_token(LServer, Owner) ->
    mongoose_rdbms:execute_successfully(LServer, auth_token_delete, [Owner]).

-spec get_sequence_number_t(jid:lserver(), jid:literal_jid()) -> mongoose_rdbms:query_result().
get_sequence_number_t(LServer, Owner) ->
    {updated, _} =
        rdbms_queries:execute_upsert(LServer, auth_token_upsert, [Owner, 1], [], [Owner]),
    mongoose_rdbms:execute_successfully(LServer, auth_token_select, [Owner]).
