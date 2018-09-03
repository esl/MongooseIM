-module(mod_auth_token_rdbms).
-behaviour(mod_auth_token).

-export([get_valid_sequence_number/1,
         revoke/1,
         clean_tokens/1]).

-include("jlib.hrl").
-include("mongoose.hrl").

%% Assumption: all sequence numbers less than the current valid one
%%             are not valid.
-spec get_valid_sequence_number(JID) -> integer() when
      JID :: jid:jid().
get_valid_sequence_number(#jid{lserver = LServer} = JID) ->
    BBareJID = jid:to_binary(jid:to_bare(JID)),
    EBareJID = mongoose_rdbms:escape_string(BBareJID),
    Q = valid_sequence_number_query(EBareJID),
    {atomic, [{updated, _}, {selected, [{BSeqNo}]}]} = mongoose_rdbms:sql_transaction(LServer, Q),
    mongoose_rdbms:result_to_integer(BSeqNo).

valid_sequence_number_query(EOwner) ->
    SqlOwner = iolist_to_binary(mongoose_rdbms:use_escaped_string(EOwner)),
    [<<"WITH existing AS (SELECT at.seq_no "
       "                  FROM auth_token at "
       "                  WHERE at.owner = ", SqlOwner/bytes, ") "
       "INSERT INTO auth_token "
         "SELECT ", SqlOwner/bytes, ", 1 "
       "WHERE NOT EXISTS (SELECT * FROM existing); ">>,
     <<"SELECT seq_no "
       "FROM auth_token "
       "WHERE owner = ", SqlOwner/bytes, "; ">>].

-spec revoke(JID) -> ok | not_found when
      JID :: jid:jid().
revoke(#jid{lserver = LServer} = JID) ->
    BBareJID = jid:to_binary(jid:to_bare(JID)),
    EBareJID = mongoose_rdbms:escape_string(BBareJID),
    RevokeQuery = revoke_query(EBareJID),
    QueryResult = mongoose_rdbms:sql_query(LServer, RevokeQuery),
    ?DEBUG("result ~p", [QueryResult]),
    case QueryResult of
        {updated, 1} -> ok;
        {updated, 0} -> not_found
    end.

-spec revoke_query(mongoose_rdbms:escaped_string()) ->
    mongoose_rdbms:sql_query_part().
revoke_query(EOwner) ->
    [<<"UPDATE auth_token SET seq_no=seq_no+1 WHERE owner = ">>,
     mongoose_rdbms:use_escaped_string(EOwner)].

-spec clean_tokens(Owner) -> ok when
      Owner :: jid:jid().
clean_tokens(#jid{lserver = LServer} = Owner) ->
    BBareJID = jid:to_binary(jid:to_bare(Owner)),
    EBareJID = mongoose_rdbms:escape_string(BBareJID),
    Q = clean_tokens_query(EBareJID),
    {updated, _} = mongoose_rdbms:sql_query(LServer, Q),
    ok.

-spec clean_tokens_query(mongoose_rdbms:escaped_string()) ->
    mongoose_rdbms:sql_query_part().
clean_tokens_query(EOwner) ->
    [<<"DELETE FROM auth_token WHERE owner = ">>,
     mongoose_rdbms:use_escaped_string(EOwner)].
