-module(mod_auth_token_odbc).
-behaviour(mod_auth_token).

-export([get_valid_sequence_number/1,
         revoke/1,
         clean_tokens/1]).

-include("jlib.hrl").
-include("ejabberd.hrl").

%% Assumption: all sequence numbers less than the current valid one
%%             are not valid.
-spec get_valid_sequence_number(JID) -> integer() when
      JID :: ejabberd:jid().
get_valid_sequence_number(#jid{lserver = LServer} = JID) ->
    BBareJID = jlib:jid_to_binary(jlib:jid_remove_resource(JID)),
    EBareJID = ejabberd_odbc:escape(BBareJID),
    Q = valid_sequence_number_query(EBareJID),
    [{updated, undefined},
     {updated, _},
     {selected, _, [{BSeqNo}]},
     {updated, undefined}] = ejabberd_odbc:sql_query(LServer, Q),
    binary_to_integer(BSeqNo).

valid_sequence_number_query(EOwner) when is_binary(EOwner) ->
    [<<"BEGIN; "
       "WITH existing AS (SELECT at.seq_no "
       "                  FROM auth_token at "
       "                  WHERE at.owner = '", EOwner/bytes, "') "
       "INSERT INTO auth_token "
         "SELECT '", EOwner/bytes, "', 1 "
       "WHERE NOT EXISTS (SELECT * FROM existing); "
       "SELECT seq_no "
       "FROM auth_token "
       "WHERE owner = '", EOwner/bytes, "'; "
       "COMMIT;">>].

-spec revoke(JID) -> ok | not_found when
      JID :: ejabberd:jid().
revoke(#jid{lserver = LServer} = JID) ->
    BBareJID = jlib:jid_to_binary(jlib:jid_remove_resource(JID)),
    EBareJID = ejabberd_odbc:escape(BBareJID),
    RevokeQuery = revoke_query(EBareJID),
    QueryResult = ejabberd_odbc:sql_query(LServer, RevokeQuery),
    ?DEBUG("result ~p", [QueryResult]),
    case QueryResult of
        {updated, 1} -> ok;
        {updated, 0} -> not_found
    end.

revoke_query(EOwner) when is_binary(EOwner) ->
    [<<"UPDATE auth_token SET seq_no=seq_no+1 WHERE owner = '", EOwner/bytes, "';">>].

-spec clean_tokens(Owner) -> ok when
      Owner :: ejabberd:jid().
clean_tokens(#jid{lserver = LServer} = Owner) ->
    BBareJID = jlib:jid_to_binary(jlib:jid_remove_resource(Owner)),
    EBareJID = ejabberd_odbc:escape(BBareJID),
    Q = clean_tokens_query(EBareJID),
    {updated, _} = ejabberd_odbc:sql_query(LServer, Q),
    ok.

clean_tokens_query(EOwner) when is_binary(EOwner) ->
    [<<"DELETE FROM auth_token WHERE owner = '", EOwner/bytes, "';">>].
