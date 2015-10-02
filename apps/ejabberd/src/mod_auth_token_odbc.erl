-module(mod_auth_token_odbc).
-behaviour(mod_auth_token).

-export([get_valid_sequence_number/1,
         revoke/3]).

-include("jlib.hrl").

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

%-spec revoke(Type, Owner, SeqNo) -> ok.
revoke(_, _, _) ->
    ok.
