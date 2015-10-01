-module(mod_auth_token_odbc).
-behaviour(mod_auth_token).

-export([get_sequence_number/2,
         is_revoked/3,
         revoke/3]).

%-spec get_sequence_number(Type, Owner) -> integer().
get_sequence_number(_, _) ->
    ok.

-spec is_revoked(Type, Owner, SeqNo) -> boolean() when
      Type :: mod_auth_token:token_type(),
      Owner :: ejabberd:jid(),
      SeqNo :: mod_auth_token:sequence_no().
is_revoked(_, _, _) ->
    true.

%-spec revoke(Type, Owner, SeqNo) -> ok.
revoke(_, _, _) ->
    ok.
