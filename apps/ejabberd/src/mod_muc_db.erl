-module(mod_muc_db).

-callback init(ejabberd:server(), list()) -> ok.
-callback store_room(ejabberd:server(), ejabberd:server(),
                     mod_muc:room(), list()) ->
    {'aborted', _} | {'atomic', _}.
-callback restore_room(ejabberd:server(), ejabberd:server(), mod_muc:room()) ->
    'error' | 'undefined' | [any()].
-callback forget_room(ejabberd:server(), ejabberd:server(),
                      mod_muc:room()) -> 'ok'.
-callback can_use_nick(ejabberd:server(), ejabberd:server(),
                       ejabberd:jid(), mod_muc:nick()) -> boolean().
-callback get_rooms(ejabberd:server(), ejabberd:server()) -> list().

%% Get nick associated with jid From across MucHost domain
-callback get_nick(LServer, MucHost, From) ->
    error | Nick
      when
      %% Defines which ODBC pool to use
      %% Parent host of the MUC service
      LServer :: ejabberd:server(),
      %% Host of MUC service
      MucHost :: ejabberd:server(),
      %% User's JID. Can be on another domain accessable over FED.
      %% Only bare part (user@host) is important.
      From :: ejabberd:jid(),
      %% Nick to register
      %% Only one nick can be registred for each unique bare From.
      %% Nick is registered per MUC-host (not per room).
      %% Registered nicks are not available to register.
      Nick :: mod_muc:nick().

%% Register nick
-callback set_nick(LServer, MucHost, From, Nick) ->
    ok | {error, conflict} | {error, should_not_be_empty} | {error, term()}
      when
      %% Defines which ODBC pool to use
      %% Parent host of the MUC service
      LServer :: ejabberd:server(),
      %% Host of MUC service
      MucHost :: ejabberd:server(),
      %% User's JID. Can be on another domain accessable over FED.
      %% Only bare part (user@host) is important.
      From :: ejabberd:jid(),
      %% Nick to register
      %% Only one nick can be registred for each unique bare From.
      %% Nick is registered per MUC-host (not per room).
      %% Registered nicks are not available to register.
      Nick :: mod_muc:nick().

%% Unregister nick
%% Unregistered nicks can be used by someone else
-callback unset_nick(LServer, MucHost, From) ->
    ok | {error, term()}
      when
      %% Defines which ODBC pool to use
      %% Parent host of the MUC service
      LServer :: ejabberd:server(),
      %% Host of MUC service
      MucHost :: ejabberd:server(),
      %% User's JID. Can be on another domain accessable over FED.
      %% Only bare part (user@host) is important.
      From :: ejabberd:jid().
