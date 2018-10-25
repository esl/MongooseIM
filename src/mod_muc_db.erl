-module(mod_muc_db).
-include("mod_muc.hrl").

%% Defines which RDBMS pool to use
%% Parent host of the MUC service
-type server_host() :: ejabberd:server().

%% Host of MUC service
-type muc_host() :: ejabberd:server().

%% User's JID. Can be on another domain accessable over FED.
%% Only bare part (user@host) is important.
-type client_jid() :: ejabberd:jid().

-type room_opts() :: [{OptionName :: atom(), OptionValue :: term()}].



%% Called when MUC service starts or restarts for each domain
-callback init(server_host(), ModuleOpts :: list()) -> ok.

-callback store_room(server_host(), muc_host(), mod_muc:room(), room_opts()) ->
    ok | {error, term()}.

-callback restore_room(server_host(), muc_host(), mod_muc:room()) ->
    {ok, room_opts()} | {error, room_not_found} | {error, term()}.

-callback forget_room(server_host(), muc_host(), mod_muc:room()) ->
    ok | {error, term()}.

-callback can_use_nick(server_host(), muc_host(),
                       client_jid(), mod_muc:nick()) -> boolean().

-callback get_rooms(server_host(), muc_host()) ->
    {ok, [#muc_room{}]} | {error, term()}.

%% Get nick associated with jid client_jid() across muc_host() domain
-callback get_nick(server_host(), muc_host(), client_jid()) ->
    {ok, mod_muc:nick()} | {error, not_registered} | {error, term()}.

%% Register nick
-callback set_nick(server_host(), muc_host(), client_jid(), mod_muc:nick()) ->
    ok | {error, conflict} | {error, term()}.

%% Unregister nick
%% Unregistered nicks can be used by someone else
-callback unset_nick(server_host(), muc_host(), client_jid()) ->
    ok | {error, term()}.
