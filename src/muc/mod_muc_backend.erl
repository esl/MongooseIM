-module(mod_muc_backend).
-export([init/2,
         store_room/4,
         restore_room/3,
         forget_room/3,
         get_rooms/2,
         can_use_nick/4,
         get_nick/3,
         set_nick/4,
         unset_nick/3,
         remove_domain/3
    ]).

-ignore_xref([remove_domain/3]).

-define(MAIN_MODULE, mod_muc).

-include("mod_muc.hrl").

%% Host of MUC service
-type muc_host() :: jid:server().

%% User's JID. Can be on another domain accessible over FED.
%% Only bare part (user@host) is important.
-type client_jid() :: jid:jid().

-type room_opts() :: [{OptionName :: atom(), OptionValue :: term()}].


%% Called when MUC service starts or restarts for each domain
-callback init(mongooseim:host_type(), ModuleOpts :: gen_mod:module_opts()) -> ok.

-callback store_room(mongooseim:host_type(), muc_host(), mod_muc:room(), room_opts()) ->
    ok | {error, term()}.

-callback restore_room(mongooseim:host_type(), muc_host(), mod_muc:room()) ->
    {ok, room_opts()} | {error, room_not_found} | {error, term()}.

-callback forget_room(mongooseim:host_type(), muc_host(), mod_muc:room()) ->
    ok | {error, term()}.

-callback get_rooms(mongooseim:host_type(), muc_host()) ->
    {ok, [#muc_room{}]} | {error, term()}.

-callback can_use_nick(mongooseim:host_type(), muc_host(),
                       client_jid(), mod_muc:nick()) -> boolean().

%% Get nick associated with jid client_jid() across muc_host() domain
-callback get_nick(mongooseim:host_type(), muc_host(), client_jid()) ->
    {ok, mod_muc:nick()} | {error, not_registered} | {error, term()}.

%% Register nick
-callback set_nick(mongooseim:host_type(), muc_host(), client_jid(), mod_muc:nick()) ->
    ok | {error, conflict} | {error, term()}.

%% Unregister nick
%% Unregistered nicks can be used by someone else
-callback unset_nick(mongooseim:host_type(), muc_host(), client_jid()) ->
    ok | {error, term()}.

-callback remove_domain(mongooseim:host_type(), muc_host(), jid:lserver()) -> ok.

-optional_callbacks([remove_domain/3]).

%% Called when MUC service starts or restarts for each domain
-spec init(mongooseim:host_type(), Opts :: gen_mod:module_opts()) -> ok.
init(HostType, Opts) ->
    TrackedFuns = [store_room, restore_room, forget_room, get_rooms,
                     can_use_nick, get_nick, set_nick, unset_nick],
    mongoose_backend:init(HostType, ?MAIN_MODULE, TrackedFuns, Opts),
    Args = [HostType, Opts],
    mongoose_backend:call(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec store_room(mongooseim:host_type(), muc_host(), mod_muc:room(), room_opts()) ->
    ok | {error, term()}.
store_room(HostType, MucHost, Room, RoomOpts) ->
    Args = [HostType, MucHost, Room, RoomOpts],
    mongoose_backend:call_tracked(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec restore_room(mongooseim:host_type(), muc_host(), mod_muc:room()) ->
    {ok, room_opts()} | {error, room_not_found} | {error, term()}.
restore_room(HostType, MucHost, Room) ->
    Args = [HostType, MucHost, Room],
    mongoose_backend:call_tracked(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec forget_room(mongooseim:host_type(), muc_host(), mod_muc:room()) ->
    ok | {error, term()}.
forget_room(HostType, MucHost, Room) ->
    Args = [HostType, MucHost, Room],
    mongoose_backend:call_tracked(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec get_rooms(mongooseim:host_type(), muc_host()) ->
    {ok, [#muc_room{}]} | {error, term()}.
get_rooms(HostType, MucHost) ->
    Args = [HostType, MucHost],
    mongoose_backend:call_tracked(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec can_use_nick(mongooseim:host_type(), muc_host(), client_jid(), mod_muc:nick()) ->
    boolean().
can_use_nick(HostType, MucHost, ClientJID, Nick) ->
    Args = [HostType, MucHost, ClientJID, Nick],
    mongoose_backend:call_tracked(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

%% Get nick associated with jid client_jid() across muc_host() domain
-spec get_nick(mongooseim:host_type(), muc_host(), client_jid()) ->
    {ok, mod_muc:nick()} | {error, not_registered} | {error, term()}.
get_nick(HostType, MucHost, ClientJID) ->
    Args = [HostType, MucHost, ClientJID],
    mongoose_backend:call_tracked(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

%% Register nick
-spec set_nick(mongooseim:host_type(), muc_host(), client_jid(), mod_muc:nick()) ->
    ok | {error, conflict} | {error, term()}.
set_nick(HostType, MucHost, ClientJID, Nick) ->
    Args = [HostType, MucHost, ClientJID, Nick],
    mongoose_backend:call_tracked(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

%% Unregister nick
%% Unregistered nicks can be used by someone else
-spec unset_nick(mongooseim:host_type(), muc_host(), client_jid()) ->
    ok | {error, term()}.
unset_nick(HostType, MucHost, ClientJID) ->
    Args = [HostType, MucHost, ClientJID],
    mongoose_backend:call_tracked(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec remove_domain(mongooseim:host_type(), muc_host(), jid:lserver()) -> ok.
remove_domain(HostType, MUCHost, Domain) ->
    case mongoose_backend:is_exported(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY) of
        true ->
            Args = [HostType, MUCHost, Domain],
            mongoose_backend:call(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, Args);
        false ->
            ok
    end.
