-include_lib("ejabberd/include/mod_privacy.hrl").

-define(STREAM_MGMT_H_MAX, (1 bsl 32 - 1)).
-define(STREAM_MGMT_CACHE_MAX, 100).
-define(STREAM_MGMT_ACK_FREQ, 1). %% It's the *denominator* of the frequency
-define(STREAM_MGMT_RESUME_TIMEOUT, 600).  %% seconds
-define(CONSTRAINT_CHECK_TIMEOUT, 5).  %% seconds

-type jid_set() :: gb_sets:set(ejabberd:simple_jid()).

-type authenticated_state() :: boolean() | resumed | replaced.

-type debug_presences() :: {atom(), non_neg_integer()}.

%% pres_a contains all the presence available send (either through roster mechanism or directed).
%% Directed presence unavailable remove user from pres_a.
-record(state, {socket,
                sockmod :: ejabberd:sockmod(),
                socket_monitor,
                xml_socket,
                streamid,
                sasl_state,
                access,
                shaper,
                zlib = {false, 0}          :: {boolean(), integer()},
                tls = false           :: boolean(),
                tls_required = false  :: boolean(),
                tls_enabled = false   :: boolean(),
                tls_options = [],
                authenticated = false :: authenticated_state(),
                jid                  :: ejabberd:jid() | undefined,
                user = <<>>          :: ejabberd:user(),
                server = <<>>     :: ejabberd:server(),
                resource = <<>>      :: ejabberd:resource(),
                sid                  :: ejabberd_sm:sid() | undefined,
                %% We have _subscription to_ these users' presence status;
                %% i.e. they send us presence updates.
                %% This comes from the roster.
                pres_t = gb_sets:new() :: jid_set() | debug_presences(),
                %% We have _subscription from_ these users,
                %% i.e. they have subscription to us.
                %% We send them presence updates.
                %% This comes from the roster.
                pres_f = gb_sets:new() :: jid_set() | debug_presences(),
                %% We're _available_ to these users,
                %% i.e. we broadcast presence updates to them.
                %% This may change throughout the session.
                pres_a = gb_sets:new() :: jid_set() | debug_presences(),
                %% We are _invisible_ to these users.
                %% This may change throughout the session.
                pres_i = gb_sets:new() :: jid_set() | debug_presences(),
                pending_invitations = [],
                pres_last, pres_pri,
                pres_timestamp :: calendar:datetime() | undefined,
                %% Are we invisible?
                pres_invis = false :: boolean(),
                privacy_list = #userlist{} :: mongoose_privacy:userlist(),
                conn = unknown,
                auth_module     :: ejabberd_auth:authmodule(),
                ip              :: inet:ip_address() | undefined,
                aux_fields = [] :: [{aux_key(), aux_value()}],
                lang            :: ejabberd:lang(),
                stream_mgmt = false,
                stream_mgmt_in = 0,
                stream_mgmt_id,
                stream_mgmt_out_acked = 0,
                stream_mgmt_buffer = [],
                stream_mgmt_buffer_size = 0,
                stream_mgmt_buffer_max = ?STREAM_MGMT_CACHE_MAX,
                stream_mgmt_ack_freq = ?STREAM_MGMT_ACK_FREQ,
                stream_mgmt_resume_timeout = ?STREAM_MGMT_RESUME_TIMEOUT,
                stream_mgmt_resume_tref,
                stream_mgmt_constraint_check_tref,
                csi_state = active :: mod_csi:state(),
                csi_buffer = []
                }).
-type aux_key() :: atom().
-type aux_value() :: any().
-type state() :: #state{}.

-type statename() :: atom().
-type conntype() :: 'c2s'
                  | 'c2s_compressed'
                  | 'c2s_compressed_tls'
                  | 'c2s_tls'
                  | 'http_bind'
                  | 'http_poll'
                  | 'unknown'.

%% FSM handler return value
-type fsm_return() :: {'stop', Reason :: 'normal', state()}
                    | {'next_state', statename(), state()}
                    | {'next_state', statename(), state(), Timeout :: integer()}.

-type blocking_type() :: 'block' | 'unblock'.

-type broadcast_type() :: {exit, Reason :: binary()}
                        | {item, IJID :: ejabberd:simple_jid() | ejabberd:jid(),
                           ISubscription :: from | to | both | none | remove}
                        | {privacy_list, PrivList :: mongoose_privacy:userlist(),
                           PrivListName :: binary()}
                        | {blocking, UserList :: mongoose_privacy:userlist(), What :: blocking_type(),
                                     [binary()]}
                        | unknown.

-type broadcast() :: {broadcast, broadcast_type() | mongoose_acc:t()}.

-type broadcast_result() :: {new_state, NewState :: state()}
                          | {exit, Reason :: binary()}
                          | {send_new, From :: ejabberd:jid(), To :: ejabberd:jid(),
                             Packet :: jlib:xmlel(),
                             NewState :: state()}.

-type routing_result() :: {DoRoute :: allow | atom(), NewAttrs :: [{binary(), binary()}],
                           NewState :: state()}.

%-define(DBGFSM, true).
-ifdef(DBGFSM).
-define(FSMOPTS, [{debug, [trace]}]).
-else.
-define(FSMOPTS, []).
-endif.

%% Module start with or without supervisor:
-ifdef(NO_TRANSIENT_SUPERVISORS).
-define(SUPERVISOR_START, ?GEN_FSM:start(ejabberd_c2s, [SockData, Opts],
                                         fsm_limit_opts(Opts) ++ ?FSMOPTS)).
-else.
-define(SUPERVISOR_START, supervisor:start_child(ejabberd_c2s_sup,
                                                 [SockData, Opts])).
-endif.

%% This is the timeout to apply between event when starting a new
%% session:
-define(C2S_OPEN_TIMEOUT, 60000).
-define(C2S_HIBERNATE_TIMEOUT, 90000).

-define(STREAM_HEADER,
        "<?xml version='1.0'?>"
        "<stream:stream xmlns='jabber:client' "
        "xmlns:stream='http://etherx.jabber.org/streams' "
        "id='~s' from='~s'~s~s>"
       ).

-define(STREAM_TRAILER, <<"</stream:stream>">>).

-define(INVALID_NS_ERR, ?SERR_INVALID_NAMESPACE).
-define(INVALID_XML_ERR, ?SERR_XML_NOT_WELL_FORMED).
-define(HOST_UNKNOWN_ERR, ?SERR_HOST_UNKNOWN).
-define(POLICY_VIOLATION_ERR(Lang, Text),
        ?SERRT_POLICY_VIOLATION(Lang, Text)).
-define(INVALID_FROM, ?SERR_INVALID_FROM).
-define(RESOURCE_CONSTRAINT_ERR(Lang, Text),
	?SERRT_RESOURSE_CONSTRAINT(Lang, Text)).
