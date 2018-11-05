-define(BOSH_SOCKET_SUP, ejabberd_mod_bosh_socket_sup).

-record(bosh_session, {sid,
                       socket}).

-record(bosh_socket, {sid   :: mod_bosh:sid(),
                      pid   :: pid(),
                      peer  :: {inet:ip_address(), inet:port_number()},
                      peercert :: undefined | binary()}).
