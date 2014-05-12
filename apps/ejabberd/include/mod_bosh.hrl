-define(BOSH_BACKEND, (mod_bosh_dynamic:backend())).
-define(BOSH_SOCKET_SUP, ejabberd_mod_bosh_socket_sup).

-record(bosh_session, {sid :: mod_bosh:sid(),
                       socket :: pid()}).

-record(bosh_socket, {sid   :: mod_bosh:sid(),
                      pid   :: pid(),
                      peer  :: {inet:ip_address(), inet:port_number()}}).
