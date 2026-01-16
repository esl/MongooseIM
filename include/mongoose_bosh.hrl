-define(BOSH_SOCKET_SUP, mongoose_bosh_socket_sup).

-record(bosh_session, {sid,
                       socket}).

-record(bosh_socket, {sid   :: service_bosh:sid(),
                      pid   :: pid(),
                      peer  :: mongoose_transport:peer(),
                      peercert :: undefined | binary()}).
