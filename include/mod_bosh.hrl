-record(bosh_session, {sid,
                       socket}).

-record(bosh_socket, {sid   :: mod_bosh:sid(),
                      pid   :: pid(),
                      peer  :: mongoose_transport:peer(),
                      peercert :: undefined | binary()}).
