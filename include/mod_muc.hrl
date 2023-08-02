-record(muc_room, {
          name_host,
          opts
         }).

-record(muc_online_room, {name_host,
                          host_type,
                          pid
                         }).
