-record(muc_room, {
          name_host,
          opts
         }).

-record(muc_online_room, {name_host,
                          pid
                         }).

-record(muc_registered, {
          us_host,
          nick
         }).
