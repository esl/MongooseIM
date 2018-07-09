-record(conv, {
          unread,
          from,
          to,
          content = <<>>,
          verify = fun(_C, _Stanza) -> ok end,
          time_after
         }).
