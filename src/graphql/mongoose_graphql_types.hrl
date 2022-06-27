-record(domain, {
          domain :: binary(),
          host_type = null :: binary() | null,
          enabled = null :: boolean() | null
         }).

-record(resolver_error, {reason :: atom(),
                         msg :: binary(),
                         context = #{} :: map()}).

-type resolver_error() :: #resolver_error{}.
