-record(domain, {
          domain :: binary(),
          host_type = null :: binary() | null,
          status = null :: mongoose_domain_api:status() | null
         }).

-record(resolver_error, {reason :: atom(),
                         msg :: binary(),
                         context = #{} :: map()}).

-type resolver_error() :: #resolver_error{}.
