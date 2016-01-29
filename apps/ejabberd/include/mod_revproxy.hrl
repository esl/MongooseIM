-record(upstream, {type       :: uri | host,
                   protocol   :: binary(),
                   host  = [] :: [binary() | atom()],
                   path  = [] :: [binary() | atom()]}).

-type upstream() :: #upstream{}.

-record(match, {upstream       :: upstream(),
                remainder = [] :: [binary() | atom()],
                path      = [] :: [binary() | atom()],
                bindings  = [] :: [{atom(), binary()}]}).

-type match() :: #match{}.
