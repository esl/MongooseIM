-type upstream() :: string() | binary().

-record(match, {upstream       :: upstream(),
                remainder = [] :: [binary() | atom()],
                path      = [] :: [binary() | atom()],
                bindings  = [] :: [{atom(), binary()}]}).

-type match() :: #match{}.
