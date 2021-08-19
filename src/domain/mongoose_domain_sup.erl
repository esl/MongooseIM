-module(mongoose_domain_sup).
-export([start_link/0]).
-export([init/1]).

-ignore_xref([start_link/0]).

-spec(start_link() ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => rest_for_one,
                 intensity => 100, period => 5,
                 shutdown => 1000},
    ChildSpecs = [],
    {ok, { SupFlags, ChildSpecs }}.
