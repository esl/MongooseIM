-module(mongoose_template_sup).

-behaviour(supervisor).

-export([start_link/2, init/1]).

-ignore_xref([init/1, start_link/2]).

-spec start_link(atom(), module()) -> supervisor:startlink_ret().
start_link(Name, Module) ->
    supervisor:start_link({local, Name}, ?MODULE, Module).

-spec init(module()) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init(Module) ->
    SupFlags = #{strategy => simple_one_for_one, intensity => 10, period => 1},
    Spec = #{
        id => undefined,
        start => {Module, start_link, []},
        restart => temporary,
        shutdown => brutal_kill,
        type => worker,
        modules => [Module]
    },
    {ok, {SupFlags, [Spec]}}.
