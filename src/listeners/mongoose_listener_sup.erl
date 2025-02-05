%% @doc Supervisor for the socket listeners
-module(mongoose_listener_sup).

-behaviour(supervisor).

-export([start_link/0, start_child/1, init/1]).

-ignore_xref([start_link/0, init/1]).

%% API

-spec start_link() -> supervisor:startlink_ret().
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, noargs).

-spec start_child(supervisor:child_spec()) -> ok.
start_child(ChildSpec) ->
    %% Use ejabberd_sup function for extra logging on errors
    ejabberd_sup:start_child(?MODULE, ChildSpec),
    ok.

%% Supervisor callbacks

-spec init(noargs) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init(noargs) ->
    {ok, {#{strategy => one_for_one,
            intensity => 10,
            period => 1}, []}}.
