%% @doc Supervisor for the socket listeners

-module(mongoose_listener_sup).

-behaviour(supervisor).

-export([start_link/0, start_child/1, init/1]).

-ignore_xref([start_link/0, init/1]).

%% API

-spec start_link() -> {ok, pid()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec start_child(supervisor:child_spec()) -> ok.
start_child(ChildSpec) ->
    {ok, _Pid} = supervisor:start_child(?MODULE, ChildSpec),
    ok.

%% Supervisor callbacks

-spec init([]) -> {ok, {supervisor:sup_flags(), []}}.
init([]) ->
    {ok, {#{strategy => one_for_one,
            intensity => 10,
            period => 1}, []}}.
