-module(mongoose_c2s_listener).

-behaviour(mongoose_listener).
-export([socket_type/0, start_listener/1]).

-behaviour(ranch_protocol).
-export([start_link/3]).

-behaviour(supervisor).
-export([start_link/1, init/1]).
-ignore_xref([start_link/1]).

-type options() :: #{atom() => any()}.

%% mongoose_listener
-spec socket_type() -> mongoose_listener:socket_type().
socket_type() ->
    xml_stream.

-spec start_listener(options()) -> ok.
start_listener(Opts) ->
    ListenerId = mongoose_listener_config:listener_id(Opts),
    ChildSpec = listener_child_spec(ListenerId, Opts),
    mongoose_listener_sup:start_child(ChildSpec),
    ok.

%% ranch_protocol
start_link(Ref, Transport, Opts) ->
	gen_statem:start_link(mongoose_c2s, {Ref, Transport, Opts}, []).

%% supervisor
-spec start_link(options()) -> any().
start_link(Opts) ->
    supervisor:start_link(?MODULE, Opts).

-spec init(options()) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init(#{module := Module} = Opts) ->
    Child = ranch:child_spec(
              ?MODULE, ranch_tcp, #{socket_opts => [{port, 6222}]}, Module, Opts),
    {ok, {#{strategy => one_for_one, intensity => 100, period => 1}, [Child]}}.

listener_child_spec(ListenerId, Opts) ->
    #{id => ListenerId,
      start => {?MODULE, start_link, [Opts]},
      restart => permanent,
      shutdown => infinity,
      type => supervisor,
      modules => [?MODULE]}.
