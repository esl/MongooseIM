-module(mongoose_wpool_riak).
-behaviour(mongoose_wpool).

-export([init/0]).
-export([start/4]).
-export([stop/2]).
-export([is_supported_strategy/1]).
-export([get_riak_opt/2]).
-export([get_riak_opt/3]).

init() ->
    ok.

start(Host, Tag, WpoolOptsIn, ConnOpts) ->
    Name = mongoose_wpool:make_pool_name(riak, Host, Tag),
    WpoolOpts = wpool_spec(WpoolOptsIn, ConnOpts),
    mongoose_wpool:start_sup_pool(riak, Name, WpoolOpts).

stop(_, _) ->
    ok.

is_supported_strategy(available_worker) -> false;
is_supported_strategy(_) -> true.

wpool_spec(WpoolOptsIn, ConnOpts) ->
    {_, RiakAddr} = mongoose_wpool_riak:get_riak_opt(address, ConnOpts),
    {_, RiakPort} = mongoose_wpool_riak:get_riak_opt(port, ConnOpts),
    SecurityOptsKeys = [credentials, cacertfile, ssl_opts],
    SecurityOpts = [get_riak_opt(OptKey, ConnOpts) || OptKey <- SecurityOptsKeys],
    RiakPBOpts = [auto_reconnect, keepalive],
    WorkerArgs = maybe_add_additional_opts(RiakPBOpts, SecurityOpts),
    Worker = {riakc_pb_socket, [RiakAddr, RiakPort, WorkerArgs]},
    [{worker, Worker} | WpoolOptsIn].


%% @doc Gets a particular option from `Opts'. They're expressed as a list
%% of tuples where the first element is `OptKey'. If provided `OptKey' doesn't
%% exist the `Default' is returned.
-spec get_riak_opt(OptKey :: atom(), Opts :: [tuple()], Default :: tuple()) ->
                                   tuple().
get_riak_opt(OptKey, Opts, Default) ->
    Opt = get_riak_opt(OptKey, Opts),
    verify_if_riak_opt_exists(Opt, Default).

get_riak_opt(OptKey, Opts) ->
    lists:keyfind(OptKey, 1, Opts).

verify_if_riak_opt_exists(false, Default) -> Default;
verify_if_riak_opt_exists(Opt, _) -> Opt.

%% @doc Merges `AdditionalOpts' into `Opts' if a particular additional option
%% exists.
-spec maybe_add_additional_opts(Opts :: [term()],
                                   AdditionalOpts :: [tuple()]) -> [term()].
maybe_add_additional_opts(Opts, AdditionalOpts) ->
    Opts2 = [verify_if_riak_opt_exists(Opt, []) || Opt <- AdditionalOpts],
    lists:flatten(Opts ++ Opts2).
