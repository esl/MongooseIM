%%% @doc
%% options and defaults:
%%     * server - (required)
%%     * path_prefix - ""
%%     * request_timeout - 2000,
%%     * http_opts - [] % passed to fusco
%%%
%%% @end
-module(mongoose_wpool_http).
-behaviour(mongoose_wpool).

-export([init/0]).
-export([start/4, stop/2]).
-export([get_params/2]).

-type path_prefix() :: binary().
-type request_timeout() :: non_neg_integer().

%% --------------------------------------------------------------
%% mongoose_wpool callbacks
-spec init() -> ok.
init() ->
    ejabberd_sup:create_ets_table(?MODULE, [named_table, public, {read_concurrency, true}]).

-spec start(mongooseim:host_type_or_global(), mongoose_wpool:tag(),
            mongoose_wpool:pool_opts(), mongoose_wpool:conn_opts()) -> {ok, pid()} | {error, any()}.
start(HostType, Tag, WpoolOptsIn, ConnOpts) ->
    Name = mongoose_wpool:make_pool_name(http, HostType, Tag),
    WpoolOpts = wpool_spec(WpoolOptsIn, ConnOpts),
    #{path_prefix := PathPrefix, request_timeout := RequestTimeout} = ConnOpts,
    case mongoose_wpool:start_sup_pool(http, Name, WpoolOpts) of
        {ok, Pid} ->
            ets:insert(?MODULE, {{HostType, Tag}, PathPrefix, RequestTimeout}),
            {ok, Pid};
        Other ->
            Other
    end.

-spec stop(mongooseim:host_type_or_global(), mongoose_wpool:tag()) -> ok.
stop(HostType, Tag) ->
    true = ets:delete(?MODULE, {HostType, Tag}),
    ok.

%% --------------------------------------------------------------
%% Other API functions
-spec get_params(HostType :: mongooseim:host_type_or_global(),
                 Tag :: mongoose_wpool:tag()) ->
    {ok, PathPrefix :: path_prefix(), RequestTimeout :: request_timeout()}
    | {error, pool_not_started}.
get_params(HostType, Tag) ->
    case {ets:lookup(?MODULE, {HostType, Tag}), HostType} of
        {[], global} -> {error, pool_not_started};
        {[], _} -> get_params(global, Tag);
        {[{_, PathPrefix, RequestTimeout}], _} -> {ok, PathPrefix, RequestTimeout}
    end.

%% --------------------------------------------------------------
%% Internal functions

wpool_spec(WpoolOptsIn, #{host := Host} = ConnOpts) ->
    HTTPOpts = http_opts(ConnOpts),
    Worker = {fusco, {Host, [{connect_options, HTTPOpts}]}},
    [{worker, Worker} | WpoolOptsIn].

http_opts(#{tls := TLSOpts}) ->
    just_tls:make_client_opts(TLSOpts);
http_opts(#{}) ->
    [].
