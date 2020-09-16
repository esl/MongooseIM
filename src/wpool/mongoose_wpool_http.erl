-module(mongoose_wpool_http).
-behaviour(mongoose_wpool).

-export([init/0]).
-export([start/4, stop/2]).
-export([get_params/2]).

%% --------------------------------------------------------------
%% API
%% --------------------------------------------------------------

init() ->
    case ets:info(?MODULE) of
        undefined ->
            Heir = case whereis(ejabberd_sup) of
                       undefined -> [];
                       Pid -> [{heir, Pid, undefined}]
                   end,
            ets:new(?MODULE,
                    [named_table, public, {read_concurrency, true} | Heir]),
            ok;
        _ ->
            ok
    end.

start(Host, Tag, WpoolOptsIn, ConnOpts) ->
    Name = mongoose_wpool:make_pool_name(http, Host, Tag),
    WpoolOpts = wpool_spec(Host, WpoolOptsIn, ConnOpts),
    PathPrefix = list_to_binary(gen_mod:get_opt(path_prefix, ConnOpts, "/")),
    RequestTimeout = gen_mod:get_opt(request_timeout, ConnOpts, 2000),
    case mongoose_wpool:start_sup_pool(http, Name, WpoolOpts) of
        {ok, Pid} ->
            ets:insert(?MODULE, {{Host, Tag}, PathPrefix, RequestTimeout}),
            {ok, Pid};
        Other ->
            Other
    end.

stop(Host, Tag) ->
    true = ets:delete(?MODULE, {Host, Tag}),
    ok.

-spec get_params(Host :: jid:lserver() | global, Tag :: atom()) ->
    {ok, PathPrefix :: binary(), RequestTimeout :: non_neg_integer()}
    | {error, pool_not_started}.
get_params(Host, Tag) ->
    case {ets:lookup(?MODULE, {Host, Tag}), Host} of
        {[], global} -> {error, pool_not_started};
        {[], _} -> get_params(global, Tag);
        {[{_, PathPrefix, RequestTimeout}], _} -> {ok, PathPrefix, RequestTimeout}
    end.

%% --------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------

wpool_spec(Host, WpoolOptsIn, ConnOpts) ->
    TargetServer = gen_mod:get_opt(server, ConnOpts),
    HttpOpts = gen_mod:get_opt(http_opts, ConnOpts, []),
    Worker = {fusco, {TargetServer, HttpOpts}},
    [{worker, Worker} | WpoolOptsIn].

