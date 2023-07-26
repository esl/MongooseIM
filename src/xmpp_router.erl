%%%-------------------------------------------------------------------
%%% @doc
%%% A behaviour which should be used by all modules being used in a
%%% routing pipeline. The pipeline, manage by ejabberd_router:route
%%% func, calls filter and route for each successful module.
%%%
%%% Module has to implement both functions, can be a no-op just returning
%%% a tuple of its args.
%%% @end
%%%-------------------------------------------------------------------
-module(xmpp_router).

-define(ARGS, From :: jid:jid(),
              To :: jid:jid(),
              Acc :: mongoose_acc:t(),
              Packet :: exml:element()).
-record(router_handler, {
          filter :: filter_fun(),
          route :: route_fun()
         }).
-type t() :: #router_handler{}.
-type filter_fun() :: fun((?ARGS) -> drop | filter()).
-type route_fun() :: fun((?ARGS) -> {done, mongoose_acc:t()} | filter()).
-type filter() :: {?ARGS}.
-export_type([filter/0, t/0]).

-callback route(?ARGS) -> {done, mongoose_acc:t()} | filter().
-callback filter(?ARGS) -> drop | filter().

-export([expand_routing_modules/1]).
-export([call_route/5, call_filter/5]).

-spec call_route(Handler :: t(), From :: jid:jid(),
    To :: jid:jid(), Acc :: mongoose_acc:t(), Packet :: exml:element()) ->
    {done, mongoose_acc:t()} | filter().
call_route(#router_handler{route = Route}, From, To, Acc, Packet) ->
    Route(From, To, Acc, Packet).

-spec call_filter(Handler :: t(), From :: jid:jid(),
    To :: jid:jid(), Acc :: mongoose_acc:t(), Packet :: exml:element()) -> drop | filter().
call_filter(#router_handler{filter = Filter}, From, To, Acc, Packet) ->
    Filter(From, To, Acc, Packet).

-spec expand_routing_modules([module()]) -> [t()].
expand_routing_modules(ModuleList) ->
    [ #router_handler{filter = fun Module:filter/4,
                      route = fun Module:route/4}
      || Module <- ModuleList ].
