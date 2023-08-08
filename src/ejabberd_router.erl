%%%----------------------------------------------------------------------
%%% File    : ejabberd_router.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Main router
%%% Created : 27 Nov 2002 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2011   ProcessOne
%%%
%%% This program is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation; either version 2 of the
%%% License, or (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with this program; if not, write to the Free Software
%%% Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
%%%
%%%----------------------------------------------------------------------

-module(ejabberd_router).
-author('alexey@process-one.net').

-behaviour(gen_server).
%% API
-export([route/3,
         route/4,
         route_error/4,
         route_error_reply/4]).

-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-ignore_xref([route_error/4, start_link/0]).

-include("mongoose.hrl").
-include("jlib.hrl").

-record(state, {}).

%%====================================================================
%% API
%%====================================================================

-spec start_link() -> 'ignore' | {'error', _} | {'ok', pid()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc The main routing function. It puts the message through a chain
%% of filtering/routing modules, as defined in config 'routing_modules'
%% setting (default is hardcoded in default_routing_modules function
%% of this module). Each of those modules should use xmpp_router behaviour
%% and implement two functions:
%% filter/3 - should return either 'drop' atom or its args
%% route/3, which should either:
%%   - deliver the message locally by calling mongoose_local_delivery:do_route/5
%%     and return 'done'
%%   - deliver the message it its own way and return 'done'
%%   - return its args
%%   - return a tuple of {From, To, Packet} which might be modified
%% For both functions, returning a 'drop' or 'done' atom terminates the procedure,
%% while returning a tuple means 'proceed' and the tuple is passed to
%% the next module in sequence.
-spec route(From   :: jid:jid(),
    To     :: jid:jid(),
    Packet :: mongoose_acc:t()|exml:element()) -> mongoose_acc:t().
route(From, To, #xmlel{} = Packet) ->
    % ?LOG_ERROR("Deprecated - it should be Acc: ~p", [Packet]),
    Acc = mongoose_acc:new(#{ location => ?LOCATION,
                              lserver => From#jid.lserver,
                              element => Packet,
                              from_jid => From,
                              to_jid => To }),
    % (called by broadcasting)
    route(From, To, Acc);
route(From, To, Acc) ->
    ?LOG_DEBUG(#{what => route, acc => Acc}),
    El = mongoose_acc:element(Acc),
    RoutingModules = mongoose_router:routing_modules_list(),
    NewAcc = route(From, To, Acc, El, RoutingModules),
    ?LOG_DEBUG(#{what => routing_result,
                 routing_result => mongoose_acc:get(router, result, {drop, undefined}, NewAcc),
                 routing_modules => RoutingModules, acc => Acc}),
    NewAcc.

-spec route(From   :: jid:jid(),
            To     :: jid:jid(),
            Acc :: mongoose_acc:t(),
            El :: exml:element() | {error, term()}) -> mongoose_acc:t().
route(_From, _To, Acc, {error, Reason} = Err) ->
    ?LOG_INFO(#{what => cannot_route_stanza, acc => Acc, reason => Reason}),
    mongoose_acc:append(router, result, Err, Acc);
route(From, To, Acc, El) ->
    Acc1 = mongoose_acc:update_stanza(#{ from_jid => From,
                                         to_jid => To,
                                         element => El }, Acc),
    ?LOG_DEBUG(#{what => route, acc => Acc1}),
    RoutingModules = mongoose_router:routing_modules_list(),
    NewAcc = route(From, To, Acc1, El, RoutingModules),
    ?LOG_DEBUG(#{what => routing_result,
                 routing_result => mongoose_acc:get(router, result, {drop, undefined}, NewAcc),
                 routing_modules => RoutingModules, acc => Acc}),
    NewAcc.

%% Route the error packet only if the originating packet is not an error itself.
%% RFC3920 9.3.1
-spec route_error(From   :: jid:jid(),
                  To     :: jid:jid(),
                  Acc :: mongoose_acc:t(),
                  ErrPacket :: exml:element()) -> mongoose_acc:t().
route_error(From, To, Acc, ErrPacket) ->
    case mongoose_acc:stanza_type(Acc) of
        <<"error">> ->
            Acc;
        _ ->
            route(From, To, Acc, ErrPacket)
    end.

-spec route_error_reply(jid:jid(), jid:jid(), mongoose_acc:t(), exml:element()) ->
    mongoose_acc:t().
route_error_reply(From, To, Acc, Error) ->
    {Acc1, ErrorReply} = jlib:make_error_reply(Acc, Error),
    route_error(From, To, Acc1, ErrorReply).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    mongoose_metrics:ensure_metric(global, routingErrors, spiral),
    mongoose_component:start(),
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    mongoose_component:stop(),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

-spec route(From   :: jid:jid(),
            To     :: jid:jid(),
            Acc    :: mongoose_acc:t(),
            Packet :: exml:element(),
            [xmpp_router:t()]) -> mongoose_acc:t().
route(_From, _To, Acc, _Packet, []) ->
    ?LOG_ERROR(#{what => no_more_routing_modules, acc => Acc}),
    mongoose_metrics:update(global, routingErrors, 1),
    mongoose_acc:append(router, result, {error, out_of_modules}, Acc);
route(OrigFrom, OrigTo, Acc0, OrigPacket, [M|Tail]) ->
    try xmpp_router:call_filter(M, OrigFrom, OrigTo, Acc0, OrigPacket) of
        drop ->
            mongoose_acc:append(router, result, {drop, M}, Acc0);
        {OrigFrom, OrigTo, Acc1, OrigPacketFiltered} ->
            try xmpp_router:call_route(M, OrigFrom, OrigTo, Acc1, OrigPacketFiltered) of
                {done, Acc2} ->
                    mongoose_acc:append(router, result, {done, M}, Acc2);
                {From, To, NAcc1, Packet} ->
                    route(From, To, NAcc1, Packet, Tail)
                catch Class:Reason:Stacktrace ->
                    ?LOG_WARNING(#{what => routing_failed,
                                   router_module => M, acc => Acc1,
                                   class => Class, reason => Reason, stacktrace => Stacktrace}),
                    mongoose_acc:append(router, result, {error, {M, Reason}}, Acc1)
            end
        catch Class:Reason:Stacktrace ->
            ?LOG_WARNING(#{what => route_filter_failed,
                           text => <<"Error when filtering packet in router">>,
                           router_module => M, acc => Acc0,
                           class => Class, reason => Reason, stacktrace => Stacktrace}),
            mongoose_acc:append(router, result, {error, {M, Reason}}, Acc0)
    end.
