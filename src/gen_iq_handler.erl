%%%----------------------------------------------------------------------
%%% File    : gen_iq_handler.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : IQ handler support
%%% Created : 22 Jan 2003 by Alexey Shchepin <alexey@process-one.net>
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

-module(gen_iq_handler).
-author('alexey@process-one.net').

%% Old API. Get rid of it once all the modules adopted.
-export([add_iq_handler/6,
         remove_iq_handler/3]).

%% New API.
-export([add_iq_handler_for_domain/6,
         add_iq_handler_for_subdomain/7,
         remove_iq_handler_for_domain/3,
         remove_iq_handler_for_subdomain/4]).

-type execution_type() :: mongoose_iq_handler:execution_type().
-type subdomain_pattern() :: mongoose_subdomain_utils:subdomain_pattern().

%%====================================================================
%% Old API. Get rid of it once all the modules adopted.
%%====================================================================
-spec add_iq_handler(Component :: module(),
                     Domain :: jid:lserver(),
                     Namespace :: binary(),
                     Module :: atom(),
                     Function :: atom(),
                     ExecutionType :: mongoose_iq_handler:execution_type()) -> any().
add_iq_handler(Component, Domain, Namespace, Module, Function, ExecutionType) ->
    Extra = #{delete_on_unregister => true, module => Module, function => Function},
    IQHandlerFn = make_iq_handler_fn(Module, Function),
    IQHandler = mongoose_iq_handler:new(IQHandlerFn, Extra, ExecutionType),
    gen_iq_component:register_iq_handler(Component, Domain, Namespace, IQHandler).

-spec remove_iq_handler(Component :: module(),
                        Domain :: jid:lserver(),
                        Namespace :: binary()) -> any().
remove_iq_handler(Component, Domain, Namespace) ->
    gen_iq_component:unregister_iq_handler(Component, Domain, Namespace).

%%====================================================================
%% New API.
%%====================================================================
-spec add_iq_handler_for_domain(HostType :: mongooseim:host_type(),
                                Namespace :: binary(),
                                Component :: module(),
                                IQHandlerFn :: mongoose_iq_handler:handler_fn(),
                                Extra :: map(),
                                ExecutionType :: execution_type()) ->
    ok | {error, atom()}.
add_iq_handler_for_domain(HostType, Namespace, Component, IQHandlerFn,
                          Extra, ExecutionType) ->
    %% TODO: `delete_on_unregister` extra field is not needed once old API is removed
    NewExtra = Extra#{delete_on_unregister => false},
    IQHandler = mongoose_iq_handler:new(IQHandlerFn, NewExtra, ExecutionType),
    mongoose_lazy_routing:register_iq_handler_for_domain(HostType, Namespace,
                                                         Component, IQHandler).

-spec add_iq_handler_for_subdomain(HostType :: mongooseim:host_type(),
                                   SubdomainPattern :: subdomain_pattern(),
                                   Namespace :: binary(),
                                   Component :: module(),
                                   IQHandlerFn :: mongoose_iq_handler:handler_fn(),
                                   Extra :: map(),
                                   ExecutionType :: execution_type()) ->
    ok | {error, atom()}.
add_iq_handler_for_subdomain(HostType, SubdomainPattern, Namespace, Component,
                             IQHandlerFn, Extra, ExecutionType) ->
    %% TODO: `delete_on_unregister` extra field is not needed once old API is removed
    NewExtra = Extra#{delete_on_unregister => false},
    IQHandler = mongoose_iq_handler:new(IQHandlerFn, NewExtra, ExecutionType),
    mongoose_lazy_routing:register_iq_handler_for_subdomain(HostType, SubdomainPattern,
                                                            Namespace, Component,
                                                            IQHandler).

-spec remove_iq_handler_for_domain(HostType :: mongooseim:host_type(),
                                   Namespace :: binary(),
                                   Component :: module()) ->
    ok | {error, not_registered}.
remove_iq_handler_for_domain(HostType, Namespace, Component) ->
    case mongoose_lazy_routing:unregister_iq_handler_for_domain(
             HostType, Namespace, Component) of
        {ok, IQHandler} ->
            mongoose_iq_handler:delete(IQHandler);
        {error, not_found} -> {error, not_registered}
    end.

-spec remove_iq_handler_for_subdomain(HostType :: mongooseim:host_type(),
                                      SubdomainPattern :: subdomain_pattern(),
                                      Namespace :: binary(),
                                      Component :: module()) ->
    ok | {error, not_registered}.
remove_iq_handler_for_subdomain(HostType, SubdomainPattern, Namespace, Component) ->
    case mongoose_lazy_routing:unregister_iq_handler_for_subdomain(
             HostType, SubdomainPattern, Namespace, Component) of
        {ok, IQHandler} ->
            mongoose_iq_handler:delete(IQHandler);
        {error, not_found} -> {error, not_registered}
    end.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
-spec make_iq_handler_fn(module(), atom()) -> mongoose_iq_handler:handler_fn().
make_iq_handler_fn(Module, Function) ->
    %TODO: remove this function with removal of the old API
    fun(Acc, From, To, IQ, _Extra) ->
        Module:Function(From, To, Acc, IQ)
    end.
