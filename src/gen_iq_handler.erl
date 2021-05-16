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

%% Old API
-export([add_iq_handler/6,
         remove_iq_handler/3,
         check_type/1]).

%%====================================================================
%% Old API
%%====================================================================
-spec add_iq_handler(Component :: module(),
                     Domain :: jid:server(),
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
                        Domain :: jid:server(),
                        Namespace :: binary()) -> any().
remove_iq_handler(Component, Domain, Namespace) ->
    gen_iq_component:unregister_iq_handler(Component, Domain, Namespace).

-spec check_type(mongoose_iq_handler:execution_type()) ->
    mongoose_iq_handler:execution_type().
check_type(no_queue)  -> no_queue;
check_type(parallel)  -> parallel;
check_type(one_queue) -> one_queue;
check_type({queues, Int}) when is_integer(Int), Int > 0 ->
    {queues, Int}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
-spec make_iq_handler_fn(module(), atom()) -> mongoose_iq_handler:iq_handler().
make_iq_handler_fn(Module, Function) ->
    fun(Acc, From, To, IQ, _Extra) ->
        Module:Function(From, To, Acc, IQ)
    end.
