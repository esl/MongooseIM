%%% ====================================================================
%%% ``The contents of this file are subject to the Erlang Public License,
%%% Version 1.1, (the "License"); you may not use this file except in
%%% compliance with the License. You should have received a copy of the
%%% Erlang Public License along with this software. If not, it can be
%%% retrieved via the world wide web at http://www.erlang.org/.
%%%
%%%
%%% Software distributed under the License is distributed on an "AS IS"
%%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%%% the License for the specific language governing rights and limitations
%%% under the License.
%%%
%%%
%%% The Initial Developer of the Original Code is ProcessOne.
%%% Portions created by ProcessOne are Copyright 2006-2015, ProcessOne
%%% All Rights Reserved.''
%%% This software is copyright 2006-2015, ProcessOne.
%%%
%%%
%%% @copyright 2006-2015 ProcessOne
%%% @author Christophe Romain <christophe.romain@process-one.net>
%%%   [http://www.process-one.net/]
%%% @end
%%% ====================================================================

%% important note:
%% new/1 and free/2 MUST be called inside a transaction bloc

-module(pubsub_index).
-author('christophe.romain@process-one.net').

-include("pubsub.hrl").

-export([init/0, new/1]).

init() ->
    mnesia:create_table(pubsub_index,
                        [{disc_copies, [node()]},
                         {attributes, record_info(fields, pubsub_index)}]).

new(Index) ->
    %% Create a new short lived transaction to reduce lock contention
    spawn_and_call(fun() -> mnesia:transaction(fun() -> new_transaction(Index) end) end).

new_transaction(Index) ->
    case mnesia:wread({pubsub_index, Index}) of
        [I] ->
            Id = I#pubsub_index.last + 1,
            mnesia:write(I#pubsub_index{last = Id}),
            Id;
        _ ->
            mnesia:write(#pubsub_index{index = Index, last = 1, free = []}),
            1
    end.

spawn_and_call(F) ->
    Ref = make_ref(),
    Parent = self(),
    FF = fun() -> Result = F(), Parent ! {call_result, Ref, Result} end,
    Pid = spawn_monitor(FF),
    receive
        {call_result, Ref, Result} ->
            {atomic, NewId} = Result,
            erlang:demonitor(Ref, [flush]),
            NewId;
        {'DOWN', Ref, process, Pid, Reason} ->
            erlang:error({spawn_and_call_failed, Reason})
    after 5000 ->
            erlang:demonitor(Ref, [flush]),
            erlang:error(spawn_and_call_timeout)
    end.
