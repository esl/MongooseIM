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
%%% @version {@vsn}, {@date} {@time}
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
    Parent = self(),
    Ref = make_ref(),
    F = fun() -> {atomic, Id} = mnesia:transaction(fun() -> get_next(Index) end), Parent ! {new_id_result, Ref, Id} end,
    spawn_link(F),
    receive
        {new_id_result, Ref, Id} -> Id
    after
        5000 -> erlang:error(timeout)
    end.

get_next(Index) ->
    case mnesia:read(pubsub_index, Index, sticky_write) of
        [I] ->
            Id = I#pubsub_index.last + 1,
            mnesia:write(I#pubsub_index{last = Id}),
            Id;
        _ ->
            mnesia:write(#pubsub_index{index = Index, last = 1, free = []}),
            1
    end.
