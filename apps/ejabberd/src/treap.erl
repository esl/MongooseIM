%%%----------------------------------------------------------------------
%%% File    : treap.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Treaps implementation
%%% Created : 22 Apr 2008 by Alexey Shchepin <alexey@process-one.net>
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
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
%%% 02111-1307 USA
%%%
%%%----------------------------------------------------------------------

-module(treap).

-export([empty/0,
         insert/4,
         delete/2,
         delete_root/1,
         get_root/1,
         lookup/2,
         is_empty/1,
         fold/3,
         from_list/1,
         to_list/1]).

-type treap() :: {hashkey(),
                  Prio :: _,
                  Value :: _,
                  Left :: treap(),
                  Right :: treap()
                 } | 'nil'.
-type hashkey() :: {non_neg_integer(), _}.
-type kpv() :: {Key :: _, Prio :: _, Value :: _}.


-spec empty() -> treap().
empty() ->
    nil.

-spec insert(_,_,_,treap()) -> treap().
insert(Key, Priority, Value, Tree) ->
    HashKey = {erlang:phash2(Key), Key},
    insert1(Tree, HashKey, Priority, Value).


-spec insert1(treap(), hashkey(),_,_) -> treap().
insert1(nil, HashKey, Priority, Value) ->
    {HashKey, Priority, Value, nil, nil};
insert1({HashKey1, Priority1, Value1, Left, Right} = Tree,
        HashKey, Priority, Value) ->
    if
        HashKey < HashKey1 ->
            heapify({HashKey1, Priority1, Value1,
                     insert1(Left, HashKey, Priority, Value),
                     Right});
        HashKey > HashKey1 ->
            heapify({HashKey1, Priority1, Value1,
                     Left,
                     insert1(Right, HashKey, Priority, Value)});
        Priority == Priority1 ->
            {HashKey, Priority, Value, Left, Right};
        true ->
            insert1(delete_root(Tree), HashKey, Priority, Value)
    end.


-spec heapify({_,_,_,treap(),treap()}) -> treap().
heapify(nil) ->
    nil;
heapify({_HashKey, _Priority, _Value, nil, nil} = Tree) ->
    Tree;
heapify({HashKey, Priority, Value,
         nil = Left,
         {HashKeyR, PriorityR, ValueR, LeftR, RightR}} = Tree) ->
    if
        PriorityR > Priority ->
            {HashKeyR, PriorityR, ValueR,
             {HashKey, Priority, Value, Left, LeftR},
             RightR};
        true ->
            Tree
    end;
heapify({HashKey, Priority, Value,
         {HashKeyL, PriorityL, ValueL, LeftL, RightL},
         nil = Right} = Tree) ->
    if
        PriorityL > Priority ->
            {HashKeyL, PriorityL, ValueL,
             LeftL,
             {HashKey, Priority, Value, RightL, Right}};
        true ->
            Tree
    end;
heapify({HashKey, Priority, Value,
         {HashKeyL, PriorityL, ValueL, LeftL, RightL} = Left,
         {HashKeyR, PriorityR, ValueR, LeftR, RightR} = Right} = Tree) ->
    if
        PriorityR > Priority ->
            {HashKeyR, PriorityR, ValueR,
             {HashKey, Priority, Value, Left, LeftR},
             RightR};
        PriorityL > Priority ->
            {HashKeyL, PriorityL, ValueL,
             LeftL,
             {HashKey, Priority, Value, RightL, Right}};
        true ->
            Tree
    end.


-spec delete(_, treap()) -> treap().
delete(Key, Tree) ->
    HashKey = {erlang:phash2(Key), Key},
    delete1(HashKey, Tree).


-spec delete1(hashkey(), treap()) -> treap().
delete1(_HashKey, nil) ->
    nil;
delete1(HashKey, {HashKey1, Priority1, Value1, Left, Right} = Tree) ->
    if
        HashKey < HashKey1 ->
            {HashKey1, Priority1, Value1, delete1(HashKey, Left), Right};
        HashKey > HashKey1 ->
            {HashKey1, Priority1, Value1, Left, delete1(HashKey, Right)};
        true ->
            delete_root(Tree)
    end.


-spec delete_root(treap()) -> treap().
delete_root({HashKey, Priority, Value, Left, Right}) ->
    case {Left, Right} of
        {nil, nil} ->
            nil;
        {_, nil} ->
            Left;
        {nil, _} ->
            Right;
        {{HashKeyL, PriorityL, ValueL, LeftL, RightL},
         {HashKeyR, PriorityR, ValueR, LeftR, RightR}} ->
            if
                PriorityL > PriorityR ->
                    {HashKeyL, PriorityL, ValueL,
                     LeftL,
                     delete_root({HashKey, Priority, Value, RightL, Right})};
                true ->
                    {HashKeyR, PriorityR, ValueR,
                     delete_root({HashKey, Priority, Value, Left, LeftR}),
                     RightR}
            end
    end.


-spec is_empty(treap()) -> boolean().
is_empty(nil) ->
    true;
is_empty({_HashKey, _Priority, _Value, _Left, _Right}) ->
    false.


-spec get_root(treap()) -> {_,_,_}.
get_root({{_Hash, Key}, Priority, Value, _Left, _Right}) ->
    {Key, Priority, Value}.


-spec lookup(_, treap()) -> 'error' | {'ok',_,_}.
lookup(Key, Tree) ->
    HashKey = {erlang:phash2(Key), Key},
    lookup1(Tree, HashKey).


-spec lookup1(treap(), hashkey()) -> 'error' | {'ok',_,_}.
lookup1(nil, _HashKey) ->
    error;
lookup1({HashKey1, Priority1, Value1, Left, Right}, HashKey) ->
    if
        HashKey < HashKey1 ->
            lookup1(Left, HashKey);
        HashKey > HashKey1 ->
            lookup1(Right, HashKey);
        true ->
            {ok, Priority1, Value1}
    end.


-spec fold(_,_,treap()) -> any().
fold(_F, Acc, nil) ->
    Acc;
fold(F, Acc, {{_Hash, Key}, Priority, Value, Left, Right}) ->
    Acc1 = F({Key, Priority, Value}, Acc),
    Acc2 = fold(F, Acc1, Left),
    fold(F, Acc2, Right).


-spec to_list(treap()) -> [kpv()].
to_list(Tree) ->
    to_list(Tree, []).


-spec to_list(treap(), [kpv()]) -> [kpv()].
to_list(nil, Acc) ->
    Acc;
to_list(Tree, Acc) ->
    Root = get_root(Tree),
    to_list(delete_root(Tree), [Root|Acc]).


-spec from_list([kpv()]) -> treap().
from_list(List) ->
    from_list(List, nil).


-spec from_list([kpv()], treap()) -> treap().
from_list([{Key, Priority, Value}|Tail], Tree) ->
    from_list(Tail, insert(Key, Priority, Value, Tree));
from_list([], Tree) ->
    Tree.
