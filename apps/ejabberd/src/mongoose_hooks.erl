%%%-------------------------------------------------------------------
%%% @author bartek
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. Sep 2016 09:44
%%% A new hooks api.
%%%
%%% Logic:
%%% The goal is that every hook will receive a Packet as an argument, and return it, possibly with
%%% some modification to metadata and/or packet contents. The 'run' will then be similar to run_fold, which
%%% at some point in the future might go away.
%%% The issue we need to solve is that there is a lot of hooks, some of which we have no control of, which may
%%% call hooks themselves. We then have to make sure things keep working in the old way.
%%% So, we have new hooks and old hooks, and they can be run new-style and old-style.
%%% The rules now are:
%%% - a hook can be registered by ejabberd_hooks:add (old-style) and mongoose_hooks:add (new style)
%%% - hook is internally marked as 'new' or 'old'
%%% - a new-style hook must follow some rules:
%%% - the first argument it receives is either a tuple {packet, #xmlel} or an atom 'nopacket'
%%%   - if it is {packet, #xmlel} then the hook must return either a similar tuple, or a nested tuple like {{packet, #xmlel}, SomeValue}
%%%   - if it is 'nopacket' then return value must be also 'nopacket' or {'nopacket', SomeValue}
%%% - the hook machine can be called old-style, with just args, or new-style, giving {packet, #xmlel} as the first item in the list of args
%%% The hook machine, if called new-style, passes the packet through all new-style hooks and returns it.
%%%-------------------------------------------------------------------
-module(mongoose_hooks).
-author("bartek").

-include("jlib.hrl").

%% API
-export([add/4,
         add/5,
         run/3,
         run/4,
         run_fold/4,
         run_fold/5]).

add(Hook, Host, Function, Seq) ->
    ejabberd_hooks:add2(Hook, Host, Function, Seq).

add(Hook, Host, Module, Function, Seq) ->
    ejabberd_hooks:add2(Hook, Host, Module, Function, Seq).

run(Hook, #xmlel{} = Packet, Args) ->
    ejabberd_hooks:run(Hook, [{packet, Packet}|Args]).

run(Hook, Host, #xmlel{} = Packet, Args) ->
    ejabberd_hooks:run(Hook, Host, [{packet, Packet}|Args]).

run_fold(Hook, V, #xmlel{} = Packet, Args) ->
    ejabberd_hooks:run_fold(Hook, V, [{packet, Packet}|Args]).

run_fold(Hook, Host, V, #xmlel{} = Packet, Args) ->
    ejabberd_hooks:run_fold(Hook, Host, V, [{packet, Packet}|Args]).
