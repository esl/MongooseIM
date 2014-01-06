%%%-------------------------------------------------------------------
%%% @author Uvarov Michael <arcusfelis@gmail.com>
%%% @copyright (C) 2013, Uvarov Michael
%%% @doc A dummy backend for preferencies.
%%% @end
%%%-------------------------------------------------------------------
-module(mod_mam_null_prefs).
-export([start/2,
         get_behaviour/6,
         get_prefs/5,
         set_prefs/7,
         remove_archive/4]).

-include_lib("ejabberd/include/ejabberd.hrl").
-include_lib("ejabberd/include/jlib.hrl").
-include_lib("exml/include/exml.hrl").

start(_Host, _Mod) ->
    ok.

get_behaviour(_Host, _Mod, _ArcID, _LocJID, _RemJID, DefaultBehaviour) ->
    DefaultBehaviour.

set_prefs(_Host, _Mod, _ArcID, _ArcJID, _DefaultMode, _AlwaysJIDs, _NeverJIDs) ->
    ok.

get_prefs(_Host, _Mod, _ArcID, _ArcJID, GlobalDefaultMode) ->
    {GlobalDefaultMode, [], []}.

remove_archive(_Host, _Mod, _ArcID, _ArcJID) ->
    ok.
