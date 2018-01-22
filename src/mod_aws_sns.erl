%%%-------------------------------------------------------------------
%%% @author Rafal Slota
%%% @copyright (C) 2017 Erlang Solutions Ltd.
%%% This software is released under the Apache License, Version 2.0
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Amazon SNS notifications. This module gathers all message send by users and all presence
%%% changes and publishes those events to AWS SNS.
%%% @end
%%%-------------------------------------------------------------------
-module(mod_aws_sns).
-author("Rafal Slota").

-include("mongoose.hrl").

-behavior(gen_mod).

%% MIM module callbacks
-export([deps/2, start/2, stop/1]).

%%%===================================================================
%%% API functions
%%%===================================================================

deps(_Host, Opts) ->
    [{mod_event_pusher, [{backends, [{sns, Opts}]}], hard}].

-spec start(Host :: jid:server(), Opts :: proplists:proplist()) -> any().
start(_Host, _Opts) ->
    ?WARNING_MSG("mod_aws_sns is deprecated and will be removed in the future.~n"
                 "Please use mod_event_pusher with sns backend.~n"
                 "Refer to mod_event_pusher documentation for more information.", []).

-spec stop(Host :: jid:server()) -> ok.
stop(_Host) ->
    ok.
