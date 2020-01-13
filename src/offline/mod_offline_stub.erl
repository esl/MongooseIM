%%%----------------------------------------------------------------------
%%% File    : mod_offline_stub.erl
%%% Author  : Radek Szymczyszyn
%%%           <radoslaw.szymczyszyn@erlang-solutions.com>
%%% Purpose : Silence <service-unavailable/> when not using mod_offline
%%%           but using mod_mam for message persistence.
%%%
%%%           RFC 6121 requires a <service-unavailable/> stanza error
%%%           to be sent to a user messaging an unavailable recipient
%%%           if the message is not stored for delayed delivery
%%%           (i.e. as an "offline message").
%%%           If the recipient exists (i.e. auth module returns `true`
%%%           from `is_user_exists`) mod_mam stores the message,
%%%           but <service-unavailable/> is still returned,
%%%           what is not compliant with the RFC.
%%%
%%%           This module prevents returning <service-unavailable/>.
%%% See     : RFC 6121 8.5.2.2.1
%%% License : GNU GPLv2 or (at your option) any later version
%%%
%%% MongooseIM, Copyright (C) 2014 Erlang Solutions
%%%----------------------------------------------------------------------

-module(mod_offline_stub).
-author('mongoose-im@erlang-solutions.com').
-behaviour(gen_mod).
-behaviour(mongoose_module_metrics).

%% gen_mod callbacks
-export([start/2,
         stop/1]).

%% Hook handlers
-export([stop_hook_processing/4]).

-spec start(any(), any()) -> 'ok'.
start(Host, _Opts) ->
    [ejabberd_hooks:add(Hook, Host, M, F, Prio)
     || {Hook, M, F, Prio} <- handlers()],
    ok.

-spec stop(any()) -> 'ok'.
stop(Host) ->
    [ejabberd_hooks:delete(Hook, Host, M, F, Prio)
     || {Hook, M, F, Prio} <- handlers()],
    ok.

%% Don't repeat yourself.
handlers() ->
    [{offline_message_hook, ?MODULE, stop_hook_processing, 75}].

-spec stop_hook_processing(map(), any(), any(), any()) -> {stop, map()}.
stop_hook_processing(Acc, _From, _To, _Packet) ->
    {stop, Acc}.
