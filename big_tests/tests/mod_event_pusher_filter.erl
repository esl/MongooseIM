-module(mod_event_pusher_filter).
-moduledoc """
An example filter and metadata provider for mod_event_pusher

It performs two actions:
1. Filters out any events other than 'user_status_event'.
2. Adds the following event metadata:
     - timestamp in milliseconds (taken from mongoose_acc)
     - number of user's active sessions
   The metadata can be injected into the event published by the configured backends
   (currently only 'rabbit' supports this).
""".

-behaviour(gen_mod).

%% gen_mod callbacks
-export([start/2, stop/1, hooks/1]).

%% hook handlers
-export([push_event/3]).

-include_lib("../../include/mod_event_pusher_events.hrl").

%% gen_mod callbacks

-spec start(mongooseim:host_type(), gen_mod:module_opts()) -> ok.
start(_HostType, _Opts) ->
    ok.

-spec stop(mongooseim:host_type()) -> ok.
stop(_HostType) ->
    ok.

-spec hooks(mongooseim:host_type()) -> gen_hook:hook_list().
hooks(HostType) ->
    [{push_event, HostType, fun ?MODULE:push_event/3, #{}, 10}]. % needs to run before other handlers

%% hook handlers

-doc "For user status events, add timestamp and session count. Filter out other events.".
-spec push_event(mod_event_pusher:push_event_acc(), mod_event_pusher:push_event_params(),
                 gen_hook:extra()) -> gen_hook:hook_fn_ret(mod_event_pusher:push_event_acc()).
push_event(HookAcc = #{acc := Acc}, #{event := #user_status_event{jid = JID}}, _Extra) ->
    #{metadata := Metadata} = HookAcc,
    NewMetadata = Metadata#{timestamp => mongoose_acc:timestamp(Acc),
                            session_count => count_user_sessions(JID)},
    {ok, HookAcc#{metadata := NewMetadata}};
push_event(HookAcc, _Params, _Extra) ->
    {stop, HookAcc}.

%% helpers

-spec count_user_sessions(jid:jid()) -> non_neg_integer().
count_user_sessions(JID) ->
    length(ejabberd_sm:get_user_present_resources(JID)).
