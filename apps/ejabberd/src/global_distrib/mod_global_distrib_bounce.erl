%%==============================================================================
%% Copyright 2017 Erlang Solutions Ltd.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%==============================================================================

-module(mod_global_distrib_bounce).
-author('konrad.zemek@erlang-solutions.com').

-behaviour(gen_mod).
-behaviour(gen_server).

-include("ejabberd.hrl").
-include("jlib.hrl").

-define(MESSAGE_STORE, mod_global_distrib_bounce_message_store).

-export([start_link/0, start/2, stop/1]).
-export([init/1, handle_info/2, handle_cast/2, handle_call/3, code_change/3, terminate/2]).
-export([maybe_store_message/1]).

%%--------------------------------------------------------------------
%% gen_mod API
%%--------------------------------------------------------------------

-spec start(Host :: ejabberd:lserver(), Opts :: proplists:proplist()) -> any().
start(Host, Opts0) ->
    ResendAfterMs = proplists:get_value(resend_after_ms, Opts0, 200),
    ResendAfter = p1_time_compat:convert_time_unit(ResendAfterMs, milli_seconds, native),
    Opts = [{resend_after, ResendAfter}, {max_retries, 4} | Opts0],
    mod_global_distrib_utils:start(?MODULE, Host, Opts, fun start/0).

-spec stop(Host :: ejabberd:lserver()) -> any().
stop(Host) ->
    mod_global_distrib_utils:stop(?MODULE, Host, fun stop/0).

-spec start_link() -> {ok, pid()} | {error, any()}.
start_link() ->
    gen_server:start_link(?MODULE, [], []).

%%--------------------------------------------------------------------
%% gen_server API
%%--------------------------------------------------------------------

init(_) ->
    self() ! resend,
    {ok, no_state}.

handle_info(resend, State) ->
    erlang:send_after(100, self(), resend),
    Now = p1_time_compat:monotonic_time(),
    resend_messages(Now),
    {noreply, State}.

handle_cast(_Message, _State) ->
    exit(bad_cast).

handle_call(_Message, _From, _State) ->
    exit(bad_call).

code_change(_Version, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ignore.

%%--------------------------------------------------------------------
%% Hooks implementation
%%--------------------------------------------------------------------

-spec maybe_store_message(drop) -> drop;
                         ({jid(), jid(), mongoose_acc:t()}) ->
                                 drop | {jid(), jid(), mongoose_acc:t()}.
maybe_store_message(drop) -> drop;
maybe_store_message({From, To, Acc0} = FPacket) ->
    LocalHost = opt(local_host),
    case mod_global_distrib:get_metadata(Acc0, {bounce_ttl, LocalHost}, opt(max_retries)) of
        0 ->
            FPacket;
        OldTTL ->
            Acc = mod_global_distrib:put_metadata(Acc0, {bounce_ttl, LocalHost}, OldTTL - 1),
            ResendAt = p1_time_compat:monotonic_time() + opt(resend_after),
            do_insert_in_store(ResendAt, {From, To, Acc}),
            drop
    end.

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

-spec start() -> any().
start() ->
    Host = opt(global_host),
    mod_global_distrib_utils:create_ets(?MESSAGE_STORE, ordered_set),
    ejabberd_hooks:add(mod_global_distrib_unknown_recipient, Host, ?MODULE, maybe_store_message, 80),
    ChildSpec = {?MODULE, {?MODULE, start_link, []}, permanent, 1000, worker, [?MODULE]},
    {ok, _} = supervisor:start_child(ejabberd_sup, ChildSpec).

-spec stop() -> any().
stop() ->
    Host = opt(global_host),
    supervisor:terminate_child(ejabberd_sup, ?MODULE),
    supervisor:delete_child(ejabberd_sup, ?MODULE),
    ejabberd_hooks:delete(mod_global_distrib_unknown_recipient, Host, ?MODULE, maybe_store_message, 80),
    ets:delete(?MESSAGE_STORE).

-spec do_insert_in_store(ResendAt :: integer(), {jid(), jid(), mongoose_acc:t()}) -> ok.
do_insert_in_store(ResendAt, FPacket) ->
    case ets:insert_new(?MESSAGE_STORE, {ResendAt, FPacket}) of
        true -> ok;
        false -> do_insert_in_store(ResendAt + 1, FPacket)
    end.

-spec resend_messages(Now :: integer()) -> ok.
resend_messages(Now) ->
    case ets:first(?MESSAGE_STORE) of
        Key when is_integer(Key) andalso Key < Now ->
            [{Key, {From, To, _Acc} = FPacket}] = ets:lookup(?MESSAGE_STORE, Key),
            ets:delete(?MESSAGE_STORE, Key),
            mod_global_distrib_mapping:clear_cache(To),
            Worker = mod_global_distrib_worker_sup:get_worker(jid:to_binary(From)),
            gen_server:cast(Worker, {route, FPacket}),
            resend_messages(Now);
        _ ->
            ok
    end.

-spec opt(Key :: atom()) -> term().
opt(Key) ->
    mod_global_distrib_utils:opt(?MODULE, Key).
