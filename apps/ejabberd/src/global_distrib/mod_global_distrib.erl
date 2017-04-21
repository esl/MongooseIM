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

-module(mod_global_distrib).
-author('konrad.zemek@erlang-solutions.com').

-behaviour(gen_mod).

-include("ejabberd.hrl").
-include("jlib.hrl").

-export([deps/2, start/2, stop/1, maybe_reroute/1, maybe_unwrap_message/1]).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------

deps(Host, Opts) ->
    mod_global_distrib_utils:deps(?MODULE, Host, Opts, fun deps/1).

-spec start(Host :: ejabberd:server(), Opts :: list()) -> any().
start(Host, Opts) ->
    mod_global_distrib_utils:start(?MODULE, Host, Opts, fun start/0).

-spec stop(Host :: ejabberd:server()) -> any().
stop(Host) ->
    mod_global_distrib_utils:stop(?MODULE, Host, fun stop/0).

%%--------------------------------------------------------------------
%% Hooks implementation
%%--------------------------------------------------------------------

maybe_reroute(drop) -> drop;
maybe_reroute({_From, To, _Packet} = FPacket) ->
    LocalHost = opt(local_host),
    GlobalHost = opt(global_host),
    Cookie = opt(cookie),
    case lookup_recipients_host(To, LocalHost, GlobalHost) of
        {ok, LocalHost} -> FPacket;
        {ok, TargetHost} -> wrap_message(Cookie, LocalHost, TargetHost, FPacket);
        _ -> FPacket
    end.

maybe_unwrap_message(drop) -> drop;
maybe_unwrap_message({_From, _To, Packet} = FPacket) ->
    Cookie = opt(cookie),
    case {exml_query:attr(Packet, <<"type">>), exml_query:attr(Packet, <<"distrib">>)} of
        {<<"error">>, Cookie} -> unwrap_error_message(FPacket);
        {_, Cookie} -> unwrap_message(FPacket);
        _ -> FPacket
    end.

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

deps(Opts) ->
    Deps0 = [{mod_global_distrib_mapping, Opts, hard},
             {mod_global_distrib_disco, Opts, hard}],
    case proplists:get_value(bounce, Opts, []) of
        false -> Deps0;
        BounceOpts -> [{mod_global_distrib_bounce, BounceOpts ++ Opts, hard} | Deps0]
    end.

start() ->
    LocalHost = opt(local_host),
    ejabberd_hooks:add(filter_packet, global, ?MODULE, maybe_reroute, 99),
    ejabberd_hooks:add(filter_local_packet, LocalHost, ?MODULE, maybe_unwrap_message, 99).

stop() ->
    LocalHost = opt(local_host),
    ejabberd_hooks:delete(filter_local_packet, LocalHost, ?MODULE, maybe_unwrap_message, 99),
    ejabberd_hooks:delete(filter_packet, global, ?MODULE, maybe_reroute, 99).

-spec lookup_recipients_host(jid(), binary(), binary()) -> {ok, binary()} | error.
lookup_recipients_host(#jid{lserver = HostAddressedTo} = To, LocalHost, GlobalHost) ->
    case HostAddressedTo of
        LocalHost -> undefined;
        GlobalHost -> mod_global_distrib_mapping:for_jid(To);
        _ -> mod_global_distrib_mapping:for_domain(HostAddressedTo)
    end.

unwrap_error_message({_From, _To, #xmlel{children = [Child, Error]}}) ->
    ToBin = exml_query:attr(Child, <<"from">>),
    FromBin = exml_query:attr(Child, <<"to">>),
    UpdatedAttrs = lists:ukeysort(1, [{<<"from">>, FromBin}, {<<"to">>, ToBin},
                                      {<<"type">>, <<"error">>} | Child#xmlel.attrs]),
    UpdatedChild = Child#xmlel{attrs = UpdatedAttrs, children = Child#xmlel.children ++ [Error]},
    ?DEBUG("Unwrapping error message from=~s [~s] to=~s [~s]",
           [FromBin, jid:to_binary(_From), ToBin, jid:to_binary(_To)]),
    ejabberd_router:route(jid:from_binary(FromBin), jid:from_binary(ToBin), UpdatedChild),
    drop.

unwrap_message({_From, _To, #xmlel{children = [Child]}}) ->
    From = jid:from_binary(exml_query:attr(Child, <<"from">>)),
    To = jid:from_binary(exml_query:attr(Child, <<"to">>)),
    ?DEBUG("Unwrapping message from=~s [~s] to=~s [~s]",
           [exml_query:attr(Child, <<"from">>), jid:to_binary(_From),
            exml_query:attr(Child, <<"to">>), jid:to_binary(_To)]),
    ejabberd_router:route(From, To, Child),
    drop.

wrap_message(Cookie, LocalHost, TargetHost, {From, To, Packet}) ->
    wrap_message(Cookie, LocalHost, TargetHost, jlib:replace_from_to(From, To, Packet));
wrap_message(Cookie, LocalHost, TargetHost, Child) ->
    WrappedMessage = #xmlel{name = <<"message">>,
                            attrs = [{<<"distrib">>, Cookie},
                                     {<<"from">>, LocalHost},
                                     {<<"to">>, TargetHost}],
                            children = [Child]},
    ?DEBUG("Wrapping message from=~s [~s] to=~s [~s]",
           [exml_query:attr(Child, <<"from">>), LocalHost,
            exml_query:attr(Child, <<"to">>), TargetHost]),
    ejabberd_router:route(jid:from_binary(LocalHost), jid:from_binary(TargetHost), WrappedMessage),
    drop.

opt(Key) ->
    mod_global_distrib_utils:opt(?MODULE, Key).
