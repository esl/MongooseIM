%%% @doc
%%%    RDBMS backend for mod_smart_markers
%%% @end
%%% @copyright (C) 2022, Erlang Solutions Ltd.
-module(mod_smart_markers_rdbms_async).
-behavior(mod_smart_markers_backend).
-behaviour(mongoose_aggregator_worker).

-include("jlib.hrl").

-export([init/2, stop/1, update_chat_marker/2, get_chat_markers/4, get_conv_chat_marker/6]).
-export([remove_domain/2, remove_user/2, remove_to/2, remove_to_for_user/3]).

%% Worker callbacks
-export([request/2, aggregate/3, verify/3]).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------
-spec init(mongooseim:host_type(), gen_mod:module_opts()) -> ok.
init(HostType, Opts) ->
    AsyncOpts = prepare_pool_opts(Opts),
    mod_smart_markers_rdbms:init(HostType, Opts),
    start_pool(HostType, AsyncOpts),
    ok.

stop(HostType) ->
    mongoose_async_pools:stop_pool(HostType, smart_markers).

prepare_pool_opts(#{async_writer := AsyncOpts}) ->
    AsyncOpts#{pool_type => aggregate,
               request_callback => fun ?MODULE:request/2,
               aggregate_callback => fun ?MODULE:aggregate/3,
               verify_callback => fun ?MODULE:verify/3}.

-spec start_pool(mongooseim:host_type(), mongoose_async_pools:pool_opts()) -> term().
start_pool(HostType, Opts) ->
    mongoose_async_pools:start_pool(HostType, smart_markers, Opts).

%%% @doc
%%% 'from', 'to', 'thread' and 'type' keys of the ChatMarker map serve
%%% as a composite database key. If key is not available in the database,
%%% then chat marker must be added. Otherwise this function must update
%%% chat marker record for that composite key.
%%% @end
-spec update_chat_marker(mongooseim:host_type(),
                         mod_smart_markers:chat_marker()) -> ok.
update_chat_marker(HostType, #{from := #jid{luser = LU, lserver = LS},
                               to := To, thread := Thread, type := Type} = Marker) ->
    Key = {LU, LS, To, Thread, Type},
    mongoose_async_pools:put_task(HostType, smart_markers, Key, Marker).

%% Synchronous calls
-spec get_conv_chat_marker(HostType :: mongooseim:host_type(),
                           From :: jid:jid(),
                           To :: jid:jid(),
                           Thread :: mod_smart_markers:maybe_thread(),
                           Timestamp :: integer(),
                           Private :: boolean()) -> [mod_smart_markers:chat_marker()].
get_conv_chat_marker(HostType, From, To, Thread, TS, Private) ->
    mod_smart_markers_rdbms:get_conv_chat_marker(HostType, From, To, Thread, TS, Private).

-spec get_chat_markers(HostType :: mongooseim:host_type(),
                       To :: jid:jid(),
                       Thread :: mod_smart_markers:maybe_thread(),
                       Timestamp :: integer()) -> [mod_smart_markers:chat_marker()].
get_chat_markers(HostType, To, Thread, TS) ->
    mod_smart_markers_rdbms:get_chat_markers(HostType, To, Thread, TS).

-spec remove_domain(mongooseim:host_type(), jid:lserver()) -> mongoose_rdbms:query_result().
remove_domain(HostType, Domain) ->
    mod_smart_markers_rdbms:remove_domain(HostType, Domain).

-spec remove_user(mongooseim:host_type(), jid:jid()) -> mongoose_rdbms:query_result().
remove_user(HostType, User) ->
    mod_smart_markers_rdbms:remove_user(HostType, User).

-spec remove_to(mongooseim:host_type(), jid:jid()) -> mongoose_rdbms:query_result().
remove_to(HostType, To) ->
    mod_smart_markers_rdbms:remove_to(HostType, To).

-spec remove_to_for_user(mongooseim:host_type(), From :: jid:jid(), To :: jid:jid()) ->
    mongoose_rdbms:query_result().
remove_to_for_user(HostType, From, To) ->
    mod_smart_markers_rdbms:remove_to_for_user(HostType, From, To).

%% callbacks
-spec aggregate(mod_smart_markers:chat_marker(),
                mod_smart_markers:chat_marker(),
                mongoose_async_pools:pool_extra()) -> {ok, mod_smart_markers:chat_marker()}.
aggregate(_, NewTask, _Extra) ->
    {ok, NewTask}.

-spec request(mod_smart_markers:chat_marker(),
              mongoose_async_pools:pool_extra()) -> gen_server:request_id().
request(#{from := #jid{luser = LU, lserver = LS}, to := To, thread := Thread,
          type := Type, timestamp := TS, id := Id}, #{host_type := HostType}) ->
    ToEncoded = mod_smart_markers_rdbms:encode_jid(To),
    ThreadEncoded = mod_smart_markers_rdbms:encode_thread(Thread),
    TypeEncoded = mod_smart_markers_rdbms:encode_type(Type),
    KeyValues = [LS, LU, ToEncoded, ThreadEncoded, TypeEncoded],
    UpdateValues = [Id, TS],
    InsertValues = KeyValues ++ UpdateValues,
    rdbms_queries:request_upsert(HostType, smart_markers_upsert, InsertValues, UpdateValues).

-spec verify(term(), mod_smart_markers:chat_marker(), mongoose_async_pools:pool_extra()) -> ok.
verify(Answer, Marker, _Extra) ->
    mod_smart_markers_rdbms:verify(Answer, Marker).
