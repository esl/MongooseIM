%% Generally, you should not call anything from this module.
%% Use mongoose_domain_api module instead.
-module(mongoose_domain_core).
-include("mongoose_logger.hrl").

-export([start/2, stop/0]).
-export([start_link/2]).
-export([get_host_type/1]).
-export([is_locked/1]).

%% API, used by DB module
-export([insert_unlocked/2,
         remove_unlocked/1]).

-export([get_all_locked/0,
         get_domains_by_host_type/1]).

-export([set_last_event_id/1,
         get_last_event_id/0]).

-export([is_host_type_allowed/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(TABLE, ?MODULE).
-define(HOST_TYPE_TABLE, mongoose_domain_core_host_types).

start(Pairs, AllowedHostTypes) ->
    ChildSpec =
        {?MODULE,
         {?MODULE, start_link, [Pairs, AllowedHostTypes]},
         permanent, infinity, worker, [?MODULE]},
    just_ok(supervisor:start_child(ejabberd_sup, ChildSpec)).

%% For tests
stop() ->
    supervisor:terminate_child(ejabberd_sup, ?MODULE),
    supervisor:delete_child(ejabberd_sup, ?MODULE),
    ok.

start_link(Pairs, AllowedHostTypes) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Pairs, AllowedHostTypes], []).

get_host_type(Domain) ->
    case ets:lookup(?TABLE, Domain) of
        [] ->
            {error, not_found};
        [{_Domain, HostType, _Locked}] ->
            {ok, HostType}
    end.

is_locked(Domain) ->
    case ets:lookup(?TABLE, Domain) of
        [{_Domain, _HostType, _Locked = true}] ->
            true;
        _ ->
            false
    end.

is_host_type_allowed(HostType) ->
    ets:member(?HOST_TYPE_TABLE, HostType).

get_all_locked() ->
    pairs(ets:match(?TABLE, {'$1', '$2', true})).

get_domains_by_host_type(HostType) when is_binary(HostType) ->
    heads(ets:match(?TABLE, {'$1', HostType, '_'})).

heads(List) ->
    [H || [H|_] <- List].

pairs(List) ->
    [{K, V} || [K, V] <- List].

insert_unlocked(Domain, HostType) ->
    gen_server:call(?MODULE, {insert_unlocked, Domain, HostType}).

remove_unlocked(Domain) ->
    gen_server:call(?MODULE, {remove_unlocked, Domain}).

set_last_event_id(LastEventId) ->
    gen_server:call(?MODULE, {set_last_event_id, LastEventId}).

get_last_event_id() ->
    gen_server:call(?MODULE, get_last_event_id).

%%--------------------------------------------------------------------
%% gen_server callbacks
init([Pairs, AllowedHostTypes]) ->
    ets:new(?TABLE, [set, named_table, protected, {read_concurrency, true}]),
    ets:new(?HOST_TYPE_TABLE, [set, named_table, protected, {read_concurrency, true}]),
    insert_host_types(?HOST_TYPE_TABLE, AllowedHostTypes),
    insert_initial(?TABLE, Pairs),
    {ok, #{last_event_id => undefined}}.

handle_call({remove_unlocked, Domain}, _From, State) ->
    Result = handle_remove_unlocked(Domain),
    {reply, Result, State};
handle_call({insert_unlocked, Domain, HostType}, _From, State) ->
    Result = handle_insert_unlocked(Domain, HostType),
    {reply, Result, State};
handle_call({set_last_event_id, LastEventId}, _From, State) ->
    {reply, ok, State#{last_event_id => LastEventId}};
handle_call(get_last_event_id, _From, State) ->
    LastEventId = maps:get(last_event_id, State),
    {reply, LastEventId, State};
handle_call(Request, From, State) ->
    ?UNEXPECTED_CALL(Request, From),
    {reply, ok, State}.

handle_cast(Msg, State) ->
    ?UNEXPECTED_CAST(Msg),
    {noreply, State}.

handle_info(Info, State) ->
    ?UNEXPECTED_INFO(Info),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
insert_initial(Tab, Pairs) ->
    lists:foreach(fun({Domain, HostType}) ->
                          insert_initial_pair(Tab, Domain, HostType)
                  end, Pairs).

insert_initial_pair(Tab, Domain, HostType) ->
    ets:insert_new(Tab, new_object(Domain, HostType, true)).

new_object(Domain, HostType, Locked) ->
    {Domain, HostType, Locked}.

just_ok({ok,_}) -> ok;
just_ok(Other) -> Other.

insert_host_types(Tab, AllowedHostTypes) ->
    lists:foreach(fun(HostType) ->
                          ets:insert_new(Tab, {HostType})
                  end, AllowedHostTypes),
    ok.

handle_remove_unlocked(Domain) ->
    case is_locked(Domain) of
        true ->
            %% Ignore any locked domains
            ?LOG_ERROR(#{what => domain_locked_but_was_in_db, domain => Domain});
        false ->
            ets:delete(?TABLE, Domain)
    end,
    ok.

handle_insert_unlocked(Domain, HostType) ->
    case is_locked(Domain) of
        true ->
            %% Ignore any locked domains
            ?LOG_ERROR(#{what => domain_locked_but_in_db, domain => Domain}),
            {error, locked};
        false ->
            case is_host_type_allowed(HostType) of
                true ->
                    case get_host_type(Domain) of
                        {ok, HT} when HT =:= HostType ->
                            ok;
                        {error, not_found} ->
                            ets:insert_new(?TABLE, new_object(Domain, HostType, false)),
                            ok;
                        {ok, HT} ->
                            ?LOG_ERROR(#{what => ignore_domain_from_db_with_different_host_type,
                                         core_host_type => HT,
                                         db_host_type => HostType}),
                            {error, bad_insert}
                    end;
                false ->
                    ?LOG_ERROR(#{what => ignore_domain_from_db_with_unknown_host_type,
                                 domain => Domain, host_type => HostType}),
                    {error, unknown_host_type}
            end
    end.

