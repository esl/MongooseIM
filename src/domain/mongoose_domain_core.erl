%% Generally, you should not call anything from this module.
%% Use mongoose_domain_api module instead.
-module(mongoose_domain_core).
-include("mongoose_logger.hrl").

-export([start/1, stop/0]).
-export([start_link/1]).
-export([get_host_type/1]).
-export([is_locked/1]).

%% API for tests
-export([dump/0, restore/1]).

%% API, used by DB module
-export([remove_all_unlocked/0,
         upsert_unlocked/2,
         remove_unlocked/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(TABLE, ?MODULE).

start(Pairs) ->
    ChildSpec =
        {?MODULE,
         {?MODULE, start_link, [Pairs]},
         permanent, infinity, worker, [?MODULE]},
    just_ok(supervisor:start_child(ejabberd_sup, ChildSpec)).

%% For tests
stop() ->
    supervisor:terminate_child(ejabberd_sup, ?MODULE),
    supervisor:delete_child(ejabberd_sup, ?MODULE),
    ok.

start_link(Pairs) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Pairs], []).

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

remove_all_unlocked() ->
    ets:match_delete(?TABLE, {'_', '_', false}).

upsert_unlocked(Domain, HostType) ->
    case is_locked(Domain) of
        true ->
            %% Ignore any locked domains
            ?LOG_ERROR(#{what => domain_locked_but_in_db, domain => Domain});
        false ->
            ets:insert(?TABLE, new_object(Domain, HostType, false))
    end,
    ok.

remove_unlocked(Domain) ->
    case is_locked(Domain) of
        true ->
            %% Ignore any locked domains
            ?LOG_ERROR(#{what => domain_locked_but_was_in_db, domain => Domain});
        false ->
            ets:delete(?TABLE, Domain)
    end,
    ok.

%% For tests
dump() ->
    ets:tab2list(?TABLE).

restore(Dump) ->
    ets:delete_all_objects(?TABLE),
    ets:insert_new(?TABLE, Dump),
    ok.

%%--------------------------------------------------------------------
%% gen_server callbacks
init([Pairs]) ->
    ets:new(?TABLE, [set, named_table, public]),
    insert_initial(?TABLE, Pairs),
    {ok, #{}}.

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
