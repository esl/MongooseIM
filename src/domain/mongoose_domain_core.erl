%% Generally, you should not call anything from this module.
%% Use mongoose_domain_api module instead.
-module(mongoose_domain_core).
-behaviour(gen_server).

-include("mongoose_logger.hrl").

%% required for ets:fun2ms/1 pseudo function
-include_lib("stdlib/include/ms_transform.hrl").

-export([start_link/2]).
-export([get_host_type/1]).
-export([is_static/1]).

%% API, used by DB module
-export([insert/3,
         delete/1]).

-export([get_all_static/0,
         get_all_dynamic/0,
         get_all_outdated/1,
         get_domains_by_host_type/1,
         domains_count/0]).

-export([for_each_domain/2]).

-export([is_host_type_allowed/1]).

%% For testing
-export([get_start_args/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-ignore_xref([get_start_args/0, start_link/2]).

-define(TABLE, ?MODULE).
-define(HOST_TYPE_TABLE, mongoose_domain_core_host_types).

-type host_type() :: mongooseim:host_type().
-type domain() :: mongooseim:domain_name().

start_link(Pairs, AllowedHostTypes) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Pairs, AllowedHostTypes], []).

get_host_type(Domain) ->
    case ets:lookup(?TABLE, Domain) of
        [] ->
            {error, not_found};
        [{_Domain, HostType, _Source}] ->
            {ok, HostType}
    end.

is_static(Domain) ->
    case ets:lookup(?TABLE, Domain) of
        [{_Domain, _HostType, _Source = config}] ->
            true;
        _ ->
            false
    end.

is_host_type_allowed(HostType) ->
    ets:member(?HOST_TYPE_TABLE, HostType).

get_all_static() ->
    pairs(ets:match(?TABLE, {'$1', '$2', config})).

get_all_dynamic() ->
    pairs(ets:match(?TABLE, {'$1', '$2', {dynamic, '_'}})).

get_domains_by_host_type(HostType) when is_binary(HostType) ->
    heads(ets:match(?TABLE, {'$1', HostType, '_'})).

domains_count() ->
    ets:info(?TABLE, size).

-spec for_each_domain(host_type(), fun((host_type(), domain())-> any())) -> ok.
for_each_domain(HostType, Func) ->
    ets:safe_fixtable(?TABLE, true),
    MS = ets:fun2ms(fun({Domain, HT, _}) when HT =:= HostType ->
                        [HostType, Domain]
                    end),
    Selection = ets:select(?TABLE, MS, 100),
    for_each_selected_domain(Selection, Func),
    ets:safe_fixtable(?TABLE, false),
    ok.

get_all_outdated(CurrentSource) ->
    MS = ets:fun2ms(fun({Domain, HostType, {dynamic, Src}}) when Src =/= CurrentSource ->
                        {Domain, HostType}
                    end),
    ets:select(?TABLE, MS).

heads(List) ->
    [H || [H|_] <- List].

pairs(List) ->
    [{K, V} || [K, V] <- List].

insert(Domain, HostType, Source) ->
    gen_server:call(?MODULE, {insert, Domain, HostType, Source}).

delete(Domain) ->
    gen_server:call(?MODULE, {delete, Domain}).

get_start_args() ->
    gen_server:call(?MODULE, get_start_args).

%%--------------------------------------------------------------------
%% gen_server callbacks
%%--------------------------------------------------------------------
init([Pairs, AllowedHostTypes]) ->
    mongoose_loader_state:init(),
    ets:new(?TABLE, [set, named_table, protected, {read_concurrency, true}]),
    ets:new(?HOST_TYPE_TABLE, [set, named_table, protected, {read_concurrency, true}]),
    insert_host_types(?HOST_TYPE_TABLE, AllowedHostTypes),
    insert_initial(?TABLE, Pairs),
    {ok, #{initial_pairs => Pairs,
           initial_host_types => AllowedHostTypes}}.

handle_call({delete, Domain}, _From, State) ->
    Result = handle_delete(Domain),
    {reply, Result, State};
handle_call({insert, Domain, HostType, Source}, _From, State) ->
    Result = handle_insert(Domain, HostType, Source),
    {reply, Result, State};
handle_call(get_start_args, _From, State = #{initial_pairs := Pairs,
                                             initial_host_types := AllowedHostTypes}) ->
    {reply, [Pairs, AllowedHostTypes], State};
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
%% internal functions
%%--------------------------------------------------------------------

for_each_selected_domain('$end_of_table', _) -> ok;
for_each_selected_domain({MatchList, Continuation}, Func) ->
    [safely:apply_and_log(Func, Args, log_context(Args)) || Args <- MatchList],
    Selection = ets:select(Continuation),
    for_each_selected_domain(Selection, Func).

log_context(Args) ->
    #{what => domain_operation_failed,
      args => Args}.

insert_initial(Tab, Pairs) ->
    lists:foreach(fun({Domain, HostType}) ->
                          insert_initial_pair(Tab, Domain, HostType)
                  end, Pairs).

insert_initial_pair(Tab, Domain, HostType) ->
    ets:insert_new(Tab, new_object(Domain, HostType, config)).

new_object(Domain, HostType, Source) ->
    {Domain, HostType, Source}.

insert_host_types(Tab, AllowedHostTypes) ->
    lists:foreach(fun(HostType) ->
                          ets:insert_new(Tab, {HostType})
                  end, AllowedHostTypes),
    ok.

handle_delete(Domain) ->
    case ets:lookup(?TABLE, Domain) of
        [{Domain, _HostType, _Source = config}] ->
            %% Ignore any static domains
            ?LOG_ERROR(#{what => domain_static_but_was_in_db, domain => Domain}),
            {error, static};
        [] ->
            %% nothing to remove
            ok;
        [{Domain, HostType, _Source}] ->
            ets:delete(?TABLE, Domain),
            mongoose_lazy_routing:maybe_remove_domain(HostType, Domain),
            mongoose_subdomain_core:remove_domain(HostType, Domain),
            ok
    end.

handle_insert(Domain, HostType, Source) ->
    case is_host_type_allowed(HostType) of
        true ->
            case ets:lookup(?TABLE, Domain) of
                [{Domain, _HostType, _Source = config}] ->
                    %% Ignore any static domains
                    ?LOG_ERROR(#{what => domain_static_but_in_db, domain => Domain}),
                    {error, static};
                [] ->
                    ets:insert_new(?TABLE, new_object(Domain, HostType, {dynamic, Source})),
                    mongoose_subdomain_core:add_domain(HostType, Domain),
                    ok;
                [{Domain, HT, _Source}] when HT =:= HostType ->
                    ets:insert(?TABLE, new_object(Domain, HostType, {dynamic, Source})),
                    ok;
                [{Domain, HT, _Source}] when HT =/= HostType ->
                    ?LOG_ERROR(#{what => ignore_domain_from_db_with_different_host_type,
                                 domain => Domain,
                                 core_host_type => HT,
                                 db_host_type => HostType}),
                    {error, bad_insert}
            end;
        false ->
            ?LOG_ERROR(#{what => ignore_domain_from_db_with_unknown_host_type,
                         domain => Domain, host_type => HostType}),
            {error, unknown_host_type}

    end.

