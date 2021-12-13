%%%-------------------------------------------------------------------
%%% @author Uvarov Michael <arcusfelis@gmail.com>
%%% @copyright (C) 2013, Uvarov Michael
%%% @doc Shared shapers.
%%% @end
%%%-------------------------------------------------------------------
-module(shaper_srv).
-behaviour(gen_server).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1,
         child_specs/0,
         wait/5,
         reset_shapers/1,
         reset_all_shapers/1]).

-ignore_xref([reset_all_shapers/1, reset_shapers/1, start_link/1]).

%% ------------------------------------------------------------------
%% Record definitions
%% ------------------------------------------------------------------

-record(state, {
        %% Maximum ammount of milliseconds to wait
        max_delay :: non_neg_integer(),
        %% How many seconds to store each shaper
        ttl :: non_neg_integer(),
        shapers :: dict:dict(),
        a_times :: dict:dict()
    }).
-type state() :: #state{}.

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

-spec child_specs() -> [supervisor:child_spec()].
child_specs() ->
    [child_spec(ProcName) ||  ProcName <- worker_names(<<>>)].

-spec child_spec(atom()) -> supervisor:child_spec().
child_spec(ProcName) ->
    {ProcName,
     {?MODULE, start_link, [ProcName]},
     permanent,
     5000,
     worker,
     [?MODULE]}.

-spec start_link(atom()) -> 'ignore' | {'error', _} | {'ok', pid()}.
start_link(ProcName) ->
    gen_server:start_link({local, ProcName}, ?MODULE, [], []).

-spec worker_prefix() -> string().
worker_prefix() ->
    "ejabberd_shaper_".

worker_count(_HostType) ->
    10.

-spec worker_names(mongooseim:host_type()) -> [atom()].
worker_names(HostType) ->
    [worker_name(HostType, N) || N <- lists:seq(0, worker_count(HostType) - 1)].

-spec worker_name(jid:server(), integer()) -> atom().
worker_name(_Host, N) ->
    list_to_atom(worker_prefix() ++ integer_to_list(N)).

-spec select_worker(mongooseim:host_type(), _) -> atom().
select_worker(HostType, Tag) ->
    N = worker_number(HostType, Tag),
    worker_name(HostType, N).

-spec worker_number(mongooseim:host_type(), _) -> non_neg_integer().
worker_number(HostType, Tag) ->
    erlang:phash2(Tag, worker_count(HostType)).

%% @doc Shapes the caller from executing the action.
-spec wait(HostType :: mongooseim:host_type(),
           Domain :: jid:server(), Action :: atom(),
           FromJID :: jid:jid() | global, Size :: integer()
           ) -> ok | {error, max_delay_reached}.
wait(HostType, Domain, Action, FromJID, Size) ->
    gen_server:call(select_worker(HostType, FromJID),
                    {wait, HostType, Domain, Action, FromJID, Size}).

%% @doc Ask all shaper servers to forget current shapers and read settings again
reset_all_shapers(HostType) ->
    [reset_shapers(ProcName) || ProcName <- worker_names(HostType)].

%% @doc Ask server to forget its shapers
reset_shapers(ProcName) ->
    gen_server:call(ProcName, reset_shapers).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
    State = #state{max_delay = proplists:get_value(max_delay, Args, 3000),
                   ttl = proplists:get_value(ttl, Args, 120),
                   shapers = dict:new(),
                   a_times = dict:new()
                  },
    GCInt = proplists:get_value(gc_interval, Args, 30),
    timer:send_interval(timer:seconds(GCInt), delete_old_shapers),
    {ok, State}.

handle_call({wait, HostType, Domain, Action, FromJID, Size},
            From, State=#state{max_delay=MaxDelayMs}) ->
    Key = new_key(Domain, Action, FromJID),
    Shaper = find_or_create_shaper(HostType, Key, State),
    State1 = update_access_time(Key, erlang:system_time(), State),
    case shaper:update(Shaper, Size) of
        {UpdatedShaper, 0} ->
            {reply, ok, save_shaper(Key, UpdatedShaper, State1)};
        {UpdatedShaper, DelayMs} when DelayMs < MaxDelayMs ->
            reply_after(DelayMs, From, ok),
            {noreply, save_shaper(Key, UpdatedShaper, State1)};
        {_, _} ->
            {reply, {error, max_delay_reached}, State1}
    end;
handle_call(reset_shapers, _From, State=#state{}) ->
    {reply, ok, init_dicts(State)}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(delete_old_shapers, State) ->
    {noreply, delete_old_shapers(State)};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

-type key() :: {global | jid:server(), atom(), jid:jid()}.
-spec new_key(jid:server() | global, atom(), jid:jid()) -> key().
new_key(Domain, Action, FromJID) ->
    {Domain, Action, FromJID}.

-spec find_or_create_shaper(mongooseim:host_type(), key(), state()) ->
    shaper:shaper().
find_or_create_shaper(HostType, Key, #state{shapers=Shapers}) ->
    case dict:find(Key, Shapers) of
        {ok, Shaper} -> Shaper;
        error -> create_shaper(HostType, Key)
    end.

-spec update_access_time(key(), _, state()) -> state().
update_access_time(Key, Now, State=#state{a_times=Times}) ->
    State#state{a_times=dict:store(Key, Now, Times)}.

-spec save_shaper(key(), shaper:shaper(), state()) -> state().
save_shaper(Key, Shaper, State=#state{shapers=Shapers}) ->
    State#state{shapers=dict:store(Key, Shaper, Shapers)}.

-spec init_dicts(state()) -> state().
init_dicts(State) ->
    State#state{shapers=dict:new(), a_times=dict:new()}.

-spec delete_old_shapers(state()) -> state().
delete_old_shapers(State=#state{shapers=Shapers, a_times=Times, ttl=TTL}) ->
    Min = subtract_seconds(TTL),
    %% Copy recently modified shapers
    dict:fold(fun
        (_, ATime, Acc) when ATime < Min -> Acc; %% skip too old
        (Key, ATime, Acc) ->
            Shaper = dict:fetch(Key, Shapers),
            update_access_time(Key, ATime, save_shaper(Key, Shaper, Acc))
        end, init_dicts(State), Times).

-spec create_shaper(mongooseim:host_type(), key()) ->
    'none' | {'maxrate', _, 0, non_neg_integer()}.
create_shaper(HostType, Key) ->
    shaper:new(request_shaper_name(HostType, Key)).

-spec request_shaper_name(mongooseim:host_type(), key()) -> atom().
request_shaper_name(HostType, {Domain, Action, FromJID}) ->
    get_shaper_name(HostType, Domain, Action, FromJID, default_shaper()).

default_shaper() ->
    none.

-spec get_shaper_name(HostType :: mongooseim:host_type(),
                      Domain :: 'global' | jid:server(),
                      Action :: atom(), jid:jid(),
                      Default :: 'none') -> 'allow' | 'none'.
get_shaper_name(HostType, Domain, Action, FromJID, Default) ->
    case acl:match_rule_for_host_type(HostType, Domain, Action, FromJID) of
        deny -> Default;
        Value -> Value
    end.

%% @doc It is a small hack
%% This function calls this in more efficient way:
%% timer:apply_after(DelayMs, gen_server, reply, [From, Reply]).
-spec reply_after(pos_integer(), {atom() | pid(), _}, 'ok') -> reference().
reply_after(DelayMs, {Pid, Tag}, Reply) ->
    erlang:send_after(DelayMs, Pid, {Tag, Reply}).


-spec subtract_seconds(integer()) -> integer().
subtract_seconds(TTL) ->
    TimestampThreshold = erlang:system_time(second) - TTL,
    erlang:convert_time_unit(TimestampThreshold, second, native).
