%%%-------------------------------------------------------------------
%%% @author Uvarov Michael <arcusfelis@gmail.com>
%%% @copyright (C) 2013, Uvarov Michael
%%% @doc Shared shapers.
%%% @end
%%%-------------------------------------------------------------------
-module(shaper_srv).
-behaviour(gen_server).
-include_lib("mongoose.hrl").
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1,
         child_specs/0,
         wait/4,
         reset_shapers/1,
         reset_all_shapers/1]).

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

worker_count(_Host) ->
    10.


-spec worker_names(jid:server()) -> [atom()].
worker_names(Host) ->
    [worker_name(Host, N) || N <- lists:seq(0, worker_count(Host) - 1)].


-spec worker_name(jid:server(), integer()) -> atom().
worker_name(_Host, N) ->
    list_to_atom(worker_prefix() ++ integer_to_list(N)).


-spec select_worker(jid:server(), _) -> atom().
select_worker(Host, Tag) ->
    N = worker_number(Host, Tag),
    worker_name(Host, N).


-spec worker_number(jid:server(), _) -> non_neg_integer().
worker_number(Host, Tag) ->
    erlang:phash2(Tag, worker_count(Host)).


%% @doc Shapes the caller from executing the action.
-spec wait(_Host :: jid:server(), _Action :: atom(),
           _FromJID :: jid:jid() | global, _Size :: integer()
           ) -> ok | {error, max_delay_reached}.
wait(Host, Action, FromJID, Size) ->
    gen_server:call(select_worker(Host, FromJID), {wait, Host, Action, FromJID, Size}).

%% @doc Ask all shaper servers to forget current shapers and read settings again
reset_all_shapers(Host) ->
    [reset_shapers(ProcName) || ProcName <- worker_names(Host)].

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

handle_call({wait, Host, Action, FromJID, Size},
            From, State=#state{max_delay=MaxDelayMs}) ->
    Key = new_key(Host, Action, FromJID),
    Shaper = find_or_create_shaper(Key, State),
    State1 = update_access_time(Key, p1_time_compat:timestamp(), State),
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
    ?DEBUG("Deleted old shapers", []),
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
new_key(Host, Action, FromJID) ->
    {Host, Action, FromJID}.


-spec find_or_create_shaper(key(), state()) -> shaper:shaper().
find_or_create_shaper(Key, #state{shapers=Shapers}) ->
    case dict:find(Key, Shapers) of
        {ok, Shaper} -> Shaper;
        error -> create_shaper(Key)
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
    Min = subtract_seconds(p1_time_compat:timestamp(), TTL),
    %% Copy recently modified shapers
    dict:fold(fun
        (_, ATime, Acc) when ATime < Min -> Acc; %% skip too old
        (Key, ATime, Acc) ->
            Shaper = dict:fetch(Key, Shapers),
            update_access_time(Key, ATime, save_shaper(Key, Shaper, Acc))
        end, init_dicts(State), Times).


-spec create_shaper(key()) -> 'none' | {'maxrate', _, 0, non_neg_integer()}.
create_shaper(Key) ->
    shaper:new(request_shaper_name(Key)).


-spec request_shaper_name(key()) -> atom().
request_shaper_name({Host, Action, FromJID}) ->
    get_shaper_name(Host, Action, FromJID, default_shaper()).


default_shaper() ->
    none.


-spec get_shaper_name('global' | jid:server(),
                      Action :: atom(),
                      jid:jid(),
                      Default :: 'none') -> 'allow' | 'none'.
get_shaper_name(Host, Action, FromJID, Default) ->
    case acl:match_rule(Host, Action, FromJID) of
        deny -> Default;
        Value -> Value
    end.

%% @doc It is a small hack
%% This function calls this in more efficient way:
%% timer:apply_after(DelayMs, gen_server, reply, [From, Reply]).
-spec reply_after(pos_integer(), {atom() | pid(), _}, 'ok') -> reference().
reply_after(DelayMs, {Pid, Tag}, Reply) ->
    erlang:send_after(DelayMs, Pid, {Tag, Reply}).


-spec subtract_seconds(erlang:timestamp(), non_neg_integer()) -> erlang:timestamp().
subtract_seconds({MegaSecs, Secs, MicroSecs}, SubSecs) ->
    {MegaSecs - (SubSecs div 1000000), Secs - (SubSecs rem 1000000), MicroSecs}.

