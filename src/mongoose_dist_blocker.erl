%% @doc Disallow MongooseIM node connections until cleaning is done
%%
%% This module prevents a node from reconnecting, until cleaning activity is
%% finished. It prevents race conditions.
%%
%% This module assume all nodes share the same cookie.
-module(mongoose_dist_blocker).
-behaviour(gen_server).

-include("mongoose_logger.hrl").

%% API
-export([start_link/0,
         add_cleaner/1,
         cleaning_done/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-ignore_xref([start_link/0]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% Register Pid as a cleaner.
add_cleaner(CleanerPid) ->
    gen_server:call(?MODULE, {add_cleaner, CleanerPid}).

%% Cleaner calls must call this function.
cleaning_done(CleanerPid, Node) ->
    gen_server:call(?MODULE, {cleaning_done, CleanerPid, Node}).

%%--------------------------------------------------------------------
%% gen_server callbacks
%%--------------------------------------------------------------------
init([]) ->
    net_kernel:monitor_nodes(true),
    State = #{cleaners => [], waiting => []},
    State2 = lists:foldl(fun handle_nodeup/2, State, nodes()),
    {ok, State2}.

handle_call({add_cleaner, CleanerPid}, _From, State) ->
    {reply, ok, handle_add_cleaner(CleanerPid, State)};
handle_call({cleaning_done, CleanerPid, Node}, _From, State) ->
    {reply, ok, maybe_unblock(State, handle_cleaning_done(CleanerPid, Node, State))};
handle_call(Request, From, State) ->
    ?UNEXPECTED_CALL(Request, From),
    {reply, ok, State}.

handle_cast(Msg, State) ->
    ?UNEXPECTED_CAST(Msg),
    {noreply, State}.

handle_info({nodeup, Node}, State) ->
    {noreply, handle_nodeup(Node, State)};
handle_info({nodedown, Node}, State) ->
    {noreply, handle_nodedown(Node, State)};
handle_info({'DOWN', _Ref, process, Pid, _Info}, State) ->
    {noreply, maybe_unblock(State, handle_cleaner_down(Pid, State))};
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

handle_nodeup(Node, State) ->
    %% We change the cookie as soon as the node is connected.
    %% Alternative is to do it on nodedown, but because nodedown-s are async,
    %% we would have a high chance of race conditions (so, node could reconnect
    %% before we set cookie).
    erlang:set_cookie(Node, blocking_cookie()),
    State.

%% Make cookie, that would prevent node from connecting
blocking_cookie() ->
    list_to_atom(atom_to_list(erlang:get_cookie()) ++ "_blocked_by_" ++ atom_to_list(node())).

%% Allow the node to connect to us again
unblock_node(Node) ->
    erlang:set_cookie(Node, erlang:get_cookie()).

handle_nodedown(Node, State = #{cleaners := []}) ->
    %% Skip waiting when no cleaners
    unblock_node(Node),
    State;
handle_nodedown(Node, State = #{cleaners := Cleaners, waiting := Waiting}) ->
    New = [{Node, CleanerPid} || CleanerPid <- Cleaners],
    State#{waiting := lists:usort(New ++ Waiting)}.

handle_add_cleaner(CleanerPid, State = #{cleaners := Cleaners}) ->
    erlang:monitor(process, CleanerPid),
    State#{cleaners := lists:usort([CleanerPid | Cleaners])}.

handle_cleaning_done(CleanerPid, Node, State = #{waiting := Waiting}) ->
    State#{waiting := lists:delete({Node, CleanerPid}, Waiting)}.

handle_cleaner_down(CleanerPid, State = #{cleaners := Cleaners, waiting := Waiting}) ->
    State#{cleaners := lists:delete(CleanerPid, Cleaners),
           waiting := [X || {_Node, CleanerPid2} = X <- Waiting, CleanerPid =/= CleanerPid2]}.

%% Unblock nodes when the last cleaner confirms the cleaning is done.
%% Call this function each time you remove entries from the waiting list.
maybe_unblock(_OldState = #{waiting := OldWaiting}, NewState = #{waiting := NewWaiting}) ->
    OldNodes = cast_waiting_to_nodes(OldWaiting),
    NewNodes = cast_waiting_to_nodes(NewWaiting),
    CleanedNodes = OldNodes -- NewNodes,
    [unblock_node(Node) || Node <- CleanedNodes],
    NewState.

cast_waiting_to_nodes(Waiting) ->
    lists:usort([Node || {Node, _CleanerPid} <- Waiting]).
