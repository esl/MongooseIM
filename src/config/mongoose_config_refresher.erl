%% @doc This process subscribes to Mnesia's global config table
%% so that persistent term cache is updated whenever needed
-module(mongoose_config_refresher).
-author("bartlomiejgorny").
-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-record(mongoose_config_refresher_state, {}).

-spec(start_link() ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    ejabberd_config:monitor_global_config_changes(),
    {ok, #mongoose_config_refresher_state{}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({mnesia_table_event, {write, {config, Key, Value}, _}} , State) ->
    ejabberd_config:put_global_option(Key, Value),
    {noreply, State};
handle_info({mnesia_table_event, {delete, {config, Key}, _}}, State) ->
    ejabberd_config:erase_global_option(Key),
    {noreply, State}.

terminate(_Reason, _State = #mongoose_config_refresher_state{}) ->
    ok.

code_change(_OldVsn, State = #mongoose_config_refresher_state{}, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
