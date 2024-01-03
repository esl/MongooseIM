%% @doc Stores a table of custom IQ-handlers for mod_muc_room.
-module(mod_muc_iq).
-behaviour(gen_iq_component).

-export([start_link/0,
         process_iq/5,
         register_iq_handler/3,
         unregister_iq_handler/2,
         sync/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-ignore_xref([code_change/3, handle_call/3, handle_cast/2, handle_info/2, init/1,
              start_link/0, terminate/2]).

-compile({inline, [srv_name/0, tbl_name/0]}).

-include("mongoose.hrl").
-include("jlib.hrl").

-record(state, {}).

%% @private
srv_name() ->
    ejabberd_mod_muc_iq.

tbl_name() ->
    ejabberd_mod_muc_iq_table.

%%====================================================================
%% API
%%====================================================================

-spec start_link() -> 'ignore' | {'error', _} | {'ok', pid()}.
start_link() ->
    gen_server:start_link({local, srv_name()}, ?MODULE, [], []).

%% @doc Handle custom IQ.
%% Called from mod_muc_room.
-spec process_iq(jid:lserver(), jid:jid(), jid:jid(), mongoose_acc:t(),
        jlib:iq()) -> mongoose_acc:t() | {mongoose_acc:t(), error}.
process_iq(MucHost, From, RoomJID, Acc, IQ = #iq{xmlns = XMLNS}) ->
    case ets:lookup(tbl_name(), {XMLNS, MucHost}) of
        [{_, IQHandler}] ->
            gen_iq_component:handle(IQHandler, Acc, From, RoomJID, IQ);
        [] -> {Acc, error}
    end.

-spec register_iq_handler(jid:lserver(), binary(), mongoose_iq_handler:t()) -> ok.
register_iq_handler(MucHost, XMLNS, IQHandler) ->
    gen_server:cast(srv_name(),
                    {register_iq_handler, MucHost, XMLNS, IQHandler}).

-spec unregister_iq_handler(jid:lserver(), binary()) -> ok.
unregister_iq_handler(MucHost, XMLNS) ->
    gen_server:cast(srv_name(),
                    {unregister_iq_handler, MucHost, XMLNS}).

-spec sync() -> ok.
sync() ->
    gen_server:call(srv_name(), sync).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    ets:new(tbl_name(), [named_table, protected]),
    {ok, #state{}}.

handle_call(sync, _From, State) ->
    {reply, ok, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({register_iq_handler, MucHost, XMLNS, IQHandler}, State) ->
    ets:insert(tbl_name(), {{XMLNS, MucHost}, IQHandler}),
    {noreply, State};
handle_cast({unregister_iq_handler, MucHost, XMLNS}, State) ->
    case ets:lookup(tbl_name(), {XMLNS, MucHost}) of
        [{_, IQHandler}] ->
            gen_iq_component:stop_iq_handler(IQHandler),
            ets:delete(tbl_name(), {XMLNS, MucHost});
        _ ->
            ok
    end,
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
