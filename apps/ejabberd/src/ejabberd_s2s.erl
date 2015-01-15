%%%----------------------------------------------------------------------
%%% File    : ejabberd_s2s.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : S2S connections manager
%%% Created :  7 Dec 2002 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2011   ProcessOne
%%%
%%% This program is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation; either version 2 of the
%%% License, or (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with this program; if not, write to the Free Software
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
%%% 02111-1307 USA
%%%
%%%----------------------------------------------------------------------

-module(ejabberd_s2s).
-author('alexey@process-one.net').

-behaviour(gen_server).

%% API
-export([start_link/0,
         route/3,
         have_connection/1,
         has_key/2,
         get_connections_pids/1,
         try_register/1,
         remove_connection/3,
         find_connection/2,
         dirty_get_connections/0,
         allow_host/2,
         incoming_s2s_number/0,
         outgoing_s2s_number/0,
         domain_utf8_to_ascii/1
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
%% ejabberd API
-export([get_info_s2s_connections/1]).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("ejabberd_commands.hrl").

-define(DEFAULT_MAX_S2S_CONNECTIONS_NUMBER, 1).
-define(DEFAULT_MAX_S2S_CONNECTIONS_NUMBER_PER_NODE, 1).

-type fromto() :: {'global' | ejabberd:server(), ejabberd:server()}.
-record(s2s, {fromto :: fromto(),
              pid :: pid(),
              key
             }).
-type s2s() :: #s2s{}.
-record(state, {}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Description: Starts the server
%%--------------------------------------------------------------------
-spec start_link() -> 'ignore' | {'error',_} | {'ok',pid()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec route(From :: ejabberd:jid(),
            To :: ejabberd:jid(),
            Packet :: jlib:xmlel()) -> 'ok' | {'error','lager_not_running'}.
route(From, To, Packet) ->
    case catch do_route(From, To, Packet) of
        {'EXIT', Reason} ->
            ?ERROR_MSG("~p~nwhen processing: ~p",
                       [Reason, {From, To, Packet}]);
        _ ->
            ok
    end.

-spec remove_connection(_, pid(), _) -> 'ok' | {'aborted',_} | {'atomic',_}.
remove_connection(FromTo, Pid, Key) ->
    case catch mnesia:dirty_match_object(s2s, #s2s{fromto = FromTo,
                                                   pid = Pid,
                                                   _ = '_'}) of
        [#s2s{pid = Pid, key = Key}] ->
            F = fun() ->
                        mnesia:delete_object(#s2s{fromto = FromTo,
                                                  pid = Pid,
                                                  key = Key})
                end,
            mnesia:transaction(F);
        _ ->
            ok
    end.

have_connection(FromTo) ->
    case catch mnesia:dirty_read(s2s, FromTo) of
        [_] ->
            true;
        _ ->
            false
    end.

has_key(FromTo, Key) ->
    case mnesia:dirty_select(s2s,
                             [{#s2s{fromto = FromTo, key = Key, _ = '_'},
                               [],
                               ['$_']}]) of
        [] ->
            false;
        _ ->
            true
    end.

-spec get_connections_pids(_) -> ['undefined' | pid()].
get_connections_pids(FromTo) ->
    case catch mnesia:dirty_read(s2s, FromTo) of
        L when is_list(L) ->
            [Connection#s2s.pid || Connection <- L];
        _ ->
            []
    end.

-spec try_register(fromto()) -> any().
try_register(FromTo) ->
    Key = list_to_binary(randoms:get_string()),
    MaxS2SConnectionsNumber = max_s2s_connections_number(FromTo),
    MaxS2SConnectionsNumberPerNode =
        max_s2s_connections_number_per_node(FromTo),
    F = fun() ->
                L = mnesia:read({s2s, FromTo}),
                NeededConnections = needed_connections_number(
                                      L, MaxS2SConnectionsNumber,
                                      MaxS2SConnectionsNumberPerNode),
                if
                    NeededConnections > 0 ->
                        mnesia:write(#s2s{fromto = FromTo,
                                          pid = self(),
                                          key = Key}),
                        {key, Key};
                    true ->
                        false
                end
        end,
    case mnesia:transaction(F) of
        {atomic, Res} ->
            Res;
        _ ->
            false
    end.

dirty_get_connections() ->
    mnesia:dirty_all_keys(s2s).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
    update_tables(),
    mnesia:create_table(s2s, [{ram_copies, [node()]}, {type, bag},
                              {attributes, record_info(fields, s2s)}]),
    mnesia:add_table_copy(s2s, node(), ram_copies),
    mnesia:subscribe(system),
    ejabberd_commands:register_commands(commands()),
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({mnesia_system_event, {mnesia_down, Node}}, State) ->
    clean_table_from_bad_node(Node),
    {noreply, State};
handle_info({route, From, To, Packet}, State) ->
    case catch do_route(From, To, Packet) of
        {'EXIT', Reason} ->
            ?ERROR_MSG("~p~nwhen processing: ~p",
                       [Reason, {From, To, Packet}]);
        _ ->
            ok
    end,
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ejabberd_commands:unregister_commands(commands()),
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
clean_table_from_bad_node(Node) ->
    F = fun() ->
                Es = mnesia:select(
                       s2s,
                       [{#s2s{pid = '$1', _ = '_'},
                         [{'==', {node, '$1'}, Node}],
                         ['$_']}]),
                lists:foreach(fun(E) ->
                                      mnesia:delete_object(E)
                              end, Es)
        end,
    mnesia:async_dirty(F).

-spec do_route(From :: ejabberd:jid(),
               To :: ejabberd:jid(),
               Packet :: jlib:xmlel()) -> 'false' | 'ok'.
do_route(From, To, Packet) ->
    ?DEBUG("s2s manager~n\tfrom ~p~n\tto ~p~n\tpacket ~P~n",
           [From, To, Packet, 8]),
    case find_connection(From, To) of
        {atomic, Pid} when is_pid(Pid) ->
            ?DEBUG("sending to process ~p~n", [Pid]),
            #xmlel{attrs = Attrs} = Packet,
            NewAttrs = jlib:replace_from_to_attrs(jlib:jid_to_binary(From),
                                                  jlib:jid_to_binary(To),
                                                  Attrs),
            #jid{lserver = MyServer} = From,
            ejabberd_hooks:run(
              s2s_send_packet,
              MyServer,
              [From, To, Packet]),
            send_element(Pid, Packet#xmlel{attrs = NewAttrs}),
            ok;
        {aborted, _Reason} ->
            case xml:get_tag_attr_s(<<"type">>, Packet) of
                <<"error">> -> ok;
                <<"result">> -> ok;
                _ ->
                    Err = jlib:make_error_reply(
                            Packet, ?ERR_SERVICE_UNAVAILABLE),
                    ejabberd_router:route(To, From, Err)
            end,
            false
    end.

-spec find_connection(From :: ejabberd:jid(),
                      To :: ejabberd:jid()) -> {'aborted',_} | {'atomic',_}.
find_connection(From, To) ->
    #jid{lserver = MyServer} = From,
    #jid{lserver = Server} = To,
    FromTo = {MyServer, Server},
    MaxS2SConnectionsNumber = max_s2s_connections_number(FromTo),
    MaxS2SConnectionsNumberPerNode =
        max_s2s_connections_number_per_node(FromTo),
    ?DEBUG("Finding connection for ~p~n", [FromTo]),
    case catch mnesia:dirty_read(s2s, FromTo) of
        {'EXIT', Reason} ->
            {aborted, Reason};
        [] ->
            %% We try to establish all the connections if the host is not a
            %% service and if the s2s host is not blacklisted or
            %% is in whitelist:
            case not is_service(From, To) andalso allow_host(MyServer, Server) of
                true ->
                    NeededConnections = needed_connections_number(
                                          [], MaxS2SConnectionsNumber,
                                          MaxS2SConnectionsNumberPerNode),
                    open_several_connections(
                      NeededConnections, MyServer,
                      Server, From, FromTo,
                      MaxS2SConnectionsNumber, MaxS2SConnectionsNumberPerNode);
                false ->
                    {aborted, error}
            end;
        L when is_list(L) ->
            NeededConnections = needed_connections_number(
                                  L, MaxS2SConnectionsNumber,
                                  MaxS2SConnectionsNumberPerNode),
            if
                NeededConnections > 0 ->
                    %% We establish the missing connections for this pair.
                    open_several_connections(
                      NeededConnections, MyServer,
                      Server, From, FromTo,
                      MaxS2SConnectionsNumber, MaxS2SConnectionsNumberPerNode);
                true ->
                    %% We choose a connexion from the pool of opened ones.
                    {atomic, choose_connection(From, L)}
            end
    end.

-spec choose_connection(From :: ejabberd:jid(),
                        Connections :: [s2s()]) -> any().
choose_connection(From, Connections) ->
    choose_pid(From, [C#s2s.pid || C <- Connections]).

-spec choose_pid(From :: ejabberd:jid(), Pids :: [pid()]) -> pid().
choose_pid(From, Pids) ->
    Pids1 = case [P || P <- Pids, node(P) == node()] of
                [] -> Pids;
                Ps -> Ps
            end,
    % Use sticky connections based on the JID of the sender (whithout
    % the resource to ensure that a muc room always uses the same
    % connection)
    Pid = lists:nth(erlang:phash(jlib:jid_remove_resource(From), length(Pids1)),
                    Pids1),
    ?DEBUG("Using ejabberd_s2s_out ~p~n", [Pid]),
    Pid.

-spec open_several_connections(N :: pos_integer(), MyServer :: ejabberd:server(),
    Server :: ejabberd:server(), From :: ejabberd:jid(), FromTo :: fromto(),
    MaxS2S :: pos_integer(), MaxS2SPerNode :: pos_integer())
      -> {'aborted',_} | {'atomic',_}.
open_several_connections(N, MyServer, Server, From, FromTo,
                         MaxS2SConnectionsNumber,
                         MaxS2SConnectionsNumberPerNode) ->
    ConnectionsResult =
        [new_connection(MyServer, Server, From, FromTo,
                        MaxS2SConnectionsNumber, MaxS2SConnectionsNumberPerNode)
         || _N <- lists:seq(1, N)],
    case [PID || {atomic, PID} <- ConnectionsResult] of
        [] ->
            hd(ConnectionsResult);
        PIDs ->
            {atomic, choose_pid(From, PIDs)}
    end.

-spec new_connection(MyServer :: ejabberd:server(), Server :: ejabberd:server(),
    From :: ejabberd:jid(), FromTo :: fromto(), MaxS2S :: pos_integer(),
    MaxS2SPerNode :: pos_integer()) -> {'aborted',_} | {'atomic',_}.
new_connection(MyServer, Server, From, FromTo,
               MaxS2SConnectionsNumber, MaxS2SConnectionsNumberPerNode) ->
    Key = list_to_binary(randoms:get_string()),
    {ok, Pid} = ejabberd_s2s_out:start(
                  MyServer, Server, {new, Key}),
    F = fun() ->
                L = mnesia:read({s2s, FromTo}),
                NeededConnections = needed_connections_number(
                                      L, MaxS2SConnectionsNumber,
                                      MaxS2SConnectionsNumberPerNode),
                if
                    NeededConnections > 0 ->
                        mnesia:write(#s2s{fromto = FromTo,
                                          pid = Pid,
                                          key = Key}),
                        ?INFO_MSG("New s2s connection started ~p", [Pid]),
                        Pid;
                    true ->
                        choose_connection(From, L)
                end
        end,
    TRes = mnesia:transaction(F),
    case TRes of
        {atomic, Pid} ->
            ejabberd_s2s_out:start_connection(Pid);
        _ ->
            ejabberd_s2s_out:stop_connection(Pid)
    end,
    TRes.

-spec max_s2s_connections_number(fromto()) -> pos_integer().
max_s2s_connections_number({From, To}) ->
    case acl:match_rule(
           From, max_s2s_connections, jlib:make_jid(<<"">>, To, <<"">>)) of
        Max when is_integer(Max) -> Max;
        _ -> ?DEFAULT_MAX_S2S_CONNECTIONS_NUMBER
    end.

-spec max_s2s_connections_number_per_node(fromto()) -> pos_integer().
max_s2s_connections_number_per_node({From, To}) ->
    case acl:match_rule(
           From, max_s2s_connections_per_node, jlib:make_jid(<<"">>, To, <<"">>)) of
        Max when is_integer(Max) -> Max;
        _ -> ?DEFAULT_MAX_S2S_CONNECTIONS_NUMBER_PER_NODE
    end.

-spec needed_connections_number([any()], pos_integer(), pos_integer()) -> integer().
needed_connections_number(Ls, MaxS2SConnectionsNumber,
                          MaxS2SConnectionsNumberPerNode) ->
    LocalLs = [L || L <- Ls, node(L#s2s.pid) == node()],
    lists:min([MaxS2SConnectionsNumber - length(Ls),
               MaxS2SConnectionsNumberPerNode - length(LocalLs)]).

%%--------------------------------------------------------------------
%% Function: is_service(From, To) -> true | false
%% Description: Return true if the destination must be considered as a
%% service.
%% --------------------------------------------------------------------
-spec is_service(_, _) -> boolean().
is_service(From, To) ->
    LFromDomain = From#jid.lserver,
    case ejabberd_config:get_local_option({route_subdomains, LFromDomain}) of
        s2s -> % bypass RFC 3920 10.3
            false;
        _ ->
            Hosts = ?MYHOSTS,
            P = fun(ParentDomain) -> lists:member(ParentDomain, Hosts) end,
            lists:any(P, parent_domains(To#jid.lserver))
    end.

-spec parent_domains(ejabberd:server()) -> [any(),...].
parent_domains(Domain) ->
    parent_domains(Domain, [Domain]).

-spec parent_domains(binary(),[any(),...]) -> [any(),...].
parent_domains(<<>>, Acc) ->
    lists:reverse(Acc);
parent_domains(<<$., Rest/binary>>, Acc) ->
    parent_domains(Rest, [Rest | Acc]);
parent_domains(<<_, Rest/binary>>, Acc) ->
    parent_domains(Rest, Acc).

-spec send_element(pid(), jlib:xmlel()) -> {'send_element', jlib:xmlel()}.
send_element(Pid, El) ->
    Pid ! {send_element, El}.

%%--------------------------------------------------------------------
%% Function: domain_utf8_to_ascii(Domain) -> binary() | false
%% Description: Converts a UTF-8 domain to ASCII (IDNA)
%% --------------------------------------------------------------------
-spec domain_utf8_to_ascii(binary() | string()) -> binary() | false.
domain_utf8_to_ascii(Domain) ->
    case catch idna:utf8_to_ascii(Domain) of
        {'EXIT', _} ->
            false;
        AsciiDomain ->
            list_to_binary(AsciiDomain)
    end.

%%%----------------------------------------------------------------------
%%% ejabberd commands

-spec commands() -> [ejabberd_commands:cmd(),...].
commands() ->
    [
     #ejabberd_commands{name = incoming_s2s_number,
                       tags = [stats, s2s],
                       desc = "Number of incoming s2s connections on the node",
                       module = ?MODULE, function = incoming_s2s_number,
                       args = [],
                       result = {s2s_incoming, integer}},
     #ejabberd_commands{name = outgoing_s2s_number,
                       tags = [stats, s2s],
                       desc = "Number of outgoing s2s connections on the node",
                       module = ?MODULE, function = outgoing_s2s_number,
                       args = [],
                       result = {s2s_outgoing, integer}}
    ].

-spec incoming_s2s_number() -> non_neg_integer().
incoming_s2s_number() ->
    length(supervisor:which_children(ejabberd_s2s_in_sup)).

-spec outgoing_s2s_number() -> non_neg_integer().
outgoing_s2s_number() ->
    length(supervisor:which_children(ejabberd_s2s_out_sup)).


%%%----------------------------------------------------------------------
%%% Update Mnesia tables

update_tables() ->
    case catch mnesia:table_info(s2s, type) of
        bag ->
            ok;
        {'EXIT', _} ->
            ok;
        _ ->
            % XXX TODO convert it ?
            mnesia:delete_table(s2s)
    end,
    case catch mnesia:table_info(s2s, attributes) of
        [fromto, node, key] ->
            mnesia:transform_table(s2s, ignore, [fromto, pid, key]),
            mnesia:clear_table(s2s);
        [fromto, pid, key] ->
            ok;
        {'EXIT', _} ->
            ok
    end,
    case lists:member(local_s2s, mnesia:system_info(tables)) of
        true ->
            mnesia:delete_table(local_s2s);
        false ->
            ok
    end.

%% Check if host is in blacklist or white list
allow_host(MyServer, S2SHost) ->
    Hosts = ?MYHOSTS,
    case lists:dropwhile(
           fun(ParentDomain) ->
                   not lists:member(ParentDomain, Hosts)
           end, parent_domains(MyServer)) of
        [MyHost|_] ->
            allow_host1(MyHost, S2SHost);
        [] ->
            allow_host1(MyServer, S2SHost)
    end.

allow_host1(MyHost, S2SHost) ->
    case ejabberd_config:get_local_option({{s2s_host, S2SHost}, MyHost}) of
        deny -> false;
        allow -> true;
        _ ->
            case ejabberd_config:get_local_option({s2s_default_policy, MyHost}) of
                deny -> false;
                _ ->
                    case ejabberd_hooks:run_fold(s2s_allow_host, MyHost,
                                                 allow, [MyHost, S2SHost]) of
                        deny -> false;
                        allow -> true;
                        _ -> true
                    end
            end
    end.

%% @doc Get information about S2S connections of the specified type.
-spec get_info_s2s_connections('in' | 'out') -> [{atom(), any()},...].
get_info_s2s_connections(Type) ->
    ChildType = case Type of
                    in -> ejabberd_s2s_in_sup;
                    out -> ejabberd_s2s_out_sup
                end,
    Connections = supervisor:which_children(ChildType),
    get_s2s_info(Connections,Type).

-type connstate() :: 'restarting' | 'undefined' | pid().
-type conn() :: { any(), connstate(), 'supervisor' | 'worker', 'dynamic' | [_] }.
-spec get_s2s_info(Connections :: [conn()],
                  Type :: 'in' | 'out'
                  ) -> [[{any(), any()},...]]. % list of lists
get_s2s_info(Connections,Type)->
    complete_s2s_info(Connections,Type,[]).

-spec complete_s2s_info(Connections :: [conn()],
                        Type :: 'in' | 'out',
                        Result :: [[{any(), any()},...]] % list of lists
                        ) -> [[{any(), any()},...]]. % list of lists
complete_s2s_info([],_,Result)->
    Result;
complete_s2s_info([Connection|T],Type,Result)->
    {_,PID,_,_}=Connection,
    State = get_s2s_state(PID),
    complete_s2s_info(T,Type,[State|Result]).

-spec get_s2s_state(connstate()) -> [{atom(), any()},...].
get_s2s_state(S2sPid)->
    Infos = case gen_fsm:sync_send_all_state_event(S2sPid,get_state_infos) of
                {state_infos, Is} -> [{status, open} | Is];
                {noproc,_} -> [{status, closed}]; %% Connection closed
                {badrpc,_} -> [{status, error}]
            end,
    [{s2s_pid, S2sPid} | Infos].
