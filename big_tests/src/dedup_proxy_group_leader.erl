%% This code removes duplicate io:formats.
%% Each io:format would be written only once with this group leader.
-module(dedup_proxy_group_leader).
-export([start_proxy_group_leader_for/1]).

start_proxy_group_leader_for(ForPid) ->
    OldGroupLeader = get_process_group_leader(ForPid),
    ProxyGroupLeader = start_proxy_group_leader(OldGroupLeader),
    erlang:group_leader(ProxyGroupLeader, ForPid),
    ok.

%% Returns group_leader of a process
-spec get_process_group_leader(pid()) -> pid().
get_process_group_leader(Pid) ->
    {group_leader, GroupLeader} = erlang:process_info(Pid, group_leader),
    GroupLeader.

%% Starts a new process, that would filter out io:format duplicates.
%% It would forward all other messages to OldGroupLeader.
-spec start_proxy_group_leader(pid()) -> pid().
start_proxy_group_leader(OldGroupLeader) ->
    State = #{old_group_leader => OldGroupLeader, prev_requests => []},
    spawn_link(fun() -> proxy_group_leader_loop(State) end).

proxy_group_leader_loop(State) ->
    receive
        Msg ->
            State2 = handle_proxy_group_leader_message(Msg, State),
            proxy_group_leader_loop(State2)
    end.

handle_proxy_group_leader_message(Msg, State = #{old_group_leader := OldGroupLeader}) ->
    {State2, Action} = filter_io_request(Msg, State),
    case Action of
        drop ->
            reply_ok(Msg);
        pass ->
            %% OldGroupLeader would handle Msg and reply to the client directly
            OldGroupLeader ! Msg
    end,
    State2.

filter_io_request(Msg, State = #{prev_requests := PrevRequests}) ->
    case request_type(Msg) of
        put_chars ->
            Request = unwrap_io_request(Msg),
            case lists:member(Request, PrevRequests) of
                true ->
                    %% Filter out io:format duplicate call
                    {State, drop};
                false ->
                    %% Allow to execute this request just once and remember it
                    State2 = State#{prev_requests => [Request|PrevRequests]},
                    {State2, pass}
            end;
        _ ->
            %% unknown or non-relevant message
            {State, pass}
    end.

%% Erlang IO-protocol docs
%% http://erlang.org/doc/apps/stdlib/io_protocol.html
request_type({io_request, _From, _ReplyAs, Request}) when is_tuple(Request) ->
    element(1, Request);
request_type(_) ->
    unknown.

unwrap_io_request({io_request, _From, _ReplyAs, Request}) ->
    Request.

%% Simulate reply to the client.
reply_ok({io_request, From, ReplyAs, _Request}) ->
    From ! {io_reply, ReplyAs, ok}.
